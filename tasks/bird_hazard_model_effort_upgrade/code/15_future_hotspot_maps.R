#!/usr/bin/env Rscript
# 15_future_hotspot_maps.R
# 未来预测的中国省级+栅格空间热点分布地图
# Provincial + grid-level spatial hotspot maps for future predictions

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(RColorBrewer)
  library(here)
})

sf_use_s2(FALSE)

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

theme_map <- theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0),
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "grey70", linewidth = 0.3),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold")
  )

# ── 1. 读取数据 ──────────────────────────────────────────────────────────
cat("=== 未来预测空间热点地图 ===\n")

# 省级边界
china_prov <- st_read(
  "/Users/dingchenchen/Documents/SDMs/GS(2023)2767审图号/省面.shp",
  quiet = TRUE)
# 省界线
china_border <- st_read(
  "/Users/dingchenchen/Documents/SDMs/GS(2023)2767审图号/省界线.shp",
  quiet = TRUE)
# 国界
china_national <- st_read(
  "/Users/dingchenchen/Documents/SDMs/GS(2023)2767审图号/国界.shp",
  quiet = TRUE)

# XGBoost未来预测
xgb_future <- fread(file.path(TASK_ROOT, "results",
  "table_xgboost_future_predictions.csv"))
# RF未来预测
rf_future <- fread(file.path(TASK_ROOT, "results",
  "table_rf_future_predictions.csv"))

cat("XGBoost预测:", nrow(xgb_future), "行\n")
cat("RF预测:", nrow(rf_future), "行\n")

# 省级名称标准化
prov_names <- sort(unique(xgb_future$province))
cat("省份数:", length(prov_names), "\n")

# ── 2. 匹配省份名 ──────────────────────────────────────────────────────
cat("\n=== 匹配省份名 ===\n")

# china_prov的省份名
cat("地图省份名样本:", paste(head(unique(china_prov$省), 5),
                          collapse = ", "), "\n")

# 建立映射
prov_map_manual <- c(
  "北京市" = "Beijing", "天津市" = "Tianjin", "河北省" = "Hebei",
  "山西省" = "Shanxi", "内蒙古自治区" = "Inner Mongolia",
  "辽宁省" = "Liaoning", "吉林省" = "Jilin", "黑龙江省" = "Heilongjiang",
  "上海市" = "Shanghai", "江苏省" = "Jiangsu", "浙江省" = "Zhejiang",
  "安徽省" = "Anhui", "福建省" = "Fujian", "江西省" = "Jiangxi",
  "山东省" = "Shandong", "河南省" = "Henan", "湖北省" = "Hubei",
  "湖南省" = "Hunan", "广东省" = "Guangdong",
  "广西壮族自治区" = "Guangxi", "海南省" = "Hainan",
  "重庆市" = "Chongqing", "四川省" = "Sichuan",
  "贵州省" = "Guizhou", "云南省" = "Yunnan",
  "西藏自治区" = "Tibet", "陕西省" = "Shaanxi",
  "甘肃省" = "Gansu", "青海省" = "Qinghai",
  "宁夏回族自治区" = "Ningxia",
  "新疆维吾尔自治区" = "Xinjiang", "台湾省" = "Taiwan",
  "香港特别行政区" = "Hong Kong",
  "澳门特别行政区" = "Macau"
)

china_prov$province_en <- prov_map_manual[as.character(china_prov$省)]

# ── 3. 省级热点地图 ──────────────────────────────────────────────────────
cat("\n=== 省级热点地图 ===\n")

# XGBoost 2050 SSP585
xgb_2050_ssp585 <- xgb_future[year == 2050 &
  climate_scenario == "ssp585" & effort_scenario == "trend",
  .(province, hazard_mean)]

# 合并到地图
map_xgb_2050 <- merge(china_prov, xgb_2050_ssp585,
  by.x = "province_en", by.y = "province", all.x = TRUE)

p_xgb_2050 <- ggplot(map_xgb_2050) +
  geom_sf(aes(fill = hazard_mean), color = "grey60", linewidth = 0.2) +
  geom_sf(data = china_national, color = "grey30", linewidth = 0.3) +
  scale_fill_viridis_c(option = "C", direction = -1,
    name = "Predicted\nhazard",
    na.value = "grey90") +
  coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
  labs(title = "2050 new-record hotspot (XGBoost)",
       subtitle = "SSP585 + trend effort growth") +
  theme_map

ggsave(file.path(TASK_ROOT, "figures",
  "fig_map_xgb_2050_ssp585.png"),
  p_xgb_2050, width = 9, height = 8, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_map_xgb_2050_ssp585.pdf"),
  p_xgb_2050, width = 9, height = 8)

# 多情景对比图：SSP245 vs SSP585 × 2030/2040/2050
scenarios_to_plot <- CJ(
  year = c(2030, 2040, 2050),
  climate_scenario = c("ssp245", "ssp585")
)

map_list <- list()
for (i in seq_len(nrow(scenarios_to_plot))) {
  yr <- scenarios_to_plot$year[i]
  sc <- scenarios_to_plot$climate_scenario[i]

  dt_sub <- xgb_future[year == yr & climate_scenario == sc &
    effort_scenario == "trend", .(province, hazard_mean)]

  map_sub <- merge(china_prov, dt_sub,
    by.x = "province_en", by.y = "province", all.x = TRUE)

  map_list[[i]] <- ggplot(map_sub) +
    geom_sf(aes(fill = hazard_mean), color = "grey60", linewidth = 0.15) +
    geom_sf(data = china_national, color = "grey30", linewidth = 0.2) +
    scale_fill_viridis_c(option = "C", direction = -1,
      name = "Hazard",
      limits = c(0, max(xgb_future$hazard_mean, na.rm = TRUE)),
      na.value = "grey90") +
    coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
    labs(title = sprintf("%s %s", yr, toupper(sc))) +
    theme_map +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "bold"))
}

# 2x3 grid: 2 scenarios x 3 time periods
p_multi_scenario <- (map_list[[1]] | map_list[[2]]) /
                    (map_list[[3]] | map_list[[4]]) /
                    (map_list[[5]] | map_list[[6]]) +
  plot_annotation(
    title = "Future provincial hotspot maps (XGBoost, trend effort)",
    subtitle = "Left: SSP2-4.5, Right: SSP5-8.5",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave(file.path(TASK_ROOT, "figures",
  "fig_map_multi_scenario.png"),
  p_multi_scenario, width = 12, height = 16, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_map_multi_scenario.pdf"),
  p_multi_scenario, width = 12, height = 16)

# ── 4. XGBoost vs RF对比地图 ────────────────────────────────────────────
cat("\n=== XGBoost vs RF对比地图 ===\n")

# 2050 SSP585
rf_2050_ssp585 <- rf_future[year == 2050 &
  climate_scenario == "ssp585" & effort_scenario == "trend",
  .(province, hazard_mean)]

map_rf_2050 <- merge(china_prov, rf_2050_ssp585,
  by.x = "province_en", by.y = "province", all.x = TRUE)

# 共同色标
hazard_max <- max(c(xgb_2050_ssp585$hazard_mean,
                    rf_2050_ssp585$hazard_mean), na.rm = TRUE)

p_xgb_map <- ggplot(map_xgb_2050) +
  geom_sf(aes(fill = hazard_mean), color = "grey60", linewidth = 0.15) +
  geom_sf(data = china_national, color = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(option = "C", direction = -1,
    name = "Hazard", limits = c(0, hazard_max),
    na.value = "grey90") +
  coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
  labs(title = "XGBoost") +
  theme_map

p_rf_map <- ggplot(map_rf_2050) +
  geom_sf(aes(fill = hazard_mean), color = "grey60", linewidth = 0.15) +
  geom_sf(data = china_national, color = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(option = "C", direction = -1,
    name = "Hazard", limits = c(0, hazard_max),
    na.value = "grey90") +
  coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
  labs(title = "Random Forest") +
  theme_map

p_compare_maps <- (p_xgb_map | p_rf_map) +
  plot_annotation(
    title = "2050 predicted hotspot: XGBoost vs Random Forest",
    subtitle = "SSP585 + trend effort growth",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave(file.path(TASK_ROOT, "figures",
  "fig_map_xgb_vs_rf_2050.png"),
  p_compare_maps, width = 14, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_map_xgb_vs_rf_2050.pdf"),
  p_compare_maps, width = 14, height = 7)

# ── 5. 努力情景敏感性地图 ──────────────────────────────────────────────
cat("\n=== 努力情景敏感性地图 ===\n")

effort_scenarios <- c("baseline", "trend", "doubled")
effort_labels <- c("Baseline", "Trend", "Doubled")

effort_map_list <- list()
for (i in seq_along(effort_scenarios)) {
  es <- effort_scenarios[i]
  el <- effort_labels[i]

  dt_sub <- xgb_future[year == 2050 &
    climate_scenario == "ssp585" & effort_scenario == es,
    .(province, hazard_mean)]

  map_sub <- merge(china_prov, dt_sub,
    by.x = "province_en", by.y = "province", all.x = TRUE)

  effort_map_list[[i]] <- ggplot(map_sub) +
    geom_sf(aes(fill = hazard_mean), color = "grey60", linewidth = 0.15) +
    geom_sf(data = china_national, color = "grey30", linewidth = 0.2) +
    scale_fill_viridis_c(option = "C", direction = -1,
      name = "Hazard",
      limits = c(0, hazard_max),
      na.value = "grey90") +
    coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
    labs(title = el) +
    theme_map +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "bold"))
}

p_effort_maps <- (effort_map_list[[1]] | effort_map_list[[2]] |
                   effort_map_list[[3]]) +
  plot_annotation(
    title = "2050 hotspot sensitivity to effort scenarios (SSP585)",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave(file.path(TASK_ROOT, "figures",
  "fig_map_effort_sensitivity.png"),
  p_effort_maps, width = 18, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_map_effort_sensitivity.pdf"),
  p_effort_maps, width = 18, height = 7)

cat("\n=== 15_future_hotspot_maps.R 完成 ===\n")
