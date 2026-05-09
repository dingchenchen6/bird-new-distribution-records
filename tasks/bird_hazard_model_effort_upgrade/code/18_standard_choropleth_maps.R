#!/usr/bin/env Rscript
# ============================================================
# Scientific question / 科学问题:
# How do future new-record hazards distribute spatially at
# province, prefecture, county, and grid levels?
# 未来新纪录风险在省/市/县/栅格尺度上的空间分布如何？
#
# Objective / 分析目标:
# Generate standard choropleth maps with admin boundaries,
# blue-to-red color scale, for both current model results
# and future predictions at all spatial scales.
# 生成标准choropleth地图：行政区划边界+蓝红配色+
# 多尺度当前结果和未来预测。
#
# Workflow / 分析流程:
# 1. Load admin boundary shapefiles (省/市/县)
# 2. Map model results and predictions to spatial units
# 3. Generate choropleth maps with proper boundaries
# 4. Grid-level polygon maps at 100km and 50km
# 5. Multi-panel comparison figures
#
# Main packages / 主要包: data.table, sf, ggplot2, patchwork
#
# Output directory / 输出路径:
#   tasks/bird_hazard_model_effort_upgrade/figures/
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(here)
})

sf_use_s2(FALSE)

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

# ── 配色：蓝→红 / Color palette: blue→red ────────────────

scale_hazard <- scale_fill_gradient2(
  low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
  midpoint = NULL,  # 动态设置 / Set dynamically
  name = "Predicted\nhazard",
  na.value = "grey95",
  limits = NULL
)

scale_hazard_viridis <- scale_fill_viridis_c(
  option = "C", direction = -1,
  name = "Predicted\nhazard",
  na.value = "grey95"
)

# 蓝白红配色 / Blue-White-Red palette
scale_bwr <- function(mid_val) {
  scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = mid_val,
    name = "Predicted\nhazard",
    na.value = "grey95"
  )
}

theme_map <- theme_bw(base_size = 11, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0),
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "grey60", linewidth = 0.3),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold")
  )

# China extent
XLIM <- c(73, 136)
YLIM <- c(17, 55)

# ═══════════════════════════════════════════════════════════
# 1. 读取行政区划 / Read admin boundaries
# ═══════════════════════════════════════════════════════════
cat("=== 读取行政区划 ===\n")

# GS(2019)1822号 官方矢量底图 / Official vector base map
SHP_BASE <- file.path(here::here("tasks"),
  "2019中国地图-审图号GS(2019)1822号")

china_prov <- st_read(file.path(SHP_BASE, "省（等积投影）.shp"),
  quiet = TRUE)
china_pref <- st_read(file.path(SHP_BASE, "市（等积投影）.shp"),
  quiet = TRUE)
china_county <- st_read(file.path(SHP_BASE, "县（等积投影）.shp"),
  quiet = TRUE)
china_national <- st_read(file.path(SHP_BASE, "国界.shp"),
  quiet = TRUE)
china_nine_dash <- st_read(file.path(SHP_BASE, "九段线.shp"),
  quiet = TRUE)
china_outline <- st_read(file.path(SHP_BASE, "中国轮廓线.shp"),
  quiet = TRUE)

# ── 转换为WGS84（关键修复：shapefile为Albers等积投影米制坐标） ──
# Transform all shapefiles to WGS84 (CRITICAL FIX: native CRS is Albers meters)
cat("Transforming shapefiles to WGS84...\n")
china_prov      <- st_transform(china_prov, 4326)
china_pref      <- st_transform(china_pref, 4326)
china_county    <- st_transform(china_county, 4326)
china_national  <- st_transform(china_national, 4326)
china_nine_dash <- st_transform(china_nine_dash, 4326)
china_outline   <- st_transform(china_outline, 4326)
cat(sprintf("  Province bbox (WGS84): %.1f-%.1f, %.1f-%.1f\n",
  st_bbox(china_prov)[[1]], st_bbox(china_prov)[[3]],
  st_bbox(china_prov)[[2]], st_bbox(china_prov)[[4]]))

# ── 过滤"中朝共有"（无预测数据对应） / Filter shared area ──
china_prov   <- china_prov[china_prov$省 != "中朝共有", ]
china_pref   <- china_pref[china_pref$省 != "中朝共有", ]
china_county <- china_county[china_county$NAME != "中朝共有", ]

# 省名中英映射 / Province name mapping
prov_map_cn2en <- c(
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

china_prov$province_en <- prov_map_cn2en[as.character(china_prov$省)]
china_pref$province_en <- prov_map_cn2en[as.character(china_pref$省)]

# 诊断：省名映射缺失 / Diagnose unmapped provinces
unmapped_prov <- china_prov$省[is.na(china_prov$province_en)]
if (length(unmapped_prov) > 0) {
  cat(sprintf("  WARNING: %d provinces unmapped: %s\n",
    length(unmapped_prov), paste(unique(unmapped_prov), collapse = ", ")))
}

# ═══════════════════════════════════════════════════════════
# 2. 读取预测数据 / Read prediction data
# ═══════════════════════════════════════════════════════════
cat("\n=== 读取预测数据 ===\n")

prov_future <- fread(file.path(TASK_ROOT, "results",
  "table_multi_scale_province_future.csv"))
pref_future <- fread(file.path(TASK_ROOT, "results",
  "table_multi_scale_prefecture_future.csv"))
county_future <- fread(file.path(TASK_ROOT, "results",
  "table_multi_scale_county_future.csv"))
grid_100km <- fread(file.path(TASK_ROOT, "results",
  "table_grid_100km_2050_ssp585_hazard.csv"))
grid_50km <- fread(file.path(TASK_ROOT, "results",
  "table_grid_50km_2050_ssp585_hazard.csv"))

# ═══════════════════════════════════════════════════════════
# 3. 省级choropleth地图 / Province choropleth
# ═══════════════════════════════════════════════════════════
cat("\n=== 省级choropleth地图 ===\n")

# 2050 SSP585 trend
prov_2050 <- prov_future[year == 2050 &
  climate_scenario == "ssp585" & effort_scenario == "trend",
  .(province, hazard_mean)]

map_prov <- merge(china_prov, prov_2050,
  by.x = "province_en", by.y = "province", all.x = TRUE)

hazard_mid <- mean(prov_2050$hazard_mean, na.rm = TRUE)

p_prov <- ggplot(map_prov) +
  geom_sf(aes(fill = hazard_mean), color = "grey50", linewidth = 0.3) +
  geom_sf(data = china_national, color = "black", linewidth = 0.4) +
  geom_sf(data = china_nine_dash, color = "grey40", linewidth = 0.3) +
  scale_bwr(hazard_mid) +
  coord_sf(xlim = XLIM, ylim = YLIM) +
  labs(title = "Province-level predicted hazard (2050, SSP5-8.5)",
    subtitle = "Trend effort growth | XGBoost prediction") +
  theme_map

ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_province_2050_ssp585.png"),
  p_prov, width = 9, height = 8, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_province_2050_ssp585.pdf"),
  p_prov, width = 9, height = 8)

# 多情景对比 / Multi-scenario comparison
for (cs in c("ssp245", "ssp585")) {
  for (yr in c(2030, 2040, 2050)) {
    dt_sub <- prov_future[year == yr &
      climate_scenario == cs & effort_scenario == "trend",
      .(province, hazard_mean)]

    map_sub <- merge(china_prov, dt_sub,
      by.x = "province_en", by.y = "province", all.x = TRUE)

    p_sub <- ggplot(map_sub) +
      geom_sf(aes(fill = hazard_mean), color = "grey50", linewidth = 0.25) +
      geom_sf(data = china_prov, color = "grey30", linewidth = 0.15) +
      geom_sf(data = china_national, color = "black", linewidth = 0.3) +
      scale_bwr(hazard_mid) +
      coord_sf(xlim = XLIM, ylim = YLIM) +
      labs(title = sprintf("%s %s", yr, toupper(cs))) +
      theme_map +
      theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))

    ggsave(file.path(TASK_ROOT, "figures",
      sprintf("fig_choropleth_prov_%s_%s.png", yr, cs)),
      p_sub, width = 7, height = 6, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures",
      sprintf("fig_choropleth_prov_%s_%s.pdf", yr, cs)),
      p_sub, width = 7, height = 6)
  }
}

# 2×3多情景拼图 / 2×3 multi-scenario panel
scenario_panels <- list()
for (yr in c(2030, 2040, 2050)) {
  for (cs in c("ssp245", "ssp585")) {
    dt_sub <- prov_future[year == yr &
      climate_scenario == cs & effort_scenario == "trend",
      .(province, hazard_mean)]

    map_sub <- merge(china_prov, dt_sub,
      by.x = "province_en", by.y = "province", all.x = TRUE)

    scenario_panels[[sprintf("%d_%s", yr, cs)]] <- ggplot(map_sub) +
      geom_sf(aes(fill = hazard_mean), color = "grey50", linewidth = 0.15) +
      geom_sf(data = china_prov, color = "grey30", linewidth = 0.1) +
      geom_sf(data = china_national, color = "black", linewidth = 0.25) +
      scale_bwr(hazard_mid) +
      coord_sf(xlim = XLIM, ylim = YLIM) +
      labs(title = sprintf("%s %s", yr, toupper(cs))) +
      theme_map +
      theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))
  }
}

p_multi_scenario <- (scenario_panels[[1]] | scenario_panels[[2]]) /
                    (scenario_panels[[3]] | scenario_panels[[4]]) /
                    (scenario_panels[[5]] | scenario_panels[[6]]) +
  plot_annotation(
    title = "Future provincial hazard: SSP2-4.5 vs SSP5-8.5",
    subtitle = "Left: SSP2-4.5, Right: SSP5-8.5 | Trend effort growth",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_province_multi_scenario.png"),
  p_multi_scenario, width = 14, height = 18, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_province_multi_scenario.pdf"),
  p_multi_scenario, width = 14, height = 18)
cat("省级choropleth地图完成\n")

# ═══════════════════════════════════════════════════════════
# 4. 市级choropleth地图 / Prefecture choropleth
# ═══════════════════════════════════════════════════════════
cat("\n=== 市级choropleth地图 ===\n")

pref_2050 <- pref_future[year == 2050 &
  climate_scenario == "ssp585" & effort_scenario == "trend",
  .(prefecture, hazard_mean)]

map_pref <- merge(china_pref, pref_2050,
  by.x = "市", by.y = "prefecture", all.x = TRUE)

p_pref <- ggplot(map_pref) +
  geom_sf(aes(fill = hazard_mean), color = "grey70", linewidth = 0.08) +
  geom_sf(data = china_prov, color = "grey30", linewidth = 0.2) +
  geom_sf(data = china_national, color = "black", linewidth = 0.3) +
  scale_bwr(hazard_mid) +
  coord_sf(xlim = XLIM, ylim = YLIM) +
  labs(title = "Prefecture-level predicted hazard (2050, SSP5-8.5)",
    subtitle = "Province-level boundary overlay | Trend effort") +
  theme_map

ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_prefecture_2050_ssp585.png"),
  p_pref, width = 9, height = 8, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_prefecture_2050_ssp585.pdf"),
  p_pref, width = 9, height = 8)
cat("市级choropleth地图完成\n")

# ═══════════════════════════════════════════════════════════
# 5. 县级choropleth地图 / County choropleth
# ═══════════════════════════════════════════════════════════
cat("\n=== 县级choropleth地图 ===\n")

county_2050 <- county_future[year == 2050 &
  climate_scenario == "ssp585" & effort_scenario == "trend",
  .(county, hazard_mean)]

map_county <- merge(china_county, county_2050,
  by.x = "NAME", by.y = "county", all.x = TRUE)

p_county <- ggplot(map_county) +
  geom_sf(aes(fill = hazard_mean), color = NA, linewidth = 0) +
  geom_sf(data = china_county, color = "grey80", linewidth = 0.05) +
  geom_sf(data = china_prov, color = "grey30", linewidth = 0.2) +
  geom_sf(data = china_national, color = "black", linewidth = 0.3) +
  scale_bwr(hazard_mid) +
  coord_sf(xlim = XLIM, ylim = YLIM) +
  labs(title = "County-level predicted hazard (2050, SSP5-8.5)",
    subtitle = "Province + county boundary overlay | Trend effort") +
  theme_map

ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_county_2050_ssp585.png"),
  p_county, width = 9, height = 8, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_county_2050_ssp585.pdf"),
  p_county, width = 9, height = 8)
cat("县级choropleth地图完成\n")

# ═══════════════════════════════════════════════════════════
# 6. 栅格多边形地图 / Grid polygon maps
# ═══════════════════════════════════════════════════════════
cat("\n=== 栅格多边形地图 ===\n")

# 将栅格点转为多边形 / Convert grid centroids to polygons
make_grid_polygons <- function(dt, res_km) {
  half <- res_km / 2 / 111  # 近似度数 / Approximate degrees
  polys <- lapply(seq_len(nrow(dt)), function(i) {
    lon <- dt$centroid_lon[i]
    lat <- dt$centroid_lat[i]
    if (is.na(lon) || is.na(lat)) return(NULL)
    # 经度修正 / Longitude correction
    half_lon <- half / cos(lat * pi / 180)
    st_polygon(list(matrix(c(
      lon - half_lon, lat - half,
      lon + half_lon, lat - half,
      lon + half_lon, lat + half,
      lon - half_lon, lat + half,
      lon - half_lon, lat - half
    ), ncol = 2, byrow = TRUE)))
  })
  valid <- !sapply(polys, is.null)
  st_sf(
    grid_id = dt$grid_id[valid],
    hazard = dt$hazard[valid],
    geometry = st_sfc(polys[valid], crs = 4326)
  )
}

for (res in c("100km", "50km")) {
  cat(sprintf("\n--- %s 栅格 ---\n", res))

  grid_dt <- if (res == "100km") grid_100km else grid_50km
  res_km <- as.numeric(gsub("km", "", res))

  grid_sf <- make_grid_polygons(grid_dt, res_km)
  cat(sprintf("生成 %d 个栅格多边形\n", nrow(grid_sf)))

  p_grid <- ggplot() +
    geom_sf(data = china_prov, fill = "grey95", color = "grey70",
      linewidth = 0.2) +
    geom_sf(data = grid_sf, aes(fill = hazard), color = "grey80",
      linewidth = 0.05, alpha = 0.85) +
    geom_sf(data = china_prov, color = "grey30", linewidth = 0.2) +
    geom_sf(data = china_national, color = "black", linewidth = 0.3) +
    scale_bwr(hazard_mid) +
    coord_sf(xlim = XLIM, ylim = YLIM) +
    labs(title = sprintf("%s grid predicted hazard (2050, SSP5-8.5)",
      res),
      subtitle = "Grid polygons with province boundary overlay") +
    theme_map

  ggsave(file.path(TASK_ROOT, "figures",
    sprintf("fig_choropleth_grid_%s_2050_ssp585.png", res)),
    p_grid, width = 9, height = 8, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    sprintf("fig_choropleth_grid_%s_2050_ssp585.pdf", res)),
    p_grid, width = 9, height = 8)
  cat(sprintf("%s栅格地图完成\n", res))
}

# ═══════════════════════════════════════════════════════════
# 7. 多尺度对比拼图 / Multi-scale comparison panel
# ═══════════════════════════════════════════════════════════
cat("\n=== 多尺度对比拼图 ===\n")

# 统一配色范围 / Unify color range
all_hazard <- c(prov_2050$hazard_mean, pref_2050$hazard_mean,
  county_2050$hazard_mean, grid_100km$hazard)
hazard_range <- range(all_hazard, na.rm = TRUE)
hazard_mid_unified <- mean(hazard_range)

# 省级 / Province
p1 <- ggplot(map_prov) +
  geom_sf(aes(fill = hazard_mean), color = "grey50", linewidth = 0.25) +
  geom_sf(data = china_prov, color = "grey30", linewidth = 0.15) +
  geom_sf(data = china_national, color = "black", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = hazard_mid_unified,
    limits = hazard_range, na.value = "grey95",
    name = "Hazard") +
  coord_sf(xlim = XLIM, ylim = YLIM) +
  labs(title = "(a) Province") +
  theme_map +
  theme(legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"))

# 市级 / Prefecture
p2 <- ggplot(map_pref) +
  geom_sf(aes(fill = hazard_mean), color = "grey70", linewidth = 0.06) +
  geom_sf(data = china_prov, color = "grey30", linewidth = 0.15) +
  geom_sf(data = china_national, color = "black", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = hazard_mid_unified,
    limits = hazard_range, na.value = "grey95",
    name = "Hazard") +
  coord_sf(xlim = XLIM, ylim = YLIM) +
  labs(title = "(b) Prefecture") +
  theme_map +
  theme(legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"))

# 县级 / County
p3 <- ggplot(map_county) +
  geom_sf(aes(fill = hazard_mean), color = NA, linewidth = 0) +
  geom_sf(data = china_prov, color = "grey30", linewidth = 0.15) +
  geom_sf(data = china_national, color = "black", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = hazard_mid_unified,
    limits = hazard_range, na.value = "grey95",
    name = "Hazard") +
  coord_sf(xlim = XLIM, ylim = YLIM) +
  labs(title = "(c) County") +
  theme_map +
  theme(legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"))

# 100km栅格 / 100km grid
grid_100_sf <- make_grid_polygons(grid_100km, 100)
p4 <- ggplot() +
  geom_sf(data = china_prov, fill = "grey95", color = "grey70",
    linewidth = 0.15) +
  geom_sf(data = grid_100_sf, aes(fill = hazard), color = "grey80",
    linewidth = 0.03, alpha = 0.85) +
  geom_sf(data = china_prov, color = "grey30", linewidth = 0.15) +
  geom_sf(data = china_national, color = "black", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = hazard_mid_unified,
    limits = hazard_range, na.value = "grey95",
    name = "Hazard") +
  coord_sf(xlim = XLIM, ylim = YLIM) +
  labs(title = "(d) 100-km grid") +
  theme_map +
  theme(legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"))

# 50km栅格 / 50km grid
grid_50_sf <- make_grid_polygons(grid_50km, 50)
p5 <- ggplot() +
  geom_sf(data = china_prov, fill = "grey95", color = "grey70",
    linewidth = 0.15) +
  geom_sf(data = grid_50_sf, aes(fill = hazard), color = "grey80",
    linewidth = 0.01, alpha = 0.85) +
  geom_sf(data = china_prov, color = "grey30", linewidth = 0.15) +
  geom_sf(data = china_national, color = "black", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
    midpoint = hazard_mid_unified,
    limits = hazard_range, na.value = "grey95",
    name = "Hazard") +
  coord_sf(xlim = XLIM, ylim = YLIM) +
  labs(title = "(e) 50-km grid") +
  theme_map +
  theme(legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"))

# 拼图 / Combine
p_combined <- (p1 | p2) / (p3 | p4) / p5 +
  plot_annotation(
    title = "2050 predicted hazard: multi-scale comparison (SSP5-8.5)",
    subtitle = "Blue→White→Red color scale | Trend effort growth",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_multi_scale_comparison.png"),
  p_combined, width = 16, height = 22, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_multi_scale_comparison.pdf"),
  p_combined, width = 16, height = 22)
cat("多尺度对比拼图完成\n")

# ═══════════════════════════════════════════════════════════
# 8. 努力情景敏感性地图 / Effort scenario sensitivity
# ═══════════════════════════════════════════════════════════
cat("\n=== 努力情景敏感性 ===\n")

effort_scenarios <- c("baseline", "trend", "doubled")
effort_labels <- c("Baseline", "Trend", "Doubled")

effort_panels <- list()
for (i in seq_along(effort_scenarios)) {
  es <- effort_scenarios[i]
  el <- effort_labels[i]

  dt_sub <- prov_future[year == 2050 &
    climate_scenario == "ssp585" & effort_scenario == es,
    .(province, hazard_mean)]

  map_sub <- merge(china_prov, dt_sub,
    by.x = "province_en", by.y = "province", all.x = TRUE)

  effort_panels[[i]] <- ggplot(map_sub) +
    geom_sf(aes(fill = hazard_mean), color = "grey50", linewidth = 0.2) +
    geom_sf(data = china_prov, color = "grey30", linewidth = 0.15) +
    geom_sf(data = china_national, color = "black", linewidth = 0.3) +
    scale_bwr(hazard_mid) +
    coord_sf(xlim = XLIM, ylim = YLIM) +
    labs(title = el) +
    theme_map +
    theme(legend.position = "none",
      plot.title = element_text(size = 10, face = "bold"))
}

p_effort <- (effort_panels[[1]] | effort_panels[[2]] | effort_panels[[3]]) +
  plot_annotation(
    title = "2050 effort scenario sensitivity (SSP5-8.5)",
    subtitle = "Left: Baseline, Middle: Trend, Right: Doubled effort",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_effort_sensitivity.png"),
  p_effort, width = 18, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_choropleth_effort_sensitivity.pdf"),
  p_effort, width = 18, height = 7)
cat("努力情景敏感性地图完成\n")

cat("\n=== 18_standard_choropleth_maps.R 完成 ===\n")
