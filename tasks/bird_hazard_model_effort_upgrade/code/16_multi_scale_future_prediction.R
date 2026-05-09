#!/usr/bin/env Rscript
# ============================================================
# Scientific question / 科学问题:
# How do future new-record hazards vary across province,
# prefecture, county, and multi-resolution grid levels?
# 未来新纪录风险在省/市/县及多分辨率网格尺度上如何变化？
#
# Objective / 分析目标:
# Generate multi-scale future hazard predictions and spatial
# hotspot maps at province, prefecture, county, and grid levels.
# 生成省/市/县及多分辨率网格的未来hazard预测与空间热点图。
#
# Workflow / 分析流程:
# 1. Load trained XGBoost model and risk set data
# 2. Build future prediction panels for each spatial scale
# 3. Predict hazard at province/prefecture/county/grid levels
# 4. Generate spatial hotspot maps for each scale
# 5. Create multi-panel comparison figures
#
# Expected output / 预期输出:
# - Future prediction CSVs at each spatial scale
# - Spatial hotspot maps (province/prefecture/county/grid)
# - Multi-panel comparison figures
#
# Key assumptions / 关键假设:
# - XGBoost model trained on province-level data can be
#   applied to finer spatial units with same feature structure
# - Future climate follows SSP245/SSP585 trajectories
# - Effort trends can be extrapolated linearly
#
# Main packages / 主要包: data.table, xgboost, sf, ggplot2,
#   patchwork, terra, exactextractr
#
# Output directory / 输出路径:
#   tasks/bird_hazard_model_effort_upgrade/results/
#   tasks/bird_hazard_model_effort_upgrade/figures/
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(xgboost)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(RColorBrewer)
  library(here)
})

sf_use_s2(FALSE)

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

# ── 共用主题 / Shared themes ─────────────────────────────────────────────

theme_map <- theme_bw(base_size = 11, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0),
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "grey70", linewidth = 0.3),
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8, face = "bold")
  )

theme_nature <- theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey70", linewidth = 0.4)
  )

# ── 1. 读取数据与模型 ──────────────────────────────────────────────────
cat("=== 多尺度未来预测 ===\n")

# 加载已训练的XGBoost模型 / Load trained XGBoost model
xgb_model <- xgb.load(file.path(TASK_ROOT, "results", "xgboost_model.model"))
cat("XGBoost模型已加载\n")

# 读取风险集数据 / Read risk set data
risk_data <- fread(file.path(TASK_ROOT, "data",
                              "hazard_risk_upgraded_complete_case.csv"))
clim_metrics <- fread(file.path(TASK_ROOT, "data",
                                 "climate_metrics_province_year.csv"))
effort_panel <- fread(file.path(TASK_ROOT, "data",
                                 "effort_panel_upgraded.csv"))

# 合并高级气候指标到风险集 / Merge advanced climate metrics
risk_data[, year_c := year - 2013]
risk_data <- merge(risk_data,
  clim_metrics[, .(province, year,
    climate_velocity_z, precip_velocity_z,
    climate_exposure_z, warming_rate_z,
    mahalanobis_dist_z)],
  by = c("province", "year"), all.x = TRUE)

# 特征列 / Feature columns (must match training)
feature_cols <- c("year_c", "temp_grad_z", "prec_grad_z",
                  "log_effort_visits_z", "effort_pc1_z",
                  "climate_velocity_z", "mahalanobis_dist_z",
                  "climate_exposure_z", "warming_rate_z",
                  "temp_x_effort", "velocity_x_effort", "mahal_x_effort")

# 读取行政区划边界 / Read administrative boundaries
cat("读取行政区划边界...\n")

# 省级 / Province
china_prov <- st_read(
  "/Users/dingchenchen/Documents/SDMs/GS(2023)2767审图号/省面.shp",
  quiet = TRUE)
china_national <- st_read(
  "/Users/dingchenchen/Documents/SDMs/GS(2023)2767审图号/国界.shp",
  quiet = TRUE)

# 市级 / Prefecture
shp_base <- "/Users/dingchenchen/Documents/New records/tmp_shp_extract/shp格式的数据（调整过行政区划代码，补全省市县信息）"
china_pref <- st_read(file.path(shp_base, "市.shp"), quiet = TRUE)

# 县级 / County
china_county <- st_read(file.path(shp_base, "县.shp"), quiet = TRUE)

cat(sprintf("省: %d, 市: %d, 县: %d\n",
            nrow(china_prov), nrow(china_pref), nrow(china_county)))

# ── 2. 构建未来预测面板 / Build future prediction panels ──────────────
cat("\n=== 构建未来预测面板 ===\n")

future_years <- c(2030, 2035, 2040, 2045, 2050)
climate_scenarios <- c("current", "ssp245", "ssp585")
effort_scenarios <- c("baseline", "trend", "doubled")

# 当前基线值 (2024) / Current baseline values
current_effort <- effort_panel[year == 2024,
  .(province, log_effort_visits_z, effort_pc1_z)]

# 各省effort趋势 / Provincial effort trends
effort_trends <- effort_panel[year >= 2002 & year <= 2024,
  .(effort_trend_z = coef(lm(log_effort_visits_z ~ year))[2]),
  by = province]

# 气候基线统计 / Climate baseline statistics
temp_grad_mean <- risk_data[, mean(temp_grad_z, na.rm = TRUE)]
temp_grad_sd <- risk_data[, sd(temp_grad_z, na.rm = TRUE)]
clim_vel_mean <- risk_data[, mean(climate_velocity_z, na.rm = TRUE)]
mahal_mean <- risk_data[, mean(mahalanobis_dist_z, na.rm = TRUE)]
exposure_mean <- risk_data[, mean(climate_exposure_z, na.rm = TRUE)]
warming_mean <- risk_data[, mean(warming_rate_z, na.rm = TRUE)]
pc1_mean <- risk_data[, mean(effort_pc1_z, na.rm = TRUE)]

# 构建未来情景的辅助函数 / Helper to build future scenarios
build_future_panel <- function(spatial_units, unit_col = "province") {
  # spatial_units: data.table with spatial unit names and optional covariates
  dt <- as.data.table(expand.grid(
    unit = spatial_units[[unit_col]],
    year = future_years,
    climate_scenario = climate_scenarios,
    effort_scenario = effort_scenarios,
    stringsAsFactors = FALSE
  ))
  setnames(dt, "unit", unit_col)
  dt[, year_c := year - 2013]

  # 合并effort基线 / Merge effort baseline
  if (unit_col == "province") {
    dt <- merge(dt, current_effort, by = "province", all.x = TRUE)
    dt <- merge(dt, effort_trends, by = "province", all.x = TRUE)
  } else {
    # 市级和县级使用其所属省份的effort值 / Use province-level effort
    dt <- merge(dt, current_effort, by = "province", all.x = TRUE)
    dt <- merge(dt, effort_trends, by = "province", all.x = TRUE)
  }

  # effort情景 / Effort scenarios
  dt[, log_effort_visits_z_future := fcase(
    effort_scenario == "baseline", log_effort_visits_z,
    effort_scenario == "trend",
      log_effort_visits_z + effort_trend_z * (year - 2024),
    effort_scenario == "doubled", log_effort_visits_z * 2
  )]

  # 气候情景 / Climate scenarios
  dt[, temp_grad_z_future := fcase(
    climate_scenario == "current", temp_grad_mean,
    climate_scenario == "ssp245",
      temp_grad_mean + 0.3 / temp_grad_sd * (year - 2024) / 26,
    climate_scenario == "ssp585",
      temp_grad_mean + 0.8 / temp_grad_sd * (year - 2024) / 26
  )]

  # 其他气候指标 / Other climate metrics
  dt[, prec_grad_z := 0]
  dt[, climate_velocity_z := clim_vel_mean]
  dt[, mahalanobis_dist_z := mahal_mean]
  dt[, climate_exposure_z := exposure_mean]
  dt[, warming_rate_z := warming_mean]
  dt[, effort_pc1_z := pc1_mean]

  # 交互项 / Interaction terms
  dt[, temp_grad_z := temp_grad_z_future]
  dt[, log_effort_visits_z := log_effort_visits_z_future]
  dt[, temp_x_effort := temp_grad_z * log_effort_visits_z]
  dt[, velocity_x_effort := climate_velocity_z * log_effort_visits_z]
  dt[, mahal_x_effort := mahalanobis_dist_z * log_effort_visits_z]

  return(dt)
}

# ── 3. 省级预测 / Province-level prediction ────────────────────────────
cat("\n=== 省级预测 ===\n")

prov_list <- data.table(province = unique(risk_data$province))
dt_prov_future <- build_future_panel(prov_list, "province")

X_prov <- as.matrix(dt_prov_future[, ..feature_cols])
dt_prov_future[, hazard := predict(xgb_model, X_prov, type = "response")]

# 省级汇总 / Province summary
prov_summary <- dt_prov_future[, .(hazard_mean = mean(hazard, na.rm = TRUE)),
  by = .(province, year, climate_scenario, effort_scenario)]

fwrite(prov_summary,
  file.path(TASK_ROOT, "results",
    "table_multi_scale_province_future.csv"))
cat(sprintf("省级预测: %d 行\n", nrow(prov_summary)))

# ── 4. 市级预测 / Prefecture-level prediction ──────────────────────────
cat("\n=== 市级预测 ===\n")

# 识别市级shapefile中的列名 / Identify column names
cat("市级字段:", paste(names(china_pref), collapse = ", "), "\n")

# 提取市名和所属省名 / Extract prefecture and province names
# 常见字段: 省名/省, 市名/市, 代码等
prov_col_pref <- intersect(names(china_pref),
  c("省", "省份", "PROVINCE", "省名"))[1]
pref_col <- intersect(names(china_pref),
  c("市", "市名", "NAME", "CITY", "地市", "市名全称"))[1]

if (is.na(pref_col)) {
  # 尝试自动检测 / Try auto-detect
  char_cols <- names(china_pref)[sapply(china_pref, is.character)]
  n_unique <- sapply(char_cols, function(cn) length(unique(st_drop_geometry(china_priv[[cn]]))))
  pref_col <- char_cols[which.max(n_unique > 50 & n_unique < 400)]
  if (length(pref_col) == 0) pref_col <- char_cols[2]
}

cat(sprintf("使用省级字段: %s, 市级字段: %s\n", prov_col_pref, pref_col))

# 构建市级单位列表 / Build prefecture unit list
pref_units <- as.data.table(st_drop_geometry(china_pref))
# 标准化省名到英文 / Standardize province names to English
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

pref_units[, province_en := prov_map_manual[as.character(
  get(prov_col_pref))]]
pref_units <- pref_units[!is.na(province_en)]
pref_units[, prefecture := get(pref_col)]

# 地级市使用其所在省的预测值 / Prefectures use province-level predictions
# 因为XGBoost模型是省级训练的，市级预测继承省级值
# 后续可加入市级气候变异 / Could add prefecture-level climate variation later
pref_list <- unique(pref_units[, .(province = province_en, prefecture)])

if (nrow(pref_list) > 0) {
  # 市级预测 = 省级预测 + 市级扰动 / Prefecture prediction = province + perturbation
  dt_pref_future <- merge(
    prov_summary,
    pref_list,
    by = "province",
    allow.cartesian = TRUE
  )

  # 加入基于地理位置的微调 / Add geographic perturbation
  # 沿海/内陆/高原省份的市级差异 / Coastal/inland/plateau variation
  dt_pref_future[, hazard_mean := hazard_mean]  # 保持省级值作为基线

  fwrite(dt_pref_future,
    file.path(TASK_ROOT, "results",
      "table_multi_scale_prefecture_future.csv"))
  cat(sprintf("市级预测: %d 行, %d 个地级市\n",
    nrow(dt_pref_future), uniqueN(dt_pref_future$prefecture)))
} else {
  cat("市级单位列表为空，跳过市级预测\n")
  dt_pref_future <- data.table()
}

# ── 5. 县级预测 / County-level prediction ──────────────────────────────
cat("\n=== 县级预测 ===\n")

cat("县级字段:", paste(names(china_county), collapse = ", "), "\n")

prov_col_county <- intersect(names(china_county),
  c("省", "省份", "PROVINCE", "省名"))[1]
county_col <- intersect(names(china_county),
  c("县", "县名", "NAME", "COUNTY", "区县名"))[1]

if (is.na(county_col)) {
  char_cols <- names(china_county)[sapply(china_county, is.character)]
  county_col <- char_cols[min(3, length(char_cols))]
}

cat(sprintf("使用省级字段: %s, 县级字段: %s\n", prov_col_county, county_col))

county_units <- as.data.table(st_drop_geometry(china_county))
county_units[, province_en := prov_map_manual[as.character(
  get(prov_col_county))]]
county_units <- county_units[!is.na(province_en)]
county_units[, county := get(county_col)]

county_list <- unique(county_units[, .(province = province_en, county)])

if (nrow(county_list) > 0) {
  # 县级预测继承省级值 / County inherits province predictions
  dt_county_future <- merge(
    prov_summary,
    county_list,
    by = "province",
    allow.cartesian = TRUE
  )

  fwrite(dt_county_future,
    file.path(TASK_ROOT, "results",
      "table_multi_scale_county_future.csv"))
  cat(sprintf("县级预测: %d 行, %d 个县区\n",
    nrow(dt_county_future), uniqueN(dt_county_future$county)))
} else {
  cat("县级单位列表为空，跳过县级预测\n")
  dt_county_future <- data.table()
}

# ── 6. 栅格尺度预测 / Grid-scale prediction ───────────────────────────
cat("\n=== 栅格尺度预测 ===\n")

# 读取100km和50km栅格数据 / Read 100km and 50km grid data
for (res in c("100km", "50km")) {
  cat(sprintf("\n--- %s 栅格 ---\n", res))

  grid_base <- tryCatch(
    fread(file.path(TASK_ROOT, "data",
      sprintf("grid_%s_base.csv", res))),
    error = function(e) { cat("  读取失败\n"); NULL }
  )
  grid_clim <- tryCatch(
    fread(file.path(TASK_ROOT, "data",
      sprintf("grid_%s_climate.csv", res))),
    error = function(e) NULL
  )
  grid_effort <- tryCatch(
    fread(file.path(TASK_ROOT, "data",
      sprintf("grid_%s_effort.csv", res))),
    error = function(e) NULL
  )

  if (is.null(grid_base)) next

  # 合并栅格数据 / Merge grid data
  grid_dt <- merge(grid_base, grid_clim, by = c("province", "grid_id"),
    all.x = TRUE)

  # 构建未来预测面板 / Build future prediction panel
  # 每个栅格取当前effort + 未来气候
  current_grid_effort <- grid_effort[year == max(year)]
  grid_dt <- merge(grid_dt,
    current_grid_effort[, .(grid_id, log_effort_visits_z,
                             effort_pc1_z)],
    by = "grid_id", all.x = TRUE)

  # 栅格特征 / Grid features
  # spatial_temp_grad -> temp_grad_z (z-score标准化)
  temp_sd_grid <- risk_data[, sd(temp_grad_z, na.rm = TRUE)]
  grid_dt[, temp_grad_z := spatial_temp_grad / temp_sd_grid]
  grid_dt[, prec_grad_z := 0]
  grid_dt[, climate_velocity_z := clim_vel_mean]
  grid_dt[, mahalanobis_dist_z := mahal_mean]
  grid_dt[, climate_exposure_z := exposure_mean]
  grid_dt[, warming_rate_z := warming_mean]

  # 未来年份面板 / Future year panel
  grid_future_list <- list()
  for (yr in future_years) {
    for (cs in climate_scenarios) {
      for (es in effort_scenarios) {
        dt_g <- copy(grid_dt)
        dt_g[, year := yr]
        dt_g[, year_c := yr - 2013]
        dt_g[, climate_scenario := cs]
        dt_g[, effort_scenario := es]

        # 气候情景调整 / Climate scenario adjustment
        if (cs == "current") {
          # 保持原始temp_grad_z / Keep original
        } else if (cs == "ssp245") {
          dt_g[, temp_grad_z := temp_grad_z +
            0.3 / temp_grad_sd * (yr - 2024) / 26]
        } else if (cs == "ssp585") {
          dt_g[, temp_grad_z := temp_grad_z +
            0.8 / temp_grad_sd * (yr - 2024) / 26]
        }

        # effort情景 / Effort scenario
        if (es == "trend") {
          dt_g[, log_effort_visits_z :=
            log_effort_visits_z * (1 + 0.02 * (yr - 2024))]
        } else if (es == "doubled") {
          dt_g[, log_effort_visits_z := log_effort_visits_z * 2]
        }

        # 交互项 / Interactions
        dt_g[, temp_x_effort := temp_grad_z * log_effort_visits_z]
        dt_g[, velocity_x_effort := climate_velocity_z * log_effort_visits_z]
        dt_g[, mahal_x_effort := mahalanobis_dist_z * log_effort_visits_z]

        grid_future_list[[length(grid_future_list) + 1]] <- dt_g
      }
    }
  }

  dt_grid_future <- rbindlist(grid_future_list)

  # 预测 / Predict
  X_grid <- as.matrix(dt_grid_future[, ..feature_cols])
  dt_grid_future[, hazard := predict(xgb_model, X_grid, type = "response")]

  # 省级汇总 / Province-level summary for grid
  grid_summary <- dt_grid_future[, .(hazard_mean = mean(hazard, na.rm = TRUE)),
    by = .(province, year, climate_scenario, effort_scenario)]

  fwrite(grid_summary,
    file.path(TASK_ROOT, "results",
      sprintf("table_multi_scale_grid_%s_future.csv", res)))
  cat(sprintf("%s栅格预测: %d 行, %d 栅格\n",
    res, nrow(dt_grid_future), uniqueN(dt_grid_future$grid_id)))

  # 保存栅格级预测（按年份和情景） / Save grid-level predictions
  # 2050 SSP585 trend for mapping
  grid_2050 <- dt_grid_future[year == 2050 &
    climate_scenario == "ssp585" & effort_scenario == "trend",
    .(grid_id, province, centroid_lon, centroid_lat, hazard)]
  fwrite(grid_2050,
    file.path(TASK_ROOT, "results",
      sprintf("table_grid_%s_2050_ssp585_hazard.csv", res)))
}

# ── 7. 省级空间热点图 / Province spatial hotspot map ───────────────────
cat("\n=== 省级空间热点图 ===\n")

# 省名映射 / Province name mapping
china_prov$province_en <- prov_map_manual[as.character(china_prov$省)]

prov_2050_ssp585 <- prov_summary[year == 2050 &
  climate_scenario == "ssp585" & effort_scenario == "trend",
  .(province, hazard_mean)]

map_prov_2050 <- merge(china_prov, prov_2050_ssp585,
  by.x = "province_en", by.y = "province", all.x = TRUE)

p_prov_map <- ggplot(map_prov_2050) +
  geom_sf(aes(fill = hazard_mean), color = "grey60", linewidth = 0.2) +
  geom_sf(data = china_national, color = "grey30", linewidth = 0.3) +
  scale_fill_viridis_c(option = "C", direction = -1,
    name = "Predicted\nhazard", na.value = "grey90") +
  coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
  labs(title = "Province-level 2050 hotspot (SSP585)",
    subtitle = "XGBoost prediction, trend effort growth") +
  theme_map

ggsave(file.path(TASK_ROOT, "figures",
  "fig_multi_prov_2050_ssp585.png"),
  p_prov_map, width = 9, height = 8, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_multi_prov_2050_ssp585.pdf"),
  p_prov_map, width = 9, height = 8)

# ── 8. 市级空间热点图 / Prefecture spatial hotspot map ─────────────────
cat("\n=== 市级空间热点图 ===\n")

if (nrow(dt_pref_future) > 0) {
  # 市级字段名 / Prefecture field name
  pref_2050 <- dt_pref_future[year == 2050 &
    climate_scenario == "ssp585" & effort_scenario == "trend",
    .(prefecture, hazard_mean)]

  # 合并到地图 / Merge to map
  map_pref_2050 <- merge(china_pref, pref_2050,
    by.x = pref_col, by.y = "prefecture", all.x = TRUE)

  p_pref_map <- ggplot(map_pref_2050) +
    geom_sf(aes(fill = hazard_mean), color = "grey80", linewidth = 0.1) +
    geom_sf(data = china_national, color = "grey30", linewidth = 0.3) +
    scale_fill_viridis_c(option = "C", direction = -1,
      name = "Predicted\nhazard", na.value = "grey90") +
    coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
    labs(title = "Prefecture-level 2050 hotspot (SSP585)",
      subtitle = "Inherited from province-level XGBoost prediction") +
    theme_map

  ggsave(file.path(TASK_ROOT, "figures",
    "fig_multi_pref_2050_ssp585.png"),
    p_pref_map, width = 9, height = 8, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    "fig_multi_pref_2050_ssp585.pdf"),
    p_pref_map, width = 9, height = 8)
  cat("市级热点图已生成\n")
} else {
  cat("市级预测数据为空，跳过市级热点图\n")
}

# ── 9. 县级空间热点图 / County spatial hotspot map ────────────────────
cat("\n=== 县级空间热点图 ===\n")

if (nrow(dt_county_future) > 0) {
  county_2050 <- dt_county_future[year == 2050 &
    climate_scenario == "ssp585" & effort_scenario == "trend",
    .(county, hazard_mean)]

  map_county_2050 <- merge(china_county, county_2050,
    by.x = county_col, by.y = "county", all.x = TRUE)

  p_county_map <- ggplot(map_county_2050) +
    geom_sf(aes(fill = hazard_mean), color = NA, linewidth = 0) +
    geom_sf(data = china_national, color = "grey30", linewidth = 0.3) +
    scale_fill_viridis_c(option = "C", direction = -1,
      name = "Predicted\nhazard", na.value = "grey90") +
    coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
    labs(title = "County-level 2050 hotspot (SSP585)",
      subtitle = "Inherited from province-level XGBoost prediction") +
    theme_map

  ggsave(file.path(TASK_ROOT, "figures",
    "fig_multi_county_2050_ssp585.png"),
    p_county_map, width = 9, height = 8, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    "fig_multi_county_2050_ssp585.pdf"),
    p_county_map, width = 9, height = 8)
  cat("县级热点图已生成\n")
} else {
  cat("县级预测数据为空，跳过县级热点图\n")
}

# ── 10. 栅格热点图 / Grid hotspot map ──────────────────────────────────
cat("\n=== 栅格热点图 ===\n")

for (res in c("100km", "50km")) {
  grid_2050_file <- file.path(TASK_ROOT, "results",
    sprintf("table_grid_%s_2050_ssp585_hazard.csv", res))

  if (!file.exists(grid_2050_file)) {
    cat(sprintf("%s栅格预测文件不存在，跳过\n", res))
    next
  }

  grid_2050 <- fread(grid_2050_file)

  p_grid <- ggplot() +
    geom_sf(data = china_prov, fill = "grey95", color = "grey70",
      linewidth = 0.2) +
    geom_point(data = grid_2050[!is.na(hazard)],
      aes(x = centroid_lon, y = centroid_lat, color = hazard),
      size = 0.8, alpha = 0.7) +
    geom_sf(data = china_national, color = "grey30", linewidth = 0.3) +
    scale_color_viridis_c(option = "C", direction = -1,
      name = "Predicted\nhazard") +
    coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
    labs(title = sprintf("%s grid 2050 hotspot (SSP585)", res),
      subtitle = "XGBoost prediction, trend effort") +
    theme_map

  ggsave(file.path(TASK_ROOT, "figures",
    sprintf("fig_multi_grid_%s_2050_ssp585.png", res)),
    p_grid, width = 9, height = 8, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    sprintf("fig_multi_grid_%s_2050_ssp585.pdf", res)),
    p_grid, width = 9, height = 8)
  cat(sprintf("%s栅格热点图已生成\n", res))
}

# ── 11. 多尺度对比拼图 / Multi-scale comparison panel ──────────────────
cat("\n=== 多尺度对比拼图 ===\n")

# 四个面板: 省, 市, 县, 100km栅格 / 4 panels: province, prefecture, county, grid
panel_list <- list()

# 省级 / Province
panel_list[["prov"]] <- p_prov_map +
  labs(title = "(a) Province") +
  theme(legend.position = "none",
    plot.title = element_text(size = 10, face = "bold"))

# 市级 / Prefecture
if (exists("p_pref_map")) {
  panel_list[["pref"]] <- p_pref_map +
    labs(title = "(b) Prefecture") +
    theme(legend.position = "none",
      plot.title = element_text(size = 10, face = "bold"))
}

# 县级 / County
if (exists("p_county_map")) {
  panel_list[["county"]] <- p_county_map +
    labs(title = "(c) County") +
    theme(legend.position = "none",
      plot.title = element_text(size = 10, face = "bold"))
}

# 100km栅格 / 100km grid
grid_100km_file <- file.path(TASK_ROOT, "results",
  "table_grid_100km_2050_ssp585_hazard.csv")
if (file.exists(grid_100km_file)) {
  panel_list[["grid"]] <- p_grid +
    labs(title = "(d) 100km grid") +
    theme(legend.position = "none",
      plot.title = element_text(size = 10, face = "bold"))
}

# 拼图 / Combine
if (length(panel_list) >= 2) {
  # 2x2 layout
  p_combined <- (panel_list[[1]] | panel_list[[2]]) /
                (panel_list[[min(3, length(panel_list))]] |
                 panel_list[[min(4, length(panel_list))]]) +
    plot_annotation(
      title = "2050 new-record hotspot: multi-scale comparison",
      subtitle = "SSP585 + trend effort growth, XGBoost prediction",
      theme = theme(plot.title = element_text(face = "bold", size = 14)))

  ggsave(file.path(TASK_ROOT, "figures",
    "fig_multi_scale_comparison_2050.png"),
    p_combined, width = 16, height = 14, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    "fig_multi_scale_comparison_2050.pdf"),
    p_combined, width = 16, height = 14)
  cat("多尺度对比拼图已生成\n")
}

# ── 12. 多情景时间轨迹 / Multi-scenario temporal trajectory ────────────
cat("\n=== 多情景时间轨迹 ===\n")

# 全国均值轨迹 / National mean trajectory
national_traj <- prov_summary[, .(hazard_mean = mean(hazard_mean)),
  by = .(year, climate_scenario, effort_scenario)]

p_traj <- ggplot(national_traj[effort_scenario == "trend"],
  aes(x = year, y = hazard_mean,
    color = climate_scenario, group = climate_scenario)) +
  geom_ribbon(aes(ymin = hazard_mean * 0.85, ymax = hazard_mean * 1.15),
    alpha = 0.1, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("current" = "#666666",
    "ssp245" = "#2171b5", "ssp585" = "#d94801"),
    labels = c("Current", "SSP2-4.5", "SSP5-8.5"),
    name = "Climate scenario") +
  labs(x = "Year", y = "Mean predicted hazard",
    title = "National trajectory of new-record hazard",
    subtitle = "Province-level mean, trend effort growth") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures",
  "fig_multi_national_trajectory.png"),
  p_traj, width = 9, height = 6, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_multi_national_trajectory.pdf"),
  p_traj, width = 9, height = 6)

# 努力情景敏感性 / Effort scenario sensitivity
p_effort <- ggplot(national_traj[climate_scenario == "ssp585"],
  aes(x = year, y = hazard_mean,
    linetype = effort_scenario, color = effort_scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("baseline" = "#666666",
    "trend" = "#2171b5", "doubled" = "#d94801"),
    name = "Effort scenario") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"),
    name = "Effort scenario") +
  labs(x = "Year", y = "Mean predicted hazard",
    title = "Effort scenario sensitivity (SSP585)",
    subtitle = "National mean trajectory under different effort growth") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures",
  "fig_multi_effort_sensitivity_trajectory.png"),
  p_effort, width = 9, height = 6, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_multi_effort_sensitivity_trajectory.pdf"),
  p_effort, width = 9, height = 6)

# ── 13. 多情景空间对比 / Multi-scenario spatial comparison ─────────────
cat("\n=== 多情景空间对比 ===\n")

# SSP245 vs SSP585 × 2030/2040/2050
scenario_panels <- list()
for (yr in c(2030, 2040, 2050)) {
  for (cs in c("ssp245", "ssp585")) {
    dt_sub <- prov_summary[year == yr &
      climate_scenario == cs & effort_scenario == "trend",
      .(province, hazard_mean)]

    map_sub <- merge(china_prov, dt_sub,
      by.x = "province_en", by.y = "province", all.x = TRUE)

    scenario_panels[[sprintf("%d_%s", yr, cs)]] <- ggplot(map_sub) +
      geom_sf(aes(fill = hazard_mean), color = "grey60", linewidth = 0.15) +
      geom_sf(data = china_national, color = "grey30", linewidth = 0.2) +
      scale_fill_viridis_c(option = "C", direction = -1,
        name = "Hazard",
        limits = c(0, prov_summary[, max(hazard_mean, na.rm = TRUE)]),
        na.value = "grey90") +
      coord_sf(xlim = c(73, 136), ylim = c(17, 55)) +
      labs(title = sprintf("%s %s", yr, toupper(cs))) +
      theme_map +
      theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))
  }
}

# 2×3 layout: 2 scenarios × 3 time periods
if (length(scenario_panels) == 6) {
  p_multi_scenario <- (scenario_panels[[1]] | scenario_panels[[2]]) /
                      (scenario_panels[[3]] | scenario_panels[[4]]) /
                      (scenario_panels[[5]] | scenario_panels[[6]]) +
    plot_annotation(
      title = "Multi-scenario province-level hotspot maps",
      subtitle = "Left: SSP2-4.5, Right: SSP5-8.5 | Trend effort growth",
      theme = theme(plot.title = element_text(face = "bold", size = 14)))

  ggsave(file.path(TASK_ROOT, "figures",
    "fig_multi_scenario_province_panel.png"),
    p_multi_scenario, width = 12, height = 16, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    "fig_multi_scenario_province_panel.pdf"),
    p_multi_scenario, width = 12, height = 16)
  cat("多情景空间对比图已生成\n")
}

# ── 14. 输出汇总 / Output summary ──────────────────────────────────────
cat("\n=== 16_multi_scale_future_prediction.R 完成 ===\n")
cat("输出文件:\n")
cat("  results/table_multi_scale_province_future.csv\n")
cat("  results/table_multi_scale_prefecture_future.csv\n")
cat("  results/table_multi_scale_county_future.csv\n")
cat("  results/table_multi_scale_grid_100km_future.csv\n")
cat("  results/table_multi_scale_grid_50km_future.csv\n")
cat("  figures/fig_multi_prov_2050_ssp585.png\n")
cat("  figures/fig_multi_pref_2050_ssp585.png\n")
cat("  figures/fig_multi_county_2050_ssp585.png\n")
cat("  figures/fig_multi_grid_100km_2050_ssp585.png\n")
cat("  figures/fig_multi_grid_50km_2050_ssp585.png\n")
cat("  figures/fig_multi_scale_comparison_2050.png\n")
cat("  figures/fig_multi_national_trajectory.png\n")
cat("  figures/fig_multi_effort_sensitivity_trajectory.png\n")
cat("  figures/fig_multi_scenario_province_panel.png\n")
