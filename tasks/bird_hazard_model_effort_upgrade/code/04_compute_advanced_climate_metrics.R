#!/usr/bin/env Rscript
# 04_compute_advanced_climate_metrics.R
# 计算高级气候变化指标：气候速度、马氏距离、暴露指数、升温速率、降水速度
# 保留原始 temp_grad / prec_grad 作为对照基线
# Compute advanced climate change metrics from WorldClim 2.1 (2.5 arc-min)
# Metrics: climate velocity, Mahalanobis distance, climate exposure,
#          warming rate, precipitation velocity
# Baseline: original temp_grad & prec_grad retained

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(sf)
  library(exactextractr)
  library(here)
})

TASK_ROOT  <- here::here("tasks", "bird_hazard_model_effort_upgrade")
HAZ_ROOT   <- here::here("tasks", "bird_new_record_hazard_model")
SDM_ROOT   <- here::here("tasks", "bird_sdm_distribution_modeling")
SHP_ROOT   <- here::here("tasks", "bird_spatiotemporal_patterns", "data", "shapefile_base")

dir.create(file.path(TASK_ROOT, "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(TASK_ROOT, "results"), recursive = TRUE, showWarnings = FALSE)

YEAR_MIN <- 2002L
YEAR_MAX <- 2024L

# ── 0. 省份边界与名称映射 ──────────────────────────────────────────────────
shp_prov <- st_read(file.path(SHP_ROOT, "省.shp"), quiet = TRUE)
# 中文省名 → 英文映射（与气候/努力数据一致）
cn_to_en <- c(
  "北京市" = "Beijing", "天津市" = "Tianjin", "河北省" = "Hebei",
  "山西省" = "Shanxi", "内蒙古自治区" = "Inner Mongolia", "辽宁省" = "Liaoning",
  "吉林省" = "Jilin", "黑龙江省" = "Heilongjiang", "上海市" = "Shanghai",
  "江苏省" = "Jiangsu", "浙江省" = "Zhejiang", "安徽省" = "Anhui",
  "福建省" = "Fujian", "江西省" = "Jiangxi", "山东省" = "Shandong",
  "河南省" = "Henan", "湖北省" = "Hubei", "湖南省" = "Hunan",
  "广东省" = "Guangdong", "广西壮族自治区" = "Guangxi", "海南省" = "Hainan",
  "重庆市" = "Chongqing", "四川省" = "Sichuan", "贵州省" = "Guizhou",
  "云南省" = "Yunnan", "西藏自治区" = "Tibet", "陕西省" = "Shaanxi",
  "甘肃省" = "Gansu", "青海省" = "Qinghai", "宁夏回族自治区" = "Ningxia",
  "新疆维吾尔自治区" = "Xinjiang", "台湾省" = "Taiwan",
  "香港特别行政区" = "Hong Kong", "澳门特别行政区" = "Macao"
)
shp_prov$province <- cn_to_en[shp_prov$省名]
shp_prov <- shp_prov[!is.na(shp_prov$province), ]
# 转为 WGS84 以匹配 WorldClim
shp_wgs84 <- st_transform(shp_prov, crs = "EPSG:4326")

cat("省份边界加载完成:", nrow(shp_wgs84), "个省\n")

# ── 1. 加载 WorldClim 2.5m 基线栅格 ──────────────────────────────────────
wc_dir <- file.path(SDM_ROOT, "data", "climate", "wc2.1_2.5m")

r_bio1  <- rast(file.path(wc_dir, "wc2.1_2.5m_bio_1.tif"))   # 年均温 × 10
r_bio12 <- rast(file.path(wc_dir, "wc2.1_2.5m_bio_12.tif"))  # 年降水 mm
r_bio15 <- rast(file.path(wc_dir, "wc2.1_2.5m_bio_15.tif"))  # 降水季节性
r_elev  <- rast(file.path(wc_dir, "wc2.1_2.5m_elev.tif"))    # 海拔 m

cat("WorldClim 基线栅格加载完成\n")

# ── 2. 提取省级基线气候均值 ────────────────────────────────────────────────
# bio1 单位: °C × 10 → 需 / 10
prov_baseline <- data.table(
  province = shp_wgs84$province,
  bio1_mean  = exact_extract(r_bio1,  shp_wgs84, "mean", progress = FALSE) / 10,
  bio12_mean = exact_extract(r_bio12, shp_wgs84, "mean", progress = FALSE),
  bio15_mean = exact_extract(r_bio15, shp_wgs84, "mean", progress = FALSE),
  elev_mean  = exact_extract(r_elev,  shp_wgs84, "mean", progress = FALSE)
)

cat("省级基线气候提取完成\n")

# ── 3. 计算温度空间梯度（°C/km）──────────────────────────────────────────
# 在 WGS84 下用 focal (Sobel 算子) 计算空间梯度
# 先裁剪到中国范围以加速计算
china_ext <- ext(73, 136, 17, 55)  # 中国大致经纬度范围
r_temp_crop <- crop(r_bio1 / 10, china_ext)  # °C
r_prec_crop <- crop(r_bio12, china_ext)       # mm/yr

cat("栅格裁剪到中国范围完成\n")

# Sobel 算子计算空间梯度
sobel_x <- matrix(c(-1, 0, 1, -2, 0, 2, -1, 0, 1), nrow = 3) / 8
sobel_y <- matrix(c(1, 2, 1, 0, 0, 0, -1, -2, -1), nrow = 3) / 8

# 温度空间梯度 (°C/degree)
grad_tx <- focal(r_temp_crop, sobel_x, na.policy = "only", fillvalue = NA)
grad_ty <- focal(r_temp_crop, sobel_y, na.policy = "only", fillvalue = NA)

# 降水空间梯度 (mm/degree)
grad_px <- focal(r_prec_crop, sobel_x, na.policy = "only", fillvalue = NA)
grad_py <- focal(r_prec_crop, sobel_y, na.policy = "only", fillvalue = NA)

# 转为 °C/km 和 mm/yr/km
# dx (km per degree lon) = 111.32 * cos(lat) * (res_deg)
# dy (km per degree lat) = 110.57 * (res_deg)
res_deg <- 2.5 / 60  # 2.5 arc-min in degrees
lat_rast <- init(r_temp_crop, "y")
dx_km <- 111.32 * cos(lat_rast * pi / 180) * res_deg
dy_km <- 110.57 * res_deg

spatial_temp_grad <- sqrt((grad_tx / dx_km)^2 + (grad_ty / dy_km)^2)
names(spatial_temp_grad) <- "spatial_temp_grad"

spatial_prec_grad <- sqrt((grad_px / dx_km)^2 + (grad_py / dy_km)^2)
names(spatial_prec_grad) <- "spatial_prec_grad"

# 提取省级空间梯度均值
prov_spatial <- data.table(
  province = shp_wgs84$province,
  spatial_temp_grad = exact_extract(spatial_temp_grad, shp_wgs84, "mean", progress = FALSE),
  spatial_prec_grad = exact_extract(spatial_prec_grad, shp_wgs84, "mean", progress = FALSE)
)

cat("空间梯度计算完成\n")

# ── 4. 计算时序梯度（升温速率 & 降水趋势）──────────────────────────────
# 使用现有 province_year_climate 的异常值时间序列
prov_year_clim <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test",
                                   "derived_inputs", "province_year_climate.csv"))
prov_year_clim <- prov_year_clim[year >= YEAR_MIN & year <= YEAR_MAX]

# 对每个省份拟合线性趋势
temp_trends <- prov_year_clim[, {
  fit <- lm(temp_anom ~ year)
  coefs <- summary(fit)$coefficients
  .(warming_rate = coefs["year", "Estimate"] * 10,  # °C/decade
    temp_trend_se = coefs["year", "Std. Error"] * 10,
    temp_trend_p  = coefs["year", "Pr(>|t|)"],
    temp_sd_interannual = sd(temp_anom, na.rm = TRUE))
}, by = province]

prec_trends <- prov_year_clim[, {
  fit <- lm(prec_anom ~ year)
  coefs <- summary(fit)$coefficients
  .(prec_trend = coefs["year", "Estimate"] * 10,  # mm/decade
    prec_trend_se = coefs["year", "Std. Error"] * 10,
    prec_trend_p  = coefs["year", "Pr(>|t|)"],
    prec_sd_interannual = sd(prec_anom, na.rm = TRUE))
}, by = province]

cat("时序趋势计算完成\n")

# ── 5. 合并计算气候速度和暴露指数 ─────────────────────────────────────────
prov_climate_adv <- merge(prov_spatial, temp_trends, by = "province", all = TRUE)
prov_climate_adv <- merge(prov_climate_adv, prec_trends, by = "province", all = TRUE)

# 气候速度 (km/yr) = |temporal gradient (°C/yr)| / spatial gradient (°C/km)
# warming_rate 是 °C/decade → 除以 10 得 °C/yr
prov_climate_adv[, climate_velocity := abs(warming_rate / 10) / pmax(spatial_temp_grad, 1e-6)]

# 降水速度 (km/yr) = |prec_trend (mm/yr)| / spatial_prec_grad (mm/km/yr)
prov_climate_adv[, precip_velocity := abs(prec_trend / 10) / pmax(spatial_prec_grad, 1e-6)]

# 气候暴露指数 = |趋势| / 年际SD (信噪比)
prov_climate_adv[, climate_exposure := abs(warming_rate / 10) / pmax(temp_sd_interannual, 1e-6)]

cat("气候速度/暴露指数计算完成\n")
cat(sprintf("  气候速度范围: %.3f - %.3f km/yr\n",
            prov_climate_adv[, min(climate_velocity, na.rm = TRUE)],
            prov_climate_adv[, max(climate_velocity, na.rm = TRUE)]))
cat(sprintf("  气候暴露指数范围: %.3f - %.3f\n",
            prov_climate_adv[, min(climate_exposure, na.rm = TRUE)],
            prov_climate_adv[, max(climate_exposure, na.rm = TRUE)]))

# ── 6. 马氏距离：多变量气候偏移 ────────────────────────────────────────────
# 对每个 province-year，计算当前气候与基线的马氏距离
# 使用 3 个低相关变量: bio1 (年均温), bio12 (年降水), bio15 (降水季节性)

# 省级当前年气候 = 基线 + 异常
# 异常来自 temp_anom (°C) 和 prec_anom (mm)
# bio1_current = bio1_mean + temp_anom
# bio12_current = bio12_mean + prec_anom
# bio15_current = bio15_mean (季节性无年际异常数据，保持不变)

# 计算基线协方差矩阵（跨省份）
baseline_mat <- as.matrix(prov_baseline[, .(bio1_mean, bio12_mean, bio15_mean)])
cov_baseline <- cov(baseline_mat, use = "complete.obs")
centroid_baseline <- colMeans(baseline_mat, na.rm = TRUE)

# 对每个省份-年份计算马氏距离
mahal_dt <- CJ(province = prov_baseline$province,
               year = YEAR_MIN:YEAR_MAX)

# 合并基线
mahal_dt <- merge(mahal_dt, prov_baseline[, .(province, bio1_mean, bio12_mean, bio15_mean)],
                  by = "province", all.x = TRUE)
# 合并异常
mahal_dt <- merge(mahal_dt, prov_year_clim[, .(province, year, temp_anom, prec_anom)],
                  by = c("province", "year"), all.x = TRUE)

# 当前气候
mahal_dt[, bio1_current  := bio1_mean + temp_anom]
mahal_dt[, bio12_current := bio12_mean + prec_anom]
mahal_dt[, bio15_current := bio15_mean]  # 季节性无年际变化

# 马氏距离
mahal_dt[, mahalanobis_dist := {
  x <- cbind(bio1_current, bio12_current, bio15_current)
  as.numeric(mahalanobis(x, centroid_baseline, cov_baseline))
}]

cat("马氏距离计算完成\n")
cat(sprintf("  马氏距离范围: %.3f - %.3f\n",
            mahal_dt[, min(mahalanobis_dist, na.rm = TRUE)],
            mahal_dt[, max(mahalanobis_dist, na.rm = TRUE)]))

# ── 7. 构建省级-年份完整气候指标面板 ──────────────────────────────────────
# 注意：temp_grad / prec_grad 是物种-省份-年份级别的（因物种原生分布区而异），
# 不应放入省级气候面板。它们已在 risk_data 中存在。
# 本面板仅包含省级指标。

# 合并所有省级指标
clim_panel <- CJ(province = prov_baseline$province,
                 year = YEAR_MIN:YEAR_MAX)

# 高级指标（省级级别，跨年份恒定）
clim_panel <- merge(clim_panel,
                    prov_climate_adv[, .(province, climate_velocity, precip_velocity,
                                          climate_exposure, warming_rate,
                                          spatial_temp_grad, spatial_prec_grad)],
                    by = "province", all.x = TRUE)

# 马氏距离（省份-年份变化）
clim_panel <- merge(clim_panel,
                    mahal_dt[, .(province, year, mahalanobis_dist)],
                    by = c("province", "year"), all.x = TRUE)

# 省级异常值（用于格点模型中替代物种特定的temp_grad）
clim_panel <- merge(clim_panel,
                    prov_year_clim[, .(province, year, temp_anom, prec_anom)],
                    by = c("province", "year"), all.x = TRUE)

# 省级温度梯度（省份-年份级别，非物种特定）= temp_anom 的省级均值
# 这里 temp_anom 本身就是省级的，可以直接作为省级气候偏移指标
clim_panel[, temp_grad_prov := temp_anom]
clim_panel[, prec_grad_prov  := prec_anom]

# Z-score 标准化
z_vars <- c("climate_velocity", "precip_velocity",
            "climate_exposure", "warming_rate",
            "mahalanobis_dist", "temp_anom", "prec_anom",
            "temp_grad_prov", "prec_grad_prov")

for (v in z_vars) {
  zv <- paste0(v, "_z")
  clim_panel[, (zv) := scale(get(v))[, 1]]
  cat(sprintf("  %s → %s (NA: %d)\n", v, zv, sum(is.na(clim_panel[[zv]]))))
}

# 排序
setorder(clim_panel, province, year)

fwrite(clim_panel, file.path(TASK_ROOT, "data", "climate_metrics_province_year.csv"))

cat("\n气候指标面板保存完成:", nrow(clim_panel), "行\n")
cat("指标列:", paste(z_vars, collapse = ", "), "\n")

# ── 8. 诊断摘要 ────────────────────────────────────────────────────────────
diag <- clim_panel[, lapply(.SD, function(x) {
  c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
}), .SDcols = z_vars]

cat("\n=== 气候指标诊断摘要 ===\n")
for (v in z_vars) {
  cat(sprintf("  %-25s: mean=%.3f  sd=%.3f  range=[%.3f, %.3f]\n",
              v,
              diag[[v]][1], diag[[v]][2], diag[[v]][3], diag[[v]][4]))
}

# ── 9. 物种级别马氏距离（物种-省份-年份）──────────────────────────────────
# 每个物种的原生范围气候质心 vs 当前省份气候
species_native <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test",
                                   "derived_inputs", "species_year_native_climate.csv"))

# 物种原生气候 → 构建质心
species_native_summary <- species_native[, .(
  bio1_native  = mean(temp_native_anom, na.rm = TRUE),
  bio12_native = mean(prec_native_anom, na.rm = TRUE)
), by = species]

cat("\n物种原生气候质心:", nrow(species_native_summary), "种\n")

# 保存物种级原生气候质心
fwrite(species_native_summary,
       file.path(TASK_ROOT, "data", "species_native_climate_centroid.csv"))

cat("\n=== 04_compute_advanced_climate_metrics.R 完成 ===\n")
