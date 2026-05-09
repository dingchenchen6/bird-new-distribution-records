#!/usr/bin/env Rscript
# 07_hazard_model_grid.R
# 50km 和 100km 网格级 hazard model
# 使用更新后的坐标数据将事件分配到格点
# Grid-level hazard model with coordinate-located events

suppressPackageStartupMessages({
  library(data.table)
  library(glmmTMB)
  library(openxlsx)
  library(here)
})

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")
HAZ_ROOT  <- here::here("tasks", "bird_new_record_hazard_model")

# ── 1. 读取更新后的新纪录数据（含坐标）────────────────────────────────────
cat("=== 读取更新后新纪录数据 ===\n")

new_records_xlsx <- file.path(TASK_ROOT, "鸟类新纪录20260509.xlsx")
dt_cbnr <- as.data.table(read.xlsx(new_records_xlsx, sheet = "CBNR（EN）"))

cat("CBNR(EN) 行数:", nrow(dt_cbnr), "\n")
cat("有坐标的记录:", sum(!is.na(dt_cbnr$Longitude) & !is.na(dt_cbnr$Latitude)), "\n")

# 标准化列名
setnames(dt_cbnr,
         c("Taxonomy_scientific_name_China2025",
           "New_distribution_province",
           "Source_publication_year"),
         c("species", "province", "pub_year"))

# 确保经纬度为数值
dt_cbnr[, longitude := as.numeric(Longitude)]
dt_cbnr[, latitude  := as.numeric(Latitude)]
dt_cbnr[, pub_year  := as.integer(pub_year)]

cat("有有效坐标的记录:", dt_cbnr[!is.na(longitude) & !is.na(latitude), .N], "\n")
cat("省份示例:", paste(head(unique(dt_cbnr$province), 5), collapse=", "), "\n")

# ── 2. 读取网格和风险集 ───────────────────────────────────────────────────
cat("\n=== 读取网格数据 ===\n")

grid_50_base  <- fread(file.path(TASK_ROOT, "data", "grid_50km_base.csv"))
grid_100_base <- fread(file.path(TASK_ROOT, "data", "grid_100km_base.csv"))

cat("50km 网格:", nrow(grid_50_base), "格点\n")
cat("100km 网格:", nrow(grid_100_base), "格点\n")

# 读取省级风险集（用于网格模型的替代方案）
risk_prov <- fread(file.path(TASK_ROOT, "data", "hazard_risk_upgraded_complete_case.csv"))
risk_prov[, year_c := year - 2013]

# 读取气候指标
clim_metrics <- fread(file.path(TASK_ROOT, "data", "climate_metrics_province_year.csv"))

# 合并高级气候指标到省级风险集
risk_prov <- merge(risk_prov,
                   clim_metrics[, .(province, year,
                                    climate_velocity_z, precip_velocity_z,
                                    climate_exposure_z, warming_rate_z,
                                    mahalanobis_dist_z)],
                   by = c("province", "year"), all.x = TRUE)

# ── 3. 将坐标事件分配到格点 ───────────────────────────────────────────────
cat("\n=== 分配事件到格点 ===\n")

# 读取原始hazard model的事件数据
ndr_original <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test",
                                 "derived_inputs", "ndr_supported.csv"))
cat("原始事件表行数:", nrow(ndr_original), "\n")

# 用更新后的CBNR数据匹配
# 标准化省份名和物种名
dt_cbnr_coords <- dt_cbnr[!is.na(longitude) & !is.na(latitude),
                            .(species, province, longitude, latitude, pub_year)]

cat("有坐标的CBNR记录:", nrow(dt_cbnr_coords), "\n")

# 将坐标点分配到最近的格点
assign_to_grid <- function(coords_dt, grid_base, grid_name) {
  cat(sprintf("\n  分配到 %s 网格\n", grid_name))

  # 对每个坐标点，找到最近的格点质心
  assigned <- copy(coords_dt)
  assigned[, grid_id := {
    dists <- sqrt((grid_base$centroid_lon - longitude)^2 +
                  (grid_base$centroid_lat - latitude)^2)
    grid_base$grid_id[which.min(dists)]
  }, by = .(species, province, longitude, latitude)]

  # 检查分配是否合理（距离<1度≈100km）
  assigned[, dist_to_grid := {
    gi <- grid_base$grid_id == grid_id
    sqrt((grid_base$centroid_lon[gi] - longitude)^2 +
         (grid_base$centroid_lat[gi] - latitude)^2)
  }, by = .(grid_id)]

  n_close <- assigned[dist_to_grid < 1, .N]
  cat(sprintf("  %s: %d/%d 记录分配到1度内格点\n",
              grid_name, n_close, nrow(assigned)))

  assigned
}

coords_50  <- assign_to_grid(dt_cbnr_coords, grid_50_base,  "50km")
coords_100 <- assign_to_grid(dt_cbnr_coords, grid_100_base, "100km")

# 保存坐标-格点映射
fwrite(coords_50[, .(species, province, longitude, latitude,
                     grid_id, pub_year, dist_to_grid)],
       file.path(TASK_ROOT, "data", "events_50km_grid_assigned.csv"))
fwrite(coords_100[, .(species, province, longitude, latitude,
                      grid_id, pub_year, dist_to_grid)],
       file.path(TASK_ROOT, "data", "events_100km_grid_assigned.csv"))

# ── 4. 构建100km网格风险集（可管理大小）────────────────────────────────────
cat("\n=== 构建100km网格风险集 ===\n")

# 100km风险集约5.3M行，可以处理
# 但先构建一个精简版本：只包含有事件的物种-省份组合

sdm_province <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test",
                                 "derived_inputs", "sdm_province.csv"))
candidate_sp_prov <- sdm_province[potential == 1L & historical_presence == 0L,
                                   .(species, province)]

# 首次事件表（从CBNR坐标数据）
ndr_first <- dt_cbnr_coords[, .(species, province, year = pub_year)]
ndr_first <- ndr_first[!is.na(year)]
ndr_first <- ndr_first[order(species, province, year),
                        .SD[1], by = .(species, province)]
setnames(ndr_first, "year", "first_event_year")

# 100km: 物种-省份-格点候选
prov_grid_100 <- unique(grid_100_base[, .(grid_id, province)])
candidate_grid_100 <- merge(candidate_sp_prov, prov_grid_100,
                             by = "province", all.x = TRUE, allow.cartesian = TRUE)
candidate_grid_100 <- candidate_grid_100[!is.na(grid_id)]

# 只为有事件物种的子集构建（前50种，节省时间）
event_species <- unique(ndr_first$species)
candidate_subset <- candidate_grid_100[species %in% head(event_species, 50)]
cat("100km子集: 50种, ", nrow(candidate_subset), " 候选对\n")

# 扩展年份
year_seq <- 2002:2024
risk_100_sub <- candidate_subset[, {
  .(year = year_seq)
}, by = .(species, province, grid_id)]

# 合并首次事件
risk_100_sub <- merge(risk_100_sub,
                      ndr_first[, .(species, province, first_event_year)],
                      by = c("species", "province"), all.x = TRUE)

risk_100_sub[, event := 0L]
risk_100_sub[!is.na(first_event_year) & year == first_event_year, event := 1L]
risk_100_sub <- risk_100_sub[is.na(first_event_year) | year <= first_event_year]
risk_100_sub[, first_event_year := NULL]

# 合并格点级气候
grid_100_clim <- fread(file.path(TASK_ROOT, "data", "grid_100km_climate.csv"))
risk_100_sub <- merge(risk_100_sub,
                      grid_100_clim[, .(grid_id, bio1, bio12, elev)],
                      by = "grid_id", all.x = TRUE)

# 合并省级气候指标
risk_100_sub <- merge(risk_100_sub,
                      clim_metrics[, .(province, year,
                                       climate_velocity_z, precip_velocity_z,
                                       climate_exposure_z, warming_rate_z,
                                       mahalanobis_dist_z,
                                       temp_grad_prov_z, prec_grad_prov_z,
                                       temp_anom, prec_anom)],
                      by = c("province", "year"), all.x = TRUE)

# 合并努力
effort_panel <- fread(file.path(TASK_ROOT, "data", "effort_panel_upgraded.csv"))
risk_100_sub <- merge(risk_100_sub,
                      effort_panel[, .(province, year,
                                       log_effort_visits_z, log_effort_days_z,
                                       effort_pc1_z)],
                      by = c("province", "year"), all.x = TRUE)

# year_c
risk_100_sub[, year_c := year - 2013]

# 标准化格点级气候
risk_100_sub[, bio1_z := scale(bio1)[, 1]]
risk_100_sub[, bio12_z := scale(bio12)[, 1]]

# 因子化
risk_100_sub[, species  := factor(species)]
risk_100_sub[, province := factor(province)]
risk_100_sub[, grid_id  := factor(grid_id)]

cat("100km子集风险集:", nrow(risk_100_sub), "行, 事件:", sum(risk_100_sub$event), "\n")

# ── 5. 拟合网格级 hazard model ────────────────────────────────────────────
cat("\n=== 拟合网格级 hazard model ===\n")

extract_coefs_manual <- function(fit) {
  cf <- summary(fit)$coefficients$cond
  vc <- vcov(fit)
  vc_cond <- if (is.list(vc)) vc[["cond"]] else vc
  se <- sqrt(diag(vc_cond))
  z <- cf[, "Estimate"] / se
  p <- 2 * pnorm(-abs(z))
  data.table(
    term = rownames(cf), estimate = cf[, "Estimate"],
    std.error = se, statistic = z, p.value = p,
    conf.low = cf[, "Estimate"] - 1.96 * se,
    conf.high = cf[, "Estimate"] + 1.96 * se
  )
}

grid_formulas <- list(
  # 网格模型1: 省级气候梯度 × 省级努力
  grid_M1 = as.formula(
    "event ~ year_c + temp_grad_prov_z * log_effort_visits_z +
     (1|species) + (1|province)"),
  # 网格模型2: 气候速度 × 省级努力
  grid_M2 = as.formula(
    "event ~ year_c + climate_velocity_z * log_effort_visits_z +
     (1|species) + (1|province)"),
  # 网格模型3: 格点温度 + 省级努力交互
  grid_M3 = as.formula(
    "event ~ year_c + bio1_z * log_effort_visits_z +
     (1|species) + (1|province)"),
  # 网格模型4: 格点温度 + 省级气候速度 + 省级努力
  grid_M4 = as.formula(
    "event ~ year_c + bio1_z + climate_velocity_z * log_effort_visits_z +
     (1|species) + (1|province)"),
  # 网格模型5: 马氏距离 × 努力
  grid_M5 = as.formula(
    "event ~ year_c + mahalanobis_dist_z * log_effort_visits_z +
     (1|species) + (1|province)")
)

# 完整案例筛选
risk_100_complete <- risk_100_sub[!is.na(log_effort_visits_z) &
                                   !is.na(climate_velocity_z) &
                                   !is.na(temp_grad_prov_z)]

cat("完整案例:", nrow(risk_100_complete), "行, 事件:", sum(risk_100_complete$event), "\n")

grid_fit_info <- list()
grid_coefs <- list()

for (mname in names(grid_formulas)) {
  cat(sprintf("  Fitting %s ... ", mname))
  fit <- tryCatch(
    glmmTMB(grid_formulas[[mname]],
            data = risk_100_complete,
            family = binomial(link = "cloglog")),
    error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(fit)) next

  pdHess <- isTRUE(fit$sdr$pdHess)
  aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)
  cat(sprintf("AIC=%.1f, pdHess=%s\n", aic_val, pdHess))

  grid_fit_info[[mname]] <- data.table(
    model = mname,
    formula = deparse(grid_formulas[[mname]], width.cutoff = 500),
    resolution = "100km",
    status = if (pdHess) "ok" else "convergence_warning",
    nobs = nobs(fit), aic = aic_val,
    n_events = sum(risk_100_complete$event),
    convergence = fit$fit$convergence, pdHess = pdHess
  )

  coefs_dt <- tryCatch(extract_coefs_manual(fit), error = function(e) {
    data.table(term = character(0), estimate = numeric(0),
               std.error = numeric(0), statistic = numeric(0),
               p.value = numeric(0), conf.low = numeric(0), conf.high = numeric(0))
  })
  if (nrow(coefs_dt) > 0) {
    coefs_dt[, model := mname]
    coefs_dt[, resolution := "100km"]
    grid_coefs[[mname]] <- coefs_dt
  }
}

# ── 6. 省级模型对比（同物种子集）────────────────────────────────────────
cat("\n=== 省级模型对比（同物种子集）===\n")

# 用同样的50种拟合省级模型
risk_prov_sub <- risk_prov[species %in% head(event_species, 50)]
risk_prov_sub <- risk_prov_sub[!is.na(log_effort_visits_z) &
                                !is.na(temp_grad_z) &
                                !is.na(climate_velocity_z)]

prov_formulas <- list(
  prov_M1 = as.formula(
    "event ~ year_c + temp_grad_z * log_effort_visits_z +
     (1|species) + (1|province)"),
  prov_M2 = as.formula(
    "event ~ year_c + climate_velocity_z * log_effort_visits_z +
     (1|species) + (1|province)")
)

for (mname in names(prov_formulas)) {
  cat(sprintf("  Fitting %s ... ", mname))
  fit <- tryCatch(
    glmmTMB(prov_formulas[[mname]],
            data = risk_prov_sub,
            family = binomial(link = "cloglog")),
    error = function(e) { cat("FAILED\n"); NULL }
  )

  if (is.null(fit)) next

  pdHess <- isTRUE(fit$sdr$pdHess)
  aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)
  cat(sprintf("AIC=%.1f, pdHess=%s\n", aic_val, pdHess))

  grid_fit_info[[paste0("prov_", mname)]] <- data.table(
    model = paste0("prov_", mname),
    formula = deparse(prov_formulas[[mname]], width.cutoff = 500),
    resolution = "province",
    status = if (pdHess) "ok" else "convergence_warning",
    nobs = nobs(fit), aic = aic_val,
    n_events = sum(risk_prov_sub$event),
    convergence = fit$fit$convergence, pdHess = pdHess
  )

  coefs_dt <- tryCatch(extract_coefs_manual(fit), error = function(e) {
    data.table(term = character(0), estimate = numeric(0),
               std.error = numeric(0), statistic = numeric(0),
               p.value = numeric(0), conf.low = numeric(0), conf.high = numeric(0))
  })
  if (nrow(coefs_dt) > 0) {
    coefs_dt[, model := paste0("prov_", mname)]
    coefs_dt[, resolution := "province"]
    grid_coefs[[paste0("prov_", mname)]] <- coefs_dt
  }
}

# ── 7. 保存结果 ──────────────────────────────────────────────────────────
if (length(grid_fit_info) > 0) {
  dt_grid_model <- rbindlist(grid_fit_info, fill = TRUE)
  fwrite(dt_grid_model,
         file.path(TASK_ROOT, "results", "table_grid_model_comparison.csv"))
}

if (length(grid_coefs) > 0) {
  dt_grid_coefs <- rbindlist(grid_coefs, fill = TRUE)
  dt_grid_coefs[, hr := exp(estimate)]
  dt_grid_coefs[, hr_lower := exp(conf.low)]
  dt_grid_coefs[, hr_upper := exp(conf.high)]
  fwrite(dt_grid_coefs,
         file.path(TASK_ROOT, "results", "table_grid_model_coefficients.csv"))

  # 交互项对比
  cat("\n=== 交互项对比：100km vs 省级 ===\n")
  interact_coefs <- dt_grid_coefs[grepl(":", term) & !grepl("Intercept", term)]
  for (i in seq_len(nrow(interact_coefs))) {
    row_i <- interact_coefs[i]
    sig <- if (is.na(row_i$p.value)) "NA"
           else if (row_i$p.value < 0.05) "*" else "n.s."
    cat(sprintf("  %-15s | %s | HR=%.3f [%.3f-%.3f] p=%.3g %s\n",
                row_i$model, row_i$resolution,
                row_i$hr, row_i$hr_lower, row_i$hr_upper,
                row_i$p.value, sig))
  }
}

# ── 8. 诊断 ──────────────────────────────────────────────────────────────
cat("\n=== 网格模型诊断 ===\n")
cat(sprintf("100km 风险集: %d 行, 事件: %d, 事件率: %.4f%%\n",
            nrow(risk_100_complete), sum(risk_100_complete$event),
            100 * mean(risk_100_complete$event)))
cat(sprintf("省级风险集: %d 行, 事件: %d, 事件率: %.4f%%\n",
            nrow(risk_prov_sub), sum(risk_prov_sub$event),
            100 * mean(risk_prov_sub$event)))

cat("\n=== 07_hazard_model_grid.R 完成 ===\n")
