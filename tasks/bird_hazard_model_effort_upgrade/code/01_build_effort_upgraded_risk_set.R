#!/usr/bin/env Rscript
# 01_build_effort_upgraded_risk_set.R
# 用占域模型观测者型 effort 构建升级版 hazard model 风险集
# Build the SDM-constrained risk set with occupancy-model-derived effort

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(here)
})

TASK_ROOT  <- here::here("tasks", "bird_hazard_model_effort_upgrade")
HAZ_ROOT   <- here::here("tasks", "bird_new_record_hazard_model")
EFFORT_ROOT <- here::here("tasks", "bird_survey_effort_integration")

dir.create(file.path(TASK_ROOT, "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(TASK_ROOT, "results"), recursive = TRUE, showWarnings = FALSE)

# ── 配置 ─────────────────────────────────────────────────────────────────
cell_threshold <- 100L
year_min <- 2002L
year_max <- 2024L

# ── 1. 读取现有 hazard model 的 helper 函数 ─────────────────────────────
source(file.path(HAZ_ROOT, "code", "run_bird_new_record_hazard_model.R"))

# ── 2. 读取输入数据 ─────────────────────────────────────────────────────
# 新纪录事件
new_records <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test", "derived_inputs",
                                "ndr_supported.csv"))

# SDM 候选省份
sdm_province <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test", "derived_inputs",
                                 "sdm_province.csv"))

# 省级气候
province_year_climate <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test", "derived_inputs",
                                          "province_year_climate.csv"))

# 物种原生气候
species_year_native <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test", "derived_inputs",
                                        "species_year_native_climate.csv"))

# 占域模型 effort（从 Task A 桥接产出）
dt_effort_combined <- fread(file.path(EFFORT_ROOT, "data",
                                       "effort_occupancy_model_province_year_combined.csv"))
dt_effort_pca <- fread(file.path(EFFORT_ROOT, "data",
                                  "effort_pca_province_year_combined.csv"))

# 旧记录型 effort（用于复现对照）
dt_effort_old <- fread(file.path(EFFORT_ROOT, "data",
                                  "effort_py_record_only_server_sync.csv"))

# ── 3. 构建升级版 effort 面板 ─────────────────────────────────────────────
# 合并所有 effort 指标到一个面板
dt_effort_upgrade <- merge(
  dt_effort_combined[, .(year, province, n_records, n_visits, n_observers,
                         n_birding_days, n_species, total_duration_min)],
  dt_effort_pca[, .(year, province, effort_pc1, effort_zmean)],
  by = c("year", "province"), all.x = TRUE
)

# 加入旧记录型 effort
dt_effort_upgrade <- merge(
  dt_effort_upgrade,
  dt_effort_old[, .(year, province, effort_record)],
  by = c("year", "province"), all.x = TRUE
)

# 限制年份窗口
dt_effort_upgrade <- dt_effort_upgrade[year >= year_min & year <= year_max]

# 对数变换
dt_effort_upgrade[, log_effort_record   := log1p(effort_record)]
dt_effort_upgrade[, log_effort_visits   := log1p(n_visits)]
dt_effort_upgrade[, log_effort_observers := log1p(n_observers)]
dt_effort_upgrade[, log_effort_days     := log1p(n_birding_days)]
# PC1 已是标准化指标，不需对数变换

# 标准化（z-score）
z_cols <- c("log_effort_record", "log_effort_visits", "log_effort_observers",
            "log_effort_days", "effort_pc1")
for (v in z_cols) {
  zv <- paste0(v, "_z")
  dt_effort_upgrade[, (zv) := scale(get(v))[, 1]]
}

# 重命名以匹配现有代码结构
effort_panel <- dt_effort_upgrade %>%
  select(province, year, effort_record, n_visits, n_observers,
         n_birding_days, effort_pc1,
         log_effort_record_z, log_effort_visits_z, log_effort_observers_z,
         log_effort_days_z, effort_pc1_z)

fwrite(as.data.table(effort_panel),
       file.path(TASK_ROOT, "data", "effort_panel_upgraded.csv"))

# ── 4. 构建 SDM 约束风险集（复用现有逻辑）────────────────────────────────
# 使用去重后的事件表构建 first-event 表
ndr_first <- as.data.table(new_records)[order(species, province, year),
                                         .SD[1], by = .(species, province)]
setnames(ndr_first, "year", "first_event_year")

# 构建候选表
sdm_dt <- as.data.table(sdm_province)
candidate_tbl <- sdm_dt[potential == 1L & historical_presence == 0L,
                        .(species, province, risk_start_year)]

# 扩展风险集：对每个 species-province 组合，生成从 risk_start_year 到 year_max 的所有年份
year_seq <- year_min:year_max
risk_rows <- candidate_tbl[, {
  start_yr <- if (!is.na(risk_start_year)) max(risk_start_year, year_min) else year_min
  yrs <- year_seq[year_seq >= start_yr & year_seq <= year_max]
  .(year = yrs)
}, by = .(species, province)]

# 合并首次事件年份
risk_rows <- merge(risk_rows,
                   ndr_first[, .(species, province, first_event_year)],
                   by = c("species", "province"), all.x = TRUE)

# 标记事件：事件年 event=1，事件年之前 event=0，事件年之后剔除
risk_rows[, event := 0L]
risk_rows[!is.na(first_event_year) & year == first_event_year, event := 1L]

# 事件后剔除：只保留 first_event_year 及之前的行
risk_rows <- risk_rows[is.na(first_event_year) | year <= first_event_year]
risk_rows[, first_event_year := NULL]

# ── 5. 合并气候和 effort ─────────────────────────────────────────────────
risk_data <- as.data.table(risk_rows)

# 气候
risk_data <- merge(risk_data, as.data.table(province_year_climate),
                   by = c("province", "year"), all.x = TRUE)

# 原生物种气候
risk_data <- merge(risk_data, as.data.table(species_year_native),
                   by = c("species", "year"), all.x = TRUE)

# 计算 temperature gradient
risk_data[, temp_grad := temp_anom - temp_native_anom]
risk_data[, prec_grad  := prec_anom - prec_native_anom]
risk_data[, temp_grad_z := scale(temp_grad)[, 1]]
risk_data[, prec_grad_z  := scale(prec_grad)[, 1]]

# Effort
risk_data <- merge(risk_data, as.data.table(effort_panel),
                   by = c("province", "year"), all.x = TRUE)

# Migration strategy（迁徙策略）
TRAIT_ROOT <- here::here("tasks", "bird_identity_synonym_dedup_reanalysis",
                          "geb_fig3_fig4_corrected", "data")
dt_traits <- fread(file.path(TRAIT_ROOT, "bird_species_pool_with_traits.csv"))
dt_traits[, mig := migration_class_av]
# 简化分类：Resident = 0, Partial = 1, Long-distance = 2
dt_traits[, mig_code := fcase(
  grepl("^Resident", mig), 0L,
  grepl("^Partial", mig), 1L,
  grepl("^Long", mig), 2L,
  default = NA_integer_
)]
risk_data <- merge(risk_data, dt_traits[, .(species, mig, mig_code)],
                   by = "species", all.x = TRUE)

# ── 6. 完整案例筛选 ──────────────────────────────────────────────────────
# 四种 effort 指标均非 NA 的行（公平对比）
risk_data_complete <- risk_data[
  !is.na(temp_grad_z) &
  !is.na(log_effort_record_z) &
  !is.na(log_effort_visits_z) &
  !is.na(effort_pc1_z) &
  !is.na(log_effort_days_z)
]

risk_data_complete[, species  := factor(species)]
risk_data_complete[, province := factor(province)]

fwrite(risk_data_complete,
       file.path(TASK_ROOT, "data", "hazard_risk_upgraded_complete_case.csv"))

# ── 7. 诊断 ──────────────────────────────────────────────────────────────
diag <- data.table(
  metric = c("threshold", "year_range", "risk_rows_full", "risk_rows_complete",
             "events_complete", "species_complete", "provinces_complete",
             "effort_record_na", "effort_visits_na", "effort_pc1_na",
             "effort_days_na", "temp_grad_na"),
  value = c(cell_threshold,
            paste(year_min, year_max, sep = "-"),
            nrow(risk_data),
            nrow(risk_data_complete),
            sum(risk_data_complete$event),
            n_distinct(risk_data_complete$species),
            n_distinct(risk_data_complete$province),
            sum(is.na(risk_data$log_effort_record_z)),
            sum(is.na(risk_data$log_effort_visits_z)),
            sum(is.na(risk_data$effort_pc1_z)),
            sum(is.na(risk_data$log_effort_days_z)),
            sum(is.na(risk_data$temp_grad_z)))
)

fwrite(diag, file.path(TASK_ROOT, "results", "risk_set_diagnostics.csv"))

cat("=== 01_build_effort_upgraded_risk_set.R 完成 ===\n")
for (i in seq_len(nrow(diag))) {
  cat(sprintf("  %-25s: %s\n", diag$metric[i], diag$value[i]))
}
