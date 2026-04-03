#!/usr/bin/env Rscript

# ============================================================
# Bird New-Record Hazard Pipeline
# SDM-constrained discrete-time hazard modeling workflow
# 中国鸟类新纪录：基于 SDM 风险集的离散时间 hazard 建模流程
# ============================================================
#
# Why this script exists / 为什么写这份脚本
# This is the main end-to-end script for the current hazard-model stage.
# It is written in a LUCC-style stepwise structure so that collaborators can:
# 1. see what each stage is doing,
# 2. rerun only the relevant part when something changes,
# 3. understand the modeling logic without reverse-engineering dense pipes.
#
# 这份脚本是当前 hazard 建模阶段的主入口。
# 我把它改成了更接近 LUCC 项目的分步骤结构，目的是让协作者能够：
# 1. 一眼看清每一步在做什么；
# 2. 某一步输入变化后，可以快速定位重跑；
# 3. 不需要在大量嵌套括号和长管道里猜逻辑。
#
# Current scientific scope / 当前科学口径
# - Risk set: species x province x year
# - SDM candidates: birdwatch SDM + rescue SDM
# - Main model threshold: 100
# - Sensitivity thresholds: 50 and 200
# - Climate in the current main model: temperature only
# - Survey effort in the current main model: record effort only
#
# 当前主模型的核心思想是：
# 在“SDM 预测为潜在可分布省份、且尚未首次记录”的风险集中，
# 检验温度梯度、记录型调查努力以及二者交互是否解释首次新纪录发生风险。
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(readxl)
  library(tidyr)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  normalizePath("code/run_bird_new_record_hazard_model_combined_threshold_test.R", mustWork = TRUE)
}

task_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(task_root, "code", "run_bird_new_record_hazard_model.R"))

step_message <- function(step_id, title_cn, title_en = title_cn) {
  message("\n", strrep("=", 66))
  message("Step ", step_id, ". ", title_en)
  if (!identical(title_cn, title_en)) {
    message("第 ", step_id, " 步：", title_cn)
  }
  message(strrep("=", 66))
}

resolve_input_path <- function(env_name, default_path = NA_character_, must_exist = TRUE) {
  env_value <- Sys.getenv(env_name, "")
  candidate <- if (nzchar(env_value)) env_value else default_path

  if (is.na(candidate) || !nzchar(candidate)) {
    stop("Missing required path. Set env var ", env_name, " or provide a valid default path.")
  }

  if (must_exist && !file.exists(candidate)) {
    stop("Input path not found for ", env_name, ": ", candidate)
  }

  normalizePath(candidate, mustWork = must_exist)
}

ensure_output_tree <- function(output_root) {
  out_dirs <- list(
    data_clean = file.path(output_root, "data_clean"),
    derived_inputs = file.path(output_root, "derived_inputs"),
    diagnostics = file.path(output_root, "diagnostics"),
    results = file.path(output_root, "results"),
    logs = file.path(output_root, "logs")
  )
  walk(c(output_root, unlist(out_dirs, use.names = FALSE)), dir.create, recursive = TRUE, showWarnings = FALSE)
  out_dirs
}

get_runtime_config <- function(task_root, cell_threshold, year_min, year_max) {
  default_cfg <- list(
    new_record_csv = file.path(
      task_root, "server_run_worldclim_5m", "results_worldclim_5m_clean_gpkg",
      "data_clean", "bird_new_records_for_range_climate.csv"
    ),
    event_table_csv = file.path(
      task_root, "server_run_worldclim_5m", "results_worldclim_5m_clean_gpkg",
      "results", "table_new_record_province_year_climate_direction_displacement.csv"
    ),
    species_year_native_csv = file.path(
      task_root, "server_run_worldclim_5m", "results_worldclim_5m_clean_gpkg",
      "results", "species_year_historical_range_climate.csv"
    ),
    effort_xlsx = "/Users/dingchenchen/Desktop/effort补全.xlsx",
    effort_lookup_csv = "/Users/dingchenchen/Documents/New records/bird_new_records_R_output/tasks/bird_geb_fig3_fig4_effort_analysis/data/effort_clean_province_year.csv",
    sdm_birdwatch_csv = "/Users/dingchenchen/Documents/New records/bird-new-distribution-records/tasks/bird_sdm_distribution_modeling_birdwatch_2002_2025/data/tables/table_potential_province_listing_all_species.csv",
    sdm_rescue_csv = "/Users/dingchenchen/Documents/New records/bird-new-distribution-records/tasks/bird_sdm_distribution_modeling_rescue_1980_2025_gbif/data/tables/table_potential_province_listing_all_species.csv"
  )

  effort_csv_env <- Sys.getenv("HAZARD_EFFORT_CSV", "")
  use_effort_csv <- nzchar(effort_csv_env)

  list(
    cell_threshold = cell_threshold,
    year_min = year_min,
    year_max = year_max,
    new_record_csv = resolve_input_path("HAZARD_NEW_RECORD_CSV", default_cfg$new_record_csv),
    event_table_csv = resolve_input_path("HAZARD_EVENT_TABLE_CSV", default_cfg$event_table_csv),
    species_year_native_csv = resolve_input_path("HAZARD_SPECIES_YEAR_NATIVE_CSV", default_cfg$species_year_native_csv),
    effort_csv = if (use_effort_csv) normalizePath(effort_csv_env, mustWork = TRUE) else NA_character_,
    effort_xlsx = if (!use_effort_csv) resolve_input_path("HAZARD_EFFORT_XLSX", default_cfg$effort_xlsx) else NA_character_,
    effort_lookup_csv = resolve_input_path("HAZARD_EFFORT_LOOKUP_CSV", default_cfg$effort_lookup_csv),
    sdm_birdwatch_csv = resolve_input_path("HAZARD_SDM_BIRDWATCH_CSV", default_cfg$sdm_birdwatch_csv),
    sdm_rescue_csv = resolve_input_path("HAZARD_SDM_RESCUE_CSV", default_cfg$sdm_rescue_csv),
    use_effort_csv = use_effort_csv
  )
}

write_input_manifest <- function(cfg, output_path) {
  input_manifest <- tibble(
    input_name = c(
      "new_record_csv",
      "event_table_csv",
      "species_year_native_csv",
      "effort_csv",
      "effort_xlsx",
      "effort_lookup_csv",
      "sdm_birdwatch_csv",
      "sdm_rescue_csv"
    ),
    path = c(
      cfg$new_record_csv,
      cfg$event_table_csv,
      cfg$species_year_native_csv,
      cfg$effort_csv,
      cfg$effort_xlsx,
      cfg$effort_lookup_csv,
      cfg$sdm_birdwatch_csv,
      cfg$sdm_rescue_csv
    )
  )

  write_csv(input_manifest, output_path)
  input_manifest
}

summarise_event_distribution <- function(risk_data) {
  risk_data %>%
    group_by(year) %>%
    summarise(
      risk_rows = n(),
      first_events = sum(event, na.rm = TRUE),
      event_rate = first_events / risk_rows,
      species_n = n_distinct(species),
      province_n = n_distinct(province),
      .groups = "drop"
    )
}

summarise_predictor_distribution <- function(model_data) {
  predictor_names <- c("temp_grad", "temp_grad_z", "log_effort_record", "log_effort_record_z")

  tibble(variable = predictor_names) %>%
    mutate(
      non_missing = map_dbl(variable, ~ sum(!is.na(model_data[[.x]]))),
      mean = map_dbl(variable, ~ mean(model_data[[.x]], na.rm = TRUE)),
      sd = map_dbl(variable, ~ stats::sd(model_data[[.x]], na.rm = TRUE)),
      min = map_dbl(variable, ~ min(model_data[[.x]], na.rm = TRUE)),
      median = map_dbl(variable, ~ stats::median(model_data[[.x]], na.rm = TRUE)),
      max = map_dbl(variable, ~ max(model_data[[.x]], na.rm = TRUE))
    )
}

summarise_model_diagnostics <- function(fitted_models) {
  map_dfr(fitted_models, function(fit_obj) {
    if (!identical(fit_obj$status, "ok")) {
      return(tibble(
        model = fit_obj$name,
        engine = fit_obj$engine,
        status = fit_obj$status,
        singular_fit = NA,
        convergence_ok = NA,
        pd_hessian = NA,
        error = fit_obj$error
      ))
    }

    singular_fit <- if (inherits(fit_obj$model, "glmerMod")) {
      lme4::isSingular(fit_obj$model, tol = 1e-4)
    } else {
      NA
    }

    pd_hessian <- if (inherits(fit_obj$model, "glmmTMB")) {
      isTRUE(fit_obj$model$sdr$pdHess)
    } else {
      NA
    }

    convergence_ok <- if (inherits(fit_obj$model, "glmmTMB")) {
      isTRUE(fit_obj$model$fit$convergence == 0)
    } else {
      TRUE
    }

    tibble(
      model = fit_obj$name,
      engine = fit_obj$engine,
      status = fit_obj$status,
      singular_fit = singular_fit,
      convergence_ok = convergence_ok,
      pd_hessian = pd_hessian,
      error = fit_obj$error
    )
  })
}

read_core_inputs <- function(cfg) {
  list(
    new_records = read_csv(cfg$new_record_csv, show_col_types = FALSE) %>%
      select(species, province, year, record_id) %>%
      filter(year >= cfg$year_min, year <= cfg$year_max),
    event_table = read_csv(cfg$event_table_csv, show_col_types = FALSE) %>%
      filter(year >= cfg$year_min, year <= cfg$year_max),
    native_climate = read_csv(cfg$species_year_native_csv, show_col_types = FALSE) %>%
      filter(year >= cfg$year_min, year <= cfg$year_max),
    effort_lookup = read_csv(cfg$effort_lookup_csv, show_col_types = FALSE) %>%
      distinct(province_cn, province),
    sdm_birdwatch = read_csv(cfg$sdm_birdwatch_csv, show_col_types = FALSE),
    sdm_rescue = read_csv(cfg$sdm_rescue_csv, show_col_types = FALSE)
  )
}

build_effort_panel <- function(cfg, effort_lookup) {
  if (cfg$use_effort_csv) {
    effort_raw <- read_csv(cfg$effort_csv, show_col_types = FALSE)
    if (!"effort_observer" %in% names(effort_raw)) {
      effort_raw <- mutate(effort_raw, effort_observer = NA_real_)
    }

    effort_panel <- effort_raw %>%
      transmute(
        province,
        year = as.integer(year),
        effort_record = as.numeric(effort_record),
        effort_observer = as.numeric(effort_observer)
      )
  } else {
    effort_raw <- read_xlsx(cfg$effort_xlsx, sheet = 1)

    effort_panel <- effort_raw %>%
      rename(province_cn = province) %>%
      left_join(effort_lookup, by = "province_cn") %>%
      transmute(
        province,
        year = as.integer(year),
        effort_record = as.numeric(report_count),
        effort_observer = NA_real_
      )
  }

  effort_panel %>%
    filter(!is.na(province), year >= cfg$year_min, year <= cfg$year_max)
}

build_species_year_native_panel <- function(native_tbl) {
  native_tbl %>%
    transmute(
      species,
      year = as.integer(year),
      temp_native_anom = resident_breeding_range_temp_delta_from_baseline,
      prec_native_anom = resident_breeding_range_prec_delta_from_baseline
    ) %>%
    filter(!is.na(temp_native_anom))
}

build_province_year_climate_panel <- function(event_tbl, effort_py) {
  province_year_observed <- event_tbl %>%
    transmute(
      province,
      year = as.integer(year),
      temp_anom = point_temp_delta_from_point_baseline,
      prec_anom = point_prec_delta_from_point_baseline
    ) %>%
    group_by(province, year) %>%
    summarise(
      temp_anom = mean(temp_anom, na.rm = TRUE),
      prec_anom = mean(prec_anom, na.rm = TRUE),
      .groups = "drop"
    )

  tidyr::crossing(
    province = sort(unique(effort_py$province)),
    year = sort(unique(effort_py$year))
  ) %>%
    left_join(province_year_observed, by = c("province", "year")) %>%
    group_by(year) %>%
    mutate(
      year_temp_mean = mean(temp_anom, na.rm = TRUE),
      year_prec_mean = mean(prec_anom, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(province) %>%
    mutate(
      province_temp_mean = mean(temp_anom, na.rm = TRUE),
      province_prec_mean = mean(prec_anom, na.rm = TRUE),
      temp_anom = dplyr::coalesce(temp_anom, province_temp_mean, year_temp_mean, 0),
      prec_anom = dplyr::coalesce(prec_anom, province_prec_mean, year_prec_mean, 0)
    ) %>%
    ungroup() %>%
    select(province, year, temp_anom, prec_anom)
}

build_sdm_candidate_panel <- function(cfg, sdm_birdwatch, sdm_rescue, effort_lookup) {
  bind_rows(sdm_birdwatch, sdm_rescue) %>%
    filter(
      record_type == "potential_province",
      scenario == "current",
      cell_threshold == cfg$cell_threshold
    ) %>%
    rename(province_cn_sdm = province) %>%
    left_join(effort_lookup, by = c("province_cn_sdm" = "province_cn")) %>%
    mutate(
      potential = 1L,
      historical_presence = 0L,
      risk_start_year = cfg$year_min
    ) %>%
    filter(!is.na(province)) %>%
    distinct(species, province, .keep_all = TRUE) %>%
    transmute(
      species,
      province,
      potential,
      historical_presence,
      risk_start_year
    )
}

align_supported_event_set <- function(ndr_raw, sdm_province, native_panel, effort_py) {
  ndr_raw %>%
    semi_join(sdm_province, by = c("species", "province")) %>%
    semi_join(native_panel, by = c("species", "year")) %>%
    semi_join(effort_py, by = c("province", "year"))
}

trace_support_filters <- function(ndr_raw, sdm_province, native_panel, effort_py) {
  after_sdm <- ndr_raw %>%
    semi_join(sdm_province, by = c("species", "province"))

  after_native <- after_sdm %>%
    semi_join(native_panel, by = c("species", "year"))

  after_effort <- after_native %>%
    semi_join(effort_py, by = c("province", "year"))

  tibble(
    stage = c("window", "after_sdm", "after_native", "after_effort"),
    records = c(
      nrow(ndr_raw),
      nrow(after_sdm),
      nrow(after_native),
      nrow(after_effort)
    ),
    species = c(
      n_distinct(ndr_raw$species),
      n_distinct(after_sdm$species),
      n_distinct(after_native$species),
      n_distinct(after_effort$species)
    )
  )
}

build_complete_case_model_data <- function(risk_data) {
  risk_data %>%
    filter(
      !is.na(temp_grad_z),
      !is.na(log_effort_record_z)
    ) %>%
    mutate(
      species = factor(species),
      province = factor(province),
      year_f = factor(year)
    )
}

fit_main_hazard_models <- function(model_data) {
  model_specs <- build_model_specs(
    model_data,
    include_precip = FALSE,
    include_observer_effort = FALSE
  )

  fitted_models <- purrr::imap(
    model_specs,
    ~ if (is.null(.x)) {
      list(
        name = .y,
        formula = NA_character_,
        status = "skipped",
        engine = NA_character_,
        nobs = 0L,
        model = NULL,
        error = "Required predictors are unavailable."
      )
    } else {
      fit_discrete_model(make_formula("event", .x), model_data, model_name = .y, engine = "auto")
    }
  )

  model_summary <- bind_rows(map(fitted_models, extract_model_summary))
  if (any(is.finite(model_summary$aic))) {
    model_summary <- model_summary %>%
      mutate(delta_aic = aic - min(aic, na.rm = TRUE))
  } else {
    model_summary <- model_summary %>%
      mutate(delta_aic = NA_real_)
  }

  coefficient_table <- bind_rows(map(fitted_models, extract_coef_table))

  list(
    fitted_models = fitted_models,
    model_summary = model_summary,
    coefficient_table = coefficient_table
  )
}

write_pipeline_summary <- function(cfg, out_dirs, coverage_tbl, model_summary) {
  best_model <- model_summary %>%
    filter(status == "ok") %>%
    slice_min(order_by = aic, n = 1, with_ties = FALSE)

  lines <- c(
    "# Combined SDM Threshold Hazard Test",
    "",
    "## Core settings",
    paste0("- threshold: ", cfg$cell_threshold),
    paste0("- years: ", cfg$year_min, " to ", cfg$year_max),
    "- model specification: temperature + record effort + temperature x record effort",
    "- precipitation excluded from the current main model",
    "- observer effort excluded from the current main model",
    "",
    "## Data coverage"
  )

  lines <- c(lines, paste0("- ", coverage_tbl$metric, ": ", coverage_tbl$value))

  if (nrow(best_model) == 1) {
    lines <- c(
      lines,
      "",
      "## Best model",
      paste0("- best model by AIC: ", best_model$model[[1]]),
      paste0("- AIC: ", round(best_model$aic[[1]], 3))
    )
  }

  writeLines(lines, con = file.path(out_dirs$logs, "run_summary.md"))
}

args <- commandArgs(trailingOnly = TRUE)
cell_threshold <- if (length(args) >= 1) as.integer(args[[1]]) else 100L
year_min <- if (length(args) >= 2) as.integer(args[[2]]) else 2002L
year_max <- if (length(args) >= 3) as.integer(args[[3]]) else 2024L

cfg <- get_runtime_config(task_root, cell_threshold = cell_threshold, year_min = year_min, year_max = year_max)
output_root <- file.path(task_root, paste0("combined_threshold_", cfg$cell_threshold, "_test"))
out_dirs <- ensure_output_tree(output_root)

# -------------------------------
# Step 1. Read inputs and record path manifest
# 第 1 步：读取输入并记录路径清单
# -------------------------------
step_message(1, "读取输入文件并记录路径", "Read runtime inputs and write path manifest")
write_input_manifest(cfg, file.path(out_dirs$results, "input_path_manifest.csv"))
core_inputs <- read_core_inputs(cfg)

# -------------------------------
# Step 2. Build clean analysis panels
# 第 2 步：构建 clean analysis panels
# -------------------------------
step_message(2, "构建 effort、气候和 SDM 面板", "Build effort, climate, and SDM panels")
effort_py <- build_effort_panel(cfg, core_inputs$effort_lookup)
species_year_native <- build_species_year_native_panel(core_inputs$native_climate)
province_year_climate <- build_province_year_climate_panel(core_inputs$event_table, effort_py)
sdm_province <- build_sdm_candidate_panel(
  cfg,
  sdm_birdwatch = core_inputs$sdm_birdwatch,
  sdm_rescue = core_inputs$sdm_rescue,
  effort_lookup = core_inputs$effort_lookup
)

# -------------------------------
# Step 3. Align supported events
# 第 3 步：筛选真正可进入风险集的新纪录事件
# -------------------------------
step_message(3, "筛选进入主模型的新纪录事件", "Align supported new-record events")
support_trace <- trace_support_filters(
  ndr_raw = core_inputs$new_records,
  sdm_province = sdm_province,
  native_panel = species_year_native,
  effort_py = effort_py
)
ndr_supported <- align_supported_event_set(
  ndr_raw = core_inputs$new_records,
  sdm_province = sdm_province,
  native_panel = species_year_native,
  effort_py = effort_py
)

sdm_province <- sdm_province %>%
  semi_join(distinct(ndr_supported, species), by = "species")

species_trait <- distinct(ndr_supported, species)

write_csv(ndr_supported, file.path(out_dirs$derived_inputs, "ndr_supported.csv"))
write_csv(support_trace, file.path(out_dirs$diagnostics, "support_filter_trace.csv"))
write_csv(sdm_province, file.path(out_dirs$derived_inputs, "sdm_province.csv"))
write_csv(province_year_climate, file.path(out_dirs$derived_inputs, "province_year_climate.csv"))
write_csv(species_year_native, file.path(out_dirs$derived_inputs, "species_year_native_climate.csv"))
write_csv(effort_py, file.path(out_dirs$derived_inputs, "effort_py.csv"))
write_csv(species_trait, file.path(out_dirs$derived_inputs, "species_trait.csv"))

# -------------------------------
# Step 4. Build the SDM-constrained risk set
# 第 4 步：构建 SDM 约束的风险集
# -------------------------------
step_message(4, "构建风险集", "Build the SDM-constrained risk set")
ndr_first <- build_first_event_table(ndr_supported)
candidate_tbl <- build_candidate_table(sdm_province)

risk_data <- expand_risk_set(candidate_tbl, ndr_first, year_min = cfg$year_min, year_max = cfg$year_max) %>%
  left_join(province_year_climate, by = c("province", "year")) %>%
  left_join(species_year_native, by = c("species", "year")) %>%
  left_join(effort_py, by = c("province", "year")) %>%
  left_join(species_trait, by = "species") %>%
  prepare_model_data()

write_csv(risk_data, file.path(out_dirs$data_clean, "hazard_risk_full.csv"))

# -------------------------------
# Step 5. Prepare model-ready complete cases
# 第 5 步：整理主模型完整案例数据
# -------------------------------
step_message(5, "整理主模型完整案例", "Prepare complete-case data for hazard models")
complete_case <- build_complete_case_model_data(risk_data)
write_csv(complete_case, file.path(out_dirs$data_clean, "hazard_risk_complete_case.csv"))
write_csv(summarise_event_distribution(risk_data), file.path(out_dirs$diagnostics, "risk_event_distribution_by_year.csv"))
write_csv(
  summarise_predictor_distribution(complete_case),
  file.path(out_dirs$diagnostics, "predictor_distribution_complete_case.csv")
)

coverage_tbl <- tibble(
  metric = c(
    "cell_threshold",
    "new_records_window",
    "new_records_supported",
    "species_window",
    "species_supported",
    "sdm_species_combined",
    "risk_rows_full",
    "risk_rows_complete_case",
    "events_complete_case"
  ),
  value = c(
    cfg$cell_threshold,
    nrow(core_inputs$new_records),
    nrow(ndr_supported),
    n_distinct(core_inputs$new_records$species),
    n_distinct(ndr_supported$species),
    n_distinct(sdm_province$species),
    nrow(risk_data),
    nrow(complete_case),
    sum(complete_case$event, na.rm = TRUE)
  )
)
write_csv(coverage_tbl, file.path(out_dirs$results, "input_coverage_summary.csv"))

# -------------------------------
# Step 6. Fit staged hazard models
# 第 6 步：按科学问题分层拟合 hazard 模型
# -------------------------------
step_message(6, "拟合分层 hazard 模型", "Fit staged discrete-time hazard models")
fit_out <- fit_main_hazard_models(complete_case)
write_csv(fit_out$model_summary, file.path(out_dirs$results, "model_comparison.csv"))
write_csv(fit_out$coefficient_table, file.path(out_dirs$results, "model_coefficients.csv"))

# -------------------------------
# Step 7. Export diagnostics and logs
# 第 7 步：导出诊断和运行日志
# -------------------------------
step_message(7, "导出诊断与日志", "Export diagnostics and run logs")
write_csv(
  fit_out$model_summary %>% select(model, engine, status, formula, nobs, aic, bic, logLik, delta_aic, error),
  file.path(out_dirs$diagnostics, "model_fit_registry.csv")
)
write_csv(
  summarise_model_diagnostics(fit_out$fitted_models),
  file.path(out_dirs$diagnostics, "model_diagnostics.csv")
)
write_pipeline_summary(cfg, out_dirs, coverage_tbl, fit_out$model_summary)

message("\nPipeline completed successfully for threshold = ", cfg$cell_threshold)
