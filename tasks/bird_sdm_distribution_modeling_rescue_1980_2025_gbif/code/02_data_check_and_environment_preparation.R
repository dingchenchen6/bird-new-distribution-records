#!/usr/bin/env Rscript

# ============================================================
# Scientific Question | 科学问题
# ------------------------------------------------------------
# English:
# After linking the remaining rescue-target new-record bird species to the
# expanded China Birdwatch observations from 1980 to 2025 plus GBIF records,
# are the cleaned occurrence points and China-only climate predictors ready for
# robust SDM fitting?
#
# 中文：
# 当待补救的新纪录鸟类物种与 1980-2025 年中国观鸟记录及 GBIF 记录完成匹配后，
# 清洗后的出现点和中国范围气候变量是否已经准备好，可用于稳健的 SDM 建模？
#
# Goal | 目标
# ------------------------------------------------------------
# English:
# Perform a full pre-modeling check for the rescue workflow, including
# occurrence readiness, climate/elevation preparation, algorithm availability,
# and predictor screening.
#
# 中文：
# 为补救版工作流完成建模前检查，包括出现点准备情况、气候与海拔变量准备、
# 算法可用性以及环境变量筛选。
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
force_download <- any(args == "--force-download")

Sys.setenv(BIRD_SDM_SKIP_AUTORUN = "1")
Sys.setenv(BIRD_SDM_OCCURRENCE_SOURCE = "combined")

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(gsub("~\\+~", " ", sub("^--file=", "", file_arg[[1]]))))
} else {
  normalizePath("code")
}

source(file.path(script_dir, "run_bird_sdm_distribution_modeling_rescue_1980_2025_gbif.R"))

manual_overrides <- create_default_manual_overrides(MANUAL_OVERRIDE_PATH)
scenario_config <- create_default_scenario_config(SCENARIO_CONFIG_PATH)
model_config <- create_default_model_config(MODEL_CONFIG_PATH)
algo_tbl <- collect_algorithm_availability()
readr::write_csv(algo_tbl, ALGO_AVAILABILITY_PATH)

src <- load_new_record_data()
previous_status <- load_previous_species_status()
provinces <- load_province_boundaries()
china_boundary <- build_china_boundary(provinces)
species_master <- build_rescue_species_master(src$new_records, src$species_pool, previous_status)
readr::write_csv(species_master %>% select(-species_key, -genus, -epithet), MASTER_SPECIES_PATH)
readr::write_csv(species_master %>% select(-species_key, -genus, -epithet), RESCUE_TARGET_PATH)

occurrence_birds <- load_occurrence_points(china_boundary, species_master = species_master, overrides_tbl = manual_overrides, provinces = provinces)
match_tbl <- build_taxonomy_review_table(species_master, occurrence_birds, manual_overrides)
readr::write_csv(match_tbl, MATCH_TABLE_PATH)
readr::write_csv(match_tbl %>% filter(!.data$model_ready), UNMATCHED_PATH)

current_config <- scenario_config %>% filter(.data$scenario == "current", .data$enabled)
current_stack <- load_environment_stack(current_config, china_boundary, force_download = force_download)
if (is.null(current_stack)) {
  stop("Current climate stack is unavailable. Re-run with --force-download if needed.", call. = FALSE)
}

env_selection <- choose_environment_variables(
  current_stack,
  cutoff = as.numeric(get_config_value(model_config, "correlation_cutoff", "0.7"))
)
readr::write_csv(env_selection, ENV_SELECTION_PATH)

qa_tbl <- bind_rows(
  log_qa("setup", "task_root", "ok", TASK_ROOT),
  log_qa("input", "rescue_occurrence_records", "ok", as.character(nrow(occurrence_birds))),
  log_qa("climate", "current", "ok", "Current WorldClim bioclim + elevation stack is readable.")
)
readr::write_csv(qa_tbl, QA_LOG_PATH)

summary_stats <- list(
  new_species_total = nrow(species_master),
  matched_ready = sum(match_tbl$model_ready, na.rm = TRUE),
  modeled_species = 0L,
  unmatched_species = sum(!match_tbl$model_ready, na.rm = TRUE)
)

write_task_summary(
  summary_stats,
  algo_tbl,
  note_lines = c(
    "Step 2 completed for the rescue workflow based on China Birdwatch 1980-2025 plus GBIF.",
    "Review table_environment_variable_selection.csv before launching the full SDM run."
  )
)

safe_message("Step 2 completed.")
safe_message("Rescue occurrence points retained in China: ", nrow(occurrence_birds))
safe_message("Rescue target species ready for modeling: ", summary_stats$matched_ready)
safe_message("Selected predictors: ", paste(env_selection$variable[env_selection$keep], collapse = ", "))
safe_message("Occurrence source summary: ", OCCURRENCE_SOURCE_SUMMARY_PATH)
