#!/usr/bin/env Rscript

# ============================================================
# Master runner for bird identity/synonym and duplicate-based
# corrected reanalysis
# 鸟类同物异名与重复记录校正重分析总控脚本
# ============================================================
#
# Purpose / 用途
# This script orchestrates the full corrected reanalysis without touching the
# original task outputs. It first rebuilds the canonical corrected base table,
# then reruns the major downstream analyses into task-specific subfolders.
# 本脚本用于在不覆盖旧版任务输出的前提下，统一调度本轮校正重分析：
# 先重建标准化底表，再把主要下游分析重跑到独立子任务文件夹中。
# ============================================================

suppressPackageStartupMessages({
  library(fs)
})

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    candidate <- sub("^--file=", "", file_arg[1])
    if (file.exists(candidate)) return(normalizePath(candidate))
  }
  normalizePath(getwd())
}

script_path <- get_script_path()
code_dir <- if (dir.exists(script_path)) script_path else dirname(script_path)
task_root <- Sys.getenv(
  "BIRD_REANALYSIS_TASK_ROOT",
  unset = "/Users/dingchenchen/Documents/New records/bird-new-distribution-records/tasks/bird_identity_synonym_dedup_reanalysis"
)
repo_tasks_root <- normalizePath(file.path(task_root, ".."), mustWork = FALSE)

dir_create(file.path(task_root, "results"))

run_script <- function(script, env = character()) {
  message("Running: ", script)
  status <- system2("Rscript", script, env = env)
  if (!identical(status, 0L)) {
    stop("Script failed: ", script, " (exit status ", status, ")")
  }
}

base_script <- file.path(code_dir, "01_build_corrected_canonical_dataset.R")
order_script <- file.path(code_dir, "02_make_corrected_order_summary.R")
sankey_script <- file.path(code_dir, "03_make_corrected_sankey.R")
directional_script <- file.path(repo_tasks_root, "bird_directional_windrose_radar", "code", "make_bird_directional_task_bundle.R")
spatiotemporal_script <- file.path(repo_tasks_root, "bird_spatiotemporal_patterns", "code", "make_bird_spatiotemporal_patterns.R")
geb_script <- file.path(repo_tasks_root, "bird_geb_fig3_fig4_effort_analysis", "code", "run_bird_geb_fig3_fig4_effort_analysis_v2_phylo.R")

corrected_clean_csv <- file.path(task_root, "data", "bird_new_records_clean_corrected.csv")
corrected_traits_csv <- file.path(task_root, "data", "bird_species_pool_with_traits_corrected.csv")
shared_shape_dir <- file.path(repo_tasks_root, "bird_spatiotemporal_patterns", "data", "shapefile_base")

run_script(base_script)
run_script(order_script)
run_script(sankey_script)

run_script(
  directional_script,
  env = c(
    paste0("BIRD_CLEAN_PATH=", corrected_clean_csv),
    paste0("BIRD_TRAITS_PATH=", corrected_traits_csv),
    paste0("BIRD_TASK_DIR=", file.path(task_root, "directional_corrected"))
  )
)

run_script(
  spatiotemporal_script,
  env = c(
    paste0("BIRD_CLEAN_PATH=", corrected_clean_csv),
    paste0("BIRD_TASK_DIR=", file.path(task_root, "spatiotemporal_corrected")),
    paste0("BIRD_SHAPE_DIR=", shared_shape_dir)
  )
)

run_script(
  geb_script,
  env = c(
    paste0("BIRD_TASK_DIR=", file.path(task_root, "geb_fig3_fig4_corrected")),
    paste0("BIRD_CORRECTED_EVENTS_CSV=", corrected_clean_csv)
  )
)

writeLines(
  c(
    "# Corrected reanalysis run log / 校正重分析运行日志",
    "",
    "- Canonical corrected dataset: completed",
    "- Order summary reanalysis: completed",
    "- Sankey reanalysis: completed",
    "- Directional reanalysis: completed",
    "- Spatiotemporal reanalysis: completed",
    "- GEB Fig. 3 / Fig. 4 reanalysis: completed"
  ),
  file.path(task_root, "results", "run_log.md")
)

cat("All corrected reanalysis modules finished successfully.\n")
