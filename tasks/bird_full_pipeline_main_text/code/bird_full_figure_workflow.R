#!/usr/bin/env Rscript

# ============================================================
# Bird new-record full workflow runner
# 鸟类新纪录全流程总控脚本（数据整理 + 分析 + 全图表导出）
#
# This master script runs all major figure/table pipelines in sequence:
# 1) Main analytical pipeline (descriptive + exploratory figures)
# 2) Order-level summary table for supplementary-style reporting
# 3) Reference-style figure set (Sankey/Chord/Phylogeny/Radar/Projection map)
#
# 本脚本用于一键串行运行三套核心脚本，确保可复现并批量导出成果。
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(readr)
})

project_root <- "/Users/dingchenchen/Documents/New project"
output_root <- file.path(project_root, "bird_new_records_R_output")
fig_dir <- file.path(output_root, "figures")
tab_dir <- file.path(output_root, "tables")

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

scripts_to_run <- c(
  file.path(project_root, "bird_new_records_pipeline.R"),
  file.path(project_root, "make_bird_order_summary_table.R"),
  file.path(project_root, "bird_reference_style_figures.R")
)

run_script <- function(script_path) {
  if (!file.exists(script_path)) {
    stop("Script not found: ", script_path)
  }
  message("Running: ", script_path)
  source(script_path, local = new.env(parent = globalenv()))
  message("Finished: ", script_path)
}

start_time <- Sys.time()

for (s in scripts_to_run) {
  run_script(s)
}

end_time <- Sys.time()

# -------------------------------
# Build a compact export index
# 生成图表与表格的导出清单，便于投稿时快速定位文件
# -------------------------------
fig_files <- list.files(fig_dir, pattern = "\\.(png|pdf)$", full.names = TRUE, ignore.case = TRUE)
tab_files <- list.files(tab_dir, pattern = "\\.(csv|xlsx|md)$", full.names = TRUE, ignore.case = TRUE)

fig_index <- tibble(
  type = "figure",
  file = fig_files,
  file_name = basename(fig_files),
  size_kb = round(file.info(fig_files)$size / 1024, 1)
)

tab_index <- tibble(
  type = "table",
  file = tab_files,
  file_name = basename(tab_files),
  size_kb = round(file.info(tab_files)$size / 1024, 1)
)

export_index <- bind_rows(fig_index, tab_index) %>%
  arrange(type, file_name)

readr::write_csv(export_index, file.path(output_root, "export_file_index.csv"))

readr::write_lines(
  c(
    "# Bird new-record full workflow log",
    "",
    paste0("- Start: ", format(start_time, "%Y-%m-%d %H:%M:%S %Z")),
    paste0("- End: ", format(end_time, "%Y-%m-%d %H:%M:%S %Z")),
    paste0("- Duration (mins): ", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)),
    "",
    "## Scripts executed",
    paste0("- ", scripts_to_run),
    "",
    paste0("## Figure outputs: ", nrow(fig_index)),
    paste0("## Table outputs: ", nrow(tab_index)),
    "",
    "## Notes",
    "- If a formal publication requires a legally certified China standard map with 审图号,",
    "  place the vector file under:",
    "  /Users/dingchenchen/Documents/New project/bird_new_records_R_output/data_clean/china_standard_map/",
    "  then rerun this script."
  ),
  file.path(output_root, "FULL_WORKFLOW_LOG.md")
)

cat("Full bird workflow completed.\n")
cat("Output root:", output_root, "\n")
