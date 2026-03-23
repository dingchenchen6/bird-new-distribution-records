#!/usr/bin/env Rscript

# ============================================================
# Scientific Question | 科学问题
# ------------------------------------------------------------
# English:
# Which China Birdwatch records from 2002 to 2025 can be reliably linked to
# China's bird new-record species after taxonomy harmonization, synonym review,
# and species-name checking?
#
# 中文：
# 在完成分类统一、同物异名核查和学名审查后，2002-2025 年中国观鸟记录中
# 哪些记录能够可靠对应到中国鸟类新纪录物种？
#
# Goal | 目标
# ------------------------------------------------------------
# English:
# Build a transparent species pool and taxonomy-review table for the birdwatch-
# based SDM workflow without overwriting the earlier SDM task.
#
# 中文：
# 为基于中国观鸟数据集的新 SDM 工作流构建可追溯的物种池和分类校审表，
# 且不覆盖之前的 SDM 任务。
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
})

Sys.setenv(BIRD_SDM_SKIP_AUTORUN = "1")
Sys.setenv(BIRD_SDM_OCCURRENCE_SOURCE = "birdwatch_raw")

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(gsub("~\\+~", " ", sub("^--file=", "", file_arg[[1]]))))
} else {
  normalizePath("code")
}

source(file.path(script_dir, "run_bird_sdm_distribution_modeling_birdwatch_2002_2025.R"))

result <- run_pipeline(prepare_only = TRUE, workers = 1L)

safe_message("Step 1 completed.")
safe_message("New-record species pool: ", result$summary$new_species_total)
safe_message("Matched species ready for modeling: ", result$summary$matched_ready)
safe_message("Unmatched species requiring review: ", result$summary$unmatched_species)
safe_message("Taxonomy review table: ", MATCH_TABLE_PATH)
safe_message("Birdwatch processing summary: ", BIRDWATCH_PROCESSING_SUMMARY_PATH)
