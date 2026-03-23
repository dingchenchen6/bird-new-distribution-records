#!/usr/bin/env Rscript

# ============================================================
# Scientific Question | 科学问题
# ------------------------------------------------------------
# English:
# Which new-record bird species in China can be linked to the available
# occurrence records after taxonomy review, synonym checking, and species-name
# harmonization?
#
# 中文：
# 在完成分类校审、同物异名核查和学名统一后，哪些中国鸟类新纪录物种
# 能够与现有出现点数据建立可靠对应关系？
#
# Goal | 目标
# ------------------------------------------------------------
# English:
# Build the modeling species pool and create a transparent taxonomy-review
# table that records direct matches, manual overrides, and unmatched species.
#
# 中文：
# 构建建模物种池，并生成可追溯的分类校审表，明确记录直接匹配、
# 人工校正与未匹配物种。
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
if (any(args == "--rgbif")) {
  Sys.setenv(BIRD_SDM_OCCURRENCE_SOURCE = "rgbif")
}

Sys.setenv(BIRD_SDM_SKIP_AUTORUN = "1")

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  normalizePath("code")
}

source(file.path(script_dir, "run_bird_sdm_distribution_modeling.R"))

result <- run_pipeline(prepare_only = TRUE, workers = 1L)

safe_message("Step 1 completed.")
safe_message("New-record species pool: ", result$summary$new_species_total)
safe_message("Matched species ready for modeling: ", result$summary$matched_ready)
safe_message("Unmatched species requiring review: ", result$summary$unmatched_species)
safe_message("Taxonomy review table: ", MATCH_TABLE_PATH)
safe_message("Unmatched species table: ", UNMATCHED_PATH)
