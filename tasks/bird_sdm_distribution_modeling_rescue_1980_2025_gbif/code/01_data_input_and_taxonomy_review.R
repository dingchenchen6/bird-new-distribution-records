#!/usr/bin/env Rscript

# ============================================================
# Scientific Question | 科学问题
# ------------------------------------------------------------
# English:
# For the remaining new-record bird species that were not successfully modeled
# in the previous workflow, which China Birdwatch records from 1980 to 2025
# and which taxonomy links can be reliably recovered after synonym review and
# species-name checking?
#
# 中文：
# 对此前未成功建模的新纪录鸟种，在完成分类统一、同物异名核查和学名审查后，
# 1980-2025 年中国观鸟记录中哪些记录能够被可靠恢复并对应到这些目标物种？
#
# Goal | 目标
# ------------------------------------------------------------
# English:
# Build a transparent rescue-species pool and taxonomy-review table for the
# expanded China Birdwatch plus GBIF rescue workflow without overwriting the
# earlier SDM tasks.
#
# 中文：
# 为扩展到中国观鸟 1980-2025 与 GBIF 的补救工作流构建可追溯的目标物种池
# 和分类校审表，且不覆盖之前的 SDM 任务。
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
})

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

result <- run_pipeline(prepare_only = TRUE, workers = 1L)

safe_message("Step 1 completed.")
safe_message("Rescue target species pool: ", result$summary$new_species_total)
safe_message("Matched species ready for modeling: ", result$summary$matched_ready)
safe_message("Unmatched species requiring review: ", result$summary$unmatched_species)
safe_message("Taxonomy review table: ", MATCH_TABLE_PATH)
safe_message("Rescue target table: ", RESCUE_TARGET_PATH)
safe_message("Birdwatch processing summary: ", BIRDWATCH_PROCESSING_SUMMARY_PATH)
safe_message("Occurrence source summary: ", OCCURRENCE_SOURCE_SUMMARY_PATH)
