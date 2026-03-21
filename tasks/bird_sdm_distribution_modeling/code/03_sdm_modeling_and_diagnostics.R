#!/usr/bin/env Rscript

# ============================================================
# Scientific Question | 科学问题
# ------------------------------------------------------------
# English:
# How do the matched new-record bird species respond to current and future
# climate conditions in China when modeled under a unified SDM workflow?
#
# 中文：
# 在统一的 SDM 工作流下，这些已完成匹配的新纪录鸟类物种在中国范围内
# 对当前与未来气候条件会呈现怎样的潜在分布响应？
#
# Goal | 目标
# ------------------------------------------------------------
# English:
# Run SDMs, evaluate model performance, generate probability/binary rasters,
# draw species maps, and summarize province-level potential distributions.
#
# 中文：
# 运行 SDM、评估模型表现、输出概率/二值栅格、绘制物种分布图，并汇总
# 省级潜在分布结果。
# ============================================================

args <- commandArgs(trailingOnly = TRUE)
force_download <- any(args == "--force-download")
current_only <- any(args == "--current-only")
selected_species_arg <- args[grepl("^--species=", args)]
species_limit_arg <- args[grepl("^--species-limit=", args)]
worker_arg <- args[grepl("^--workers=", args)]

selected_species <- if (length(selected_species_arg)) sub("^--species=", "", selected_species_arg[[1]]) else NA_character_
species_limit <- if (length(species_limit_arg)) as.integer(sub("^--species-limit=", "", species_limit_arg[[1]])) else NA_integer_
workers <- if (length(worker_arg)) as.integer(sub("^--workers=", "", worker_arg[[1]])) else 1L

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

result <- run_pipeline(
  prepare_only = FALSE,
  selected_species = selected_species,
  species_limit = species_limit,
  force_download = force_download,
  current_only = current_only,
  workers = workers
)

safe_message("Step 3 completed.")
safe_message("Successfully modeled species: ", result$summary$modeled_species)
safe_message("Species status table: ", SPECIES_STATUS_PATH)
safe_message("Province listing table: ", PROVINCE_POTENTIAL_LIST_PATH)
safe_message("Raster manifest table: ", RASTER_MANIFEST_PATH)
