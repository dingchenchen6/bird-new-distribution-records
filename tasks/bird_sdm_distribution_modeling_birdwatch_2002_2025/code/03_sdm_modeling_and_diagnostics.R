#!/usr/bin/env Rscript

# ============================================================
# Scientific Question | 科学问题
# ------------------------------------------------------------
# English:
# When China's new-record bird species are modeled only with cleaned China
# Birdwatch observations from 2002 to 2025, what current and future potential
# distributions emerge across Chinese provinces?
#
# 中文：
# 当仅使用 2002-2025 年中国观鸟清洗记录来建模时，中国鸟类新纪录物种在
# 中国各省会呈现怎样的当前和未来潜在分布格局？
#
# Goal | 目标
# ------------------------------------------------------------
# English:
# Run the end-to-end SDM workflow, evaluate model quality, export rasters,
# overlay cleaned modeling points on species maps, and summarize province-level
# suitable area and proportion for every successfully modeled species.
#
# 中文：
# 运行端到端 SDM 流程，评估模型质量，导出栅格结果，在物种地图中叠加清洗后
# 的建模点，并汇总每个成功建模物种在各潜在省份中的适生面积与比例。
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
safe_message("Points-used table: ", POINTS_USED_ALL_PATH)
safe_message("Province listing table: ", PROVINCE_POTENTIAL_LIST_PATH)
safe_message("Raster manifest table: ", RASTER_MANIFEST_PATH)
