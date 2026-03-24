#!/usr/bin/env Rscript

# ============================================================
# Scientific Question | 科学问题
# ------------------------------------------------------------
# English:
# For bird species that were not successfully modeled in the earlier
# China Birdwatch SDM task, can expanded 1980-2025 China Birdwatch
# records together with additional GBIF occurrence records recover
# enough information for renewed SDM fitting and map production?
#
# 中文：
# 对于上一轮中国观鸟数据 SDM 任务中尚未成功建模出图的中国鸟类新纪录
# 物种，若补充使用 1980-2025 年中国观鸟记录以及 GBIF 记录点，是否可以
# 恢复更多物种的建模与出图，并重新评估其在中国范围内的潜在适生分布？
#
# Research Goals | 研究目标
# ------------------------------------------------------------
# English:
# 1. 仅针对上一轮未成功建模出图的新纪录物种建立补救建模物种池。
# 2. 整合 1980-2025 年中国观鸟记录与 GBIF 记录点作为新的出现点来源。
# 3. 再次执行 Avibase/IOC 和 BirdLife 支持的同物异名与分类审查。
# 4. 在中国范围内重新开展 SDM 建模、模型诊断和省级潜在分布汇总。
#
# 中文：
# 1. 严格以鸟类新纪录物种名单构建建模物种池。
# 2. 仅使用“物种分布.shp”中的出现点作为 SDM 输入。
# 3. 以 Avibase/IOC 和 BirdLife 审查字段规范同物异名与分类校正。
# 4. 在中国范围内开展 SDM 建模、模型诊断，并在多个适生栅格阈值下
#    汇总潜在分布省份。
#
# Workflow Framework | 研究思路框架
# ------------------------------------------------------------
# English:
# Step A. Input data and taxonomy review
# Step B. Occurrence cleaning and China-range filtering
# Step C. Climate/elevation preparation and variable screening
# Step D. SDM fitting, validation, and prediction
# Step E. Province sensitivity analysis, maps, and summary tables
#
# 中文：
# 步骤 A：剩余目标物种识别与分类审查
# 步骤 B：1980-2025 观鸟记录与 GBIF 出现点整合、清洗及中国范围筛选
# 步骤 C：气候/海拔变量准备与变量筛选
# 步骤 D：SDM 建模、验证与预测
# 步骤 E：省级敏感性分析、地图输出与结果汇总
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(sf)
  library(terra)
  library(ggplot2)
  library(pROC)
  library(geodata)
  library(dismo)
  library(mgcv)
  library(officer)
})

select <- dplyr::select
rename <- dplyr::rename

set.seed(20260319)
options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalize_path_slashes <- function(path) {
  gsub("\\\\", "/", as.character(path))
}

make_repo_relative_path <- function(path) {
  values <- as.character(path)
  vapply(values, function(one_path) {
    if (is.na(one_path) || !nzchar(one_path)) {
      return(one_path)
    }
    normalized_path <- tryCatch(
      normalize_path_slashes(normalizePath(one_path, mustWork = FALSE)),
      error = function(e) normalize_path_slashes(one_path)
    )
    project_root_normalized <- normalize_path_slashes(normalizePath(PROJECT_ROOT, mustWork = FALSE))
    task_root_normalized <- normalize_path_slashes(normalizePath(TASK_ROOT, mustWork = FALSE))
    repo_marker <- "/bird-new-distribution-records/"
    if (startsWith(normalized_path, paste0(project_root_normalized, "/"))) {
      return(substring(normalized_path, nchar(project_root_normalized) + 2L))
    }
    if (startsWith(normalized_path, paste0(task_root_normalized, "/"))) {
      return(file.path("tasks", "bird_sdm_distribution_modeling_rescue_1980_2025_gbif", substring(normalized_path, nchar(task_root_normalized) + 2L)) %>% normalize_path_slashes())
    }
    if (grepl(repo_marker, normalized_path, fixed = TRUE)) {
      return(sub("^.*/bird-new-distribution-records/", "", normalized_path))
    }
    normalized_path
  }, character(1))
}

sanitize_log_detail <- function(detail) {
  values <- as.character(detail)
  path_like <- !is.na(values) & grepl("(/|\\\\)", values)
  values[path_like] <- make_repo_relative_path(values[path_like])
  values
}

safe_message <- function(...) {
  cat(paste0(..., "\n"))
}

resolve_existing_dir <- function(candidates) {
  existing <- candidates[dir.exists(candidates)]
  if (length(existing) > 0) existing[[1]] else candidates[[1]]
}

WORKER_CURRENT_STACK <- NULL
WORKER_FUTURE_STACKS <- NULL

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

build_china_boundary <- function(provinces) {
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  provinces %>%
    st_make_valid() %>%
    st_union() %>%
    st_make_valid() %>%
    st_as_sf()
}

script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    script_file <- sub("^--file=", "", file_arg[[1]])
    script_file <- gsub("~\\+~", " ", script_file)
    return(normalizePath(script_file))
  }
  this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile), error = function(e) NULL)
  this_file %||% normalizePath(".")
}

normalize_species_name <- function(x) {
  x %>%
    str_replace_all("\\[|\\]", "") %>%
    str_replace_all("[^A-Za-z ]", " ") %>%
    str_squish() %>%
    str_to_sentence(locale = "en")
}

normalize_species_key <- function(x) {
  normalize_species_name(x) %>% str_to_lower()
}

normalize_admin_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("\\s+", "") %>%
    stringr::str_squish()
}

simplify_province_name <- function(x) {
  normalize_admin_name(x) %>%
    stringr::str_replace("(壮族自治区|回族自治区|维吾尔自治区|自治区|特别行政区|省|市)$", "")
}

extract_genus <- function(x) {
  str_split(normalize_species_name(x), " ", simplify = TRUE)[, 1] %>% str_squish()
}

extract_epithet <- function(x) {
  tokens <- str_split(normalize_species_name(x), " ", simplify = TRUE)
  if (ncol(tokens) < 2) {
    return(rep(NA_character_, nrow(tokens)))
  }
  str_squish(tokens[, 2])
}

args <- commandArgs(trailingOnly = TRUE)
prepare_only <- any(args == "--prepare-only")
force_download <- any(args == "--force-download")
selected_species_arg <- args[grepl("^--species=", args)]
species_limit_arg <- args[grepl("^--species-limit=", args)]
worker_arg <- args[grepl("^--workers=", args)]
selected_species <- if (length(selected_species_arg)) sub("^--species=", "", selected_species_arg[[1]]) else NA_character_
species_limit <- if (length(species_limit_arg)) as.integer(sub("^--species-limit=", "", species_limit_arg[[1]])) else NA_integer_
workers <- if (length(worker_arg)) as.integer(sub("^--workers=", "", worker_arg[[1]])) else 1L
current_only <- any(args == "--current-only")

SCRIPT_FILE <- script_path()
TASK_ROOT <- normalizePath(file.path(dirname(SCRIPT_FILE), ".."), mustWork = FALSE)
CODE_DIR <- file.path(TASK_ROOT, "code")
DATA_DIR <- ensure_dir(file.path(TASK_ROOT, "data"))
FIGURE_DIR <- ensure_dir(file.path(TASK_ROOT, "figures"))
RESULT_DIR <- ensure_dir(file.path(TASK_ROOT, "results"))
CLIMATE_DIR <- ensure_dir(file.path(DATA_DIR, "climate"))
RASTER_DIR <- ensure_dir(file.path(DATA_DIR, "rasters"))
TABLE_DIR <- ensure_dir(file.path(DATA_DIR, "tables"))
FIGURE_MAP_DIR <- ensure_dir(file.path(FIGURE_DIR, "species_maps"))
FIGURE_SUMMARY_DIR <- ensure_dir(file.path(FIGURE_DIR, "summaries"))
OCCURRENCE_INPUT_DIR <- ensure_dir(file.path(DATA_DIR, "occurrence"))
POINTS_USED_DIR <- ensure_dir(file.path(DATA_DIR, "points_used"))

PROJECT_ROOT <- normalizePath(file.path(TASK_ROOT, "..", ".."), mustWork = FALSE)
SOURCE_DATA_DIR <- file.path(PROJECT_ROOT, "source_data")
NEW_RECORD_PATH <- file.path(SOURCE_DATA_DIR, "bird_new_records_clean.csv")
SPECIES_POOL_PATH <- file.path(SOURCE_DATA_DIR, "bird_species_pool_with_traits.csv")
DEFAULT_BIRDWATCH_RAW_DIR <- resolve_existing_dir(c(
  file.path(DATA_DIR, "raw_china_birdwatch_1980_2025", "中国观鸟数据集"),
  file.path(DATA_DIR, "raw_china_birdwatch_1980_2025"),
  file.path(DATA_DIR, "raw_china_birdwatch_2002_2025", "中国观鸟数据集"),
  file.path(DATA_DIR, "raw_china_birdwatch_2002_2025"),
  file.path(PROJECT_ROOT, "tasks", "bird_sdm_distribution_modeling", "data", "中国观鸟数据集", "中国观鸟数据集"),
  file.path(PROJECT_ROOT, "tasks", "bird_sdm_distribution_modeling", "data", "中国观鸟数据集")
))
BIRDWATCH_RAW_DIR <- Sys.getenv("BIRD_SDM_BIRDWATCH_RAW_DIR", unset = DEFAULT_BIRDWATCH_RAW_DIR)
BIRDWATCH_DETAIL_DIR <- file.path(BIRDWATCH_RAW_DIR, "详情数据")
BIRDWATCH_SPECIES_DIR <- file.path(BIRDWATCH_RAW_DIR, "鸟种数据")
BIRDWATCH_REPORT_DIR <- file.path(BIRDWATCH_RAW_DIR, "观鸟统计报告")
BIRDWATCH_YEAR_START <- 1980L
BIRDWATCH_YEAR_END <- 2025L
BIRDWATCH_YEAR_LABEL <- paste0(BIRDWATCH_YEAR_START, "_", BIRDWATCH_YEAR_END)
BIRDWATCH_MERGED_OCCURRENCE_PATH <- file.path(OCCURRENCE_INPUT_DIR, paste0("china_birdwatch_observation_records_", BIRDWATCH_YEAR_LABEL, ".csv.gz"))
BIRDWATCH_PROCESSING_SUMMARY_PATH <- file.path(TABLE_DIR, "table_birdwatch_data_processing_summary.csv")
# Prefer the cleaned birdwatch-derived CSV for reproducible runs. An optional
# shapefile can still be supplied through BIRD_SDM_OCCURRENCE_SHP_PATH.
DEFAULT_OCCURRENCE_CSV_PATH <- file.path(OCCURRENCE_INPUT_DIR, "occurrence_records_clean_standardized.csv")
DEFAULT_OCCURRENCE_SHP_PATH <- file.path(OCCURRENCE_INPUT_DIR, "occurrence_points.shp")
DEFAULT_PROVINCE_SHP_PATH <- file.path(
  PROJECT_ROOT,
  "tasks",
  "bird_spatiotemporal_patterns",
  "data",
  "shapefile_base",
  "省.shp"
)
DEFAULT_DASHLINE_SHP_PATH <- file.path(
  PROJECT_ROOT,
  "tasks",
  "bird_spatiotemporal_patterns",
  "data",
  "shapefile_base",
  "十段线.shp"
)
DEFAULT_SHARED_CLIMATE_CACHE_ROOT <- file.path(
  PROJECT_ROOT,
  "tasks",
  "bird_sdm_distribution_modeling",
  "data",
  "climate"
)
OCCURRENCE_SOURCE <- Sys.getenv("BIRD_SDM_OCCURRENCE_SOURCE", unset = "combined")
OCCURRENCE_CSV_PATH <- Sys.getenv("BIRD_SDM_OCCURRENCE_CSV_PATH", unset = DEFAULT_OCCURRENCE_CSV_PATH)
OCCURRENCE_SHP_PATH <- Sys.getenv("BIRD_SDM_OCCURRENCE_SHP_PATH", unset = DEFAULT_OCCURRENCE_SHP_PATH)
PROVINCE_SHP_PATH <- Sys.getenv("BIRD_SDM_PROVINCE_SHP_PATH", unset = DEFAULT_PROVINCE_SHP_PATH)
DASHLINE_SHP_PATH <- Sys.getenv("BIRD_SDM_DASHLINE_SHP_PATH", unset = DEFAULT_DASHLINE_SHP_PATH)
RGBIF_COUNTRY_FILTER <- Sys.getenv("BIRD_SDM_RGBIF_COUNTRY", unset = "CN")
ALLOW_REMOTE_CLIMATE_DOWNLOAD <- tolower(Sys.getenv("BIRD_SDM_ALLOW_REMOTE_CLIMATE_DOWNLOAD", unset = "false")) %in% c("1", "true", "yes")

MANUAL_OVERRIDE_PATH <- file.path(DATA_DIR, "taxonomy_manual_overrides.csv")
SCENARIO_CONFIG_PATH <- file.path(DATA_DIR, "climate_scenario_config.csv")
MODEL_CONFIG_PATH <- file.path(DATA_DIR, "model_config.csv")
MATCH_TABLE_PATH <- file.path(TABLE_DIR, "table_species_name_review.csv")
MASTER_SPECIES_PATH <- file.path(TABLE_DIR, "table_species_master.csv")
RESCUE_TARGET_PATH <- file.path(TABLE_DIR, "table_rescue_target_species.csv")
ENV_SELECTION_PATH <- file.path(TABLE_DIR, "table_environment_variable_selection.csv")
ALGO_AVAILABILITY_PATH <- file.path(TABLE_DIR, "table_algorithm_availability.csv")
SPECIES_STATUS_PATH <- file.path(TABLE_DIR, "table_species_status_summary.csv")
QA_LOG_PATH <- file.path(TABLE_DIR, "table_qa_log.csv")
UNMATCHED_PATH <- file.path(TABLE_DIR, "table_unmatched_species_list.csv")
PROVINCE_SUMMARY_PATH <- file.path(TABLE_DIR, "table_province_prediction_summary.csv")
PROVINCE_POTENTIAL_LIST_PATH <- file.path(TABLE_DIR, "table_potential_province_listing_all_species.csv")
AREA_CHANGE_PATH <- file.path(TABLE_DIR, "table_scenario_area_change_summary.csv")
METRIC_SUMMARY_PATH <- file.path(TABLE_DIR, "table_model_metrics_summary.csv")
METRIC_FOLD_PATH <- file.path(TABLE_DIR, "table_model_metrics_by_fold.csv")
THRESHOLD_SUMMARY_PATH <- file.path(TABLE_DIR, "table_model_threshold_summary.csv")
RASTER_MANIFEST_PATH <- file.path(TABLE_DIR, "table_raster_output_manifest.csv")
CLIMATE_MANIFEST_PATH <- file.path(TABLE_DIR, "table_climate_data_manifest.csv")
POINTS_USED_ALL_PATH <- file.path(TABLE_DIR, "table_model_occurrence_points_used_all_species.csv")
OCCURRENCE_CLEANING_PATH <- file.path(TABLE_DIR, "table_occurrence_cleaning_steps.csv")
OCCURRENCE_SOURCE_SUMMARY_PATH <- file.path(TABLE_DIR, "table_occurrence_source_summary.csv")
PROGRESS_DIR <- file.path(DATA_DIR, "progress")
PROGRESS_SPECIES_DIR <- file.path(PROGRESS_DIR, "species")
PROGRESS_SUMMARY_PATH <- file.path(PROGRESS_DIR, "progress_summary.csv")
PROGRESS_EVENT_LOG_PATH <- file.path(PROGRESS_DIR, "progress_events.log")
TASK_SUMMARY_PATH <- file.path(RESULT_DIR, "task_summary.md")
PPTX_SUMMARY_PATH <- file.path(RESULT_DIR, "bird_sdm_summary.pptx")
MAP_CRS <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +datum=WGS84 +units=m +no_defs"
PREVIOUS_TASK_ROOT <- file.path(PROJECT_ROOT, "tasks", "bird_sdm_distribution_modeling_birdwatch_2002_2025")
PREVIOUS_STATUS_PATH <- file.path(PREVIOUS_TASK_ROOT, "data", "tables", "table_species_status_summary.csv")

create_default_manual_overrides <- function(path) {
  if (!file.exists(path)) {
    template <- tibble(
      new_record_species = character(),
      matched_species = character(),
      taxonomy_source = character(),
      review_flag = character(),
      avibase_ioc_review = character(),
      birdlife_review = character(),
      note = character()
    )
    readr::write_csv(template, path)
  }
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

create_default_scenario_config <- function(path) {
  if (!file.exists(path)) {
    scenarios <- tribble(
      ~scenario, ~period, ~ssp, ~gcm, ~res_minutes, ~enabled,
      "current", "current", "current", "WorldClim2.1", 2.5, TRUE,
      "2050s_SSP245_BCC-CSM2-MR", "2050s", "245", "BCC-CSM2-MR", 2.5, TRUE,
      "2050s_SSP245_CNRM-CM6-1", "2050s", "245", "CNRM-CM6-1", 2.5, TRUE,
      "2050s_SSP245_MIROC6", "2050s", "245", "MIROC6", 2.5, TRUE,
      "2050s_SSP585_BCC-CSM2-MR", "2050s", "585", "BCC-CSM2-MR", 2.5, TRUE,
      "2050s_SSP585_CNRM-CM6-1", "2050s", "585", "CNRM-CM6-1", 2.5, TRUE,
      "2050s_SSP585_MIROC6", "2050s", "585", "MIROC6", 2.5, TRUE,
      "2070s_SSP245_BCC-CSM2-MR", "2070s", "245", "BCC-CSM2-MR", 2.5, TRUE,
      "2070s_SSP245_CNRM-CM6-1", "2070s", "245", "CNRM-CM6-1", 2.5, TRUE,
      "2070s_SSP245_MIROC6", "2070s", "245", "MIROC6", 2.5, TRUE,
      "2070s_SSP585_BCC-CSM2-MR", "2070s", "585", "BCC-CSM2-MR", 2.5, TRUE,
      "2070s_SSP585_CNRM-CM6-1", "2070s", "585", "CNRM-CM6-1", 2.5, TRUE,
      "2070s_SSP585_MIROC6", "2070s", "585", "MIROC6", 2.5, TRUE
    ) %>%
      mutate(
        raster_path = file.path(CLIMATE_DIR, scenario),
        ensemble_group = case_when(
          scenario == "current" ~ "current",
          TRUE ~ paste(period, paste0("SSP", ssp), sep = "_")
        )
      )
    readr::write_csv(scenarios, path)
  }
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

create_default_model_config <- function(path) {
  if (!file.exists(path)) {
    cfg <- tibble(
      key = c(
        "presence_cell_threshold",
        "presence_cell_maximum",
        "pseudoabsence_multiplier",
        "pseudoabsence_minimum",
        "pseudoabsence_maximum",
        "cv_folds",
        "cv_presence_threshold_spatial",
        "tss_selection_threshold",
        "province_threshold_a",
        "province_threshold_b",
        "province_threshold_c",
        "province_threshold_d",
        "province_threshold_e",
        "province_threshold_f",
        "buffer_km",
        "correlation_cutoff"
      ),
      value = c("10", "10000", "3", "1000", "20000", "5", "20", "0.6", "3", "10", "20", "50", "100", "200", "500", "0.7")
    )
    readr::write_csv(cfg, path)
  }
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

get_config_value <- function(cfg, key, default = NA_character_) {
  value <- cfg %>% filter(.data$key == !!key) %>% pull(value)
  if (length(value) == 0) default else value[[1]]
}

log_qa <- function(category, item, status, detail = NA_character_) {
  tibble(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    category = category,
    item = item,
    status = status,
    detail = sanitize_log_detail(detail)
  )
}

# ------------------------------------------------------------
# Step A. Data input and taxonomy review | 数据输入与分类审查
# ------------------------------------------------------------
# English:
# Read province boundaries, dash-line overlay, new-record tables, and the
# occurrence shapefile. All occurrence points are filtered to China before
# any taxonomy matching or model fitting is attempted.
#
# 中文：
# 读取省级边界、九段线、新纪录数据表和出现点 shapefile。所有出现点
# 都会先限制在中国范围内，再进入分类匹配与后续建模步骤。
load_province_boundaries <- function() {
  if (!file.exists(PROVINCE_SHP_PATH)) {
    stop("Missing province boundary shapefile: ", PROVINCE_SHP_PATH)
  }
  provinces <- st_read(PROVINCE_SHP_PATH, quiet = TRUE)
  provinces <- provinces %>%
    transmute(
      province_cn = as.character(.data$省名),
      province_code = as.character(.data$省代码),
      geometry = geometry
    )
  provinces %>% st_make_valid() %>% st_transform(4326)
}

load_dash_line_overlay <- function() {
  if (!file.exists(DASHLINE_SHP_PATH)) {
    stop("Missing dash-line shapefile: ", DASHLINE_SHP_PATH)
  }
  st_read(DASHLINE_SHP_PATH, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(4326)
}

load_new_record_data <- function() {
  if (!file.exists(NEW_RECORD_PATH)) stop("Missing new-record source data: ", NEW_RECORD_PATH)
  if (!file.exists(SPECIES_POOL_PATH)) stop("Missing species pool data: ", SPECIES_POOL_PATH)
  new_records <- suppressMessages(readr::read_csv(NEW_RECORD_PATH, show_col_types = FALSE))
  species_pool <- suppressMessages(readr::read_csv(SPECIES_POOL_PATH, show_col_types = FALSE))
  list(new_records = new_records, species_pool = species_pool)
}

load_previous_species_status <- function() {
  if (!file.exists(PREVIOUS_STATUS_PATH)) {
    return(tibble(
      species = character(),
      match_status = character(),
      n_raw = numeric(),
      n_clean = numeric(),
      n_cell_unique = numeric(),
      model_status = character(),
      skip_reason = character(),
      cv_strategy = character(),
      maxent_engine = character()
    ))
  }
  suppressMessages(readr::read_csv(PREVIOUS_STATUS_PATH, show_col_types = FALSE)) %>%
    mutate(species = normalize_species_name(.data$species))
}

normalize_excel_column_names <- function(x) {
  x %>%
    str_replace_all("([a-z0-9])([A-Z])", "\\1_\\2") %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    str_to_lower()
}

read_birdwatch_excel_directory <- function(dir_path, dataset_name, transform_fn = NULL) {
  if (!dir.exists(dir_path)) {
    stop("Missing birdwatch raw directory: ", dir_path)
  }
  files <- list.files(dir_path, pattern = "\\.xlsx$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No xlsx files found in: ", dir_path)
  }
  bind_rows(lapply(files, function(path) {
    sheets <- readxl::excel_sheets(path)
    bind_rows(lapply(sheets, function(sheet_name) {
      tbl <- suppressMessages(readxl::read_excel(path, sheet = sheet_name, guess_max = 50000))
      names(tbl) <- normalize_excel_column_names(names(tbl))
      tbl <- as_tibble(tbl) %>%
        mutate(across(everything(), as.character))
      if (!is.null(transform_fn)) {
        tbl <- transform_fn(tbl, source_file = basename(path), source_sheet = sheet_name)
      }
      tbl %>%
        mutate(
          source_file = basename(path),
          source_sheet = sheet_name,
          source_dataset = dataset_name
        )
    }))
  }))
}

parse_birdwatch_location_columns <- function(x) {
  xy <- str_split_fixed(coalesce(as.character(x), ""), ",", 2)
  tibble(
    longitude = suppressWarnings(as.numeric(str_squish(xy[, 1]))),
    latitude = suppressWarnings(as.numeric(str_squish(xy[, 2])))
  )
}

deduplicate_checklist_rows <- function(tbl) {
  tbl %>%
    filter(!is.na(.data$serial_id), .data$serial_id != "") %>%
    arrange(.data$serial_id) %>%
    group_by(.data$serial_id) %>%
    slice_head(n = 1) %>%
    ungroup()
}

load_occurrence_points_from_birdwatch_raw <- function(china_boundary, species_master = NULL, overrides_tbl = NULL) {
  candidate_species <- normalize_species_name(c(
    species_master$species %||% character(),
    overrides_tbl$matched_species %||% character(),
    overrides_tbl$new_record_species %||% character()
  ))
  candidate_species <- unique(candidate_species[!is.na(candidate_species) & nzchar(candidate_species)])

  species_filter_fn <- function(tbl, source_file = NA_character_, source_sheet = NA_character_) {
    if (!"latinname" %in% names(tbl) || length(candidate_species) == 0) {
      return(tbl)
    }
    latin_norm <- normalize_species_name(tbl$latinname)
    keep <- !is.na(latin_norm) & latin_norm %in% candidate_species
    tbl[keep, , drop = FALSE]
  }

  species_raw <- read_birdwatch_excel_directory(BIRDWATCH_SPECIES_DIR, "species", transform_fn = species_filter_fn)
  target_serial_ids <- species_raw %>%
    mutate(serial_id = as.character(pick_first_existing_column(., c("serial_id", "serialid"), default = NA_character_))) %>%
    filter(!is.na(.data$serial_id), .data$serial_id != "") %>%
    pull(.data$serial_id) %>%
    unique()

  serial_filter_fn <- function(tbl, source_file = NA_character_, source_sheet = NA_character_) {
    serial_ids <- as.character(pick_first_existing_column(tbl, c("serial_id", "serialid"), default = NA_character_))
    tbl[!is.na(serial_ids) & serial_ids %in% target_serial_ids, , drop = FALSE]
  }

  detail_raw <- read_birdwatch_excel_directory(BIRDWATCH_DETAIL_DIR, "detail", transform_fn = serial_filter_fn)
  report_raw <- read_birdwatch_excel_directory(BIRDWATCH_REPORT_DIR, "report", transform_fn = serial_filter_fn)

  detail_tbl <- detail_raw %>%
    mutate(
      serial_id = as.character(pick_first_existing_column(., c("serial_id", "serialid"), default = NA_character_)),
      address = as.character(pick_first_existing_column(., c("address"), default = NA_character_)),
      point_name_detail = as.character(pick_first_existing_column(., c("point_name"), default = NA_character_)),
      start_time_detail_raw = as.character(pick_first_existing_column(., c("start_time"), default = NA_character_)),
      end_time_detail_raw = as.character(pick_first_existing_column(., c("end_time"), default = NA_character_)),
      location_raw = as.character(pick_first_existing_column(., c("location"), default = NA_character_))
    ) %>%
    bind_cols(parse_birdwatch_location_columns(.$location_raw)) %>%
    transmute(
      serial_id = .data$serial_id,
      address = .data$address,
      point_name_detail = .data$point_name_detail,
      start_time_detail = suppressWarnings(as.POSIXct(.data$start_time_detail_raw, tz = "Asia/Shanghai")),
      end_time_detail = suppressWarnings(as.POSIXct(.data$end_time_detail_raw, tz = "Asia/Shanghai")),
      longitude = .data$longitude,
      latitude = .data$latitude,
      detail_source_file = .data$source_file,
      detail_source_sheet = .data$source_sheet
    ) %>%
    deduplicate_checklist_rows()

  report_tbl <- report_raw %>%
    mutate(
      serial_id = as.character(pick_first_existing_column(., c("serial_id", "serialid"), default = NA_character_)),
      username = as.character(pick_first_existing_column(., c("username"), default = NA_character_)),
      start_time_report_raw = as.character(pick_first_existing_column(., c("start_time"), default = NA_character_)),
      end_time_report_raw = as.character(pick_first_existing_column(., c("end_time"), default = NA_character_)),
      province_name = as.character(pick_first_existing_column(., c("province_name"), default = NA_character_)),
      city_name = as.character(pick_first_existing_column(., c("city_name"), default = NA_character_)),
      district_name = as.character(pick_first_existing_column(., c("district_name"), default = NA_character_)),
      point_name_report = as.character(pick_first_existing_column(., c("point_name"), default = NA_character_))
    ) %>%
    transmute(
      serial_id = .data$serial_id,
      username = .data$username,
      start_time_report = suppressWarnings(as.POSIXct(.data$start_time_report_raw, tz = "Asia/Shanghai")),
      end_time_report = suppressWarnings(as.POSIXct(.data$end_time_report_raw, tz = "Asia/Shanghai")),
      province_name = .data$province_name,
      city_name = .data$city_name,
      district_name = .data$district_name,
      point_name_report = .data$point_name_report,
      report_source_file = .data$source_file,
      report_source_sheet = .data$source_sheet
    ) %>%
    deduplicate_checklist_rows()

  species_tbl <- species_raw %>%
    mutate(
      serial_id = as.character(pick_first_existing_column(., c("serial_id", "serialid"), default = NA_character_)),
      activity_id = as.character(pick_first_existing_column(., c("activity_id"), default = NA_character_)),
      shp_species = normalize_species_name(pick_first_existing_column(., c("latinname"), default = NA_character_)),
      taxon_name_cn = as.character(pick_first_existing_column(., c("taxon_name"), default = NA_character_)),
      order_occurrence = as.character(pick_first_existing_column(., c("taxonordername"), default = NA_character_)),
      family_occurrence = as.character(pick_first_existing_column(., c("taxonfamilyname"), default = NA_character_)),
      observation_count = suppressWarnings(as.numeric(pick_first_existing_column(., c("taxon_count", "taxoncount"), default = NA_real_)))
    ) %>%
    transmute(
      serial_id = .data$serial_id,
      activity_id = .data$activity_id,
      shp_species = .data$shp_species,
      taxon_name_cn = .data$taxon_name_cn,
      order_occurrence = .data$order_occurrence,
      family_occurrence = .data$family_occurrence,
      observation_count = .data$observation_count,
      species_source_file = .data$source_file,
      species_source_sheet = .data$source_sheet
    ) %>%
    filter(!is.na(.data$shp_species), .data$shp_species != "")

  merged_tbl <- species_tbl %>%
    left_join(detail_tbl, by = "serial_id", relationship = "many-to-one") %>%
    left_join(report_tbl, by = "serial_id", relationship = "many-to-one") %>%
    mutate(
      point_name = coalesce(.data$point_name_report, .data$point_name_detail),
      obs_start_time = coalesce(.data$start_time_detail, .data$start_time_report),
      obs_end_time = coalesce(.data$end_time_detail, .data$end_time_report),
      year = suppressWarnings(as.integer(format(.data$obs_start_time, "%Y"))),
      occurrence_source = paste0("china_birdwatch_dataset_", BIRDWATCH_YEAR_LABEL)
    ) %>%
    filter(!is.na(.data$year), between(.data$year, BIRDWATCH_YEAR_START, BIRDWATCH_YEAR_END)) %>%
    filter(!is.na(.data$longitude), !is.na(.data$latitude)) %>%
    filter(between(.data$longitude, 70, 140), between(.data$latitude, 0, 60))

  readr::write_csv(merged_tbl, BIRDWATCH_MERGED_OCCURRENCE_PATH)
  readr::write_csv(
    tibble(
      detail_rows = nrow(detail_raw),
      report_rows = nrow(report_raw),
      species_rows = nrow(species_raw),
      merged_rows_occurrence_window = nrow(merged_tbl),
      unique_species_occurrence_window = n_distinct(merged_tbl$shp_species),
      min_year = min(merged_tbl$year, na.rm = TRUE),
      max_year = max(merged_tbl$year, na.rm = TRUE),
      occurrence_window = BIRDWATCH_YEAR_LABEL
    ),
    BIRDWATCH_PROCESSING_SUMMARY_PATH
  )

  standardized <- merged_tbl %>%
    transmute(
      shp_species = .data$shp_species,
      order_occurrence = .data$order_occurrence,
      family_occurrence = .data$family_occurrence,
      longitude = .data$longitude,
      latitude = .data$latitude,
      serial_id = .data$serial_id,
      activity_id = .data$activity_id,
      obs_start_time = format(.data$obs_start_time, "%Y-%m-%d %H:%M:%S"),
      obs_end_time = format(.data$obs_end_time, "%Y-%m-%d %H:%M:%S"),
      year = .data$year,
      province_name = .data$province_name,
      city_name = .data$city_name,
      district_name = .data$district_name,
      point_name = .data$point_name,
      address = .data$address,
      username = .data$username,
      taxon_name_cn = .data$taxon_name_cn,
      observation_count = .data$observation_count,
      occurrence_source = .data$occurrence_source
    )
  readr::write_csv(standardized, OCCURRENCE_CSV_PATH)
  finalize_occurrence_points(standardized, china_boundary)
}

build_species_master <- function(new_records, species_pool) {
  order_lookup <- species_pool %>%
    filter(.data$new_record == 1) %>%
    distinct(species, order)

  new_records %>%
    mutate(species = normalize_species_name(.data$species)) %>%
    group_by(species) %>%
    summarise(
      record_count_new_record = n(),
      first_year = suppressWarnings(min(.data$year, na.rm = TRUE)),
      provinces_recorded = paste(sort(unique(.data$province)), collapse = " | "),
      .groups = "drop"
    ) %>%
    left_join(order_lookup, by = "species") %>%
    rename(new_record_order = order) %>%
    mutate(
      first_year = ifelse(is.infinite(.data$first_year), NA_integer_, .data$first_year),
      species_key = normalize_species_key(.data$species),
      genus = extract_genus(.data$species),
      epithet = extract_epithet(.data$species)
    ) %>%
    arrange(species)
}

build_rescue_species_master <- function(new_records, species_pool, previous_status) {
  full_master <- build_species_master(new_records, species_pool)
  if (nrow(previous_status) == 0) {
    return(full_master %>% mutate(previous_match_status = "not_available", previous_model_status = "not_available", previous_skip_reason = NA_character_))
  }
  modeled_species <- previous_status %>%
    filter(.data$model_status == "modeled") %>%
    distinct(.data$species)
  residual_status <- previous_status %>%
    filter(.data$model_status != "modeled") %>%
    distinct(.data$species, .keep_all = TRUE) %>%
    transmute(
      species,
      previous_match_status = .data$match_status,
      previous_model_status = .data$model_status,
      previous_skip_reason = .data$skip_reason
    )
  full_master %>%
    anti_join(modeled_species, by = "species") %>%
    left_join(residual_status, by = "species") %>%
    mutate(
      previous_match_status = coalesce(.data$previous_match_status, "not_evaluated"),
      previous_model_status = coalesce(.data$previous_model_status, "not_evaluated")
    ) %>%
    arrange(.data$species)
}

summarize_occurrence_sources <- function(tbl) {
  if (!nrow(tbl)) {
    return(tibble(
      occurrence_source = character(),
      n_rows = numeric(),
      n_species = numeric()
    ))
  }
  tbl_plain <- tbl %>% st_drop_geometry()
  if (!"occurrence_source" %in% names(tbl_plain)) {
    tbl_plain$occurrence_source <- "unknown"
  }
  tbl_plain %>%
    mutate(occurrence_source = coalesce(as.character(.data$occurrence_source), "unknown")) %>%
    group_by(.data$occurrence_source) %>%
    summarise(
      n_rows = n(),
      n_species = n_distinct(.data$shp_species),
      .groups = "drop"
    ) %>%
    arrange(desc(.data$n_rows), .data$occurrence_source)
}

finalize_occurrence_points <- function(tbl, china_boundary, provinces = NULL) {
  required_cols <- c("shp_species", "longitude", "latitude")
  if (!all(required_cols %in% names(tbl))) {
    stop("Occurrence table is missing required columns: ", paste(setdiff(required_cols, names(tbl)), collapse = ", "))
  }
  cleaning_steps <- list()
  append_cleaning_step <- function(step_name, n_rows, detail) {
    cleaning_steps[[length(cleaning_steps) + 1]] <<- tibble(
      step_name = step_name,
      n_rows = as.numeric(n_rows),
      detail = as.character(detail)
    )
  }
  append_cleaning_step("raw_input", nrow(tbl), "Rows supplied to the occurrence cleaning function.")
  tbl <- tbl %>%
    mutate(
      shp_species = normalize_species_name(.data$shp_species),
      shp_species_key = normalize_species_key(.data$shp_species),
      order_occurrence = if ("order_occurrence" %in% names(tbl)) str_to_title(str_to_lower(str_squish(as.character(.data$order_occurrence)))) else NA_character_,
      family_occurrence = if ("family_occurrence" %in% names(tbl)) str_squish(as.character(.data$family_occurrence)) else NA_character_,
      province_name = if ("province_name" %in% names(tbl)) normalize_admin_name(.data$province_name) else NA_character_,
      longitude = as.numeric(.data$longitude),
      latitude = as.numeric(.data$latitude)
    ) %>%
    filter(!is.na(.data$shp_species), .data$shp_species != "")
  append_cleaning_step("after_species_cleaning", nrow(tbl), "Rows with non-empty standardized species names.")
  tbl <- tbl %>%
    filter(!is.na(.data$longitude), !is.na(.data$latitude))
  append_cleaning_step("after_coordinate_nonmissing", nrow(tbl), "Rows with non-missing longitude and latitude.")
  tbl <- tbl %>%
    filter(between(.data$longitude, 70, 140), between(.data$latitude, 0, 60)) %>%
    distinct(.data$shp_species, .data$longitude, .data$latitude, .keep_all = TRUE)
  append_cleaning_step("after_bbox_and_dedup", nrow(tbl), "Rows within the China bounding box after species-coordinate de-duplication.")
  safe_message("Occurrence rows after numeric cleaning and de-duplication: ", nrow(tbl))
  province_matched_tbl <- tbl[0, , drop = FALSE]
  spatial_fallback_tbl <- tbl
  if (!is.null(provinces) && "province_name" %in% names(tbl)) {
    province_lookup <- provinces %>%
      st_drop_geometry() %>%
      transmute(
        province_full = normalize_admin_name(.data$province_cn),
        province_simple = simplify_province_name(.data$province_cn)
      ) %>%
      distinct()
    province_full_set <- unique(province_lookup$province_full)
    province_simple_set <- unique(province_lookup$province_simple)
    province_matched_tbl <- tbl %>%
      filter(
        !is.na(.data$province_name),
        .data$province_name %in% province_full_set |
          simplify_province_name(.data$province_name) %in% province_simple_set
      )
    spatial_fallback_tbl <- tbl %>%
      anti_join(
        province_matched_tbl %>% select("shp_species", "longitude", "latitude"),
        by = c("shp_species", "longitude", "latitude")
      )
    append_cleaning_step("after_province_name_shortcut", nrow(province_matched_tbl), "Rows retained directly by matching province_name to the China province list.")
    append_cleaning_step("spatial_fallback_candidates", nrow(spatial_fallback_tbl), "Rows still requiring polygon-based China filtering after the province-name shortcut.")
  }
  output_parts <- list()
  if (nrow(province_matched_tbl) > 0) {
    output_parts[[length(output_parts) + 1]] <- st_as_sf(province_matched_tbl, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  }
  if (nrow(spatial_fallback_tbl) > 0) {
    points_sf <- st_as_sf(spatial_fallback_tbl, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    spatial_fallback_tbl <- points_sf %>%
      st_filter(st_as_sf(china_boundary), .predicate = st_intersects) %>%
      distinct(.data$shp_species, .data$longitude, .data$latitude, .keep_all = TRUE)
    output_parts[[length(output_parts) + 1]] <- spatial_fallback_tbl
  }
  append_cleaning_step("after_spatial_fallback", nrow(spatial_fallback_tbl), "Rows retained by polygon-based China filtering.")
  if (length(output_parts) == 0) {
    final_tbl <- st_as_sf(tbl[0, , drop = FALSE], coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  } else {
    final_tbl <- do.call(rbind, output_parts) %>%
      distinct(.data$shp_species, .data$longitude, .data$latitude, .keep_all = TRUE)
  }
  append_cleaning_step("final_retained", nrow(final_tbl), "Final cleaned occurrence rows retained for taxonomy matching and SDM fitting.")
  readr::write_csv(bind_rows(cleaning_steps), OCCURRENCE_CLEANING_PATH)
  final_tbl
}

pick_first_existing_column <- function(tbl, candidates, default = NA) {
  matched <- candidates[candidates %in% names(tbl)]
  if (length(matched) == 0) {
    return(rep(default, nrow(tbl)))
  }
  value <- tbl[[matched[[1]]]]
  if (is.null(value)) {
    return(rep(default, nrow(tbl)))
  }
  value
}

load_occurrence_points_from_csv <- function(china_boundary, species_master = NULL, overrides_tbl = NULL, provinces = NULL) {
  if (!file.exists(OCCURRENCE_CSV_PATH)) {
    stop("Missing occurrence CSV: ", OCCURRENCE_CSV_PATH)
  }
  safe_message("Reading occurrence CSV from ", make_repo_relative_path(OCCURRENCE_CSV_PATH), " ...")
  occ <- suppressMessages(
    readr::read_csv(
      OCCURRENCE_CSV_PATH,
      show_col_types = FALSE,
      col_select = any_of(c(
        "species",
        "scientific_name",
        "shp_species",
        "order_occurrence",
        "order",
        "family_occurrence",
        "family",
        "province_name",
        "longitude",
        "decimalLongitude",
        "latitude",
        "decimalLatitude"
      ))
    )
  )
  safe_message("Occurrence CSV loaded: ", nrow(occ), " rows and ", ncol(occ), " selected columns.")
  standardized <- occ %>%
    mutate(
      shp_species = pick_first_existing_column(., c("species", "scientific_name", "shp_species"), default = NA_character_),
      order_occurrence = pick_first_existing_column(., c("order_occurrence", "order"), default = NA_character_),
      family_occurrence = pick_first_existing_column(., c("family_occurrence", "family"), default = NA_character_),
      longitude = pick_first_existing_column(., c("longitude", "decimalLongitude"), default = NA_real_),
      latitude = pick_first_existing_column(., c("latitude", "decimalLatitude"), default = NA_real_)
    )
  candidate_species <- normalize_species_name(c(
    species_master$species %||% character(),
    overrides_tbl$matched_species %||% character(),
    overrides_tbl$new_record_species %||% character()
  ))
  candidate_species <- unique(candidate_species[!is.na(candidate_species) & nzchar(candidate_species)])
  if (length(candidate_species) > 0) {
    standardized <- standardized %>%
      mutate(shp_species = normalize_species_name(.data$shp_species)) %>%
      filter(!is.na(.data$shp_species), .data$shp_species %in% candidate_species)
    safe_message("Occurrence rows retained after new-record species prefilter: ", nrow(standardized))
  }
  finalize_occurrence_points(standardized, china_boundary, provinces = provinces)
}

load_occurrence_points_from_shp <- function(china_boundary) {
  if (!file.exists(OCCURRENCE_SHP_PATH)) stop("Missing occurrence shapefile: ", OCCURRENCE_SHP_PATH)
  shp <- st_read(OCCURRENCE_SHP_PATH, quiet = TRUE) %>% st_transform(4326)
  birds <- shp %>%
    mutate(
      class_value = str_to_lower(str_squish(as.character(.data$Class))),
      useclass_value = str_to_lower(str_squish(as.character(.data$useclass))),
      shp_species = normalize_species_name(coalesce(as.character(.data$Scientific), as.character(.data$name_sci))),
      order_occurrence = str_to_title(str_to_lower(str_squish(as.character(.data$Order_)))),
      family_occurrence = str_squish(as.character(.data$Family)),
      longitude = as.numeric(.data$Longitude),
      latitude = as.numeric(.data$Latitude)
    ) %>%
    filter(.data$class_value == "aves" | .data$useclass_value == "bird") %>%
    st_drop_geometry() %>%
    select(.data$shp_species, .data$order_occurrence, .data$family_occurrence, .data$longitude, .data$latitude)
  finalize_occurrence_points(birds, china_boundary)
}

fetch_rgbif_occurrences_for_species <- function(query_name, limit_per_call = 1000, max_records = 20000) {
  if (!requireNamespace("rgbif", quietly = TRUE)) {
    stop("Package 'rgbif' is required for OCCURRENCE_SOURCE='rgbif'.")
  }
  all_rows <- list()
  start_at <- 0
  while (start_at < max_records) {
    query_args <- list(
      scientificName = query_name,
      hasCoordinate = TRUE,
      limit = limit_per_call,
      start = start_at,
      fields = "minimal"
    )
    if (!is.na(RGBIF_COUNTRY_FILTER) && nzchar(RGBIF_COUNTRY_FILTER)) {
      query_args$country <- RGBIF_COUNTRY_FILTER
    }
    occ_res <- do.call(rgbif::occ_search, query_args)
    dat <- occ_res$data
    if (is.null(dat) || !nrow(dat)) {
      break
    }
    all_rows[[length(all_rows) + 1]] <- dat
    if (nrow(dat) < limit_per_call) {
      break
    }
    start_at <- start_at + limit_per_call
  }
  bind_rows(all_rows)
}

build_rgbif_query_table <- function(species_master, overrides_tbl) {
  prepared_overrides <- prepare_manual_overrides(overrides_tbl)
  species_master %>%
    transmute(new_record_species = .data$species, species_key = .data$species_key) %>%
    left_join(
      prepared_overrides %>% select(.data$new_record_species_key, .data$matched_species),
      by = c("species_key" = "new_record_species_key")
    ) %>%
    mutate(query_species = coalesce(.data$matched_species, .data$new_record_species)) %>%
    distinct(.data$new_record_species, .data$query_species)
}

load_occurrence_points_from_rgbif <- function(china_boundary, species_master, overrides_tbl) {
  query_tbl <- build_rgbif_query_table(species_master, overrides_tbl)
  rgbif_rows <- lapply(seq_len(nrow(query_tbl)), function(i) {
    row <- query_tbl[i, , drop = FALSE]
    safe_message("Fetching RGBIF occurrences for ", row$query_species[[1]], " ...")
    dat <- tryCatch(fetch_rgbif_occurrences_for_species(row$query_species[[1]]), error = function(e) tibble())
    if (!nrow(dat)) {
      return(tibble())
    }
    dat %>%
      transmute(
        shp_species = row$query_species[[1]],
        order_occurrence = coalesce(.data$order, NA_character_),
        family_occurrence = coalesce(.data$family, NA_character_),
        longitude = coalesce(.data$decimalLongitude, .data$longitude),
        latitude = coalesce(.data$decimalLatitude, .data$latitude),
        gbif_key = as.character(.data$key),
        occurrence_source = "rgbif"
      )
  })
  occ_tbl <- bind_rows(rgbif_rows)
  if (!nrow(occ_tbl)) {
    stop("No RGBIF occurrence points were returned for the new-record bird species.")
  }
  readr::write_csv(occ_tbl, OCCURRENCE_CSV_PATH)
  finalize_occurrence_points(occ_tbl, china_boundary)
}

load_occurrence_points_from_combined_sources <- function(china_boundary, species_master, overrides_tbl, provinces = NULL) {
  safe_message("Loading rescue occurrences from China Birdwatch ", BIRDWATCH_YEAR_LABEL, " and GBIF ...")
  birdwatch_points <- load_occurrence_points_from_birdwatch_raw(
    china_boundary,
    species_master = species_master,
    overrides_tbl = overrides_tbl
  )
  birdwatch_tbl <- birdwatch_points %>%
    st_drop_geometry() %>%
    mutate(occurrence_source = coalesce(as.character(.data$occurrence_source), paste0("china_birdwatch_dataset_", BIRDWATCH_YEAR_LABEL)))
  rgbif_points <- tryCatch(
    load_occurrence_points_from_rgbif(china_boundary, species_master, overrides_tbl),
    error = function(e) {
      safe_message("GBIF retrieval returned no usable rows or failed: ", conditionMessage(e))
      st_as_sf(birdwatch_tbl[0, , drop = FALSE], coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    }
  )
  rgbif_tbl <- rgbif_points %>%
    st_drop_geometry() %>%
    mutate(occurrence_source = coalesce(as.character(.data$occurrence_source), "rgbif"))
  combined_tbl <- bind_rows(birdwatch_tbl, rgbif_tbl) %>%
    mutate(
      occurrence_source = coalesce(as.character(.data$occurrence_source), "unknown")
    )
  readr::write_csv(summarize_occurrence_sources(combined_tbl), OCCURRENCE_SOURCE_SUMMARY_PATH)
  readr::write_csv(combined_tbl, OCCURRENCE_CSV_PATH)
  finalize_occurrence_points(combined_tbl, china_boundary, provinces = provinces)
}

load_occurrence_points <- function(china_boundary, species_master = NULL, overrides_tbl = NULL, provinces = NULL) {
  source_mode <- str_to_lower(str_squish(OCCURRENCE_SOURCE))
  if (source_mode == "combined") {
    return(load_occurrence_points_from_combined_sources(china_boundary, species_master, overrides_tbl, provinces = provinces))
  }
  if (source_mode == "birdwatch_raw") {
    return(load_occurrence_points_from_birdwatch_raw(china_boundary, species_master = species_master, overrides_tbl = overrides_tbl))
  }
  if (source_mode == "csv") {
    return(load_occurrence_points_from_csv(china_boundary, species_master = species_master, overrides_tbl = overrides_tbl, provinces = provinces))
  }
  if (source_mode == "rgbif") {
    return(load_occurrence_points_from_rgbif(china_boundary, species_master, overrides_tbl))
  }
  if (source_mode == "shp") {
    return(load_occurrence_points_from_shp(china_boundary))
  }
  if (file.exists(OCCURRENCE_CSV_PATH)) {
    return(load_occurrence_points_from_csv(china_boundary, species_master = species_master, overrides_tbl = overrides_tbl, provinces = provinces))
  }
  if (dir.exists(BIRDWATCH_DETAIL_DIR) && dir.exists(BIRDWATCH_SPECIES_DIR) && dir.exists(BIRDWATCH_REPORT_DIR) &&
      requireNamespace("rgbif", quietly = TRUE) && !is.null(species_master) && !is.null(overrides_tbl)) {
    return(load_occurrence_points_from_combined_sources(china_boundary, species_master, overrides_tbl, provinces = provinces))
  }
  if (dir.exists(BIRDWATCH_DETAIL_DIR) && dir.exists(BIRDWATCH_SPECIES_DIR) && dir.exists(BIRDWATCH_REPORT_DIR)) {
    return(load_occurrence_points_from_birdwatch_raw(china_boundary, species_master = species_master, overrides_tbl = overrides_tbl))
  }
  if (requireNamespace("rgbif", quietly = TRUE) && !is.null(species_master) && !is.null(overrides_tbl)) {
    return(load_occurrence_points_from_rgbif(china_boundary, species_master, overrides_tbl))
  }
  if (file.exists(OCCURRENCE_SHP_PATH)) {
    return(load_occurrence_points_from_shp(china_boundary))
  }
  stop(
    paste(
      "No usable occurrence source was found.",
      "Provide data/occurrence/occurrence_records_clean_standardized.csv,",
      "or provide the extracted China birdwatch raw workbooks under the configured raw-data directory,",
      "set BIRD_SDM_OCCURRENCE_SOURCE=combined or rgbif with the rgbif package installed,",
      "or place an optional shapefile at", OCCURRENCE_SHP_PATH, "."
    )
  )
}

prepare_manual_overrides <- function(overrides_tbl) {
  if (nrow(overrides_tbl) == 0) {
    return(tibble(
      new_record_species = character(),
      matched_species = character(),
      taxonomy_source = character(),
      review_flag = character(),
      avibase_ioc_review = character(),
      birdlife_review = character(),
      note = character(),
      new_record_species_key = character(),
      matched_species_key = character()
    ))
  }
  overrides_tbl %>%
    mutate(
      new_record_species = normalize_species_name(.data$new_record_species),
      matched_species = normalize_species_name(.data$matched_species),
      new_record_species_key = normalize_species_key(.data$new_record_species),
      matched_species_key = normalize_species_key(.data$matched_species),
      taxonomy_source = if_else(is.na(.data$taxonomy_source) | .data$taxonomy_source == "", "Manual review", .data$taxonomy_source),
      review_flag = if_else(is.na(.data$review_flag) | .data$review_flag == "", "manual_confirmed", .data$review_flag),
      avibase_ioc_review = .data$avibase_ioc_review %||% "reviewed",
      birdlife_review = .data$birdlife_review %||% "reviewed"
    )
}

build_taxonomy_review_table <- function(species_master, occurrence_birds, overrides_tbl) {
  occurrence_species <- occurrence_birds %>%
    st_drop_geometry() %>%
    distinct(shp_species, shp_species_key, order_occurrence, family_occurrence) %>%
    mutate(
      genus = extract_genus(.data$shp_species),
      epithet = extract_epithet(.data$shp_species)
    )

  overrides_tbl <- prepare_manual_overrides(overrides_tbl)

  review_rows <- lapply(seq_len(nrow(species_master)), function(i) {
    sp <- species_master[i, ]
    manual_match <- overrides_tbl %>% filter(.data$new_record_species_key == sp$species_key)
    exact_match <- occurrence_species %>% filter(.data$shp_species_key == sp$species_key)
    same_order_epithet <- occurrence_species %>%
      filter(!is.na(.data$epithet), .data$epithet == sp$epithet)
    if (!is.na(sp$new_record_order)) {
      same_order_epithet <- same_order_epithet %>% filter(.data$order_occurrence == sp$new_record_order)
    }
    any_order_epithet <- occurrence_species %>%
      filter(!is.na(.data$epithet), .data$epithet == sp$epithet)

    candidate_reason <- NA_character_
    match_status <- "unmatched"
    match_type <- "unmatched"
    matched_species <- NA_character_
    taxonomy_source <- "Pending Avibase/IOC + BirdLife review"
    review_flag <- "review_needed"
    avibase_ioc_review <- "pending"
    birdlife_review <- "pending"
    candidate_species <- character(0)
    note <- NA_character_

    if (nrow(manual_match) > 0) {
      matched_species <- manual_match$matched_species[[1]]
      match_status <- "matched"
      match_type <- "manual_override"
      taxonomy_source <- manual_match$taxonomy_source[[1]]
      review_flag <- manual_match$review_flag[[1]]
      avibase_ioc_review <- manual_match$avibase_ioc_review[[1]]
      birdlife_review <- manual_match$birdlife_review[[1]]
      note <- manual_match$note[[1]]
    } else if (nrow(exact_match) > 0) {
      matched_species <- exact_match$shp_species[[1]]
      match_status <- "matched"
      match_type <- "direct_exact"
      taxonomy_source <- "Direct match in occurrence records"
      review_flag <- "not_required"
      avibase_ioc_review <- "not_required"
      birdlife_review <- "not_required"
    } else if (nrow(same_order_epithet) == 1) {
      matched_species <- same_order_epithet$shp_species[[1]]
      match_status <- "matched"
      match_type <- "auto_epithet_same_order"
      taxonomy_source <- "High-confidence auto candidate; review basis Avibase/IOC + BirdLife"
      review_flag <- "auto_high_confidence"
      avibase_ioc_review <- "candidate"
      birdlife_review <- "candidate"
    } else if (nrow(any_order_epithet) == 1) {
      matched_species <- any_order_epithet$shp_species[[1]]
      match_status <- "matched"
      match_type <- "auto_epithet_unique"
      taxonomy_source <- "Auto candidate; cross-check with Avibase/IOC + BirdLife recommended"
      review_flag <- "auto_medium_confidence"
      avibase_ioc_review <- "candidate"
      birdlife_review <- "candidate"
    } else {
      candidate_pool <- bind_rows(same_order_epithet, any_order_epithet) %>%
        distinct(shp_species, order_occurrence, family_occurrence)
      if (nrow(candidate_pool) == 0) {
        genus_candidates <- occurrence_species %>% filter(.data$genus == sp$genus)
        candidate_pool <- genus_candidates %>% distinct(shp_species, order_occurrence, family_occurrence)
        candidate_reason <- "same genus"
      } else {
        candidate_reason <- "same epithet"
      }
      if (nrow(candidate_pool) > 0) {
        candidate_species <- head(candidate_pool$shp_species, 5)
      }
      note <- if (length(candidate_species)) paste(candidate_reason, paste(candidate_species, collapse = " | ")) else NA_character_
    }

    tibble(
      new_record_species = sp$species,
      new_record_order = sp$new_record_order,
      record_count_new_record = sp$record_count_new_record,
      first_year = sp$first_year,
      provinces_recorded = sp$provinces_recorded,
      matched_species = matched_species,
      match_status = match_status,
      match_type = match_type,
      taxonomy_source = taxonomy_source,
      review_flag = review_flag,
      avibase_ioc_review = avibase_ioc_review,
      birdlife_review = birdlife_review,
      candidate_species = paste(candidate_species, collapse = " | "),
      note = note,
      model_ready = match_status == "matched"
    )
  })

  bind_rows(review_rows) %>% arrange(new_record_species)
}

collect_algorithm_availability <- function() {
  java_check <- suppressWarnings(tryCatch(
    system2("java", args = "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) structure(character(), status = 1)
  ))
  java_ok <- is.null(attr(java_check, "status")) || identical(attr(java_check, "status"), 0L)
  maxent_jar_paths <- c(
    file.path(Sys.getenv("HOME"), ".dismo", "maxent.jar"),
    file.path(Sys.getenv("HOME"), "Library", "Application Support", "dismo", "maxent.jar"),
    file.path(DATA_DIR, "maxent.jar")
  )
  maxent_jar <- maxent_jar_paths[file.exists(maxent_jar_paths)]
  tibble(
    algorithm = c("GLM", "GAM", "GBM", "RF", "MaxEnt"),
    available = c(
      TRUE,
      requireNamespace("mgcv", quietly = TRUE),
      requireNamespace("gbm", quietly = TRUE),
      requireNamespace("randomForest", quietly = TRUE),
      (java_ok && length(maxent_jar) > 0) || requireNamespace("maxnet", quietly = TRUE)
    ),
    engine = c(
      "stats::glm",
      ifelse(requireNamespace("mgcv", quietly = TRUE), "mgcv::gam", "missing"),
      ifelse(requireNamespace("gbm", quietly = TRUE), "gbm::gbm", "missing"),
      ifelse(requireNamespace("randomForest", quietly = TRUE), "randomForest::randomForest", "missing"),
      ifelse(java_ok && length(maxent_jar) > 0, "dismo::maxent + maxent.jar",
             ifelse(requireNamespace("maxnet", quietly = TRUE), "maxnet::maxnet", "missing"))
    ),
    detail = c(
      "Base R available",
      "Generalized additive model",
      "Gradient boosting machine",
      "Random forest classifier",
      paste0("Java detected: ", java_ok, "; maxent.jar found: ", length(maxent_jar) > 0, "; maxnet installed: ", requireNamespace("maxnet", quietly = TRUE))
    )
  )
}

# ------------------------------------------------------------
# Step C2. Variable screening | 环境变量筛选
# ------------------------------------------------------------
# English:
# Use correlation screening to retain a biologically interpretable subset of
# predictors. Priority is given to elevation and a core set of temperature/
# precipitation variables with broad ecological meaning.
#
# 中文：
# 通过相关性筛选保留生态解释性更强的一组环境变量。优先保留海拔以及
# 一组具有明确生态意义的核心温度/降水变量。
choose_environment_variables <- function(current_stack, cutoff = 0.7) {
  sample_df <- terra::spatSample(current_stack, size = min(15000, max(1000, terra::ncell(current_stack))), method = "regular", as.data.frame = TRUE, na.rm = TRUE)
  sample_df <- sample_df[, colnames(sample_df) %in% names(current_stack), drop = FALSE]
  cor_mat <- stats::cor(sample_df, use = "pairwise.complete.obs")
  priority <- intersect(c(
    "elev", "bio_1", "bio_4", "bio_12", "bio_15", "bio_5", "bio_6", "bio_13", "bio_14",
    "bio_2", "bio_3", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11", "bio_16", "bio_17", "bio_18", "bio_19"
  ), colnames(sample_df))

  selected <- character(0)
  dropped <- character(0)
  for (var in priority) {
    if (length(selected) == 0) {
      selected <- c(selected, var)
      next
    }
    cor_to_selected <- abs(cor_mat[var, selected])
    if (all(is.na(cor_to_selected)) || all(cor_to_selected < cutoff)) {
      selected <- c(selected, var)
    } else {
      dropped <- c(dropped, var)
    }
  }

  bind_rows(
    tibble(variable = selected, keep = TRUE, rationale = "Retained after correlation screening"),
    tibble(variable = dropped, keep = FALSE, rationale = paste0("Dropped because |r| >= ", cutoff, " with a higher-priority variable"))
  ) %>% arrange(desc(keep), variable)
}

# ------------------------------------------------------------
# Step C1. Climate and elevation file discovery | 气候与海拔数据定位
# ------------------------------------------------------------
# English:
# Prefer cached WorldClim files inside the task data directory so repeated
# runs do not trigger unnecessary downloads. If cached files are incomplete
# or unreadable, the workflow falls back to geodata::worldclim_global().
#
# 中文：
# 优先使用任务目录下已经缓存的 WorldClim 文件，避免重复下载；如果
# 本地缓存不完整或不可读，则回退到 geodata::worldclim_global() 下载。
worldclim_resolution_suffix <- function(res_value) {
  ifelse(as.character(res_value) == "0.5", "30s", paste0(as.character(res_value), "m"))
}

get_climate_cache_roots <- function(climate_root = DATA_DIR) {
  extra_roots <- Sys.getenv("BIRD_SDM_CLIMATE_CACHE_ROOTS", unset = "") %>%
    strsplit(split = .Platform$path.sep, fixed = TRUE) %>%
    .[[1]]
  roots <- c(
    file.path(climate_root, "climate"),
    climate_root,
    DEFAULT_SHARED_CLIMATE_CACHE_ROOT,
    file.path(DEFAULT_SHARED_CLIMATE_CACHE_ROOT, "climate"),
    extra_roots
  )
  roots[nzchar(roots) & dir.exists(roots)] %>% unique()
}

validate_raster_file <- function(path, expected_layers = 1L) {
  if (!length(path) || is.na(path) || !file.exists(path)) {
    return(FALSE)
  }
  tryCatch(
    {
      rst <- terra::rast(path)
      terra::nlyr(rst) >= expected_layers
    },
    error = function(e) FALSE
  )
}

write_climate_manifest <- function(entries) {
  if (is.null(entries) || !nrow(entries)) {
    return(invisible(NULL))
  }
  ensure_dir(dirname(CLIMATE_MANIFEST_PATH))
  if (file.exists(CLIMATE_MANIFEST_PATH)) {
    existing <- suppressMessages(readr::read_csv(CLIMATE_MANIFEST_PATH, show_col_types = FALSE)) %>%
      transmute(
        scenario = as.character(.data$scenario),
        res_minutes = as.character(.data$res_minutes),
        target_name = as.character(.data$target_name),
        path = make_repo_relative_path(.data$path),
        source = as.character(.data$source),
        readable = as.logical(.data$readable),
        checked_at = as.character(.data$checked_at)
      )
  } else {
    existing <- tibble(
      scenario = character(),
      res_minutes = character(),
      target_name = character(),
      path = character(),
      source = character(),
      readable = logical(),
      checked_at = character()
    )
  }
  combined <- bind_rows(existing, entries) %>%
    mutate(
      scenario = as.character(.data$scenario),
      res_minutes = as.character(.data$res_minutes),
      target_name = as.character(.data$target_name),
      path = make_repo_relative_path(.data$path),
      source = as.character(.data$source),
      readable = as.logical(.data$readable),
      checked_at = as.character(.data$checked_at)
    ) %>%
    arrange(desc(.data$checked_at)) %>%
    distinct(.data$scenario, .data$res_minutes, .data$target_name, .keep_all = TRUE)
  readr::write_csv(combined, CLIMATE_MANIFEST_PATH)
  invisible(combined)
}

scan_cached_worldclim_current_files <- function(res_value, search_roots = get_climate_cache_roots()) {
  fres <- worldclim_resolution_suffix(res_value)
  target_names <- c(paste0("wc2.1_", fres, "_bio_", seq_len(19), ".tif"), paste0("wc2.1_", fres, "_elev.tif"))
  pattern <- paste0("^wc2\\.1_", gsub("\\.", "\\\\.", fres), "_(bio_[0-9]+|elev)\\.tif$")
  candidate_list <- lapply(
    search_roots,
    function(root) {
      hits <- list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE)
      if (!length(hits)) {
        return(tibble(root = character(), path = character(), target_name = character()))
      }
      tibble(root = root, path = hits, target_name = basename(hits))
    }
  )
  candidate_files <- bind_rows(candidate_list) %>%
    filter(.data$target_name %in% target_names)
  if (!nrow(candidate_files)) {
    return(tibble(
      scenario = character(),
      res_minutes = character(),
      target_name = character(),
      path = character(),
      source = character(),
      readable = logical(),
      checked_at = character()
    ))
  }
  candidate_files <- candidate_files %>%
    group_by(.data$target_name) %>%
    mutate(readable = vapply(.data$path, validate_raster_file, logical(1))) %>%
    arrange(desc(.data$readable), nchar(.data$path), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(
      scenario = "current",
      res_minutes = as.character(res_value),
      source = "scan_existing",
      checked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ) %>%
    select("scenario", "res_minutes", "target_name", "path", "source", "readable", "checked_at")
  write_climate_manifest(candidate_files)
  candidate_files
}

# Backward-compatible alias used by legacy helper scripts and smoke tests.
scan_existing_worldclim_current_files <- scan_cached_worldclim_current_files

load_worldclim_current_from_paths <- function(bio_paths, china_boundary) {
  if (length(bio_paths) != 19 || any(!file.exists(bio_paths))) {
    return(NULL)
  }
  stack <- tryCatch(terra::rast(bio_paths), error = function(e) NULL)
  if (is.null(stack)) {
    return(NULL)
  }
  names(stack) <- paste0("bio_", seq_len(terra::nlyr(stack)))
  china_vect <- terra::vect(china_boundary)
  stack <- terra::crop(stack, china_vect)
  terra::mask(stack, china_vect)
}

repair_worldclim_bioclim_files <- function(res_value, climate_root) {
  fres <- worldclim_resolution_suffix(res_value)
  out_dir <- file.path(climate_root, "climate", paste0("wc2.1_", fres))
  zip_path <- file.path(out_dir, paste0("wc2.1_", fres, "_bio.zip"))
  tif_paths <- file.path(out_dir, paste0("wc2.1_", fres, "_bio_", seq_len(19), ".tif"))
  if (!all(file.exists(tif_paths)) && file.exists(zip_path)) {
    try(utils::unzip(zip_path, exdir = out_dir), silent = TRUE)
  }
  tif_paths[file.exists(tif_paths)]
}


repair_worldclim_elevation_file <- function(res_value, climate_root) {
  fres <- worldclim_resolution_suffix(res_value)
  out_dir <- file.path(climate_root, "climate", paste0("wc2.1_", fres))
  tif_path <- file.path(out_dir, paste0("wc2.1_", fres, "_elev.tif"))
  if (file.exists(tif_path)) tif_path else character(0)
}

load_worldclim_elevation <- function(res_value, climate_root, china_boundary) {
  fres <- worldclim_resolution_suffix(res_value)
  target_name <- paste0("wc2.1_", fres, "_elev.tif")
  existing_tbl <- scan_cached_worldclim_current_files(res_value = res_value, search_roots = get_climate_cache_roots(climate_root))
  existing_path <- existing_tbl %>%
    filter(.data$target_name == target_name, .data$readable) %>%
    pull(.data$path)
  elev <- NULL
  if (length(existing_path) >= 1) {
    elev <- tryCatch(terra::rast(existing_path[[1]]), error = function(e) NULL)
  }
  if (is.null(elev)) {
    elev <- tryCatch(
      geodata::worldclim_global(var = "elev", res = res_value, path = climate_root),
      error = function(e) NULL
    )
  }
  if (is.null(elev)) {
    elev_path <- repair_worldclim_elevation_file(res_value = res_value, climate_root = climate_root)
    if (length(elev_path) == 1) {
      elev <- tryCatch(terra::rast(elev_path), error = function(e) NULL)
    }
  }
  if (is.null(elev)) {
    return(NULL)
  }
  names(elev) <- "elev"
  china_vect <- terra::vect(china_boundary)
  elev <- terra::crop(elev, china_vect)
  elev <- terra::mask(elev, china_vect)
  elev
}

load_environment_stack <- function(row, china_boundary, force_download = FALSE) {
  scenario <- row$scenario[[1]]
  res_value <- as.character(row$res_minutes[[1]])
  climate_root <- DATA_DIR
  allow_remote_download <- isTRUE(force_download) || isTRUE(ALLOW_REMOTE_CLIMATE_DOWNLOAD)
  ensure_dir(file.path(climate_root, "climate"))

  stack <- NULL
  if (scenario == "current" && !force_download) {
    existing_tbl <- scan_cached_worldclim_current_files(res_value = res_value, search_roots = get_climate_cache_roots(climate_root))
    bio_paths <- existing_tbl %>%
      filter(grepl("_bio_[0-9]+\\.tif$", .data$target_name), .data$readable) %>%
      mutate(bio_id = readr::parse_number(.data$target_name)) %>%
      arrange(.data$bio_id) %>%
      pull(.data$path)
    stack <- load_worldclim_current_from_paths(bio_paths = bio_paths, china_boundary = china_boundary)
    if (!is.null(stack)) {
      safe_message("Loaded current WorldClim bioclim stack from cached local climate files.")
    }
  }

  if (is.null(stack) && scenario == "current" && allow_remote_download) {
    safe_message("No readable cached current WorldClim stack found; trying direct download for ", row$scenario[[1]], ".")
    stack <- tryCatch(
      geodata::worldclim_global(var = "bio", res = res_value, path = climate_root),
      error = function(e) NULL
    )
  } else if (is.null(stack) && allow_remote_download) {
    stack <- tryCatch(
      geodata::cmip6_world(
        model = row$gcm[[1]],
        ssp = row$ssp[[1]],
        time = row$period[[1]],
        var = "bio",
        res = res_value,
        path = climate_root
      ),
      error = function(e) NULL
    )
  }

  if (is.null(stack) && scenario == "current") {
    repaired_files <- repair_worldclim_bioclim_files(res_value = res_value, climate_root = climate_root)
    if (length(repaired_files) == 19) {
      stack <- tryCatch(terra::rast(repaired_files), error = function(e) NULL)
    }
  }

  if (is.null(stack)) {
    if (!allow_remote_download) {
      safe_message("Offline-only climate mode: no readable local cache found for scenario ", scenario, ". Skipping this climate stack.")
    }
    return(NULL)
  }

  names(stack) <- paste0("bio_", seq_len(terra::nlyr(stack)))
  china_vect <- terra::vect(china_boundary)
  stack <- terra::crop(stack, china_vect)
  stack <- terra::mask(stack, china_vect)
  elev <- load_worldclim_elevation(res_value = res_value, climate_root = climate_root, china_boundary = china_boundary)
  if (!is.null(elev)) {
    stack <- c(stack, elev)
  }
  stack
}

make_accessible_area <- function(species_points, china_boundary, buffer_km = 500) {
  china_3857 <- st_transform(china_boundary, 3857)
  points_3857 <- st_transform(species_points, 3857)
  area <- points_3857 %>% st_union() %>% st_buffer(buffer_km * 1000)
  area <- st_intersection(st_as_sf(area), st_as_sf(china_3857))
  st_transform(area, 4326)
}

thin_presence_by_cells <- function(species_points, current_stack) {
  xy <- st_coordinates(species_points)
  cell_id <- terra::cellFromXY(current_stack[[1]], xy)
  species_points$cell_id <- cell_id
  species_points %>%
    filter(!is.na(.data$cell_id)) %>%
    distinct(.data$species, .data$cell_id, .keep_all = TRUE)
}

spatially_balance_presence_points <- function(species_points, max_points) {
  if (nrow(species_points) <= max_points) {
    return(species_points)
  }
  coords <- st_coordinates(species_points)
  n_bins <- max(4L, min(20L, floor(sqrt(max_points))))
  sampled <- species_points %>%
    mutate(
      .x = coords[, 1],
      .y = coords[, 2],
      .x_bin = dplyr::ntile(.data$.x, n_bins),
      .y_bin = dplyr::ntile(.data$.y, n_bins),
      .spatial_bin = paste(.data$.x_bin, .data$.y_bin, sep = "_")
    ) %>%
    group_by(.data$.spatial_bin) %>%
    mutate(.bin_n = dplyr::n()) %>%
    ungroup() %>%
    mutate(.sample_weight = 1 / .data$.bin_n) %>%
    slice_sample(n = max_points, weight_by = .data$.sample_weight) %>%
    select(-any_of(c(".x", ".y", ".x_bin", ".y_bin", ".spatial_bin", ".bin_n", ".sample_weight")))
  sampled
}

sample_background_points <- function(area_sf, current_stack, presence_points, multiplier, minimum_n, maximum_n = Inf) {
  n_presence <- nrow(presence_points)
  n_target <- max(minimum_n, n_presence * multiplier)
  n_target <- min(n_target, maximum_n)
  area_vect <- terra::vect(area_sf)
  mask_raster <- terra::crop(current_stack[[1]], area_vect)
  mask_raster <- terra::mask(mask_raster, area_vect)
  pres_cells <- unique(terra::cellFromXY(mask_raster, st_coordinates(presence_points)))
  samples <- terra::spatSample(mask_raster, size = n_target * 4, method = "random", na.rm = TRUE, xy = TRUE)
  sample_df <- as.data.frame(samples)
  coord_cols <- intersect(c("x", "y"), names(sample_df))
  if (length(coord_cols) != 2) {
    stop("Background sampling did not return x/y coordinates.")
  }
  value_cols <- setdiff(names(sample_df), coord_cols)
  if (length(value_cols) > 0) {
    names(sample_df)[match(value_cols[[1]], names(sample_df))] <- "mask_value"
  } else {
    sample_df$mask_value <- 1
  }
  sample_df$cell_id <- terra::cellFromXY(mask_raster, sample_df[, coord_cols, drop = FALSE])
  sample_df <- sample_df %>%
    filter(!.data$cell_id %in% pres_cells) %>%
    distinct(.data$cell_id, .keep_all = TRUE) %>%
    slice_head(n = n_target)
  st_as_sf(sample_df, coords = coord_cols, crs = 4326)
}

build_model_dataset <- function(presence_points, background_points, env_stack) {
  pres_env <- terra::extract(env_stack, vect(presence_points)) %>% as_tibble() %>% select(-ID)
  back_env <- terra::extract(env_stack, vect(background_points)) %>% as_tibble() %>% select(-ID)

  bind_rows(
    bind_cols(tibble(pa = 1, longitude = st_coordinates(presence_points)[, 1], latitude = st_coordinates(presence_points)[, 2]), pres_env),
    bind_cols(tibble(pa = 0, longitude = st_coordinates(background_points)[, 1], latitude = st_coordinates(background_points)[, 2]), back_env)
  ) %>% tidyr::drop_na()
}

make_fold_ids <- function(model_df, k = 5, spatial_threshold = 20) {
  n_presence <- sum(model_df$pa == 1)
  if (n_presence >= spatial_threshold) {
    pres_idx <- which(model_df$pa == 1)
    centers <- min(k, max(2, length(pres_idx)))
    km <- stats::kmeans(scale(model_df[pres_idx, c("longitude", "latitude")]), centers = centers)
    fold <- sample(rep(seq_len(k), length.out = nrow(model_df)))
    fold[pres_idx] <- ((km$cluster - 1) %% k) + 1
    fold[model_df$pa == 0] <- sample(rep(seq_len(k), length.out = sum(model_df$pa == 0)))
  } else {
    fold <- sample(rep(seq_len(k), length.out = nrow(model_df)))
  }
  fold
}

compute_metrics <- function(obs, pred) {
  if (length(unique(obs)) < 2 || all(is.na(pred))) {
    return(tibble(auc = NA_real_, tss = NA_real_, threshold = NA_real_))
  }
  roc_obj <- pROC::roc(obs, pred, quiet = TRUE)
  auc_value <- as.numeric(pROC::auc(roc_obj))
  thr_seq <- unique(stats::quantile(pred, probs = seq(0.05, 0.95, by = 0.05), na.rm = TRUE))
  eval_tbl <- lapply(thr_seq, function(thr) {
    bin <- ifelse(pred >= thr, 1, 0)
    tp <- sum(obs == 1 & bin == 1)
    fn <- sum(obs == 1 & bin == 0)
    tn <- sum(obs == 0 & bin == 0)
    fp <- sum(obs == 0 & bin == 1)
    sens <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))
    spec <- ifelse(tn + fp == 0, NA_real_, tn / (tn + fp))
    tibble(threshold = thr, tss = sens + spec - 1)
  }) %>% bind_rows()
  best <- eval_tbl %>% filter(!is.na(.data$tss)) %>% arrange(desc(.data$tss), .data$threshold) %>% slice_head(n = 1)
  tibble(auc = auc_value, tss = best$tss[[1]] %||% NA_real_, threshold = best$threshold[[1]] %||% NA_real_)
}

fit_glm_model <- function(train_df, predictors) {
  formula <- stats::as.formula(paste("pa ~", paste(predictors, collapse = " + ")))
  stats::glm(formula, data = train_df, family = stats::binomial())
}

fit_gam_model <- function(train_df, predictors) {
  smooth_terms <- paste0("s(", predictors, ", k = 4)")
  formula <- stats::as.formula(paste("pa ~", paste(smooth_terms, collapse = " + ")))
  mgcv::gam(formula, data = train_df, family = stats::binomial(), method = "REML")
}

fit_gbm_model <- function(train_df, predictors) {
  gbm::gbm(
    stats::as.formula(paste("pa ~", paste(predictors, collapse = " + "))),
    data = train_df,
    distribution = "bernoulli",
    n.trees = 1200,
    interaction.depth = 3,
    shrinkage = 0.01,
    bag.fraction = 0.7,
    n.minobsinnode = 5,
    verbose = FALSE
  )
}

fit_rf_model <- function(train_df, predictors) {
  train_df$pa_factor <- factor(train_df$pa, levels = c(0, 1), labels = c("abs", "pres"))
  randomForest::randomForest(
    x = train_df[, predictors, drop = FALSE],
    y = train_df$pa_factor,
    ntree = 300
  )
}

fit_maxnet_model <- function(train_df, predictors) {
  maxnet::maxnet(
    p = train_df$pa,
    data = train_df[, predictors, drop = FALSE],
    f = maxnet::maxnet.formula(train_df$pa, train_df[, predictors, drop = FALSE], classes = "lqh")
  )
}

fit_maxent_jar_model <- function(env_stack, presence_points, background_points) {
  jar_paths <- c(
    file.path(Sys.getenv("HOME"), ".dismo", "maxent.jar"),
    file.path(Sys.getenv("HOME"), "Library", "Application Support", "dismo", "maxent.jar"),
    file.path(DATA_DIR, "maxent.jar")
  )
  jar_path <- jar_paths[file.exists(jar_paths)][1]
  if (is.na(jar_path) || is.null(jar_path)) stop("maxent.jar not found")
  options(dismo_maxent = jar_path)
  dismo::maxent(
    x = raster::stack(env_stack),
    p = st_coordinates(presence_points),
    a = st_coordinates(background_points),
    args = c("responsecurves=false", "pictures=false", "jackknife=false")
  )
}

predict_table_values <- function(model, algorithm, newdata, predictors) {
  if (algorithm == "GLM") {
    return(as.numeric(stats::predict(model, newdata = newdata[, predictors, drop = FALSE], type = "response")))
  }
  if (algorithm == "GAM") {
    return(as.numeric(stats::predict(model, newdata = newdata[, predictors, drop = FALSE], type = "response")))
  }
  if (algorithm == "GBM") {
    return(as.numeric(stats::predict(model, newdata = newdata[, predictors, drop = FALSE], n.trees = model$n.trees, type = "response")))
  }
  if (algorithm == "RF") {
    pred <- stats::predict(model, newdata = newdata[, predictors, drop = FALSE], type = "prob")
    return(as.numeric(pred[, "pres"]))
  }
  if (algorithm == "MaxEnt" && inherits(model, "maxnet")) {
    return(as.numeric(stats::predict(model, newdata = newdata[, predictors, drop = FALSE], type = "cloglog")))
  }
  rep(NA_real_, nrow(newdata))
}

predict_raster_values <- function(model, algorithm, env_stack, predictors) {
  layer_stack <- env_stack[[predictors]]
  if (algorithm %in% c("GLM", "GAM")) {
    return(terra::predict(layer_stack, model, type = "response", na.rm = TRUE))
  }
  if (algorithm == "GBM") {
    return(terra::predict(layer_stack, model, fun = function(m, d) stats::predict(m, newdata = as.data.frame(d), n.trees = m$n.trees, type = "response"), na.rm = TRUE))
  }
  if (algorithm == "RF") {
    return(terra::predict(layer_stack, model, type = "prob", index = 2, na.rm = TRUE))
  }
  if (algorithm == "MaxEnt" && inherits(model, "MaxEnt")) {
    return(terra::rast(dismo::predict(model, raster::stack(layer_stack), progress = "")))
  }
  if (algorithm == "MaxEnt" && inherits(model, "maxnet")) {
    return(terra::predict(layer_stack, model, type = "cloglog", na.rm = TRUE))
  }
  NULL
}

fit_species_models <- function(species_name, species_points, current_stack, model_cfg, algo_tbl) {
  presence_threshold <- as.integer(get_config_value(model_cfg, "presence_cell_threshold", "10"))
  presence_maximum <- as.integer(get_config_value(model_cfg, "presence_cell_maximum", "10000"))
  pseudo_mult <- as.integer(get_config_value(model_cfg, "pseudoabsence_multiplier", "3"))
  pseudo_min <- as.integer(get_config_value(model_cfg, "pseudoabsence_minimum", "1000"))
  pseudo_max <- as.integer(get_config_value(model_cfg, "pseudoabsence_maximum", "20000"))
  cv_folds <- as.integer(get_config_value(model_cfg, "cv_folds", "5"))
  spatial_threshold <- as.integer(get_config_value(model_cfg, "cv_presence_threshold_spatial", "20"))
  tss_threshold <- as.numeric(get_config_value(model_cfg, "tss_selection_threshold", "0.6"))
  buffer_km <- as.numeric(get_config_value(model_cfg, "buffer_km", "500"))

  n_raw_points <- nrow(species_points)
  species_points <- thin_presence_by_cells(species_points, current_stack)
  species_points <- spatially_balance_presence_points(species_points, max_points = presence_maximum)
  n_thinned_points <- nrow(species_points)
  if (n_thinned_points < presence_threshold) {
    return(list(
      status = tibble(
        species = species_name,
        match_status = "matched",
        n_raw = n_raw_points,
        n_clean = n_thinned_points,
        n_cell_unique = n_thinned_points,
        model_status = "skipped",
        skip_reason = "insufficient_occurrence_points",
        cv_strategy = NA_character_,
        maxent_engine = ifelse(any(algo_tbl$algorithm == "MaxEnt" & algo_tbl$available), algo_tbl$engine[algo_tbl$algorithm == "MaxEnt"][1], "missing")
      ),
      metrics = tibble(),
      metrics_by_fold = tibble(),
      thresholds = tibble(),
      selected_algorithms = character(0),
      fitted_models = list(),
      threshold = NA_real_,
      probability_current = NULL,
      binary_current = NULL,
      area_sf = NULL,
      presence_points_used = species_points[0, , drop = FALSE]
    ))
  }

  provinces <- load_province_boundaries()
  china_boundary <- build_china_boundary(provinces)
  accessible_area <- make_accessible_area(species_points, china_boundary, buffer_km = buffer_km)
  background_points <- sample_background_points(accessible_area, current_stack, species_points, pseudo_mult, pseudo_min, maximum_n = pseudo_max)
  model_df <- build_model_dataset(species_points, background_points, current_stack)
  predictors <- setdiff(names(model_df), c("pa", "longitude", "latitude"))
  model_df <- model_df %>% filter(complete.cases(across(all_of(c("pa", predictors)))))
  presence_xy_used <- model_df %>%
    filter(.data$pa == 1) %>%
    distinct(.data$longitude, .data$latitude)
  presence_points_used <- species_points %>%
    semi_join(presence_xy_used, by = c("longitude", "latitude")) %>%
    distinct(.data$species, .data$longitude, .data$latitude, .keep_all = TRUE)
  n_presence_used <- nrow(presence_points_used)
  n_background_used <- sum(model_df$pa == 0)
  safe_message(sprintf(
    "Species modeling set prepared: %s | raw=%s | cell_thinned=%s | predictors_complete=%s | pseudoabsence=%s | rows=%s",
    species_name,
    format(n_raw_points, big.mark = ","),
    format(n_thinned_points, big.mark = ","),
    format(n_presence_used, big.mark = ","),
    format(n_background_used, big.mark = ","),
    format(nrow(model_df), big.mark = ",")
  ))
  if (n_presence_used < presence_threshold) {
    return(list(
      status = tibble(
        species = species_name,
        match_status = "matched",
        n_raw = n_raw_points,
        n_clean = n_presence_used,
        n_cell_unique = n_presence_used,
        model_status = "skipped",
        skip_reason = "insufficient_complete_predictor_points",
        cv_strategy = NA_character_,
        maxent_engine = ifelse(any(algo_tbl$algorithm == "MaxEnt" & algo_tbl$available), algo_tbl$engine[algo_tbl$algorithm == "MaxEnt"][1], "missing")
      ),
      metrics = tibble(),
      metrics_by_fold = tibble(),
      thresholds = tibble(),
      selected_algorithms = character(0),
      fitted_models = list(),
      threshold = NA_real_,
      probability_current = NULL,
      binary_current = NULL,
      area_sf = accessible_area,
      presence_points_used = presence_points_used
    ))
  }
  fold_id <- make_fold_ids(model_df, k = cv_folds, spatial_threshold = spatial_threshold)
  cv_strategy <- ifelse(sum(model_df$pa == 1) >= spatial_threshold, "spatial_kmeans_blocks", "random_repeated")

  algorithm_order <- c("GLM", "GAM", "GBM", "RF", "MaxEnt")
  metrics_rows <- list()
  fitted_models <- list()

  for (algorithm in algorithm_order) {
    available_row <- algo_tbl %>% filter(.data$algorithm == !!algorithm)
    if (nrow(available_row) == 0 || !isTRUE(available_row$available[[1]])) {
      metrics_rows[[algorithm]] <- tibble(algorithm = algorithm, fold = NA_integer_, auc = NA_real_, tss = NA_real_, threshold = NA_real_, available = FALSE)
      next
    }

    fold_metrics <- vector("list", cv_folds)
    for (fold in seq_len(cv_folds)) {
      train_df <- model_df[fold_id != fold, , drop = FALSE]
      test_df <- model_df[fold_id == fold, , drop = FALSE]
      model_object <- NULL
      if (algorithm == "GLM") model_object <- fit_glm_model(train_df, predictors)
      if (algorithm == "GAM") model_object <- fit_gam_model(train_df, predictors)
      if (algorithm == "GBM" && requireNamespace("gbm", quietly = TRUE)) model_object <- fit_gbm_model(train_df, predictors)
      if (algorithm == "RF" && requireNamespace("randomForest", quietly = TRUE)) model_object <- fit_rf_model(train_df, predictors)
      if (algorithm == "MaxEnt" && requireNamespace("maxnet", quietly = TRUE)) model_object <- fit_maxnet_model(train_df, predictors)
      if (algorithm == "MaxEnt" && !requireNamespace("maxnet", quietly = TRUE) && grepl("maxent.jar", available_row$engine[[1]], fixed = TRUE)) {
        train_presence <- st_as_sf(train_df %>% filter(.data$pa == 1), coords = c("longitude", "latitude"), crs = 4326)
        train_background <- st_as_sf(train_df %>% filter(.data$pa == 0), coords = c("longitude", "latitude"), crs = 4326)
        model_object <- tryCatch(fit_maxent_jar_model(current_stack[[predictors]], train_presence, train_background), error = function(e) NULL)
      }
      if (is.null(model_object)) {
        fold_metrics[[fold]] <- tibble(algorithm = algorithm, fold = fold, auc = NA_real_, tss = NA_real_, threshold = NA_real_, available = FALSE)
      } else {
        pred <- tryCatch(predict_table_values(model_object, algorithm, test_df, predictors), error = function(e) rep(NA_real_, nrow(test_df)))
        metric <- compute_metrics(test_df$pa, pred)
        fold_metrics[[fold]] <- bind_cols(tibble(algorithm = algorithm, fold = fold, available = TRUE), metric)
      }
    }

    metrics_tbl <- bind_rows(fold_metrics)
    metrics_rows[[algorithm]] <- metrics_tbl

    full_model <- NULL
    if (algorithm == "GLM") full_model <- fit_glm_model(model_df, predictors)
    if (algorithm == "GAM") full_model <- fit_gam_model(model_df, predictors)
    if (algorithm == "GBM" && requireNamespace("gbm", quietly = TRUE)) full_model <- fit_gbm_model(model_df, predictors)
    if (algorithm == "RF" && requireNamespace("randomForest", quietly = TRUE)) full_model <- fit_rf_model(model_df, predictors)
    if (algorithm == "MaxEnt" && requireNamespace("maxnet", quietly = TRUE)) full_model <- fit_maxnet_model(model_df, predictors)
    if (algorithm == "MaxEnt" && is.null(full_model) && grepl("maxent.jar", available_row$engine[[1]], fixed = TRUE)) {
      full_model <- tryCatch(fit_maxent_jar_model(current_stack[[predictors]], species_points, background_points), error = function(e) NULL)
    }
    fitted_models[[algorithm]] <- full_model
  }

  metrics_all <- bind_rows(metrics_rows)
  summary_metrics <- metrics_all %>%
    group_by(.data$algorithm) %>%
    summarise(
      mean_auc = mean(.data$auc, na.rm = TRUE),
      mean_tss = mean(.data$tss, na.rm = TRUE),
      mean_threshold = mean(.data$threshold, na.rm = TRUE),
      available = any(.data$available),
      .groups = "drop"
    ) %>%
    mutate(mean_auc = ifelse(is.nan(.data$mean_auc), NA_real_, .data$mean_auc), mean_tss = ifelse(is.nan(.data$mean_tss), NA_real_, .data$mean_tss), mean_threshold = ifelse(is.nan(.data$mean_threshold), NA_real_, .data$mean_threshold))

  selected_algorithms <- summary_metrics %>% filter(!is.na(.data$mean_tss), .data$mean_tss >= tss_threshold) %>% pull(.data$algorithm)
  if (length(selected_algorithms) == 0) {
    best_algorithm <- summary_metrics %>% filter(!is.na(.data$mean_tss)) %>% arrange(desc(.data$mean_tss)) %>% slice_head(n = 1) %>% pull(.data$algorithm)
    selected_algorithms <- best_algorithm
  }
  selected_algorithms <- selected_algorithms[!is.na(selected_algorithms)]
  ensemble_threshold <- summary_metrics %>% filter(.data$algorithm %in% selected_algorithms) %>% summarise(value = mean(.data$mean_threshold, na.rm = TRUE)) %>% pull(value)
  ensemble_threshold <- ifelse(is.nan(ensemble_threshold), 0.5, ensemble_threshold)

  probability_current <- lapply(selected_algorithms, function(algorithm) {
    predict_raster_values(fitted_models[[algorithm]], algorithm, current_stack, predictors)
  })
  names(probability_current) <- selected_algorithms
  probability_current <- probability_current[!vapply(probability_current, is.null, logical(1))]
  ensemble_probability <- if (length(probability_current) == 0) NULL else Reduce(`+`, probability_current) / length(probability_current)
  binary_current <- if (is.null(ensemble_probability)) NULL else ensemble_probability >= ensemble_threshold

  status_tbl <- tibble(
    species = species_name,
    match_status = "matched",
    n_raw = n_raw_points,
    n_clean = n_presence_used,
    n_cell_unique = n_presence_used,
    model_status = ifelse(length(probability_current) > 0, "modeled", "failed"),
    skip_reason = ifelse(length(probability_current) > 0, NA_character_, "no_algorithm_available"),
    cv_strategy = cv_strategy,
    maxent_engine = algo_tbl %>% filter(.data$algorithm == "MaxEnt") %>% pull(engine) %>% .[[1]]
  )

  list(
    status = status_tbl,
    metrics = summary_metrics %>% mutate(species = species_name),
    metrics_by_fold = metrics_all %>% mutate(species = species_name) %>% relocate(.data$species),
    thresholds = bind_rows(
      tibble(
        species = species_name,
        threshold_type = "model_config",
        threshold_name = c("presence_cell_threshold", "presence_cell_maximum", "pseudoabsence_multiplier", "pseudoabsence_minimum", "pseudoabsence_maximum", "cv_folds", "cv_presence_threshold_spatial", "tss_selection_threshold", "buffer_km"),
        algorithm = NA_character_,
        threshold_value = c(presence_threshold, presence_maximum, pseudo_mult, pseudo_min, pseudo_max, cv_folds, spatial_threshold, tss_threshold, buffer_km)
      ),
      summary_metrics %>%
        transmute(
          species = species_name,
          threshold_type = "algorithm_mean_threshold",
          threshold_name = "mean_threshold",
          algorithm = .data$algorithm,
          threshold_value = .data$mean_threshold
        ),
      tibble(
        species = species_name,
        threshold_type = "ensemble_binary_threshold",
        threshold_name = "ensemble_binary_threshold",
        algorithm = paste(selected_algorithms, collapse = " | "),
        threshold_value = ensemble_threshold
      )
    ),
    selected_algorithms = selected_algorithms,
    fitted_models = fitted_models,
    threshold = ensemble_threshold,
    probability_current = ensemble_probability,
    binary_current = binary_current,
    area_sf = accessible_area,
    predictors = predictors,
    presence_points_used = presence_points_used
  )
}

save_raster_if_present <- function(r, path) {
  if (!is.null(r)) {
    terra::writeRaster(r, path, overwrite = TRUE)
  }
}

normalize_worker_count <- function(workers, n_tasks) {
  requested <- suppressWarnings(as.integer(workers))
  if (is.na(requested) || requested < 1) requested <- 1L
  detected <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (is.na(detected) || detected < 1) {
    detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }
  if (is.na(detected) || detected < 1) {
    detected <- requested
  }
  max_workers <- max(1L, min(as.integer(detected), as.integer(n_tasks)))
  min(requested, max_workers)
}

get_province_thresholds <- function(model_cfg) {
  threshold_keys <- model_cfg %>%
    filter(stringr::str_detect(.data$key, "^province_threshold_[a-z]+$")) %>%
    pull(.data$key)
  if (length(threshold_keys) == 0) {
    threshold_keys <- c(
      "province_threshold_a",
      "province_threshold_b",
      "province_threshold_c",
      "province_threshold_d",
      "province_threshold_e",
      "province_threshold_f"
    )
  }
  thresholds <- model_cfg %>%
    filter(.data$key %in% threshold_keys) %>%
    mutate(order_id = match(.data$key, threshold_keys)) %>%
    arrange(.data$order_id) %>%
    pull(.data$value) %>%
    as.integer()
  thresholds <- thresholds[!is.na(thresholds) & thresholds > 0]
  unique(thresholds)
}

progress_record_path <- function(species_name) {
  file.path(PROGRESS_SPECIES_DIR, paste0(normalize_species_key(species_name), ".csv"))
}

build_progress_row <- function(species_name, progress_status, model_status = NA_character_, skip_reason = NA_character_, detail = NA_character_) {
  tibble(
    species = species_name,
    progress_status = progress_status,
    model_status = model_status,
    skip_reason = skip_reason,
    detail = detail,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

write_progress_summary <- function() {
  ensure_dir(PROGRESS_DIR)
  ensure_dir(PROGRESS_SPECIES_DIR)
  progress_files <- list.files(PROGRESS_SPECIES_DIR, pattern = "\\.csv$", full.names = TRUE)
  if (length(progress_files) == 0) {
    summary_tbl <- tibble(
      total_species = 0L,
      queued = 0L,
      running = 0L,
      completed = 0L,
      skipped = 0L,
      failed = 0L,
      finished = 0L,
      remaining = 0L,
      last_update = NA_character_,
      running_species = NA_character_
    )
  } else {
    progress_tbl <- bind_rows(lapply(progress_files, function(path) suppressMessages(readr::read_csv(path, show_col_types = FALSE))))
    last_update_value <- suppressWarnings(max(progress_tbl$updated_at, na.rm = TRUE))
    if (!is.finite(last_update_value)) last_update_value <- NA_character_
    summary_tbl <- tibble(
      total_species = nrow(progress_tbl),
      queued = sum(progress_tbl$progress_status == "queued", na.rm = TRUE),
      running = sum(progress_tbl$progress_status == "running", na.rm = TRUE),
      completed = sum(progress_tbl$progress_status == "completed", na.rm = TRUE),
      skipped = sum(progress_tbl$progress_status == "skipped", na.rm = TRUE),
      failed = sum(progress_tbl$progress_status == "failed", na.rm = TRUE),
      finished = sum(progress_tbl$progress_status %in% c("completed", "skipped", "failed"), na.rm = TRUE),
      remaining = sum(progress_tbl$progress_status %in% c("queued", "running"), na.rm = TRUE),
      last_update = as.character(last_update_value),
      running_species = paste(progress_tbl$species[progress_tbl$progress_status == "running"], collapse = " | ")
    )
  }
  tmp_path <- paste0(PROGRESS_SUMMARY_PATH, ".tmp_", Sys.getpid())
  readr::write_csv(summary_tbl, tmp_path)
  file.rename(tmp_path, PROGRESS_SUMMARY_PATH)
  invisible(summary_tbl)
}

update_progress_record <- function(species_name, progress_status, model_status = NA_character_, skip_reason = NA_character_, detail = NA_character_) {
  ensure_dir(PROGRESS_DIR)
  ensure_dir(PROGRESS_SPECIES_DIR)
  row <- build_progress_row(species_name, progress_status, model_status = model_status, skip_reason = skip_reason, detail = detail)
  record_path <- progress_record_path(species_name)
  tmp_path <- paste0(record_path, ".tmp_", Sys.getpid())
  readr::write_csv(row, tmp_path)
  file.rename(tmp_path, record_path)
  cat(
    paste(row$updated_at[[1]], row$species[[1]], row$progress_status[[1]], row$model_status[[1]] %||% "", row$skip_reason[[1]] %||% "", row$detail[[1]] %||% "", sep = "	"),
    "
",
    file = PROGRESS_EVENT_LOG_PATH,
    append = TRUE
  )
  write_progress_summary()
  invisible(row)
}

initialize_progress_tracking <- function(species_names) {
  ensure_dir(PROGRESS_DIR)
  ensure_dir(PROGRESS_SPECIES_DIR)
  old_progress_files <- list.files(PROGRESS_SPECIES_DIR, pattern = "\\.csv$", full.names = TRUE)
  if (length(old_progress_files) > 0) unlink(old_progress_files, force = TRUE)
  if (file.exists(PROGRESS_SUMMARY_PATH)) unlink(PROGRESS_SUMMARY_PATH, force = TRUE)
  if (file.exists(PROGRESS_EVENT_LOG_PATH)) unlink(PROGRESS_EVENT_LOG_PATH, force = TRUE)
  if (length(species_names) > 0) {
    for (species_name in species_names) {
      row <- build_progress_row(species_name, "queued", detail = "waiting_to_start")
      readr::write_csv(row, progress_record_path(species_name))
    }
  }
  write_progress_summary()
  safe_message("Progress summary file: ", PROGRESS_SUMMARY_PATH)
}

failed_species_result <- function(species_name, species_points, algo_tbl, skip_reason = "runtime_error", detail = NA_character_) {
  list(
    status = tibble(
      species = species_name,
      match_status = "matched",
      n_raw = nrow(species_points),
      n_clean = nrow(species_points),
      n_cell_unique = nrow(species_points),
      model_status = "failed",
      skip_reason = skip_reason,
      cv_strategy = NA_character_,
      maxent_engine = algo_tbl %>% filter(.data$algorithm == "MaxEnt") %>% pull(.data$engine) %>% .[[1]]
    ),
    metrics = tibble(),
    metrics_by_fold = tibble(),
    province = tibble(),
    area_change = tibble(),
    thresholds = tibble(),
    points_used = tibble(),
    figure_paths = character(0),
    raster_manifest = tibble(),
    qa_log = log_qa("model", species_name, "failed", detail)
  )
}

execute_species_task <- function(species_name, species_points, current_stack = NULL, model_config, algo_tbl, provinces, dash_line = NULL, future_stacks = NULL, province_thresholds, current_config = NULL, china_boundary = NULL, selected_predictors = NULL, future_configs = NULL, force_download = FALSE) {
  tryCatch(
    run_species_task(
      species_name = species_name,
      species_points = species_points,
      current_stack = current_stack,
      model_config = model_config,
      algo_tbl = algo_tbl,
      provinces = provinces,
      dash_line = dash_line,
      future_stacks = future_stacks,
      province_thresholds = province_thresholds,
      current_config = current_config,
      china_boundary = china_boundary,
      selected_predictors = selected_predictors,
      future_configs = future_configs,
      force_download = force_download
    ),
    error = function(e) {
      detail <- conditionMessage(e)
      update_progress_record(species_name, "failed", model_status = "failed", skip_reason = "runtime_error", detail = detail)
      failed_species_result(species_name, species_points, algo_tbl, skip_reason = "runtime_error", detail = detail)
    }
  )
}

parallel_species_task_runner <- function(task) {
  execute_species_task(
    species_name = task$species_name,
    species_points = task$species_points,
    current_stack = NULL,
    model_config = model_config,
    algo_tbl = algo_tbl,
    provinces = provinces,
    dash_line = dash_line,
    future_stacks = NULL,
    province_thresholds = province_thresholds,
    current_config = current_config,
    china_boundary = china_boundary,
    selected_predictors = selected_predictors,
    future_configs = future_configs,
    force_download = force_download
  )
}

run_species_task <- function(species_name, species_points, current_stack = NULL, model_config, algo_tbl, provinces, dash_line = NULL, future_stacks = NULL, province_thresholds, current_config = NULL, china_boundary = NULL, selected_predictors = NULL, future_configs = NULL, force_download = FALSE) {
  if (!inherits(species_points, "sf")) {
    species_points <- st_as_sf(species_points, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  }
  safe_message("Modeling species: ", species_name)
  update_progress_record(species_name, "running", detail = "worker_started")
  qa_rows <- list()
  if (is.null(current_stack)) {
    current_stack <- WORKER_CURRENT_STACK
    if (is.null(current_stack)) {
      current_stack <- load_environment_stack(current_config, china_boundary, force_download = force_download)
      if (!is.null(current_stack)) {
        WORKER_CURRENT_STACK <<- current_stack
      }
    }
    if (is.null(current_stack)) {
      qa_rows[[length(qa_rows) + 1]] <- log_qa("climate", species_name, "missing", "Current climate raster unavailable inside worker")
      update_progress_record(species_name, "failed", model_status = "failed", skip_reason = "missing_current_climate", detail = "current climate unavailable inside worker")
      return(list(
        status = tibble(
          species = species_name,
          match_status = "matched",
          n_raw = nrow(species_points),
          n_clean = nrow(species_points),
          n_cell_unique = nrow(species_points),
          model_status = "failed",
          skip_reason = "missing_current_climate",
          cv_strategy = NA_character_,
          maxent_engine = algo_tbl %>% filter(.data$algorithm == "MaxEnt") %>% pull(engine) %>% .[[1]]
        ),
        metrics = tibble(),
        metrics_by_fold = tibble(),
        province = tibble(),
        area_change = tibble(),
        thresholds = tibble(),
        points_used = tibble(),
        figure_paths = character(0),
        raster_manifest = tibble(),
        qa_log = bind_rows(qa_rows)
      ))
    }
    if (!is.null(selected_predictors) && length(selected_predictors) > 0) {
      current_stack <- current_stack[[selected_predictors]]
    }
  }
  if (is.null(future_stacks)) {
    future_stacks <- WORKER_FUTURE_STACKS
    if (is.null(future_stacks)) {
      future_stacks <- list()
    }
    if (length(future_stacks) == 0 && !is.null(future_configs) && nrow(future_configs) > 0) {
      for (row_i in seq_len(nrow(future_configs))) {
        row <- future_configs[row_i, , drop = FALSE]
        future_stack <- load_environment_stack(row, china_boundary, force_download = force_download)
        if (is.null(future_stack)) {
          qa_rows[[length(qa_rows) + 1]] <- log_qa("climate", row$scenario[[1]], "missing", paste0("Future climate raster unavailable for ", species_name))
          next
        }
        if (!is.null(selected_predictors) && length(selected_predictors) > 0) {
          future_stack <- future_stack[[selected_predictors]]
        }
        future_stacks[[row$scenario[[1]]]] <- list(config = row, stack = future_stack)
      }
      WORKER_FUTURE_STACKS <<- future_stacks
    }
  }
  sp_points <- species_points %>% distinct(.data$species, .data$longitude, .data$latitude, .keep_all = TRUE)
  model_fit <- fit_species_models(species_name, sp_points, current_stack, model_config, algo_tbl)

  result <- list(
    status = model_fit$status,
    metrics = model_fit$metrics,
    metrics_by_fold = model_fit$metrics_by_fold,
    province = tibble(),
    area_change = tibble(),
    thresholds = model_fit$thresholds,
    points_used = tibble(),
    figure_paths = character(0),
    raster_manifest = tibble(),
    qa_log = bind_rows(qa_rows)
  )

  if (!identical(model_fit$status$model_status[[1]], "modeled")) {
    progress_state <- if (identical(model_fit$status$model_status[[1]], "skipped")) "skipped" else "failed"
    update_progress_record(
      species_name,
      progress_state,
      model_status = model_fit$status$model_status[[1]],
      skip_reason = model_fit$status$skip_reason[[1]],
      detail = "model_fit_finished"
    )
    return(result)
  }

  species_slug <- str_replace_all(tolower(species_name), "[^a-z0-9]+", "_")
  species_dir <- ensure_dir(file.path(RASTER_DIR, species_slug))
  points_used_path <- file.path(POINTS_USED_DIR, paste0(species_slug, "_points_used.csv"))
  raster_rows <- list()

  points_used_tbl <- model_fit$presence_points_used %>%
    st_drop_geometry() %>%
    mutate(
      species = species_name,
      record_type = "occurrence_point_used_for_modeling",
      points_used_csv = make_repo_relative_path(points_used_path)
    ) %>%
    select(
      .data$record_type,
      .data$species,
      everything()
    )
  if (nrow(points_used_tbl) > 0) {
    readr::write_csv(points_used_tbl, points_used_path)
  }
  result$points_used <- points_used_tbl

  current_probability_path <- file.path(species_dir, paste0(species_slug, "_current_probability.tif"))
  current_binary_path <- file.path(species_dir, paste0(species_slug, "_current_binary.tif"))
  save_raster_if_present(model_fit$probability_current, current_probability_path)
  save_raster_if_present(model_fit$binary_current, current_binary_path)
  raster_rows[[length(raster_rows) + 1]] <- tibble(species = species_name, scenario = "current", gcm = "ensemble_mean", raster_type = "probability", raster_path = current_probability_path)
  raster_rows[[length(raster_rows) + 1]] <- tibble(species = species_name, scenario = "current", gcm = "ensemble_mean", raster_type = "binary", raster_path = current_binary_path)

  province_rows <- list()
  area_change_rows <- list()

  current_province <- province_sensitivity_summary(model_fit$binary_current, provinces, species_name, "current", "ensemble_mean", thresholds = province_thresholds)
  province_rows[["current"]] <- current_province

  current_area <- current_province %>%
    filter(.data$cell_threshold == 3) %>%
    summarise(total_area_km2 = sum(.data$suitable_area_km2, na.rm = TRUE)) %>%
    pull(total_area_km2)
  area_change_rows[["current"]] <- tibble(species = species_name, scenario = "current", gcm = "ensemble_mean", total_area_km2 = current_area, delta_area_km2 = 0)

  current_map <- plot_probability_map(
    model_fit$probability_current,
    provinces,
    dash_line,
    paste0(species_name, " - current suitability"),
    occurrence_points = model_fit$presence_points_used
  )
  current_png <- file.path(FIGURE_MAP_DIR, paste0(species_slug, "_current_probability.png"))
  current_pdf <- file.path(FIGURE_MAP_DIR, paste0(species_slug, "_current_probability.pdf"))
  ggsave(current_png, current_map, width = 8.5, height = 6.0, dpi = 320, bg = "white")
  ggsave(current_pdf, current_map, width = 8.5, height = 6.0, device = cairo_pdf, bg = "white")
  result$figure_paths <- c(result$figure_paths, current_png)

  if (length(future_stacks) > 0) {
    for (stack_item in future_stacks) {
      row <- stack_item$config
      future_stack <- stack_item$stack[[model_fit$predictors]]
      probability_layers <- lapply(model_fit$selected_algorithms, function(algorithm) {
        predict_raster_values(model_fit$fitted_models[[algorithm]], algorithm, future_stack, model_fit$predictors)
      })
      probability_layers <- probability_layers[!vapply(probability_layers, is.null, logical(1))]
      if (length(probability_layers) == 0) next
      future_probability <- Reduce(`+`, probability_layers) / length(probability_layers)
      future_binary <- future_probability >= model_fit$threshold
      future_probability_path <- file.path(species_dir, paste0(species_slug, "_", row$scenario[[1]], "_probability.tif"))
      future_binary_path <- file.path(species_dir, paste0(species_slug, "_", row$scenario[[1]], "_binary.tif"))
      save_raster_if_present(future_probability, future_probability_path)
      save_raster_if_present(future_binary, future_binary_path)
      raster_rows[[length(raster_rows) + 1]] <- tibble(species = species_name, scenario = row$ensemble_group[[1]], gcm = row$gcm[[1]], raster_type = "probability", raster_path = future_probability_path)
      raster_rows[[length(raster_rows) + 1]] <- tibble(species = species_name, scenario = row$ensemble_group[[1]], gcm = row$gcm[[1]], raster_type = "binary", raster_path = future_binary_path)
      province_tbl <- province_sensitivity_summary(future_binary, provinces, species_name, row$ensemble_group[[1]], row$gcm[[1]], thresholds = province_thresholds)
      province_rows[[row$scenario[[1]]]] <- province_tbl
      future_area <- province_tbl %>%
        filter(.data$cell_threshold == 3) %>%
        summarise(total_area_km2 = sum(.data$suitable_area_km2, na.rm = TRUE)) %>%
        pull(total_area_km2)
      area_change_rows[[row$scenario[[1]]]] <- tibble(species = species_name, scenario = row$ensemble_group[[1]], gcm = row$gcm[[1]], total_area_km2 = future_area, delta_area_km2 = future_area - current_area)
    }
  }

  result$province <- bind_rows(province_rows)
  result$area_change <- bind_rows(area_change_rows)
  result$raster_manifest <- bind_rows(raster_rows)
  update_progress_record(species_name, "completed", model_status = "modeled", skip_reason = model_fit$status$skip_reason[[1]], detail = "model_outputs_written")
  result
}

province_sensitivity_summary <- function(binary_raster, provinces, species, scenario, gcm, thresholds = c(3, 10, 20, 50, 100, 200)) {
  if (is.null(binary_raster)) return(tibble())
  if (is.na(st_crs(provinces))) {
    provinces <- st_set_crs(provinces, 4326)
  }
  province_area_km2 <- tryCatch(
    as.numeric(st_area(st_transform(provinces, MAP_CRS))) / 1e6,
    error = function(e) as.numeric(st_area(provinces)) / 1e6
  )
  binary01 <- terra::ifel(binary_raster, 1, 0)
  cell_area <- terra::cellSize(binary01, unit = "km")
  province_vect <- terra::vect(provinces)
  cell_count <- terra::extract(binary01, province_vect, fun = sum, na.rm = TRUE) %>% as_tibble()
  area_sum <- terra::extract(binary01 * cell_area, province_vect, fun = sum, na.rm = TRUE) %>% as_tibble()
  base_tbl <- provinces %>%
    st_drop_geometry() %>%
    mutate(province_total_area_km2 = province_area_km2) %>%
    mutate(ID = row_number()) %>%
    left_join(cell_count %>% rename(suitable_cell_count = 2), by = "ID") %>%
    left_join(area_sum %>% rename(suitable_area_km2 = 2), by = "ID") %>%
    mutate(
      suitable_cell_count = as.integer(round(coalesce(.data$suitable_cell_count, 0))),
      suitable_area_km2 = as.numeric(coalesce(.data$suitable_area_km2, 0)),
      province_total_area_km2 = as.numeric(coalesce(.data$province_total_area_km2, 0))
    )
  bind_rows(lapply(thresholds, function(thr) {
    threshold_tbl <- base_tbl %>%
      mutate(presence_flag = .data$suitable_cell_count >= thr)
    total_suitable_area_threshold <- threshold_tbl %>%
      filter(.data$presence_flag) %>%
      summarise(value = sum(.data$suitable_area_km2, na.rm = TRUE)) %>%
      pull(.data$value)
    total_suitable_area_threshold <- coalesce(total_suitable_area_threshold, 0)
    threshold_tbl %>% transmute(
      species = species,
      scenario = scenario,
      gcm = gcm,
      province = .data$province_cn,
      cell_threshold = thr,
      suitable_cell_count = .data$suitable_cell_count,
      suitable_area_km2 = .data$suitable_area_km2,
      province_total_area_km2 = .data$province_total_area_km2,
      suitable_area_prop_of_species = ifelse(total_suitable_area_threshold > 0, .data$suitable_area_km2 / total_suitable_area_threshold, 0),
      suitable_area_prop_of_province = ifelse(.data$province_total_area_km2 > 0, .data$suitable_area_km2 / .data$province_total_area_km2, 0),
      presence_flag = .data$presence_flag
    )
  }))
}

plot_probability_map <- function(probability_raster, provinces, dash_line, title, occurrence_points = NULL) {
  if (is.na(st_crs(provinces))) {
    provinces <- st_set_crs(provinces, 4326)
  }
  map_raster <- tryCatch(
    terra::project(probability_raster, MAP_CRS, method = "bilinear"),
    error = function(e) probability_raster
  )
  raster_df <- as.data.frame(map_raster, xy = TRUE, na.rm = TRUE)
  names(raster_df) <- c("x", "y", "probability")
  province_map <- tryCatch(st_transform(provinces, MAP_CRS), error = function(e) provinces)
  dash_line_map <- if (is.null(dash_line) || nrow(dash_line) == 0) {
    NULL
  } else {
    tryCatch(st_transform(dash_line, st_crs(province_map)), error = function(e) dash_line)
  }
  point_map <- NULL
  if (!is.null(occurrence_points) && nrow(occurrence_points) > 0) {
    if (!inherits(occurrence_points, "sf")) {
      occurrence_points <- st_as_sf(occurrence_points, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    }
    point_map <- tryCatch(st_transform(occurrence_points, st_crs(province_map)), error = function(e) occurrence_points)
  }
  p <- ggplot() +
    geom_raster(data = raster_df, aes(x = .data$x, y = .data$y, fill = .data$probability)) +
    geom_sf(data = province_map, fill = NA, color = "#2F2F2F", linewidth = 0.2)
  if (!is.null(dash_line_map)) {
    p <- p + geom_sf(data = dash_line_map, inherit.aes = FALSE, color = "#4A4E69", linewidth = 0.25)
  }
  if (!is.null(point_map)) {
    p <- p + geom_sf(data = point_map, inherit.aes = FALSE, shape = 21, size = 1.2, stroke = 0.2, color = "white", fill = "#D1495B", alpha = 0.9)
  }
  p +
    scale_fill_viridis_c(option = "C", name = "Suitability") +
    coord_sf(crs = st_crs(province_map), expand = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_family = "Arial") +
    theme(plot.title = element_text(face = "bold"), legend.position = "right")
}

plot_threshold_sensitivity <- function(province_tbl) {
  province_tbl %>%
    filter(.data$presence_flag) %>%
    group_by(.data$species, .data$scenario, .data$cell_threshold) %>%
    summarise(n_provinces = n(), suitable_area_km2 = sum(.data$suitable_area_km2), .groups = "drop") %>%
    ggplot(aes(x = factor(.data$cell_threshold), y = .data$n_provinces, fill = factor(.data$cell_threshold))) +
    geom_boxplot(width = 0.55, alpha = 0.8) +
    scale_fill_manual(values = c("3" = "#4C956C", "10" = "#D68C45", "20" = "#457B9D", "50" = "#7A4EAB"), name = "Threshold") +
    labs(title = "Province sensitivity analysis", x = "Suitable-cell threshold", y = "No. potential provinces") +
    theme_minimal(base_family = "Arial") +
    theme(plot.title = element_text(face = "bold"))
}

plot_area_change_summary <- function(area_change_tbl) {
  area_change_tbl %>%
    filter(.data$scenario != "current") %>%
    group_by(.data$scenario) %>%
    summarise(mean_delta_km2 = mean(.data$delta_area_km2, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = reorder(.data$scenario, .data$mean_delta_km2), y = .data$mean_delta_km2, fill = .data$mean_delta_km2 > 0)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#4C956C", "FALSE" = "#BC4749"), guide = "none") +
    labs(title = "Mean area change across modeled species", x = NULL, y = "Delta suitable area (km2)") +
    theme_minimal(base_family = "Arial") +
    theme(plot.title = element_text(face = "bold"))
}

write_summary_pptx <- function(summary_stats, figure_paths) {
  if (!requireNamespace("officer", quietly = TRUE)) return(invisible(NULL))
  ppt <- officer::read_pptx()
  ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  ppt <- officer::ph_with(ppt, "Bird SDM summary", location = officer::ph_location_type(type = "title"))
  summary_text <- paste(
    paste0("New-record species pool: ", summary_stats$new_species_total),
    paste0("Matched species ready for modeling: ", summary_stats$matched_ready),
    paste0("Successfully modeled species: ", summary_stats$modeled_species),
    paste0("Unmatched species: ", summary_stats$unmatched_species),
    sep = "\n"
  )
  ppt <- officer::ph_with(ppt, summary_text, location = officer::ph_location_type(type = "body"))

  for (path in figure_paths[file.exists(figure_paths)]) {
    ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
    ppt <- officer::ph_with(ppt, basename(path), location = officer::ph_location_type(type = "title"))
    ppt <- officer::ph_with(ppt, external_img(path, width = 9.2, height = 5.2), location = officer::ph_location(left = 0.5, top = 1.2))
  }
  print(ppt, target = PPTX_SUMMARY_PATH)
}

write_task_summary <- function(summary_stats, algo_tbl, note_lines = character(0)) {
  lines <- c(
    "# Bird SDM distribution modeling task summary",
    "",
    paste0("- Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("- New-record species pool: ", summary_stats$new_species_total),
    paste0("- Exact/manual/auto matched species ready for modeling: ", summary_stats$matched_ready),
    paste0("- Successfully modeled species: ", summary_stats$modeled_species),
    paste0("- Unmatched species requiring Avibase/IOC + BirdLife review: ", summary_stats$unmatched_species),
    paste0("- Province sensitivity thresholds: ", paste(get_province_thresholds(create_default_model_config(MODEL_CONFIG_PATH)), collapse = ", "), " cells"),
    "",
    "## Algorithm availability",
    ""
  )
  for (i in seq_len(nrow(algo_tbl))) {
    lines <- c(lines, paste0("- ", algo_tbl$algorithm[[i]], ": ", ifelse(algo_tbl$available[[i]], "available", "unavailable"), " (", algo_tbl$engine[[i]], ")"))
  }
  if (length(note_lines)) {
    lines <- c(lines, "", "## Notes", "", paste0("- ", note_lines))
  }
  writeLines(lines, TASK_SUMMARY_PATH)
}

# ------------------------------------------------------------
# Step D-E. End-to-end SDM pipeline | 端到端 SDM 流程
# ------------------------------------------------------------
# English:
# This controller function stitches together all analysis stages:
# input loading, taxonomy review, climate preparation, SDM fitting,
# model diagnostics, map generation, and province sensitivity analysis.
#
# 中文：
# 这个总控函数负责串联整个研究流程：输入数据读取、分类审查、
# 气候变量准备、SDM 建模、模型诊断、地图输出和省级敏感性分析。
run_pipeline <- function(prepare_only = FALSE, selected_species = NA_character_, species_limit = NA_integer_, force_download = FALSE, current_only = FALSE, workers = 1L) {
  qa_log <- list()
  qa_log[[length(qa_log) + 1]] <- log_qa("setup", "task_root", "ok", TASK_ROOT)
  safe_message("Initializing birdwatch SDM pipeline at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ...")

  manual_overrides <- create_default_manual_overrides(MANUAL_OVERRIDE_PATH)
  scenario_config <- create_default_scenario_config(SCENARIO_CONFIG_PATH)
  model_config <- create_default_model_config(MODEL_CONFIG_PATH)
  algo_tbl <- collect_algorithm_availability()
  readr::write_csv(algo_tbl, ALGO_AVAILABILITY_PATH)

  safe_message("Loading new-record species tables ...")
  src <- load_new_record_data()
  previous_status <- load_previous_species_status()
  safe_message("Loading province boundaries ...")
  provinces <- load_province_boundaries()
  safe_message("Building China boundary union ...")
  china_boundary <- build_china_boundary(provinces)
  species_master <- build_rescue_species_master(src$new_records, src$species_pool, previous_status)
  readr::write_csv(species_master %>% select(-species_key, -genus, -epithet), MASTER_SPECIES_PATH)
  readr::write_csv(species_master %>% select(-species_key, -genus, -epithet), RESCUE_TARGET_PATH)

  safe_message("Loading rescue occurrence points ...")
  occurrence_birds <- load_occurrence_points(china_boundary, species_master = species_master, overrides_tbl = manual_overrides, provinces = provinces)
  if (nrow(occurrence_birds) == 0) stop("No usable rescue occurrence points were found after data cleaning and China-range filtering.")
  qa_log[[length(qa_log) + 1]] <- log_qa("input", "bird_occurrence_records", "ok", as.character(nrow(occurrence_birds)))
  safe_message("Occurrence points retained inside China: ", nrow(occurrence_birds))
  if (!file.exists(OCCURRENCE_SOURCE_SUMMARY_PATH)) {
    readr::write_csv(summarize_occurrence_sources(occurrence_birds), OCCURRENCE_SOURCE_SUMMARY_PATH)
  }

  match_tbl <- build_taxonomy_review_table(species_master, occurrence_birds, manual_overrides)
  readr::write_csv(match_tbl, MATCH_TABLE_PATH)
  readr::write_csv(match_tbl %>% filter(!.data$model_ready), UNMATCHED_PATH)

  summary_stats <- list(
    new_species_total = nrow(species_master),
    matched_ready = sum(match_tbl$model_ready, na.rm = TRUE),
    modeled_species = 0L,
    unmatched_species = sum(!match_tbl$model_ready, na.rm = TRUE)
  )

  if (prepare_only) {
    write_task_summary(summary_stats, algo_tbl, note_lines = c(
      "Prepare-only mode completed for the rescue SDM task: residual species from the earlier birdwatch workflow were re-screened.",
      "Occurrence preparation used the configured rescue source route (combined Birdwatch 1980-2025 plus GBIF by default).",
      "Fill taxonomy_manual_overrides.csv after Avibase/IOC + BirdLife review to recover more unmatched rescue species."
    ))
    readr::write_csv(bind_rows(qa_log), QA_LOG_PATH)
    return(invisible(list(summary = summary_stats, match_table = match_tbl, algorithm_table = algo_tbl)))
  }

  dash_line <- load_dash_line_overlay()

  current_config <- scenario_config %>% filter(.data$scenario == "current", .data$enabled)
  safe_message("Loading current climate stack ...")
  current_stack <- load_environment_stack(current_config, china_boundary, force_download = force_download)
  if (is.null(current_stack)) {
    qa_log[[length(qa_log) + 1]] <- log_qa("climate", "current", "missing", "Could not load or download current climate rasters.")
    write_task_summary(summary_stats, algo_tbl, note_lines = c(
      "Current climate rasters were unavailable. Matching outputs were generated, but SDM fitting did not run.",
      "Re-run after enabling WorldClim download or placing rasters under the configured climate directory."
    ))
    readr::write_csv(bind_rows(qa_log), QA_LOG_PATH)
    return(invisible(list(summary = summary_stats, match_table = match_tbl, algorithm_table = algo_tbl)))
  }

  safe_message("Selecting climate predictors ...")
  env_selection <- choose_environment_variables(current_stack, cutoff = as.numeric(get_config_value(model_config, "correlation_cutoff", "0.7")))
  province_thresholds <- get_province_thresholds(model_config)
  readr::write_csv(env_selection, ENV_SELECTION_PATH)
  selected_predictors <- env_selection %>% filter(.data$keep) %>% pull(.data$variable)
  current_stack <- current_stack[[selected_predictors]]

  modelable_species <- match_tbl %>% filter(.data$model_ready)
  if (!is.na(selected_species) && nzchar(selected_species)) {
    selected_species_values <- selected_species %>%
      str_split(",") %>%
      .[[1]] %>%
      normalize_species_name() %>%
      unique()
    modelable_species <- modelable_species %>% filter(.data$new_record_species %in% selected_species_values)
  }

  occurrence_ready <- occurrence_birds %>%
    left_join(
      modelable_species %>% select(new_record_species, matched_species),
      by = c("shp_species" = "matched_species"),
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(.data$new_record_species)) %>%
    transmute(species = .data$new_record_species, shp_species, order_occurrence, family_occurrence, longitude, latitude, geometry = geometry) %>%
    filter(!is.na(.data$longitude), !is.na(.data$latitude), between(.data$longitude, 70, 140), between(.data$latitude, 0, 60)) %>%
    st_as_sf() %>%
    st_filter(st_as_sf(china_boundary), .predicate = st_intersects)
  safe_message("Model-ready occurrence rows after taxonomy matching and China filter: ", nrow(occurrence_ready))

  future_configs <- scenario_config %>% filter(.data$scenario != "current", .data$enabled)
  if (current_only) {
    future_configs <- future_configs[0, , drop = FALSE]
  }
  future_stacks <- list()
  if (nrow(future_configs) > 0) {
    for (row_i in seq_len(nrow(future_configs))) {
      row <- future_configs[row_i, , drop = FALSE]
      future_stack <- load_environment_stack(row, china_boundary, force_download = force_download)
      if (is.null(future_stack)) {
        qa_log[[length(qa_log) + 1]] <- log_qa("climate", row$scenario[[1]], "missing", "Future climate raster unavailable")
        next
      }
      future_stacks[[row$scenario[[1]]]] <- list(config = row, stack = future_stack[[selected_predictors]])
    }
  }

  species_names <- occurrence_ready %>%
    st_drop_geometry() %>%
    count(.data$species, sort = TRUE, name = "n_points") %>%
    { if (!is.na(species_limit)) slice_head(., n = species_limit) else . } %>%
    pull(.data$species)
  occurrence_ready <- occurrence_ready %>% filter(.data$species %in% species_names)
  worker_count <- normalize_worker_count(workers, length(species_names))
  initialize_progress_tracking(species_names)
  safe_message("Running ", length(species_names), " species with workers=", worker_count)
  species_points_list <- split(occurrence_ready, occurrence_ready$species)
  species_tasks <- lapply(species_names, function(species_name) {
    list(
      species_name = species_name,
      species_points = species_points_list[[species_name]] %>% st_drop_geometry()
    )
  })
  names(species_tasks) <- species_names
  species_runner <- function(species_name) {
    execute_species_task(
      species_name = species_name,
      species_points = species_points_list[[species_name]],
      current_stack = current_stack,
      model_config = model_config,
      algo_tbl = algo_tbl,
      provinces = provinces,
      dash_line = dash_line,
      future_stacks = future_stacks,
      province_thresholds = province_thresholds
    )
  }
  species_results <- if (worker_count > 1L) {
    safe_message("Parallel setup: creating PSOCK cluster with workers=", worker_count)
    cl <- parallel::makePSOCKcluster(worker_count)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    safe_message("Parallel setup: cluster created")
    parallel::clusterCall(cl, function(script_file) {
      Sys.setenv(BIRD_SDM_SKIP_AUTORUN = "1")
      sys.source(script_file, envir = .GlobalEnv)
      NULL
    }, SCRIPT_FILE)
    safe_message("Parallel setup: worker script sourced")
    parallel::clusterExport(
      cl,
      varlist = c("model_config", "algo_tbl", "provinces", "dash_line", "province_thresholds", "current_config", "china_boundary", "selected_predictors", "future_configs", "force_download"),
      envir = environment()
    )
    safe_message("Parallel setup: worker globals exported")
    safe_message("Parallel setup: dispatching species tasks")
    parallel::parLapplyLB(cl, species_tasks, parallel_species_task_runner)
  } else {
    lapply(species_names, species_runner)
  }
  names(species_results) <- species_names

  write_progress_summary()
  species_status_tbl <- bind_rows(lapply(species_results, function(x) x$status))
  if (nrow(species_status_tbl) == 0) {
    species_status_tbl <- match_tbl %>%
      transmute(
        species = .data$new_record_species,
        match_status = ifelse(.data$model_ready, "matched", "unmatched"),
        n_raw = NA_integer_,
        n_clean = NA_integer_,
        n_cell_unique = NA_integer_,
        model_status = ifelse(.data$model_ready, "not_run", "unmatched"),
        skip_reason = ifelse(.data$model_ready, NA_character_, "taxonomy_unmatched"),
        cv_strategy = NA_character_,
        maxent_engine = algo_tbl %>% filter(.data$algorithm == "MaxEnt") %>% pull(engine) %>% .[[1]]
      )
  }
  species_status_tbl <- bind_rows(
    species_status_tbl,
    match_tbl %>%
      filter(!.data$model_ready) %>%
      transmute(
        species = .data$new_record_species,
        match_status = "unmatched",
        n_raw = NA_integer_,
        n_clean = NA_integer_,
        n_cell_unique = NA_integer_,
        model_status = "unmatched",
        skip_reason = "taxonomy_unmatched",
        cv_strategy = NA_character_,
        maxent_engine = algo_tbl %>% filter(.data$algorithm == "MaxEnt") %>% pull(engine) %>% .[[1]]
      )
  ) %>% distinct(.data$species, .keep_all = TRUE) %>% arrange(.data$species)

  metrics_tbl <- bind_rows(lapply(species_results, function(x) x$metrics))
  metrics_fold_tbl <- bind_rows(lapply(species_results, function(x) x$metrics_by_fold))
  threshold_tbl <- bind_rows(lapply(species_results, function(x) x$thresholds))
  province_tbl <- bind_rows(lapply(species_results, function(x) x$province))
  area_change_tbl <- bind_rows(lapply(species_results, function(x) x$area_change))
  points_used_all_tbl <- bind_rows(lapply(species_results, function(x) x$points_used))
  raster_manifest_tbl <- bind_rows(lapply(species_results, function(x) x$raster_manifest)) %>%
    mutate(raster_path = make_repo_relative_path(.data$raster_path))
  figure_paths <- unique(unlist(lapply(species_results, function(x) x$figure_paths), use.names = FALSE))
  summary_stats$modeled_species <- sum(species_status_tbl$model_status == "modeled", na.rm = TRUE)

  if (nrow(province_tbl) > 0) {
    province_summary <- province_tbl %>%
      group_by(.data$species, .data$scenario, .data$cell_threshold) %>%
      summarise(
        n_provinces = sum(.data$presence_flag),
        suitable_area_km2 = sum(.data$suitable_area_km2, na.rm = TRUE),
        .groups = "drop"
      )
    province_potential_listing <- province_tbl %>%
      filter(.data$presence_flag) %>%
      left_join(
        species_status_tbl %>%
          filter(.data$model_status == "modeled") %>%
          select(.data$species, .data$match_status, .data$model_status, .data$skip_reason),
        by = "species",
        relationship = "many-to-one"
      ) %>%
      filter(.data$model_status == "modeled") %>%
      arrange(.data$species, .data$scenario, .data$gcm, .data$cell_threshold, .data$province) %>%
      mutate(record_type = "potential_province") %>%
      select(.data$record_type, .data$species, .data$match_status, .data$model_status, .data$skip_reason, .data$scenario, .data$gcm, .data$province, .data$cell_threshold, .data$suitable_cell_count, .data$suitable_area_km2, .data$province_total_area_km2, .data$suitable_area_prop_of_species, .data$suitable_area_prop_of_province, .data$presence_flag)
    readr::write_csv(province_tbl, PROVINCE_SUMMARY_PATH)
    readr::write_csv(province_potential_listing, PROVINCE_POTENTIAL_LIST_PATH)
    sensitivity_plot <- plot_threshold_sensitivity(province_tbl)
    sensitivity_png <- file.path(FIGURE_SUMMARY_DIR, "fig_sensitivity_thresholds.png")
    sensitivity_pdf <- file.path(FIGURE_SUMMARY_DIR, "fig_sensitivity_thresholds.pdf")
    ggsave(sensitivity_png, sensitivity_plot, width = 8.0, height = 5.5, dpi = 320, bg = "white")
    ggsave(sensitivity_pdf, sensitivity_plot, width = 8.0, height = 5.5, device = cairo_pdf, bg = "white")
    figure_paths <- c(figure_paths, sensitivity_png)
  } else {
    province_summary <- tibble()
    readr::write_csv(
      tibble(
        record_type = character(),
        species = character(),
        match_status = character(),
        model_status = character(),
        skip_reason = character(),
        scenario = character(),
        gcm = character(),
        province = character(),
        cell_threshold = integer(),
        suitable_cell_count = integer(),
        suitable_area_km2 = double(),
        province_total_area_km2 = double(),
        suitable_area_prop_of_species = double(),
        suitable_area_prop_of_province = double(),
        presence_flag = logical()
      ),
      PROVINCE_POTENTIAL_LIST_PATH
    )
  }

  if (nrow(area_change_tbl) > 0) {
    area_plot <- plot_area_change_summary(area_change_tbl)
    area_png <- file.path(FIGURE_SUMMARY_DIR, "fig_area_change_summary.png")
    area_pdf <- file.path(FIGURE_SUMMARY_DIR, "fig_area_change_summary.pdf")
    ggsave(area_png, area_plot, width = 8.0, height = 5.5, dpi = 320, bg = "white")
    ggsave(area_pdf, area_plot, width = 8.0, height = 5.5, device = cairo_pdf, bg = "white")
    figure_paths <- c(figure_paths, area_png)
    readr::write_csv(area_change_tbl, AREA_CHANGE_PATH)
  }

  readr::write_csv(species_status_tbl, SPECIES_STATUS_PATH)
  readr::write_csv(metrics_tbl, METRIC_SUMMARY_PATH)
  readr::write_csv(metrics_fold_tbl, METRIC_FOLD_PATH)
  readr::write_csv(
    bind_rows(
      threshold_tbl,
      tibble(
        species = "ALL_MODELED_SPECIES",
        threshold_type = "province_sensitivity_threshold",
        threshold_name = paste0("province_threshold_", seq_along(province_thresholds)),
        algorithm = NA_character_,
        threshold_value = as.numeric(province_thresholds)
      )
    ),
    THRESHOLD_SUMMARY_PATH
  )
  readr::write_csv(points_used_all_tbl, POINTS_USED_ALL_PATH)
  if (nrow(raster_manifest_tbl) > 0) {
    readr::write_csv(raster_manifest_tbl, RASTER_MANIFEST_PATH)
  } else {
    readr::write_csv(
      tibble(species = character(), scenario = character(), gcm = character(), raster_type = character(), raster_path = character()),
      RASTER_MANIFEST_PATH
    )
  }
  if (nrow(province_summary) > 0) {
    readr::write_csv(province_summary, file.path(TABLE_DIR, "table_province_sensitivity_compact.csv"))
  }
  species_qa_tbl <- bind_rows(lapply(species_results, function(x) x$qa_log))
  readr::write_csv(bind_rows(qa_log, species_qa_tbl), QA_LOG_PATH)

  note_lines <- c(
    "Taxonomy review includes explicit Avibase/IOC and BirdLife review fields.",
    paste0("Province sensitivity analysis is written for ", paste0(get_province_thresholds(model_config), "-cell", collapse = ", "), " thresholds."),
    "MaxEnt uses maxent.jar when Java and the jar are available; otherwise it falls back to maxnet if installed.",
    "Occurrence points, climate rasters, background sampling, and predictions are all restricted to the China boundary.",
    "Species maps are rendered in a China-focused equal-area projection and overlay the provincial boundary together with the nine-dash line base layer.",
    "Potential distribution GeoTIFF rasters are indexed in table_raster_output_manifest.csv for every successfully modeled species.",
    "Current-climate preparation first checks the task climate cache for readable WorldClim bioclim and elevation files, and writes the validation result to table_climate_data_manifest.csv.",
    "Run progress is written to data/progress/progress_summary.csv and data/progress/progress_events.log during batch execution.",
    "The final potential-province table only keeps successfully modeled species and lists each province, suitable-cell count, and suitable area for every active threshold.",
    "WorldClim elevation is appended as an environmental predictor and enters the variable-screening workflow together with the bioclim variables."
  )
  write_task_summary(summary_stats, algo_tbl, note_lines = note_lines)
  write_summary_pptx(summary_stats, unique(figure_paths))

  invisible(list(
    summary = summary_stats,
    match_table = match_tbl,
    algorithm_table = algo_tbl,
    species_status = species_status_tbl,
    metrics = metrics_tbl,
    province = province_tbl,
    area_change = area_change_tbl,
    raster_manifest = raster_manifest_tbl
  ))
}

if (sys.nframe() == 0 && !nzchar(Sys.getenv("BIRD_SDM_SKIP_AUTORUN", unset = ""))) {
  run_pipeline(
    prepare_only = prepare_only,
    selected_species = selected_species,
    species_limit = species_limit,
    force_download = force_download,
    current_only = current_only,
    workers = workers
  )
}
