#!/usr/bin/env Rscript

# ============================================================
# Bird SDM workflow for new-record bird species in China
# Workflow: species screening -> taxonomy review -> occurrence cleaning ->
# climate preparation -> SDM fitting -> province sensitivity analysis ->
# figures / tables / summary outputs.
#
# Design goals:
# 1. Only model bird species that are part of the new-record species pool.
# 2. Use occurrence points exclusively from the local species-distribution SHP.
# 3. Keep the pipeline robust when some modeling dependencies are unavailable.
# 4. Emit taxonomy review tables and QA outputs even before heavy modeling runs.
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

safe_message <- function(...) {
  cat(paste0(..., "\n"))
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
    return(normalizePath(sub("^--file=", "", file_arg[[1]])))
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

PROJECT_ROOT <- normalizePath(file.path(TASK_ROOT, "..", ".."), mustWork = FALSE)
SOURCE_DATA_DIR <- file.path(PROJECT_ROOT, "source_data")
NEW_RECORD_PATH <- file.path(SOURCE_DATA_DIR, "bird_new_records_clean.csv")
SPECIES_POOL_PATH <- file.path(SOURCE_DATA_DIR, "bird_species_pool_with_traits.csv")
DEFAULT_OCCURRENCE_SHP_PATH <- "/Users/dingchenchen/Documents/SDMs/物种分布/物种分布.shp"
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
OCCURRENCE_SHP_PATH <- Sys.getenv("BIRD_SDM_OCCURRENCE_SHP_PATH", unset = DEFAULT_OCCURRENCE_SHP_PATH)
PROVINCE_SHP_PATH <- Sys.getenv("BIRD_SDM_PROVINCE_SHP_PATH", unset = DEFAULT_PROVINCE_SHP_PATH)
DASHLINE_SHP_PATH <- Sys.getenv("BIRD_SDM_DASHLINE_SHP_PATH", unset = DEFAULT_DASHLINE_SHP_PATH)

MANUAL_OVERRIDE_PATH <- file.path(DATA_DIR, "taxonomy_manual_overrides.csv")
SCENARIO_CONFIG_PATH <- file.path(DATA_DIR, "climate_scenario_config.csv")
MODEL_CONFIG_PATH <- file.path(DATA_DIR, "model_config.csv")
MATCH_TABLE_PATH <- file.path(TABLE_DIR, "table_species_name_review.csv")
MASTER_SPECIES_PATH <- file.path(TABLE_DIR, "table_species_master.csv")
ENV_SELECTION_PATH <- file.path(TABLE_DIR, "table_environment_variable_selection.csv")
ALGO_AVAILABILITY_PATH <- file.path(TABLE_DIR, "table_algorithm_availability.csv")
SPECIES_STATUS_PATH <- file.path(TABLE_DIR, "table_species_status_summary.csv")
QA_LOG_PATH <- file.path(TABLE_DIR, "table_qa_log.csv")
UNMATCHED_PATH <- file.path(TABLE_DIR, "table_unmatched_species_list.csv")
PROVINCE_SUMMARY_PATH <- file.path(TABLE_DIR, "table_province_prediction_summary.csv")
PROVINCE_POTENTIAL_LIST_PATH <- file.path(TABLE_DIR, "table_potential_province_listing_all_species.csv")
AREA_CHANGE_PATH <- file.path(TABLE_DIR, "table_scenario_area_change_summary.csv")
METRIC_SUMMARY_PATH <- file.path(TABLE_DIR, "table_model_metrics_summary.csv")
RASTER_MANIFEST_PATH <- file.path(TABLE_DIR, "table_raster_output_manifest.csv")
CLIMATE_MANIFEST_PATH <- file.path(TABLE_DIR, "table_climate_data_manifest.csv")
PROGRESS_DIR <- file.path(DATA_DIR, "progress")
PROGRESS_SPECIES_DIR <- file.path(PROGRESS_DIR, "species")
PROGRESS_SUMMARY_PATH <- file.path(PROGRESS_DIR, "progress_summary.csv")
PROGRESS_EVENT_LOG_PATH <- file.path(PROGRESS_DIR, "progress_events.log")
TASK_SUMMARY_PATH <- file.path(RESULT_DIR, "task_summary.md")
PPTX_SUMMARY_PATH <- file.path(RESULT_DIR, "bird_sdm_summary.pptx")
MAP_CRS <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +datum=WGS84 +units=m +no_defs"

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
        "pseudoabsence_multiplier",
        "pseudoabsence_minimum",
        "cv_folds",
        "cv_presence_threshold_spatial",
        "tss_selection_threshold",
        "province_threshold_a",
        "province_threshold_b",
        "province_threshold_c",
        "province_threshold_d",
        "buffer_km",
        "correlation_cutoff"
      ),
      value = c("10", "3", "1000", "5", "20", "0.6", "3", "10", "20", "50", "500", "0.7")
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
    detail = detail
  )
}

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

load_occurrence_points <- function(china_boundary) {
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
    filter(!is.na(.data$shp_species), .data$shp_species != "") %>%
    filter(st_geometry_type(geometry) == "POINT") %>%
    transmute(
      shp_species,
      shp_species_key = normalize_species_key(.data$shp_species),
      order_occurrence,
      family_occurrence,
      longitude,
      latitude,
      geometry = geometry
    )
    china_sf <- st_as_sf(china_boundary)
  birds <- birds %>% st_filter(china_sf, .predicate = st_intersects)
  birds
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
      taxonomy_source <- "Direct match in SHP"
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

worldclim_resolution_suffix <- function(res_value) {
  ifelse(as.character(res_value) == "0.5", "30s", paste0(as.character(res_value), "m"))
}

get_climate_search_roots <- function(climate_root = DATA_DIR) {
  override <- Sys.getenv("BIRD_SDM_CLIMATE_SEARCH_ROOTS", unset = "")
  if (nzchar(override)) {
    roots <- strsplit(override, "[;,]")[[1]]
  } else {
    roots <- c(
      file.path(climate_root, "climate"),
      climate_root,
      PROJECT_ROOT,
      file.path(path.expand("~"), "projects"),
      path.expand("~")
    )
  }
  roots <- trimws(roots)
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
        path = as.character(.data$path),
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
      path = as.character(.data$path),
      source = as.character(.data$source),
      readable = as.logical(.data$readable),
      checked_at = as.character(.data$checked_at)
    ) %>%
    arrange(desc(.data$checked_at)) %>%
    distinct(.data$scenario, .data$res_minutes, .data$target_name, .keep_all = TRUE)
  readr::write_csv(combined, CLIMATE_MANIFEST_PATH)
  invisible(combined)
}

scan_existing_worldclim_current_files <- function(res_value, search_roots = get_climate_search_roots()) {
  fres <- worldclim_resolution_suffix(res_value)
  target_names <- c(paste0("wc2.1_", fres, "_bio_", seq_len(19), ".tif"), paste0("wc2.1_", fres, "_elev.tif"))
  pattern <- paste0("^wc2\\.1_", gsub("\\.", "\\\\.", fres), "_(bio_[0-9]+|elev)\\.tif$")
  candidate_list <- lapply(
    search_roots,
    function(root) {
      hits <- list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE)
      if (!length(hits)) {
        return(tibble())
      }
      tibble(root = root, path = hits, target_name = basename(hits))
    }
  )
  candidate_files <- bind_rows(candidate_list) %>%
    filter(.data$target_name %in% target_names)
  if (!nrow(candidate_files)) {
    return(tibble())
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
  existing_tbl <- scan_existing_worldclim_current_files(res_value = res_value, search_roots = get_climate_search_roots(climate_root))
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
  ensure_dir(file.path(climate_root, "climate"))

  stack <- NULL
  if (scenario == "current" && !force_download) {
    existing_tbl <- scan_existing_worldclim_current_files(res_value = res_value, search_roots = get_climate_search_roots(climate_root))
    bio_paths <- existing_tbl %>%
      filter(grepl("_bio_[0-9]+\\.tif$", .data$target_name), .data$readable) %>%
      mutate(bio_id = readr::parse_number(.data$target_name)) %>%
      arrange(.data$bio_id) %>%
      pull(.data$path)
    stack <- load_worldclim_current_from_paths(bio_paths = bio_paths, china_boundary = china_boundary)
    if (!is.null(stack)) {
      safe_message("Loaded current WorldClim bioclim stack from existing server files.")
    }
  }

  if (is.null(stack) && scenario == "current") {
    safe_message("No readable existing current WorldClim bioclim stack found; trying geodata download for ", row$scenario[[1]], ".")
    stack <- tryCatch(
      geodata::worldclim_global(var = "bio", res = res_value, path = climate_root),
      error = function(e) NULL
    )
  } else if (is.null(stack)) {
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

sample_background_points <- function(area_sf, current_stack, presence_points, multiplier, minimum_n) {
  n_presence <- nrow(presence_points)
  n_target <- max(minimum_n, n_presence * multiplier)
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
    n.trees = 2500,
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
    ntree = 500
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
  pseudo_mult <- as.integer(get_config_value(model_cfg, "pseudoabsence_multiplier", "3"))
  pseudo_min <- as.integer(get_config_value(model_cfg, "pseudoabsence_minimum", "1000"))
  cv_folds <- as.integer(get_config_value(model_cfg, "cv_folds", "5"))
  spatial_threshold <- as.integer(get_config_value(model_cfg, "cv_presence_threshold_spatial", "20"))
  tss_threshold <- as.numeric(get_config_value(model_cfg, "tss_selection_threshold", "0.6"))
  buffer_km <- as.numeric(get_config_value(model_cfg, "buffer_km", "500"))

  species_points <- thin_presence_by_cells(species_points, current_stack)
  if (nrow(species_points) < presence_threshold) {
    return(list(
      status = tibble(
        species = species_name,
        match_status = "matched",
        n_raw = nrow(species_points),
        n_clean = nrow(species_points),
        n_cell_unique = nrow(species_points),
        model_status = "skipped",
        skip_reason = "insufficient_occurrence_points",
        cv_strategy = NA_character_,
        maxent_engine = ifelse(any(algo_tbl$algorithm == "MaxEnt" & algo_tbl$available), algo_tbl$engine[algo_tbl$algorithm == "MaxEnt"][1], "missing")
      ),
      metrics = tibble(),
      selected_algorithms = character(0),
      fitted_models = list(),
      threshold = NA_real_,
      probability_current = NULL,
      binary_current = NULL,
      area_sf = NULL
    ))
  }

  provinces <- load_province_boundaries()
  china_boundary <- build_china_boundary(provinces)
  accessible_area <- make_accessible_area(species_points, china_boundary, buffer_km = buffer_km)
  background_points <- sample_background_points(accessible_area, current_stack, species_points, pseudo_mult, pseudo_min)
  model_df <- build_model_dataset(species_points, background_points, current_stack)
  predictors <- setdiff(names(model_df), c("pa", "longitude", "latitude"))
  model_df <- model_df %>% filter(complete.cases(across(all_of(c("pa", predictors)))))
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
    n_raw = nrow(species_points),
    n_clean = nrow(species_points),
    n_cell_unique = nrow(species_points),
    model_status = ifelse(length(probability_current) > 0, "modeled", "failed"),
    skip_reason = ifelse(length(probability_current) > 0, NA_character_, "no_algorithm_available"),
    cv_strategy = cv_strategy,
    maxent_engine = algo_tbl %>% filter(.data$algorithm == "MaxEnt") %>% pull(engine) %>% .[[1]]
  )

  list(
    status = status_tbl,
    metrics = summary_metrics %>% mutate(species = species_name),
    selected_algorithms = selected_algorithms,
    fitted_models = fitted_models,
    threshold = ensemble_threshold,
    probability_current = ensemble_probability,
    binary_current = binary_current,
    area_sf = accessible_area,
    predictors = predictors
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
  threshold_keys <- c("province_threshold_a", "province_threshold_b", "province_threshold_c", "province_threshold_d")
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
    province = tibble(),
    area_change = tibble(),
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

run_species_task <- function(species_name, species_points, current_stack = NULL, model_config, algo_tbl, provinces, dash_line = NULL, future_stacks = NULL, province_thresholds, current_config = NULL, china_boundary = NULL, selected_predictors = NULL, future_configs = NULL, force_download = FALSE) {
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
        province = tibble(),
        area_change = tibble(),
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
    province = tibble(),
    area_change = tibble(),
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
  raster_rows <- list()

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

  current_map <- plot_probability_map(model_fit$probability_current, provinces, dash_line, paste0(species_name, " - current suitability"))
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

province_sensitivity_summary <- function(binary_raster, provinces, species, scenario, gcm, thresholds = c(3, 10, 20, 50)) {
  if (is.null(binary_raster)) return(tibble())
  binary01 <- terra::ifel(binary_raster, 1, 0)
  cell_area <- terra::cellSize(binary01, unit = "km")
  province_vect <- terra::vect(provinces)
  cell_count <- terra::extract(binary01, province_vect, fun = sum, na.rm = TRUE) %>% as_tibble()
  area_sum <- terra::extract(binary01 * cell_area, province_vect, fun = sum, na.rm = TRUE) %>% as_tibble()
  base_tbl <- provinces %>%
    st_drop_geometry() %>%
    mutate(ID = row_number()) %>%
    left_join(cell_count %>% rename(suitable_cell_count = 2), by = "ID") %>%
    left_join(area_sum %>% rename(suitable_area_km2 = 2), by = "ID") %>%
    mutate(
      suitable_cell_count = as.integer(round(coalesce(.data$suitable_cell_count, 0))),
      suitable_area_km2 = as.numeric(coalesce(.data$suitable_area_km2, 0))
    )
  bind_rows(lapply(thresholds, function(thr) {
    base_tbl %>% transmute(
      species = species,
      scenario = scenario,
      gcm = gcm,
      province = .data$province_cn,
      cell_threshold = thr,
      suitable_cell_count = .data$suitable_cell_count,
      suitable_area_km2 = .data$suitable_area_km2,
      presence_flag = .data$suitable_cell_count >= thr
    )
  }))
}

plot_probability_map <- function(probability_raster, provinces, dash_line, title) {
  map_raster <- terra::project(probability_raster, MAP_CRS, method = "bilinear")
  raster_df <- as.data.frame(map_raster, xy = TRUE, na.rm = TRUE)
  names(raster_df) <- c("x", "y", "probability")
  province_map <- st_transform(provinces, MAP_CRS)
  dash_line_map <- if (is.null(dash_line) || nrow(dash_line) == 0) NULL else st_transform(dash_line, MAP_CRS)
  p <- ggplot() +
    geom_raster(data = raster_df, aes(x = .data$x, y = .data$y, fill = .data$probability)) +
    geom_sf(data = province_map, fill = NA, color = "#2F2F2F", linewidth = 0.2)
  if (!is.null(dash_line_map)) {
    p <- p + geom_sf(data = dash_line_map, inherit.aes = FALSE, color = "#4A4E69", linewidth = 0.25)
  }
  p +
    scale_fill_viridis_c(option = "C", name = "Suitability") +
    coord_sf(crs = sf::st_crs(MAP_CRS), expand = FALSE) +
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

run_pipeline <- function(prepare_only = FALSE, selected_species = NA_character_, species_limit = NA_integer_, force_download = FALSE, current_only = FALSE, workers = 1L) {
  qa_log <- list()
  qa_log[[length(qa_log) + 1]] <- log_qa("setup", "task_root", "ok", TASK_ROOT)

  manual_overrides <- create_default_manual_overrides(MANUAL_OVERRIDE_PATH)
  scenario_config <- create_default_scenario_config(SCENARIO_CONFIG_PATH)
  model_config <- create_default_model_config(MODEL_CONFIG_PATH)
  algo_tbl <- collect_algorithm_availability()
  readr::write_csv(algo_tbl, ALGO_AVAILABILITY_PATH)

  src <- load_new_record_data()
  provinces <- load_province_boundaries()
  china_boundary <- build_china_boundary(provinces)
  species_master <- build_species_master(src$new_records, src$species_pool)
  readr::write_csv(species_master %>% select(-species_key, -genus, -epithet), MASTER_SPECIES_PATH)

  occurrence_birds <- load_occurrence_points(china_boundary)
  if (nrow(occurrence_birds) == 0) stop("No bird occurrence points were found after filtering the shapefile.")
  qa_log[[length(qa_log) + 1]] <- log_qa("input", "bird_occurrence_records", "ok", as.character(nrow(occurrence_birds)))

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
      "Prepare-only mode completed: generated taxonomy review tables and configuration templates.",
      "Fill taxonomy_manual_overrides.csv after Avibase/IOC + BirdLife review to recover more unmatched species."
    ))
    readr::write_csv(bind_rows(qa_log), QA_LOG_PATH)
    return(invisible(list(summary = summary_stats, match_table = match_tbl, algorithm_table = algo_tbl)))
  }

  dash_line <- load_dash_line_overlay()

  current_config <- scenario_config %>% filter(.data$scenario == "current", .data$enabled)
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
    left_join(modelable_species %>% select(new_record_species, matched_species), by = c("shp_species" = "matched_species")) %>%
    filter(!is.na(.data$new_record_species)) %>%
    transmute(species = .data$new_record_species, shp_species, order_occurrence, family_occurrence, longitude, latitude, geometry = geometry) %>%
    filter(!is.na(.data$longitude), !is.na(.data$latitude), between(.data$longitude, 70, 140), between(.data$latitude, 0, 60)) %>%
    st_as_sf() %>%
    st_filter(st_as_sf(china_boundary), .predicate = st_intersects)

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
    cl <- parallel::makePSOCKcluster(worker_count)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(script_file) {
      sys.source(script_file, envir = .GlobalEnv)
      NULL
    }, SCRIPT_FILE)
    parallel::clusterExport(
      cl,
      varlist = c("species_points_list", "model_config", "algo_tbl", "provinces", "dash_line", "province_thresholds", "current_config", "china_boundary", "selected_predictors", "future_configs", "force_download"),
      envir = environment()
    )
    parallel::parLapplyLB(cl, species_names, function(species_name) {
      execute_species_task(
        species_name = species_name,
        species_points = species_points_list[[species_name]],
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
    })
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
  province_tbl <- bind_rows(lapply(species_results, function(x) x$province))
  area_change_tbl <- bind_rows(lapply(species_results, function(x) x$area_change))
  raster_manifest_tbl <- bind_rows(lapply(species_results, function(x) x$raster_manifest))
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
      select(.data$record_type, .data$species, .data$match_status, .data$model_status, .data$skip_reason, .data$scenario, .data$gcm, .data$province, .data$cell_threshold, .data$suitable_cell_count, .data$suitable_area_km2, .data$presence_flag)
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
    "Current-climate preparation first scans existing server directories for readable WorldClim bioclim and elevation files, and writes the validation result to table_climate_data_manifest.csv.",
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

if (sys.nframe() == 0) {
  run_pipeline(
    prepare_only = prepare_only,
    selected_species = selected_species,
    species_limit = species_limit,
    force_download = force_download,
    current_only = current_only,
    workers = workers
  )
}
