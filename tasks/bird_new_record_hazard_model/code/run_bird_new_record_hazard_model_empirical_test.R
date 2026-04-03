#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  normalizePath("code/run_bird_new_record_hazard_model_empirical_test.R", mustWork = TRUE)
}

task_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(task_root, "code", "run_bird_new_record_hazard_model.R"))

output_root <- file.path(task_root, "empirical_hazard_test_v2")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "derived_inputs"), recursive = TRUE, showWarnings = FALSE)

path_new_records <- file.path(
  task_root,
  "server_run_worldclim_5m",
  "results_worldclim_5m_clean_gpkg",
  "data_clean",
  "bird_new_records_for_range_climate.csv"
)
path_event_table <- file.path(
  task_root,
  "server_run_worldclim_5m",
  "results_worldclim_5m_clean_gpkg",
  "results",
  "table_new_record_province_year_climate_direction_displacement.csv"
)
path_species_year_native <- file.path(
  task_root,
  "server_run_worldclim_5m",
  "results_worldclim_5m_clean_gpkg",
  "results",
  "species_year_historical_range_climate.csv"
)
path_effort <- "/Users/dingchenchen/Documents/New records/bird_new_records_R_output/tasks/bird_geb_fig3_fig4_effort_analysis/data/effort_clean_province_year.csv"
path_sdm <- "/Users/dingchenchen/Documents/New records/bird-new-distribution-records/tasks/bird_sdm_distribution_modeling_birdwatch_2002_2025/data/tables/table_potential_province_listing_all_species.csv"

stopifnot(file.exists(path_new_records))
stopifnot(file.exists(path_event_table))
stopifnot(file.exists(path_species_year_native))
stopifnot(file.exists(path_effort))
stopifnot(file.exists(path_sdm))

ndr_raw <- read_csv(path_new_records, show_col_types = FALSE) %>%
  select(species, province, year, record_id)

event_tbl <- read_csv(path_event_table, show_col_types = FALSE)
native_tbl <- read_csv(path_species_year_native, show_col_types = FALSE)
effort_raw <- read_csv(path_effort, show_col_types = FALSE)
sdm_raw <- read_csv(path_sdm, show_col_types = FALSE)

province_lookup <- effort_raw %>%
  distinct(province_cn, province)

effort_py <- effort_raw %>%
  transmute(
    province,
    year = as.integer(year),
    effort_record = as.numeric(report_count),
    effort_observer = as.numeric(user_count)
  )

species_trait <- ndr_raw %>%
  distinct(species)

species_year_native_climate <- native_tbl %>%
  transmute(
    species,
    year = as.integer(year),
    temp_native_anom = resident_breeding_range_temp_delta_from_baseline,
    prec_native_anom = resident_breeding_range_prec_delta_from_baseline
  ) %>%
  filter(!is.na(temp_native_anom) | !is.na(prec_native_anom))

province_year_observed <- event_tbl %>%
  transmute(
    province,
    year = as.integer(year),
    temp_anom = point_temp_delta_from_point_baseline,
    prec_anom = point_prec_delta_from_point_baseline
  ) %>%
  group_by(province, year) %>%
  summarise(
    temp_anom = mean(temp_anom, na.rm = TRUE),
    prec_anom = mean(prec_anom, na.rm = TRUE),
    .groups = "drop"
  )

year_support <- tibble(year = sort(unique(effort_py$year)))
province_support <- tibble(province = sort(unique(effort_py$province)))

province_year_climate <- tidyr::crossing(province_support, year_support) %>%
  left_join(province_year_observed, by = c("province", "year")) %>%
  group_by(year) %>%
  mutate(
    year_temp_mean = mean(temp_anom, na.rm = TRUE),
    year_prec_mean = mean(prec_anom, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(province) %>%
  mutate(
    province_temp_mean = mean(temp_anom, na.rm = TRUE),
    province_prec_mean = mean(prec_anom, na.rm = TRUE),
    temp_anom = dplyr::coalesce(temp_anom, province_temp_mean, year_temp_mean, 0),
    prec_anom = dplyr::coalesce(prec_anom, province_prec_mean, year_prec_mean, 0)
  ) %>%
  ungroup() %>%
  select(province, year, temp_anom, prec_anom)

sdm_province <- sdm_raw %>%
  filter(record_type == "potential_province", scenario == "current") %>%
  rename(province_cn_sdm = province) %>%
  left_join(province_lookup, by = c("province_cn_sdm" = "province_cn")) %>%
  mutate(
    province = province,
    potential = 1L,
    historical_presence = 0L,
    risk_start_year = 2000L
  ) %>%
  filter(!is.na(province)) %>%
  distinct(species, province, .keep_all = TRUE) %>%
  transmute(
    species,
    province,
    potential,
    historical_presence,
    risk_start_year
  )

ndr_supported <- ndr_raw %>%
  semi_join(sdm_province, by = c("species", "province")) %>%
  semi_join(species_year_native_climate, by = "species") %>%
  semi_join(effort_py, by = c("province", "year"))

sdm_province <- sdm_province %>%
  semi_join(distinct(ndr_supported, species), by = "species")

species_trait <- species_trait %>%
  semi_join(ndr_supported, by = "species")

paths <- list(
  new_record_csv = file.path(output_root, "derived_inputs", "ndr_raw_empirical_test.csv"),
  sdm_province_csv = file.path(output_root, "derived_inputs", "sdm_province_empirical_test.csv"),
  province_year_climate_csv = file.path(output_root, "derived_inputs", "province_year_climate_empirical_test.csv"),
  species_year_native_climate_csv = file.path(output_root, "derived_inputs", "species_year_native_climate_empirical_test.csv"),
  effort_csv = file.path(output_root, "derived_inputs", "effort_py_empirical_test.csv"),
  species_trait_csv = file.path(output_root, "derived_inputs", "species_trait_empirical_test.csv"),
  config_csv = file.path(output_root, "hazard_model_config_empirical_test.csv")
)

write_csv(ndr_supported, paths$new_record_csv)
write_csv(sdm_province, paths$sdm_province_csv)
write_csv(province_year_climate, paths$province_year_climate_csv)
write_csv(species_year_native_climate, paths$species_year_native_climate_csv)
write_csv(effort_py, paths$effort_csv)
write_csv(species_trait, paths$species_trait_csv)

writeLines(
  c(
    "key,value",
    paste0("new_record_csv,", paths$new_record_csv),
    paste0("sdm_province_csv,", paths$sdm_province_csv),
    paste0("province_year_climate_csv,", paths$province_year_climate_csv),
    paste0("species_year_native_climate_csv,", paths$species_year_native_climate_csv),
    paste0("effort_csv,", paths$effort_csv),
    paste0("species_trait_csv,", paths$species_trait_csv),
    paste0("output_dir,", output_root),
    "year_min,2000",
    "year_max,2024",
    "model_engine,lme4"
  ),
  con = paths$config_csv
)

result <- run_hazard_analysis(paths$config_csv)

coverage_tbl <- tibble(
  metric = c(
    "new_records_original",
    "new_records_supported",
    "species_original",
    "species_supported",
    "sdm_species",
    "province_year_climate_rows",
    "effort_rows"
  ),
  value = c(
    nrow(ndr_raw),
    nrow(ndr_supported),
    n_distinct(ndr_raw$species),
    n_distinct(ndr_supported$species),
    n_distinct(sdm_province$species),
    nrow(province_year_climate),
    nrow(effort_py)
  )
)
write_csv(coverage_tbl, file.path(output_root, "diagnostics", "empirical_test_input_coverage.csv"))

message("Empirical hazard test completed.")
