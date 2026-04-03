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
  normalizePath("code/test_bird_new_record_hazard_model_smoke.R", mustWork = TRUE)
}

task_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(task_root, "code", "run_bird_new_record_hazard_model.R"))

set.seed(20260330)

tmp_root <- file.path(tempdir(), "bird_new_record_hazard_smoke")
dir.create(tmp_root, showWarnings = FALSE, recursive = TRUE)

config_path <- file.path(tmp_root, "hazard_config.csv")
new_record_csv <- file.path(tmp_root, "ndr_raw.csv")
sdm_csv <- file.path(tmp_root, "sdm_province.csv")
clim_py_csv <- file.path(tmp_root, "province_year_climate.csv")
clim_native_csv <- file.path(tmp_root, "species_year_native_climate.csv")
effort_csv <- file.path(tmp_root, "effort_py.csv")
trait_csv <- file.path(tmp_root, "species_trait.csv")
output_dir <- file.path(tmp_root, "hazard_output")

species <- paste0("Species_", LETTERS[1:8])
provinces <- paste0("Province_", LETTERS[1:5])
years <- 2000:2006

species_trait <- tibble(
  species = species,
  mig = rep(c("resident", "migrant"), length.out = length(species))
)
write_csv(species_trait, trait_csv)

clim_py <- tidyr::expand_grid(province = provinces, year = years) %>%
  mutate(
    province_index = match(province, provinces),
    temp_anom = -0.4 + 0.22 * (year - min(years)) + 0.12 * province_index,
    prec_anom = 18 + 2.3 * sin((year - min(years)) / 1.6 + province_index / 2) + 0.7 * province_index
  ) %>%
  select(-province_index)
write_csv(clim_py, clim_py_csv)

clim_native <- tidyr::expand_grid(species = species, year = years) %>%
  mutate(
    species_index = match(species, species),
    temp_native_anom = -0.2 + 0.12 * (year - min(years)) + 0.03 * species_index,
    prec_native_anom = 11 + 1.6 * cos((year - min(years)) / 1.8 + species_index / 3) + 0.25 * species_index
  ) %>%
  select(-species_index)
write_csv(clim_native, clim_native_csv)

effort_py <- tidyr::expand_grid(province = provinces, year = years) %>%
  mutate(
    province_index = match(province, provinces),
    effort_record = round(exp(3.0 + 0.10 * province_index + 0.16 * (year - min(years)))),
    effort_observer = round(exp(2.2 + 0.08 * province_index + 0.10 * (year - min(years))))
  ) %>%
  select(-province_index)
write_csv(effort_py, effort_csv)

sdm_province <- tidyr::expand_grid(species = species, province = provinces) %>%
  mutate(
    species_index = match(species, species),
    province_index = match(province, provinces),
    potential = ifelse((species_index + province_index) %% 5 == 0, 0L, 1L),
    historical_presence = ifelse(province_index == ((species_index - 1) %% length(provinces)) + 1, 1L, 0L),
    risk_start_year = 2000L
  ) %>%
  filter(!(potential == 0L & historical_presence == 1L)) %>%
  select(species, province, potential, historical_presence, risk_start_year)
write_csv(sdm_province, sdm_csv)

eligible_pairs <- sdm_province %>%
  filter(potential == 1, historical_presence == 0) %>%
  left_join(species_trait, by = "species") %>%
  mutate(
    species_index = match(species, species),
    province_index = match(province, provinces)
  )

simulate_pair_event <- function(species_name, province_name, mig_label, species_index, province_index) {
  event_year <- NA_integer_
  for (yr in years) {
    row_clim <- clim_py %>% filter(province == province_name, year == yr)
    row_native <- clim_native %>% filter(species == species_name, year == yr)
    row_effort <- effort_py %>% filter(province == province_name, year == yr)

    temp_grad <- row_clim$temp_anom - row_native$temp_native_anom
    log_effort <- log(pmax(row_effort$effort_record, 1))
    eta <- -5.3 + 0.95 * temp_grad + 0.45 * (log_effort - mean(log(effort_py$effort_record))) +
      0.35 * as.numeric(mig_label == "migrant") + 0.06 * species_index - 0.03 * province_index
    p <- 1 - exp(-exp(eta))

    if (stats::runif(1) < p) {
      event_year <- yr
      break
    }
  }

  if (is.na(event_year)) {
    return(NULL)
  }

  tibble(
    species = species_name,
    province = province_name,
    year = event_year,
    source = "smoke_test",
    evidence_type = "synthetic"
  )
}

ndr_raw <- purrr::pmap_dfr(
  eligible_pairs,
  function(species, province, potential, historical_presence, risk_start_year, mig, species_index, province_index) {
    simulate_pair_event(species, province, mig, species_index, province_index)
  }
)

if (nrow(ndr_raw) < 6) {
  forced_events <- eligible_pairs %>%
    slice_head(n = 6 - nrow(ndr_raw)) %>%
    transmute(
      species,
      province,
      year = years[pmin(row_number() + 1, length(years))],
      source = "forced_smoke_test",
      evidence_type = "synthetic"
    )
  ndr_raw <- bind_rows(ndr_raw, forced_events) %>%
    distinct(species, province, .keep_all = TRUE)
}
write_csv(ndr_raw, new_record_csv)

writeLines(
  c(
    "key,value",
    paste0("new_record_csv,", new_record_csv),
    paste0("sdm_province_csv,", sdm_csv),
    paste0("province_year_climate_csv,", clim_py_csv),
    paste0("species_year_native_climate_csv,", clim_native_csv),
    paste0("effort_csv,", effort_csv),
    paste0("species_trait_csv,", trait_csv),
    paste0("output_dir,", output_dir),
    "year_min,2000",
    "year_max,2006",
    "model_engine,lme4"
  ),
  con = config_path
)

result <- run_hazard_analysis(config_path)

risk_tbl <- read_csv(file.path(output_dir, "data_clean", "hazard_risk_data.csv"), show_col_types = FALSE)
model_tbl <- read_csv(file.path(output_dir, "results", "model_comparison.csv"), show_col_types = FALSE)
coef_tbl <- read_csv(file.path(output_dir, "results", "model_coefficients.csv"), show_col_types = FALSE)
summary_tbl <- read_csv(file.path(output_dir, "diagnostics", "risk_set_summary.csv"), show_col_types = FALSE)

stopifnot(nrow(risk_tbl) > nrow(ndr_raw))
stopifnot(sum(risk_tbl$event, na.rm = TRUE) == nrow(ndr_raw))
stopifnot(all(c("M0", "M1", "M2", "M3", "M4") %in% model_tbl$model))
stopifnot(any(model_tbl$status == "ok"))
stopifnot(nrow(coef_tbl) > 0)
stopifnot(summary_tbl$n_events[[1]] == nrow(ndr_raw))
stopifnot(file.exists(file.path(output_dir, "logs", "run_summary.md")))
stopifnot(file.exists(file.path(output_dir, "diagnostics", "model_data_availability.csv")))

cat("Hazard model smoke test passed.\n")
