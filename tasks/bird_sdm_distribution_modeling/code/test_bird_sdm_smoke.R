#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(sf)
  library(terra)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep('^--file=', cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub('^--file=', '', file_arg[[1]])))
} else {
  script_dir <- normalizePath('code')
}
source(file.path(script_dir, 'run_bird_sdm_distribution_modeling.R'))

safe_assert <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

result <- run_pipeline(prepare_only = TRUE)
safe_assert(file.exists(MATCH_TABLE_PATH), 'Taxonomy review table was not generated.')
safe_assert(file.exists(MASTER_SPECIES_PATH), 'Species master table was not generated.')
safe_assert(nrow(result$match_table) == result$summary$new_species_total, 'Species review table does not cover the full new-record species pool.')

provinces_all <- load_province_boundaries()
china_boundary <- build_china_boundary(provinces_all)
occ <- load_occurrence_points(china_boundary)
safe_assert(nrow(occ) > 0, 'Filtered bird occurrence table is empty.')

provinces <- load_province_boundaries() |> dplyr::slice_head(n = 1) |> sf::st_set_crs(NA)
ext <- st_bbox(provinces)
r <- rast(xmin = ext[['xmin']], xmax = ext[['xmax']], ymin = ext[['ymin']], ymax = ext[['ymax']], resolution = 0.25)
values(r) <- 1
province_tbl <- province_sensitivity_summary(r > 0, provinces, 'Smoke species', 'current', 'ensemble_mean', thresholds = c(3, 10, 20, 50))
safe_assert(any(province_tbl$cell_threshold == 3 & province_tbl$presence_flag), '3-cell threshold should mark presence in the toy example.')
safe_assert(any(province_tbl$cell_threshold == 10 & province_tbl$presence_flag), '10-cell threshold should mark presence in the toy example.')
safe_assert(any(province_tbl$cell_threshold == 20 & province_tbl$presence_flag), '20-cell threshold should mark presence in the toy example.')
safe_assert(any(province_tbl$cell_threshold == 50 & province_tbl$presence_flag), '50-cell threshold should mark presence in the toy example.')

x_span <- ext[['xmax']] - ext[['xmin']]
y_span <- ext[['ymax']] - ext[['ymin']]
presence_pts <- st_as_sf(
  data.frame(
    species = 'Smoke species',
    longitude = c(ext[['xmin']] + x_span * 0.35, ext[['xmin']] + x_span * 0.65),
    latitude = c(ext[['ymin']] + y_span * 0.35, ext[['ymin']] + y_span * 0.65)
  ),
  coords = c('longitude', 'latitude'),
  crs = 4326
)
area_sf <- st_as_sf(st_as_sfc(ext))
background_pts <- sample_background_points(area_sf, r, presence_pts, multiplier = 2, minimum_n = 5)
safe_assert(nrow(background_pts) == 5, 'Background sampling should return the requested number of points.')

algo_tbl <- read_csv(ALGO_AVAILABILITY_PATH, show_col_types = FALSE)
safe_assert('MaxEnt' %in% algo_tbl$algorithm, 'Algorithm availability table is missing MaxEnt.')

cat('Smoke test completed successfully.\n')
