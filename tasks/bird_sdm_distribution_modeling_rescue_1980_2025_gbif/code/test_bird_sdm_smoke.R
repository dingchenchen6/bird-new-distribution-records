#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(sf)
  library(terra)
  library(tibble)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep('^--file=', cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_path <- gsub("~\\+~", " ", sub('^--file=', '', file_arg[[1]]))
  script_dir <- dirname(normalizePath(script_path))
} else {
  script_dir <- normalizePath('code')
}
Sys.setenv(BIRD_SDM_SKIP_AUTORUN = "1")
source(file.path(script_dir, 'run_bird_sdm_distribution_modeling_rescue_1980_2025_gbif.R'))
safe_assert <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

src <- load_new_record_data()
previous_status <- load_previous_species_status()
rescue_species_master <- build_rescue_species_master(src$new_records, src$species_pool, previous_status)
toy_species <- normalize_species_name(rescue_species_master$species[[1]])
toy_occurrence_csv <- file.path(tempdir(), "birdwatch_sdm_smoke_occurrence.csv")
readr::write_csv(
  tibble(
    shp_species = toy_species,
    order_occurrence = "Passeriformes",
    family_occurrence = "Muscicapidae",
    longitude = c(116.40, 116.55, 116.62, 116.71),
    latitude = c(39.90, 39.95, 40.01, 40.07),
    serial_id = c("smoke_1", "smoke_2", "smoke_3", "smoke_4"),
    year = c(2022, 2023, 2024, 2025),
    province_name = "北京市",
    city_name = "北京市",
    district_name = c("东城区", "西城区", "海淀区", "朝阳区"),
    point_name = c("Point A", "Point B", "Point C", "Point D"),
    username = c("tester_a", "tester_b", "tester_c", "tester_d"),
    occurrence_source = "smoke_test_csv"
  ),
  toy_occurrence_csv
)
OCCURRENCE_SOURCE <- "csv"
OCCURRENCE_CSV_PATH <- toy_occurrence_csv

result <- run_pipeline(prepare_only = TRUE)
safe_assert(file.exists(MATCH_TABLE_PATH), 'Taxonomy review table was not generated.')
safe_assert(file.exists(MASTER_SPECIES_PATH), 'Species master table was not generated.')
safe_assert(file.exists(RESCUE_TARGET_PATH), 'Rescue target species table was not generated.')
safe_assert(nrow(result$match_table) == result$summary$new_species_total, 'Species review table does not cover the full rescue species pool.')

provinces_all <- load_province_boundaries()
china_boundary <- build_china_boundary(provinces_all)
occ <- load_occurrence_points(china_boundary)
safe_assert(nrow(occ) > 0, 'Filtered bird occurrence table is empty.')

provinces <- load_province_boundaries() |> dplyr::slice_head(n = 1) |> sf::st_set_crs(NA)
ext <- st_bbox(provinces)
r <- rast(xmin = ext[['xmin']], xmax = ext[['xmax']], ymin = ext[['ymin']], ymax = ext[['ymax']], resolution = 0.25)
values(r) <- 1
province_tbl <- province_sensitivity_summary(r > 0, provinces, 'Smoke species', 'current', 'ensemble_mean', thresholds = c(3, 10, 20, 50, 100, 200))
safe_assert(any(province_tbl$cell_threshold == 3 & province_tbl$presence_flag), '3-cell threshold should mark presence in the toy example.')
safe_assert(any(province_tbl$cell_threshold == 10 & province_tbl$presence_flag), '10-cell threshold should mark presence in the toy example.')
safe_assert(any(province_tbl$cell_threshold == 20 & province_tbl$presence_flag), '20-cell threshold should mark presence in the toy example.')
safe_assert(any(province_tbl$cell_threshold == 50 & province_tbl$presence_flag), '50-cell threshold should mark presence in the toy example.')
safe_assert(any(province_tbl$cell_threshold == 100 & province_tbl$presence_flag), '100-cell threshold should mark presence in the toy example.')
safe_assert(any(province_tbl$cell_threshold == 200 & province_tbl$presence_flag), '200-cell threshold should mark presence in the toy example.')
safe_assert(all(c('province_total_area_km2', 'suitable_area_prop_of_species', 'suitable_area_prop_of_province') %in% names(province_tbl)), 'Province summary is missing new area/proportion columns.')

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

toy_plot <- plot_probability_map(r, provinces, dash_line = NULL, title = "Smoke map", occurrence_points = presence_pts)
safe_assert(inherits(toy_plot, "ggplot"), 'Probability map function should return a ggplot object when occurrence points are supplied.')

algo_tbl <- read_csv(ALGO_AVAILABILITY_PATH, show_col_types = FALSE)
safe_assert('MaxEnt' %in% algo_tbl$algorithm, 'Algorithm availability table is missing MaxEnt.')

tmp_climate_root <- file.path(tempdir(), 'bird_sdm_climate_scan')
dir.create(tmp_climate_root, recursive = TRUE, showWarnings = FALSE)
wc_dir <- file.path(tmp_climate_root, 'wc2.1_2.5m')
dir.create(wc_dir, recursive = TRUE, showWarnings = FALSE)
toy_raster <- rast(xmin = 70, xmax = 71, ymin = 20, ymax = 21, resolution = 0.5)
values(toy_raster) <- 1
for (i in seq_len(19)) {
  writeRaster(toy_raster, file.path(wc_dir, paste0('wc2.1_2.5m_bio_', i, '.tif')), overwrite = TRUE)
}
writeRaster(toy_raster, file.path(wc_dir, 'wc2.1_2.5m_elev.tif'), overwrite = TRUE)
climate_scan_tbl <- scan_existing_worldclim_current_files('2.5', search_roots = tmp_climate_root)
safe_assert(nrow(climate_scan_tbl) == 20, 'Existing climate scan should find 19 bioclim files plus one elevation file.')
safe_assert(all(climate_scan_tbl$readable), 'Scanned climate rasters should be readable by terra.')

cat('Smoke test completed successfully.\n')
