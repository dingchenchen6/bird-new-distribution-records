#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(sf)
  library(terra)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  normalizePath("code/test_bird_range_climate_shift_metrics_smoke.R", mustWork = TRUE)
}

task_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(task_root, "code", "compute_bird_range_climate_shift_metrics.R"))

tmp_root <- file.path(tempdir(), "bird_range_climate_shift_metrics_smoke")
dir.create(tmp_root, showWarnings = FALSE, recursive = TRUE)

china_path <- file.path(tmp_root, "china_boundary.gpkg")
range_path <- file.path(tmp_root, "world_bird_ranges.gpkg")
baseline_temp_path <- file.path(tmp_root, "temp_1970_2000.tif")
baseline_prec_path <- file.path(tmp_root, "prec_1970_2000.tif")
annual_temp_dir <- file.path(tmp_root, "annual_temp")
annual_prec_dir <- file.path(tmp_root, "annual_prec")
output_dir <- file.path(tmp_root, "results")
config_path <- file.path(tmp_root, "config.csv")
new_record_csv <- file.path(tmp_root, "new_records.csv")

dir.create(annual_temp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(annual_prec_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

china_poly <- st_as_sf(
  data.frame(
    id = 1,
    wkt = "POLYGON((75 15, 120 15, 120 45, 75 45, 75 15))"
  ),
  wkt = "wkt",
  crs = 4326
)
st_write(china_poly, china_path, delete_dsn = TRUE, quiet = TRUE)

range_sf <- st_as_sf(
  data.frame(
    species = c("Testus avis", "Testus avis"),
    seasonal = c(1, 2),
    wkt = c(
      "POLYGON((80 20, 100 20, 100 30, 80 30, 80 20))",
      "POLYGON((82 22, 98 22, 98 28, 82 28, 82 22))"
    )
  ),
  wkt = "wkt",
  crs = 4326
)
st_write(range_sf, range_path, delete_dsn = TRUE, quiet = TRUE)

template_raster <- rast(ncols = 4, nrows = 4, xmin = 75, xmax = 120, ymin = 15, ymax = 45, crs = "EPSG:4326")

baseline_temp <- template_raster
values(baseline_temp) <- 10
writeRaster(baseline_temp, baseline_temp_path, overwrite = TRUE)

baseline_prec <- template_raster
values(baseline_prec) <- 100
writeRaster(baseline_prec, baseline_prec_path, overwrite = TRUE)

annual_temp <- template_raster
values(annual_temp) <- 12
writeRaster(annual_temp, file.path(annual_temp_dir, "temp_2000.tif"), overwrite = TRUE)

annual_prec <- template_raster
values(annual_prec) <- 110
writeRaster(annual_prec, file.path(annual_prec_dir, "prec_2000.tif"), overwrite = TRUE)

write_csv(
  tibble::tibble(
    record_id = 1,
    species = "Testus avis",
    year = 2000,
    longitude = 105,
    latitude = 35
  ),
  new_record_csv
)

writeLines(
  c(
    "key,value",
    paste0("new_record_csv,", new_record_csv),
    paste0("range_map_path,", range_path),
    paste0("china_boundary_path,", china_path),
    paste0("baseline_temp_path,", baseline_temp_path),
    paste0("baseline_prec_path,", baseline_prec_path),
    paste0("annual_temp_dir,", annual_temp_dir),
    paste0("annual_prec_dir,", annual_prec_dir),
    paste0("output_dir,", output_dir),
    "range_species_col,species",
    "range_seasonal_col,seasonal",
    "baseline_temp_scale,1",
    "baseline_prec_scale,1",
    "annual_temp_scale,1",
    "annual_prec_scale,1",
    "resolution_label,smoke_test",
    "year_min,1900",
    "year_max,2100"
  ),
  config_path
)

result <- run_analysis(config_path)

baseline_tbl <- read_csv(file.path(output_dir, "results", "species_range_zone_baseline_climate.csv"), show_col_types = FALSE)
record_tbl <- read_csv(file.path(output_dir, "results", "new_record_range_climate_metrics.csv"), show_col_types = FALSE)
flat_tbl <- read_csv(file.path(output_dir, "results", "table_new_record_province_year_climate_direction_displacement.csv"), show_col_types = FALSE)

stopifnot(nrow(baseline_tbl) == 3)
stopifnot(nrow(record_tbl) == 1)
stopifnot(nrow(flat_tbl) == 1)
stopifnot(abs(record_tbl$point_year_temp[[1]] - 12) < 1e-9)
stopifnot(abs(record_tbl$point_baseline_temp[[1]] - 10) < 1e-9)
stopifnot(abs(record_tbl$point_temp_delta_from_point_baseline[[1]] - 2) < 1e-9)
stopifnot(abs(record_tbl$range_year_temp_mean[[1]] - 12) < 1e-9)
stopifnot(abs(record_tbl$combined_baseline_temp_mean[[1]] - 10) < 1e-9)
stopifnot(record_tbl$distance_to_range_edge_km[[1]] > 0)
stopifnot("province" %in% names(flat_tbl))
stopifnot("direction_8" %in% names(flat_tbl))
stopifnot("distance_to_range_edge_km" %in% names(flat_tbl))
stopifnot(file.exists(file.path(output_dir, "diagnostics", "input_file_manifest.csv")))
stopifnot(file.exists(file.path(output_dir, "figures", "fig_direction_8_record_counts.png")))
stopifnot(file.exists(file.path(output_dir, "logs", "run_summary.md")))

cat("Smoke test passed.\n")
