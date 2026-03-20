#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
script_dir <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])))
source(file.path(script_dir, "run_bird_sdm_distribution_modeling.R"))

roots_arg <- args[grepl("^--roots=", args)]
res_arg <- args[grepl("^--res=", args)]

search_roots <- if (length(roots_arg)) {
  strsplit(sub("^--roots=", "", roots_arg[[1]]), "[;,]")[[1]]
} else {
  c(file.path(path.expand("~"), "projects"), path.expand("~"))
}
search_roots <- trimws(search_roots)
search_roots <- search_roots[nzchar(search_roots) & dir.exists(search_roots)]
res_value <- if (length(res_arg)) sub("^--res=", "", res_arg[[1]]) else "2.5"

cat("Scanning roots:\n")
for (root in search_roots) cat(" - ", root, "\n", sep = "")

scan_tbl <- scan_existing_worldclim_current_files(res_value = res_value, search_roots = search_roots)

if (!nrow(scan_tbl)) {
  cat("No matching WorldClim files were found.\n")
  quit(status = 1)
}

bio_tbl <- scan_tbl %>%
  filter(grepl("_bio_[0-9]+\\.tif$", .data$target_name), .data$readable) %>%
  mutate(directory = dirname(.data$path))

elev_tbl <- scan_tbl %>%
  filter(grepl("_elev\\.tif$", .data$target_name), .data$readable) %>%
  mutate(directory = dirname(.data$path))

bio_dir_summary <- bio_tbl %>%
  count(.data$directory, name = "n_bio_files") %>%
  arrange(desc(.data$n_bio_files), nchar(.data$directory))

cat("\nReadable current-climate files found: ", nrow(scan_tbl %>% filter(.data$readable)), "\n", sep = "")
cat("Manifest written to: ", CLIMATE_MANIFEST_PATH, "\n", sep = "")

if (nrow(bio_dir_summary)) {
  cat("\nBioclim directories:\n")
  print(bio_dir_summary, n = Inf)
} else {
  cat("\nNo readable bioclim directory was found.\n")
}

if (nrow(elev_tbl)) {
  cat("\nReadable elevation files:\n")
  print(elev_tbl %>% select(.data$path), n = Inf)
} else {
  cat("\nNo readable elevation file was found.\n")
}

complete_bio_dir <- bio_dir_summary %>%
  filter(.data$n_bio_files == 19) %>%
  slice_head(n = 1) %>%
  pull(.data$directory)

if (length(complete_bio_dir) == 1) {
  cat("\nPreferred complete bioclim directory:\n", complete_bio_dir, "\n", sep = "")
} else {
  cat("\nNo complete 19-layer bioclim directory is currently readable.\n")
}
