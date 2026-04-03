#!/usr/bin/env Rscript

# ============================================================
# WorldClim 2.1 10m climate preparation for bird new-record analysis
# 基于 WorldClim 2.1 10 分钟分辨率的气候数据准备脚本
# ============================================================
#
# Research support goal / 支撑目标
# This script downloads and prepares climate inputs needed by the
# bird range climate shift metrics pipeline.
# 本脚本用于下载并整理鸟类新纪录历史分布与气候差值分析所需的气候输入数据。
#
# Data logic / 数据逻辑
# 1. Baseline climatology (1970-2000): use WorldClim 2.1 "base" monthly
#    tavg and precipitation layers at 10 minutes.
#    基线气候（1970-2000）：使用 WorldClim 2.1 base 的 10m 月均温和月降水。
# 2. Publication-year climate: use WorldClim "historical monthly weather"
#    data (downscaled from CRU TS 4.09), available by decade.
#    发表当年气候：使用 WorldClim 历史月尺度天气数据
#    （基于 CRU TS 4.09 下采样），按 decade 下载。
# 3. Annual temperature is computed as the mean of monthly tavg, where
#    monthly tavg = (tmin + tmax) / 2.
#    年均温 = 12 个月月均温的平均，其中月均温 = (tmin + tmax) / 2。
# 4. Annual precipitation is computed as the sum of monthly precipitation.
#    年降水 = 12 个月月降水总和。

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(terra)
  library(tibble)
})

# Large WorldClim historical archives can exceed the default R timeout on
# remote servers with unstable bandwidth. Increase the timeout globally so
# download.file has enough time to finish multi-hundred-MB archives.
# WorldClim 历史月尺度压缩包体积较大，在服务器带宽不稳定时容易超过 R 默认超时。
# 这里统一提高超时时间，避免大文件下载被 60 秒默认设置中断。
options(timeout = max(7200, getOption("timeout")))

ensure_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

read_config <- function(config_path) {
  cfg <- read_csv(config_path, show_col_types = FALSE) %>%
    transmute(
      key = str_trim(key),
      value = na_if(str_trim(value), "")
    )
  cfg_list <- cfg$value
  names(cfg_list) <- cfg$key
  cfg_list
}

cfg_get <- function(cfg, key, default = NULL) {
  if (!key %in% names(cfg)) {
    return(default)
  }
  value <- cfg[[key]]
  if (is.null(value) || is.na(value) || identical(value, "")) default else value
}

cfg_get_num <- function(cfg, key, default = NA_real_) {
  value <- suppressWarnings(as.numeric(cfg_get(cfg, key, default)))
  ifelse(is.na(value), default, value)
}

get_required_years <- function(new_record_csv, year_min = 1900, year_max = 2100) {
  read_csv(new_record_csv, show_col_types = FALSE) %>%
    transmute(year = suppressWarnings(as.integer(year))) %>%
    filter(!is.na(year), year >= year_min, year <= year_max) %>%
    distinct(year) %>%
    arrange(year) %>%
    pull(year)
}

worldclim_hist_period <- function(year) {
  case_when(
    year >= 1950 & year <= 1959 ~ "1950-1959",
    year >= 1960 & year <= 1969 ~ "1960-1969",
    year >= 1970 & year <= 1979 ~ "1970-1979",
    year >= 1980 & year <= 1989 ~ "1980-1989",
    year >= 1990 & year <= 1999 ~ "1990-1999",
    year >= 2000 & year <= 2009 ~ "2000-2009",
    year >= 2010 & year <= 2019 ~ "2010-2019",
    year >= 2020 & year <= 2024 ~ "2020-2024",
    TRUE ~ NA_character_
  )
}

download_if_missing <- function(url, destfile, mode = "wb", quiet = FALSE) {
  zip_is_valid <- function(path) {
    if (!file.exists(path) || file.info(path)$size <= 0) {
      return(FALSE)
    }

    isTRUE(tryCatch({
      suppressWarnings(utils::unzip(path, list = TRUE))
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE))
  }

  if (zip_is_valid(destfile)) {
    return(invisible(destfile))
  }

  ensure_dir(dirname(destfile))

  # Retry a few times and discard corrupted partial archives before retrying.
  # 多次重试，并在重试前删除损坏或未下载完整的部分压缩包。
  max_attempts <- 3

  for (attempt in seq_len(max_attempts)) {
    if (file.exists(destfile)) {
      unlink(destfile)
    }

    ok <- tryCatch({
      download.file(
        url = url,
        destfile = destfile,
        mode = mode,
        quiet = quiet,
        method = "libcurl"
      )
      TRUE
    }, error = function(e) {
      message("Download attempt ", attempt, " failed for ", basename(destfile), ": ", conditionMessage(e))
      FALSE
    })

    if (ok && zip_is_valid(destfile)) {
      return(invisible(destfile))
    }

    if (attempt < max_attempts) {
      Sys.sleep(5 * attempt)
    }
  }

  stop("Failed to download a valid archive after ", max_attempts, " attempts: ", url)
}

unzip_if_needed <- function(zip_path, exdir) {
  ensure_dir(exdir)
  existing_tif <- list.files(exdir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  expected_tif_n <- tryCatch({
    utils::unzip(zip_path, list = TRUE) %>%
      filter(str_detect(Name, "\\.tif$")) %>%
      nrow()
  }, error = function(e) NA_integer_)

  if (length(existing_tif) > 0 && !is.na(expected_tif_n) && length(existing_tif) >= expected_tif_n) {
    return(invisible(existing_tif))
  }

  if (dir.exists(exdir)) {
    unlink(exdir, recursive = TRUE, force = TRUE)
  }
  ensure_dir(exdir)

  utils::unzip(zipfile = zip_path, exdir = exdir)
  invisible(list.files(exdir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE))
}

build_worldclim_urls <- function(resolution = "10m", periods) {
  base_root <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base"
  hist_root <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.09"

  baseline_tbl <- tibble(
    dataset = c("baseline_tavg", "baseline_prec"),
    variable = c("tavg", "prec"),
    period = "1970-2000",
    url = c(
      paste0(base_root, "/wc2.1_", resolution, "_tavg.zip"),
      paste0(base_root, "/wc2.1_", resolution, "_prec.zip")
    )
  )

  hist_tbl <- tidyr::expand_grid(
    variable = c("tmin", "tmax", "prec"),
    period = periods
  ) %>%
    mutate(
      dataset = paste0("historical_", variable),
      url = paste0(hist_root, "/wc2.1_cruts4.09_", resolution, "_", variable, "_", period, ".zip")
    )

  bind_rows(baseline_tbl, hist_tbl)
}

find_monthly_files <- function(path, prefix) {
  files <- list.files(path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  files[str_detect(basename(files), paste0("^", prefix, "_"))]
}

sort_worldclim_month_files <- function(files) {
  tibble(path = files) %>%
    mutate(
      basename = basename(path),
      month = str_extract(basename, "_([0-9]{1,2})\\.tif$") %>%
        str_remove_all("[^0-9]") %>%
        as.integer()
    ) %>%
    arrange(month) %>%
    pull(path)
}

prepare_baseline_rasters <- function(extract_root, output_root, resolution = "10m") {
  baseline_dir <- file.path(output_root, "baseline")
  ensure_dir(baseline_dir)

  tavg_files <- sort_worldclim_month_files(find_monthly_files(file.path(extract_root, "baseline_tavg"), paste0("wc2.1_", resolution, "_tavg")))
  prec_files <- sort_worldclim_month_files(find_monthly_files(file.path(extract_root, "baseline_prec"), paste0("wc2.1_", resolution, "_prec")))

  if (length(tavg_files) != 12 || length(prec_files) != 12) {
    stop("Baseline monthly files are incomplete. Expected 12 tavg and 12 prec tif files.")
  }

  tavg_stack <- terra::rast(tavg_files)
  prec_stack <- terra::rast(prec_files)

  baseline_temp <- app(tavg_stack, mean, na.rm = TRUE)
  baseline_prec <- app(prec_stack, sum, na.rm = TRUE)

  temp_path <- file.path(baseline_dir, paste0("worldclim_v2.1_", resolution, "_baseline_temp_1970_2000.tif"))
  prec_path <- file.path(baseline_dir, paste0("worldclim_v2.1_", resolution, "_baseline_prec_1970_2000.tif"))

  writeRaster(baseline_temp, temp_path, overwrite = TRUE)
  writeRaster(baseline_prec, prec_path, overwrite = TRUE)

  tibble(
    output_type = c("baseline_temp", "baseline_prec"),
    path = c(temp_path, prec_path)
  )
}

prepare_annual_rasters <- function(years, extract_root, output_root, resolution = "10m") {
  annual_temp_dir <- file.path(output_root, "annual", "temp")
  annual_prec_dir <- file.path(output_root, "annual", "prec")
  ensure_dir(annual_temp_dir)
  ensure_dir(annual_prec_dir)

  results <- vector("list", length(years))

  for (i in seq_along(years)) {
    year_value <- years[[i]]
    period <- worldclim_hist_period(year_value)

    if (is.na(period)) {
      results[[i]] <- tibble(
        year = year_value,
        period = NA_character_,
        temp_path = NA_character_,
        prec_path = NA_character_,
        status = "unsupported_year"
      )
      next
    }

    # Historical monthly archives are unpacked into nested directories like
    # extract_root/historical_tmin/2000-2009 rather than flat names such as
    # historical_tmin_2000-2009.
    # 历史月数据解压后位于二级目录，例如：
    # extract_root/historical_tmin/2000-2009
    # 而不是 historical_tmin_2000-2009 这种单层目录。
    period_dir_tmin <- file.path(extract_root, "historical_tmin", period)
    period_dir_tmax <- file.path(extract_root, "historical_tmax", period)
    period_dir_prec <- file.path(extract_root, "historical_prec", period)

    # Historical monthly files use the CRU-TS-tagged prefix, e.g.
    # wc2.1_cruts4.09_5m_tmin_2000-01.tif
    # 历史月文件名前缀包含 cruts4.09，例如：
    # wc2.1_cruts4.09_5m_tmin_2000-01.tif
    hist_prefix_root <- paste0("wc2.1_cruts4.09_", resolution, "_")
    tmin_files <- find_monthly_files(period_dir_tmin, paste0(hist_prefix_root, "tmin"))
    tmax_files <- find_monthly_files(period_dir_tmax, paste0(hist_prefix_root, "tmax"))
    prec_files <- find_monthly_files(period_dir_prec, paste0(hist_prefix_root, "prec"))

    tmin_files <- tmin_files[str_detect(basename(tmin_files), paste0("_", year_value, "-"))]
    tmax_files <- tmax_files[str_detect(basename(tmax_files), paste0("_", year_value, "-"))]
    prec_files <- prec_files[str_detect(basename(prec_files), paste0("_", year_value, "-"))]

    tmin_files <- sort_worldclim_month_files(tmin_files)
    tmax_files <- sort_worldclim_month_files(tmax_files)
    prec_files <- sort_worldclim_month_files(prec_files)

    if (length(tmin_files) != 12 || length(tmax_files) != 12 || length(prec_files) != 12) {
      results[[i]] <- tibble(
        year = year_value,
        period = period,
        temp_path = NA_character_,
        prec_path = NA_character_,
        status = "missing_monthly_files"
      )
      next
    }

    temp_out <- file.path(annual_temp_dir, paste0("worldclim_v2.1_cruts4.09_", resolution, "_annual_temp_", year_value, ".tif"))
    prec_out <- file.path(annual_prec_dir, paste0("worldclim_v2.1_cruts4.09_", resolution, "_annual_prec_", year_value, ".tif"))

    if (!file.exists(temp_out)) {
      tmin_stack <- terra::rast(tmin_files)
      tmax_stack <- terra::rast(tmax_files)
      tavg_stack <- (tmin_stack + tmax_stack) / 2
      annual_temp <- app(tavg_stack, mean, na.rm = TRUE)
      writeRaster(annual_temp, temp_out, overwrite = TRUE)
    }

    if (!file.exists(prec_out)) {
      prec_stack <- terra::rast(prec_files)
      annual_prec <- app(prec_stack, sum, na.rm = TRUE)
      writeRaster(annual_prec, prec_out, overwrite = TRUE)
    }

    results[[i]] <- tibble(
      year = year_value,
      period = period,
      temp_path = temp_out,
      prec_path = prec_out,
      status = "ok"
    )
  }

  bind_rows(results)
}

write_markdown_summary <- function(path, years, periods, unsupported_years, outputs_root) {
  lines <- c(
    "# WorldClim 10m climate preparation summary",
    "",
    paste0("- Date: ", Sys.Date()),
    paste0("- Required years in new-record table: ", paste(years, collapse = ", ")),
    paste0("- Historical download periods: ", paste(periods, collapse = ", ")),
    paste0("- Output root: ", outputs_root)
  )

  if (length(unsupported_years) > 0) {
    lines <- c(
      lines, "", "## Unsupported years", "",
      paste0("WorldClim historical monthly weather currently covers 1950-2024. Missing years: ",
             paste(unsupported_years, collapse = ", "))
    )
  }

  writeLines(lines, path, useBytes = TRUE)
}

run_worldclim_preparation <- function(config_path) {
  cfg <- read_config(config_path)

  new_record_csv <- cfg_get(cfg, "new_record_csv")
  output_root <- cfg_get(cfg, "output_root", file.path(dirname(config_path), "..", "worldclim_10m"))
  resolution <- cfg_get(cfg, "resolution", "10m")
  year_min <- cfg_get_num(cfg, "year_min", 1900)
  year_max <- cfg_get_num(cfg, "year_max", 2100)

  if (!file.exists(new_record_csv)) {
    stop("Missing new_record_csv: ", new_record_csv)
  }

  output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
  download_root <- file.path(output_root, "downloads")
  extract_root <- file.path(output_root, "unzipped")
  prepared_root <- file.path(output_root, "prepared")
  diagnostics_root <- file.path(output_root, "diagnostics")
  logs_root <- file.path(output_root, "logs")

  walk(c(output_root, download_root, extract_root, prepared_root, diagnostics_root, logs_root), ensure_dir)

  years <- get_required_years(new_record_csv, year_min = year_min, year_max = year_max)
  periods <- unique(stats::na.omit(worldclim_hist_period(years)))
  unsupported_years <- years[is.na(worldclim_hist_period(years))]

  urls_tbl <- build_worldclim_urls(resolution = resolution, periods = periods) %>%
    mutate(
      zip_path = file.path(download_root, basename(url)),
      extract_dir = file.path(extract_root, dataset, ifelse(period == "1970-2000", "baseline", period))
    )

  write_csv(urls_tbl, file.path(diagnostics_root, "worldclim_download_manifest.csv"))

  for (i in seq_len(nrow(urls_tbl))) {
    download_if_missing(urls_tbl$url[[i]], urls_tbl$zip_path[[i]])
    unzip_if_needed(urls_tbl$zip_path[[i]], urls_tbl$extract_dir[[i]])
  }

  baseline_outputs <- prepare_baseline_rasters(extract_root = extract_root, output_root = prepared_root, resolution = resolution)
  annual_outputs <- prepare_annual_rasters(years = years, extract_root = extract_root, output_root = prepared_root, resolution = resolution)

  write_csv(baseline_outputs, file.path(diagnostics_root, "prepared_baseline_outputs.csv"))
  write_csv(annual_outputs, file.path(diagnostics_root, "prepared_annual_outputs.csv"))
  write_markdown_summary(
    path = file.path(logs_root, "worldclim_preparation_summary.md"),
    years = years,
    periods = periods,
    unsupported_years = unsupported_years,
    outputs_root = prepared_root
  )

  invisible(
    list(
      baseline_outputs = baseline_outputs,
      annual_outputs = annual_outputs,
      prepared_root = prepared_root
    )
  )
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    stop("Usage: Rscript code/prepare_worldclim_10m_climate_inputs.R <config.csv>")
  }
  run_worldclim_preparation(args[[1]])
}

if (sys.nframe() == 0) {
  main()
}
