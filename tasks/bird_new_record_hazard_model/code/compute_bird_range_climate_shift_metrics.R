#!/usr/bin/env Rscript

# ============================================================
# Bird Range Climate Shift Metrics Pipeline
# 中国鸟类新纪录历史分布区气候与空间位移分析全流程脚本
# ============================================================
#
# Research goals / 研究目标
# 1. Quantify the historical climate baseline within each focal species'
#    Chinese historical distribution (resident, breeding, and combined zones).
#    量化每个新纪录物种在中国历史分布范围内 resident、breeding 及合并范围
#    的气候基线（1970-2000 平均温度与降水）。
# 2. Extract the annual climate for the publication year of each new record,
#    both for the historical range and the newly reported point.
#    提取每条新纪录发表当年，在历史分布区和新纪录点上的年尺度温度与降水。
# 3. Measure climate differences and spatial displacement between the new
#    record point and the species' Chinese historical distribution.
#    计算新纪录点相对于中国历史分布区的气候差值、方位角、经纬度位移及距离位移。
#
# Core scientific questions / 核心科学问题
# - Are newly reported localities warmer or cooler than the historical range
#   baseline within China?
#   新纪录点当年的气候相对于中国历史分布区基线是更暖还是更冷？
# - Does the publication-year climate over the historical range differ from
#   the 1970-2000 baseline, indicating background climate change?
#   新纪录发表当年历史分布区的气候是否偏离 1970-2000 基线，从而反映背景气候变化？
# - Do new records preferentially occur in specific directions and at specific
#   distances from the historical distribution edge or centroid?
#   新纪录是否更偏向历史分布区之外的特定方向，且具有稳定的边缘距离或中心距离模式？
#
# Workflow overview / 思路框架
# Step 0. Validate configuration, create output folders, and record session info.
# 第 0 步：校验配置、建立输出目录并记录运行环境。
# Step 1. Read raw input tables and build diagnostics on completeness and schema.
# 第 1 步：读取原始输入，并生成完整性与字段结构诊断。
# Step 2. Clean the new-record point table and standardize key fields.
# 第 2 步：清洗新纪录点表并标准化关键字段。
# Step 3. Read world range maps, identify resident/breeding zones, and intersect
#         them with the China boundary to define Chinese historical ranges.
# 第 3 步：读取世界分布图，识别 resident/breeding 区并与中国边界相交，
#         构建物种在中国的历史分布范围。
# Step 4. Extract climate baselines and publication-year climate from rasters.
# 第 4 步：从气候栅格中提取历史基线和发表当年气候。
# Step 5. Compute directional and distance-based displacement metrics.
# 第 5 步：计算方位角、8 方位类别及多种位移指标。
# Step 6. Export cleaned tables, diagnostics, summary figures, and markdown logs.
# 第 6 步：导出清洗数据、诊断结果、基础图件和 Markdown 运行摘要。

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(sf)
  library(stringr)
  library(terra)
  library(tidyr)
})

options(stringsAsFactors = FALSE)
ensure_proj_environment <- function() {
  proj_dir <- system.file("proj", package = "sf")
  proj_db <- file.path(proj_dir, "proj.db")

  if (nzchar(proj_dir) && file.exists(proj_db)) {
    current_proj_lib <- Sys.getenv("PROJ_LIB", "")
    current_proj_data <- Sys.getenv("PROJ_DATA", "")

    if (!nzchar(current_proj_lib)) {
      Sys.setenv(PROJ_LIB = proj_dir)
    }

    if (!nzchar(current_proj_data)) {
      Sys.setenv(PROJ_DATA = proj_dir)
    }
  }
}

ensure_proj_environment()
# Use GEOS instead of s2 for the main vector topology workflow. The cleaned
# BirdLife polygons can still contain edge-crossing rings that cause s2 to
# abort on intersection/union, whereas the GEOS path is more tolerant here.
# 主分析的矢量拓扑运算改用 GEOS 而不是 s2。清洗后的 BirdLife 面在少数情况下
# 仍可能包含边界自交环，s2 会直接中断；GEOS 对这类残留问题更稳健。
sf::sf_use_s2(FALSE)

equal_area_crs <- 6933
lonlat_crs <- 4326
direction_levels <- c(
  "North", "Northeast", "East", "Southeast",
  "South", "Southwest", "West", "Northwest"
)

theme_pipeline <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(color = "#444444"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "#222222")
    )
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

cfg_get_codes <- function(cfg, key, default = character()) {
  raw <- cfg_get(cfg, key, NA_character_)
  if (is.na(raw)) {
    return(default)
  }

  str_split(raw, "\\|", simplify = FALSE)[[1]] %>%
    str_trim() %>%
    discard(~ .x == "")
}

ensure_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

create_output_tree <- function(output_root) {
  subdirs <- list(
    data_clean = file.path(output_root, "data_clean"),
    diagnostics = file.path(output_root, "diagnostics"),
    results = file.path(output_root, "results"),
    figures = file.path(output_root, "figures"),
    logs = file.path(output_root, "logs")
  )

  walk(c(output_root, unlist(subdirs, use.names = FALSE)), ensure_dir)
  subdirs
}

path_file_info <- function(path) {
  tibble(
    path = path,
    exists = file.exists(path),
    size_bytes = ifelse(file.exists(path), file.info(path)$size, NA_real_)
  )
}

build_field_dictionary <- function(df, dataset_name) {
  tibble(
    dataset = dataset_name,
    field_name = names(df),
    field_class = map_chr(df, ~ paste(class(.x), collapse = "|")),
    n_missing = map_int(df, ~ sum(is.na(.x))),
    pct_missing = map_dbl(df, ~ mean(is.na(.x)))
  )
}

build_missingness_summary <- function(df, dataset_name) {
  tibble(
    dataset = dataset_name,
    metric = c("rows", "columns", "complete_rows"),
    value = c(
      nrow(df),
      ncol(df),
      sum(stats::complete.cases(df))
    )
  )
}

write_session_info_file <- function(path) {
  con <- file(path, open = "wt")
  on.exit(close(con), add = TRUE)
  writeLines(capture.output(sessionInfo()), con = con, useBytes = TRUE)
}

write_preview_table <- function(df, path, n = 20) {
  preview_df <- utils::head(df, n = n)
  write_csv(preview_df, path)
}

build_climate_inventory <- function(dir_path, variable_name) {
  if (is.na(dir_path) || !dir.exists(dir_path)) {
    return(tibble(
      variable = variable_name,
      year = integer(),
      path = character()
    ))
  }

  files <- list.files(
    dir_path,
    pattern = "\\.(tif|tiff|grd|asc)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  tibble(path = files) %>%
    mutate(
      variable = variable_name,
      basename = basename(path),
      year = str_extract(basename, "(?<!\\d)(19|20)\\d{2}(?!\\d)") %>% as.integer()
    ) %>%
    select(variable, year, path)
}

save_basic_figures <- function(record_metrics, figure_dir) {
  if (nrow(record_metrics) == 0) {
    return(invisible(NULL))
  }

  direction_df <- record_metrics %>%
    filter(!is.na(resident_breeding_direction_8)) %>%
    count(resident_breeding_direction_8, name = "n") %>%
    mutate(resident_breeding_direction_8 = factor(resident_breeding_direction_8, levels = direction_levels))

  if (nrow(direction_df) > 0) {
    p_direction <- ggplot(direction_df, aes(x = resident_breeding_direction_8, y = n)) +
      geom_col(fill = "#2F6C8F", width = 0.72) +
      labs(
        title = "Directional distribution of new bird records",
        subtitle = "Eight-direction summary relative to the resident+breeding historical range centroid",
        x = NULL,
        y = "Number of records"
      ) +
      theme_pipeline() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    ggsave(
      filename = file.path(figure_dir, "fig_direction_8_record_counts.png"),
      plot = p_direction,
      width = 8.2,
      height = 5.1,
      dpi = 320,
      bg = "white"
    )
  }

  temp_df <- record_metrics %>%
    filter(!is.na(point_temp_minus_resident_breeding_baseline))

  if (nrow(temp_df) > 0) {
    p_temp <- ggplot(temp_df, aes(x = point_temp_minus_resident_breeding_baseline)) +
      geom_histogram(fill = "#C4642D", color = "white", bins = 30) +
      labs(
        title = "Temperature difference between new record year and historical range baseline",
        subtitle = "Point-year temperature minus resident+breeding Chinese historical range baseline",
        x = "Temperature difference",
        y = "Number of records"
      ) +
      theme_pipeline()

    ggsave(
      filename = file.path(figure_dir, "fig_temperature_difference_histogram.png"),
      plot = p_temp,
      width = 8.2,
      height = 5.1,
      dpi = 320,
      bg = "white"
    )
  }
}

detect_column <- function(df, explicit_name = NULL, candidates = character()) {
  if (!is.null(explicit_name) && !is.na(explicit_name) && explicit_name %in% names(df)) {
    return(explicit_name)
  }

  lowered <- tolower(names(df))
  match_idx <- match(tolower(candidates), lowered, nomatch = 0)
  match_idx <- match_idx[match_idx > 0]
  if (length(match_idx) == 0) {
    return(NA_character_)
  }

  names(df)[match_idx[[1]]]
}

clean_polygon_sf <- function(x) {
  if (nrow(x) == 0) {
    return(x)
  }

  x <- x %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON", warn = FALSE)

  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  x$.geom_area_m2 <- suppressWarnings(as.numeric(st_area(st_transform(x, equal_area_crs))))

  x %>%
    filter(!is.na(.geom_area_m2), .geom_area_m2 > 0) %>%
    select(-.geom_area_m2)
}

load_china_boundary <- function(path = NULL) {
  if (!is.null(path) && !is.na(path) && file.exists(path)) {
    china <- st_read(path, quiet = TRUE)
  } else if (requireNamespace("rnaturalearth", quietly = TRUE)) {
    china <- rnaturalearth::ne_countries(country = "China", returnclass = "sf")
  } else {
    stop(
      "Missing china_boundary_path and rnaturalearth is not available. ",
      "Please provide a local China boundary file."
    )
  }

  china %>%
    clean_polygon_sf() %>%
    st_transform(lonlat_crs) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(country = "China")
}

split_synonym_names <- function(x) {
  x %>%
    as.character() %>%
    str_split(pattern = ";|\\|", simplify = FALSE) %>%
    flatten_chr() %>%
    str_squish() %>%
    discard(~is.na(.x) || .x == "")
}

build_species_crosswalk <- function(
  checklist_path,
  checklist_layer = NA_character_,
  accepted_name_col = "ScientificName",
  synonyms_col = "Synonyms"
) {
  if (is.na(checklist_path) || !file.exists(checklist_path)) {
    return(tibble(input_name = character(), accepted_name = character(), match_type = character()))
  }

  read_args <- list(dsn = checklist_path, quiet = TRUE)
  if (!is.na(checklist_layer) && !identical(checklist_layer, "")) {
    read_args$layer <- checklist_layer
  }

  checklist <- do.call(st_read, read_args) %>%
    st_drop_geometry()

  accepted_col <- detect_column(checklist, explicit_name = accepted_name_col, candidates = c("scientificname", "scientific_name"))
  synonyms_detected_col <- detect_column(checklist, explicit_name = synonyms_col, candidates = c("synonyms", "synonym"))

  if (is.na(accepted_col) || is.na(synonyms_detected_col)) {
    return(tibble(input_name = character(), accepted_name = character(), match_type = character()))
  }

  accepted_tbl <- checklist %>%
    transmute(
      accepted_name = str_squish(as.character(.data[[accepted_col]]))
    ) %>%
    filter(!is.na(accepted_name), accepted_name != "") %>%
    mutate(
      input_name = accepted_name,
      match_type = "accepted_name"
    )

  synonym_tbl <- checklist %>%
    transmute(
      accepted_name = str_squish(as.character(.data[[accepted_col]])),
      synonym_string = as.character(.data[[synonyms_detected_col]])
    ) %>%
    filter(!is.na(accepted_name), accepted_name != "") %>%
    mutate(input_name = map(synonym_string, split_synonym_names)) %>%
    select(-synonym_string) %>%
    unnest(input_name) %>%
    filter(!is.na(input_name), input_name != "") %>%
    mutate(match_type = "synonym")

  bind_rows(accepted_tbl, synonym_tbl) %>%
    mutate(input_name = str_squish(input_name), accepted_name = str_squish(accepted_name)) %>%
    filter(input_name != "", accepted_name != "") %>%
    distinct(input_name, .keep_all = TRUE)
}

apply_species_crosswalk <- function(new_records, crosswalk_tbl) {
  if (nrow(crosswalk_tbl) == 0) {
    return(new_records %>% mutate(species_original = species, species_match_type = "unmapped"))
  }

  new_records %>%
    mutate(species_original = species) %>%
    left_join(crosswalk_tbl, by = c("species" = "input_name")) %>%
    mutate(
      species = coalesce(accepted_name, species),
      species_match_type = case_when(
        !is.na(match_type) & species_original != species ~ match_type,
        !is.na(match_type) & species_original == species ~ "accepted_name",
        TRUE ~ "unmapped"
      )
    ) %>%
    select(-accepted_name, -match_type)
}

normalize_zone <- function(x) {
  x_chr <- str_to_lower(str_trim(as.character(x)))
  numeric_value <- suppressWarnings(as.numeric(x_chr))

  case_when(
    !is.na(numeric_value) & numeric_value == 1 ~ "resident",
    !is.na(numeric_value) & numeric_value == 2 ~ "breeding",
    !is.na(numeric_value) & numeric_value == 3 ~ "nonbreeding",
    !is.na(numeric_value) & numeric_value == 4 ~ "passage",
    !is.na(numeric_value) & numeric_value == 5 ~ "seasonal_uncertain",
    str_detect(x_chr, "resident|year") ~ "resident",
    str_detect(x_chr, "breed") ~ "breeding",
    str_detect(x_chr, "winter|non[- ]?breed") ~ "nonbreeding",
    str_detect(x_chr, "passage|migrat") ~ "passage",
    str_detect(x_chr, "uncertain|season") ~ "seasonal_uncertain",
    TRUE ~ NA_character_
  )
}

build_historical_range_definitions <- function(range_by_zone) {
  resident_breeding <- range_by_zone %>%
    filter(zone %in% c("resident", "breeding")) %>%
    group_by(species) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    mutate(zone = "resident_breeding") %>%
    select(species, zone, geometry)

  all_seasons <- range_by_zone %>%
    group_by(species) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    mutate(zone = "all_seasons") %>%
    select(species, zone, geometry)

  bind_rows(resident_breeding, all_seasons)
}

apply_raster_scale <- function(r, scale_factor) {
  if (is.na(scale_factor) || identical(scale_factor, 1) || identical(scale_factor, 1.0)) {
    return(r)
  }
  r * scale_factor
}

extract_polygon_mean <- function(r, sf_polygons) {
  if (nrow(sf_polygons) == 0) {
    return(numeric())
  }

  values <- terra::extract(r, terra::vect(sf_polygons), fun = mean, na.rm = TRUE)
  as.numeric(values[[2]])
}

extract_point_values <- function(r, sf_points) {
  if (nrow(sf_points) == 0) {
    return(numeric())
  }

  values <- terra::extract(r, terra::vect(sf_points))
  as.numeric(values[[2]])
}

find_yearly_raster <- function(dir_path, year) {
  if (is.na(dir_path) || !dir.exists(dir_path)) {
    return(NA_character_)
  }

  candidates <- list.files(
    dir_path,
    pattern = "\\.(tif|tiff|grd|asc)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  year_pattern <- paste0("(^|[^0-9])", year, "([^0-9]|$)")
  matches <- candidates[str_detect(basename(candidates), year_pattern)]

  if (length(matches) == 0) {
    return(NA_character_)
  }

  sort(matches)[[1]]
}

initial_bearing_deg <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  lon1r <- lon1 * rad
  lat1r <- lat1 * rad
  lon2r <- lon2 * rad
  lat2r <- lat2 * rad

  dlon <- lon2r - lon1r
  y <- sin(dlon) * cos(lat2r)
  x <- cos(lat1r) * sin(lat2r) - sin(lat1r) * cos(lat2r) * cos(dlon)
  bearing <- atan2(y, x) * 180 / pi
  (bearing + 360) %% 360
}

classify_direction_8 <- function(angle) {
  case_when(
    is.na(angle) ~ NA_character_,
    angle >= 337.5 | angle < 22.5 ~ "North",
    angle < 67.5 ~ "Northeast",
    angle < 112.5 ~ "East",
    angle < 157.5 ~ "Southeast",
    angle < 202.5 ~ "South",
    angle < 247.5 ~ "Southwest",
    angle < 292.5 ~ "West",
    TRUE ~ "Northwest"
  )
}

safe_centroid_lonlat <- function(geom_sf) {
  centroid <- geom_sf %>%
    st_transform(equal_area_crs) %>%
    st_union() %>%
    st_centroid() %>%
    st_transform(lonlat_crs)

  coords <- st_coordinates(centroid)
  tibble(
    centroid_lon = coords[, "X"],
    centroid_lat = coords[, "Y"]
  )
}

compute_point_range_metrics <- function(point_sf, polygon_sf) {
  point_lonlat <- st_transform(point_sf, lonlat_crs)
  polygon_lonlat <- st_transform(polygon_sf, lonlat_crs)
  point_projected <- st_transform(point_sf, equal_area_crs)
  polygon_projected <- st_transform(polygon_sf, equal_area_crs)

  centroid_tbl <- safe_centroid_lonlat(polygon_lonlat)
  centroid_point <- st_as_sf(
    centroid_tbl,
    coords = c("centroid_lon", "centroid_lat"),
    crs = lonlat_crs
  )

  boundary_projected <- polygon_projected %>%
    st_boundary() %>%
    st_collection_extract("LINESTRING")

  nearest_line_projected <- st_nearest_points(point_projected, boundary_projected)
  nearest_line_lonlat <- st_transform(nearest_line_projected, lonlat_crs)
  nearest_coords <- st_coordinates(nearest_line_lonlat)
  edge_coords <- nearest_coords[nrow(nearest_coords), c("X", "Y"), drop = FALSE]
  point_coords <- st_coordinates(point_lonlat)
  centroid_coords <- st_coordinates(centroid_point)

  inside_range <- lengths(st_within(point_projected, polygon_projected)) > 0

  distance_to_edge_km <- as.numeric(st_length(nearest_line_projected)) / 1000
  outside_gap_to_range_km <- if (inside_range) {
    0
  } else {
    as.numeric(st_distance(point_projected, polygon_projected)) / 1000
  }

  centroid_point_projected <- st_transform(centroid_point, equal_area_crs)
  distance_to_centroid_km <- as.numeric(st_distance(point_projected, centroid_point_projected)) / 1000

  centroid_bearing <- initial_bearing_deg(
    lon1 = centroid_coords[, "X"],
    lat1 = centroid_coords[, "Y"],
    lon2 = point_coords[, "X"],
    lat2 = point_coords[, "Y"]
  )

  edge_bearing <- initial_bearing_deg(
    lon1 = edge_coords[, "X"],
    lat1 = edge_coords[, "Y"],
    lon2 = point_coords[, "X"],
    lat2 = point_coords[, "Y"]
  )

  tibble(
    range_centroid_lon = centroid_coords[, "X"],
    range_centroid_lat = centroid_coords[, "Y"],
    nearest_edge_lon = edge_coords[, "X"],
    nearest_edge_lat = edge_coords[, "Y"],
    bearing_from_centroid_deg = centroid_bearing,
    direction_8 = classify_direction_8(centroid_bearing),
    bearing_from_nearest_edge_deg = edge_bearing,
    direction_8_from_nearest_edge = classify_direction_8(edge_bearing),
    centroid_delta_lon_deg = point_coords[, "X"] - centroid_coords[, "X"],
    centroid_delta_lat_deg = point_coords[, "Y"] - centroid_coords[, "Y"],
    edge_delta_lon_deg = point_coords[, "X"] - edge_coords[, "X"],
    edge_delta_lat_deg = point_coords[, "Y"] - edge_coords[, "Y"],
    distance_to_range_edge_km = distance_to_edge_km,
    outside_gap_to_range_km = outside_gap_to_range_km,
    distance_to_range_centroid_km = distance_to_centroid_km,
    point_inside_historical_range = inside_range
  )
}

compute_geometry_metrics_for_definition <- function(point_sf, range_sf, zone_name, prefix) {
  zone_ranges <- range_sf %>% filter(zone == zone_name)
  range_lookup <- split(zone_ranges, zone_ranges$species)

  map_dfr(seq_len(nrow(point_sf)), function(i) {
    row_point <- point_sf[i, ]
    polygon_sf <- range_lookup[[row_point$species]]

    if (is.null(polygon_sf) || nrow(polygon_sf) == 0) {
      out <- tibble(
        record_id = row_point$record_id,
        range_centroid_lon = NA_real_,
        range_centroid_lat = NA_real_,
        nearest_edge_lon = NA_real_,
        nearest_edge_lat = NA_real_,
        bearing_from_centroid_deg = NA_real_,
        direction_8 = NA_character_,
        bearing_from_nearest_edge_deg = NA_real_,
        direction_8_from_nearest_edge = NA_character_,
        centroid_delta_lon_deg = NA_real_,
        centroid_delta_lat_deg = NA_real_,
        edge_delta_lon_deg = NA_real_,
        edge_delta_lat_deg = NA_real_,
        distance_to_range_edge_km = NA_real_,
        outside_gap_to_range_km = NA_real_,
        distance_to_range_centroid_km = NA_real_,
        point_inside_historical_range = NA
      )
    } else {
      out <- compute_point_range_metrics(row_point, polygon_sf) %>%
        mutate(record_id = row_point$record_id, .before = 1)
    }

    out %>%
      rename_with(~paste0(prefix, .x), -record_id)
  })
}

summarize_range_polygons <- function(range_sf, baseline_temp, baseline_prec) {
  area_km2 <- range_sf %>%
    st_transform(equal_area_crs) %>%
    st_area() %>%
    as.numeric() / 1e6

  baseline_temp_mean <- extract_polygon_mean(baseline_temp, range_sf)
  baseline_prec_mean <- extract_polygon_mean(baseline_prec, range_sf)

  range_sf %>%
    mutate(
      area_km2 = area_km2,
      baseline_temp_mean = baseline_temp_mean,
      baseline_prec_mean = baseline_prec_mean
    ) %>%
    st_drop_geometry()
}

compute_annual_range_climate <- function(species_year_tbl, range_sf, annual_temp_dir, annual_prec_dir, annual_temp_scale, annual_prec_scale) {
  if (nrow(species_year_tbl) == 0) {
    return(tibble())
  }

  split(species_year_tbl, species_year_tbl$year) %>%
    imap_dfr(function(tbl, year_chr) {
      year_value <- as.integer(year_chr)
      temp_path <- find_yearly_raster(annual_temp_dir, year_value)
      prec_path <- find_yearly_raster(annual_prec_dir, year_value)

      joined <- range_sf %>%
        inner_join(tbl, by = "species")

      temp_values <- rep(NA_real_, nrow(joined))
      prec_values <- rep(NA_real_, nrow(joined))

      if (!all(is.na(temp_path))) {
        temp_values <- extract_polygon_mean(
          apply_raster_scale(terra::rast(temp_path), annual_temp_scale),
          joined
        )
      }

      if (!all(is.na(prec_path))) {
        prec_values <- extract_polygon_mean(
          apply_raster_scale(terra::rast(prec_path), annual_prec_scale),
          joined
        )
      }

      joined %>%
        st_drop_geometry() %>%
        transmute(
          species,
          zone,
          year = year_value,
          range_year_temp_mean = temp_values,
          range_year_prec_mean = prec_values,
          annual_temp_path = temp_path,
          annual_prec_path = prec_path
        )
    })
}

compute_annual_point_climate <- function(point_sf, annual_temp_dir, annual_prec_dir, annual_temp_scale, annual_prec_scale) {
  if (nrow(point_sf) == 0) {
    return(tibble())
  }

  split(point_sf, point_sf$year) %>%
    imap_dfr(function(points_subset, year_chr) {
      year_value <- as.integer(year_chr)
      temp_path <- find_yearly_raster(annual_temp_dir, year_value)
      prec_path <- find_yearly_raster(annual_prec_dir, year_value)

      out <- points_subset %>%
        st_drop_geometry() %>%
        select(record_id)

      out$point_year_temp <- NA_real_
      out$point_year_prec <- NA_real_

      if (!all(is.na(temp_path))) {
        out$point_year_temp <- extract_point_values(
          apply_raster_scale(terra::rast(temp_path), annual_temp_scale),
          points_subset
        )
      }

      if (!all(is.na(prec_path))) {
        out$point_year_prec <- extract_point_values(
          apply_raster_scale(terra::rast(prec_path), annual_prec_scale),
          points_subset
        )
      }

      out %>%
        mutate(
          annual_temp_path = temp_path,
          annual_prec_path = prec_path
        )
    })
}

write_summary_md <- function(path, meta) {
  lines <- c(
    "# Run summary",
    "",
    "## Overview",
    "",
    paste0("- Date: ", Sys.Date()),
    paste0("- Resolution label: ", meta$resolution_label),
    paste0("- New records loaded: ", meta$n_records),
    paste0("- Species in new records: ", meta$n_species),
    paste0("- Species with historical range polygons in China: ", meta$n_species_with_range),
    paste0("- Resident/breeding/combined polygons exported: ", meta$n_range_rows),
    paste0("- Missing historical range species: ", meta$n_missing_species)
  )

  if (length(meta$missing_species) > 0) {
    lines <- c(lines, "", "## Missing species", "", paste(meta$missing_species, collapse = ", "))
  }

  if (length(meta$missing_temp_years) > 0) {
    lines <- c(lines, "", "## Missing annual temperature rasters", "", paste(sort(unique(meta$missing_temp_years)), collapse = ", "))
  }

  if (length(meta$missing_prec_years) > 0) {
    lines <- c(lines, "", "## Missing annual precipitation rasters", "", paste(sort(unique(meta$missing_prec_years)), collapse = ", "))
  }

  if (!is.null(meta$notes) && length(meta$notes) > 0) {
    lines <- c(lines, "", "## Notes", "", paste0("- ", meta$notes))
  }

  writeLines(lines, path, useBytes = TRUE)
}

run_analysis <- function(config_path) {
  cfg <- read_config(config_path)

  output_root <- cfg_get(cfg, "output_dir", file.path(dirname(config_path), "..", "results"))
  output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
  output_dirs <- create_output_tree(output_root)

  new_record_csv <- cfg_get(cfg, "new_record_csv")
  range_map_path <- cfg_get(cfg, "range_map_path")
  range_layer_name <- cfg_get(cfg, "range_layer_name", NA_character_)
  taxonomic_reference_path <- cfg_get(cfg, "taxonomic_reference_path", range_map_path)
  taxonomic_reference_layer <- cfg_get(cfg, "taxonomic_reference_layer", ifelse(str_detect(range_map_path, "\\.gpkg$"), "main_BL_HBW_Checklist_V9", NA_character_))
  baseline_temp_path <- cfg_get(cfg, "baseline_temp_path")
  baseline_prec_path <- cfg_get(cfg, "baseline_prec_path")
  annual_temp_dir <- cfg_get(cfg, "annual_temp_dir")
  annual_prec_dir <- cfg_get(cfg, "annual_prec_dir")
  china_boundary_path <- cfg_get(cfg, "china_boundary_path", NA_character_)

  required_files <- c(new_record_csv, range_map_path, baseline_temp_path, baseline_prec_path)
  missing_required <- required_files[!file.exists(required_files)]
  if (length(missing_required) > 0) {
    stop("Missing required inputs: ", paste(missing_required, collapse = "; "))
  }

  baseline_temp_scale <- cfg_get_num(cfg, "baseline_temp_scale", 1)
  baseline_prec_scale <- cfg_get_num(cfg, "baseline_prec_scale", 1)
  annual_temp_scale <- cfg_get_num(cfg, "annual_temp_scale", 1)
  annual_prec_scale <- cfg_get_num(cfg, "annual_prec_scale", 1)
  year_min <- cfg_get_num(cfg, "year_min", 1900)
  year_max <- cfg_get_num(cfg, "year_max", 2100)
  resolution_label <- cfg_get(cfg, "resolution_label", "unspecified")

  range_presence_keep <- cfg_get_codes(cfg, "range_presence_keep", c("1", "2", "3", "6"))
  range_origin_keep <- cfg_get_codes(cfg, "range_origin_keep", c("1", "2", "5", "6"))

  manifest_tbl <- bind_rows(
    path_file_info(config_path) %>% mutate(input_type = "config"),
    path_file_info(new_record_csv) %>% mutate(input_type = "new_record_csv"),
    path_file_info(range_map_path) %>% mutate(input_type = "range_map"),
    path_file_info(baseline_temp_path) %>% mutate(input_type = "baseline_temp"),
    path_file_info(baseline_prec_path) %>% mutate(input_type = "baseline_prec")
  ) %>%
    select(input_type, everything())

  write_csv(manifest_tbl, file.path(output_dirs$diagnostics, "input_file_manifest.csv"))
  write_session_info_file(file.path(output_dirs$logs, "session_info.txt"))

  new_records <- read_csv(new_record_csv, show_col_types = FALSE) %>%
    transmute(
      record_id = if ("record_id" %in% names(.)) record_id else row_number(),
      species = str_squish(as.character(species)),
      order = if ("order" %in% names(.)) str_squish(as.character(order)) else NA_character_,
      province = if ("province" %in% names(.)) str_squish(as.character(province)) else NA_character_,
      year = suppressWarnings(as.integer(year)),
      longitude = suppressWarnings(as.numeric(longitude)),
      latitude = suppressWarnings(as.numeric(latitude))
    ) %>%
    filter(
      !is.na(species),
      species != "",
      !is.na(year),
      year >= year_min,
      year <= year_max,
      !is.na(longitude),
      !is.na(latitude)
    )

  if (nrow(new_records) == 0) {
    stop("No valid new-record rows remained after filtering.")
  }

  species_crosswalk_tbl <- build_species_crosswalk(
    checklist_path = taxonomic_reference_path,
    checklist_layer = taxonomic_reference_layer,
    accepted_name_col = cfg_get(cfg, "taxonomic_accepted_name_col", "ScientificName"),
    synonyms_col = cfg_get(cfg, "taxonomic_synonyms_col", "Synonyms")
  )

  new_records <- apply_species_crosswalk(new_records, species_crosswalk_tbl)

  write_csv(new_records, file.path(output_dirs$data_clean, "bird_new_records_for_range_climate.csv"))
  write_csv(species_crosswalk_tbl, file.path(output_dirs$diagnostics, "bird_taxonomic_name_crosswalk.csv"))
  write_preview_table(new_records, file.path(output_dirs$diagnostics, "bird_new_records_preview_top20.csv"), n = 20)
  write_csv(
    bind_rows(
      build_missingness_summary(new_records, "bird_new_records_cleaned"),
      tibble(
        dataset = "bird_new_records_cleaned",
        metric = c("unique_species", "year_min", "year_max", "records_mapped_by_synonym", "records_unmapped_after_taxonomic_step"),
        value = c(
          n_distinct(new_records$species),
          min(new_records$year),
          max(new_records$year),
          sum(new_records$species_match_type == "synonym", na.rm = TRUE),
          sum(new_records$species_match_type == "unmapped", na.rm = TRUE)
        )
      )
    ),
    file.path(output_dirs$diagnostics, "bird_new_records_data_diagnostics.csv")
  )
  write_csv(
    build_field_dictionary(new_records, "bird_new_records_cleaned"),
    file.path(output_dirs$diagnostics, "bird_new_records_field_dictionary.csv")
  )

  new_record_points <- st_as_sf(new_records, coords = c("longitude", "latitude"), crs = lonlat_crs, remove = FALSE)

  china_boundary <- load_china_boundary(china_boundary_path)
  range_read_args <- list(dsn = range_map_path, quiet = TRUE)
  if (!is.na(range_layer_name) && !identical(range_layer_name, "")) {
    range_read_args$layer <- range_layer_name
  }

  range_map <- do.call(st_read, range_read_args) %>%
    clean_polygon_sf() %>%
    st_transform(lonlat_crs)

  write_csv(
    build_field_dictionary(st_drop_geometry(range_map), "range_map_attributes"),
    file.path(output_dirs$diagnostics, "range_map_field_dictionary.csv")
  )

  species_col <- detect_column(
    range_map,
    explicit_name = cfg_get(cfg, "range_species_col", NA_character_),
    candidates = c("species", "scientific_name", "sciname", "binomial", "sci_name", "latin")
  )
  seasonal_col <- detect_column(
    range_map,
    explicit_name = cfg_get(cfg, "range_seasonal_col", NA_character_),
    candidates = c("seasonal", "season", "seasonality")
  )
  presence_col <- detect_column(
    range_map,
    explicit_name = cfg_get(cfg, "range_presence_col", NA_character_),
    candidates = c("presence")
  )
  origin_col <- detect_column(
    range_map,
    explicit_name = cfg_get(cfg, "range_origin_col", NA_character_),
    candidates = c("origin")
  )

  if (is.na(species_col) || is.na(seasonal_col)) {
    stop("Failed to detect species/seasonal columns in the range map. Please set them in config.")
  }

  range_filtered <- range_map %>%
    transmute(
      species = str_squish(as.character(.data[[species_col]])),
      seasonal_raw = .data[[seasonal_col]],
      presence_raw = if (!is.na(presence_col)) .data[[presence_col]] else NA,
      origin_raw = if (!is.na(origin_col)) .data[[origin_col]] else NA,
      geometry = st_geometry(.)
    ) %>%
    mutate(
      zone = normalize_zone(seasonal_raw),
      presence_chr = as.character(presence_raw),
      origin_chr = as.character(origin_raw)
    ) %>%
    filter(
      species %in% unique(new_records$species),
      !is.na(zone)
    )

  if (!all(is.na(range_filtered$presence_chr))) {
    range_filtered <- range_filtered %>%
      filter(presence_chr %in% range_presence_keep)
  }

  if (!all(is.na(range_filtered$origin_chr))) {
    range_filtered <- range_filtered %>%
      filter(origin_chr %in% range_origin_keep)
  }

  write_csv(
    range_filtered %>%
      st_drop_geometry() %>%
      count(zone, name = "n_features") %>%
      arrange(desc(n_features)),
    file.path(output_dirs$diagnostics, "range_map_zone_counts_after_filtering.csv")
  )

  if (nrow(range_filtered) == 0) {
    stop("No range-map polygons remained after filtering by species/zone/presence/origin.")
  }

  china_intersection <- suppressWarnings(st_intersection(range_filtered, china_boundary))

  if (nrow(china_intersection) == 0) {
    stop("Range map did not intersect the China boundary after filtering.")
  }

  range_by_zone <- china_intersection %>%
    group_by(species, zone) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    clean_polygon_sf() %>%
    group_by(species, zone) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")

  range_definitions <- build_historical_range_definitions(range_by_zone)

  historical_ranges <- bind_rows(
    range_by_zone %>% select(species, zone, geometry),
    range_definitions
  ) %>%
    st_as_sf(crs = lonlat_crs) %>%
    clean_polygon_sf() %>%
    group_by(species, zone) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")

  write_csv(
    historical_ranges %>%
      st_drop_geometry() %>%
      count(zone, name = "n_species_zone_rows"),
    file.path(output_dirs$diagnostics, "historical_range_zone_inventory.csv")
  )

  baseline_temp <- apply_raster_scale(terra::rast(baseline_temp_path), baseline_temp_scale)
  baseline_prec <- apply_raster_scale(terra::rast(baseline_prec_path), baseline_prec_scale)

  baseline_summary <- summarize_range_polygons(historical_ranges, baseline_temp, baseline_prec) %>%
    mutate(resolution_label = resolution_label)

  baseline_wide <- baseline_summary %>%
    select(species, zone, baseline_temp_mean, baseline_prec_mean, area_km2) %>%
    tidyr::pivot_wider(
      names_from = zone,
      values_from = c(baseline_temp_mean, baseline_prec_mean, area_km2),
      names_glue = "{zone}_{.value}"
    )

  species_year_tbl <- new_records %>%
    distinct(species, year)

  climate_inventory_tbl <- bind_rows(
    build_climate_inventory(annual_temp_dir, "annual_temperature"),
    build_climate_inventory(annual_prec_dir, "annual_precipitation")
  )
  write_csv(climate_inventory_tbl, file.path(output_dirs$diagnostics, "climate_year_inventory.csv"))

  annual_range_tbl_long <- compute_annual_range_climate(
    species_year_tbl = species_year_tbl,
    range_sf = historical_ranges %>% filter(zone %in% c("resident_breeding", "all_seasons")),
    annual_temp_dir = annual_temp_dir,
    annual_prec_dir = annual_prec_dir,
    annual_temp_scale = annual_temp_scale,
    annual_prec_scale = annual_prec_scale
  ) %>%
    left_join(
      baseline_summary %>%
        filter(zone %in% c("resident_breeding", "all_seasons")) %>%
        select(species, zone, baseline_temp_mean, baseline_prec_mean),
      by = c("species", "zone")
    ) %>%
    mutate(
      range_temp_delta_from_baseline = range_year_temp_mean - baseline_temp_mean,
      range_prec_delta_from_baseline = range_year_prec_mean - baseline_prec_mean
    )

  annual_range_tbl <- annual_range_tbl_long %>%
    select(
      species,
      year,
      zone,
      range_year_temp_mean,
      range_year_prec_mean,
      range_temp_delta_from_baseline,
      range_prec_delta_from_baseline
    ) %>%
    tidyr::pivot_wider(
      names_from = zone,
      values_from = c(
        range_year_temp_mean,
        range_year_prec_mean,
        range_temp_delta_from_baseline,
        range_prec_delta_from_baseline
      ),
      names_glue = "{zone}_{.value}"
    )

  point_baseline_tbl <- new_record_points %>%
    st_drop_geometry() %>%
    select(record_id) %>%
    mutate(
      point_baseline_temp = extract_point_values(baseline_temp, new_record_points),
      point_baseline_prec = extract_point_values(baseline_prec, new_record_points)
    )

  point_year_tbl <- compute_annual_point_climate(
    point_sf = new_record_points,
    annual_temp_dir = annual_temp_dir,
    annual_prec_dir = annual_prec_dir,
    annual_temp_scale = annual_temp_scale,
    annual_prec_scale = annual_prec_scale
  )

  geometry_metrics <- compute_geometry_metrics_for_definition(
    point_sf = new_record_points,
    range_sf = historical_ranges,
    zone_name = "resident_breeding",
    prefix = "resident_breeding_"
  ) %>%
    full_join(
      compute_geometry_metrics_for_definition(
        point_sf = new_record_points,
        range_sf = historical_ranges,
        zone_name = "all_seasons",
        prefix = "all_seasons_"
      ),
      by = "record_id"
    )

  final_column_order <- c(
    "record_id",
    "species_original",
    "species",
    "species_match_type",
    "order",
    "province",
    "year",
    "longitude",
    "latitude",
    "resident_baseline_temp_mean",
    "resident_baseline_prec_mean",
    "resident_area_km2",
    "breeding_baseline_temp_mean",
    "breeding_baseline_prec_mean",
    "breeding_area_km2",
    "nonbreeding_baseline_temp_mean",
    "nonbreeding_baseline_prec_mean",
    "nonbreeding_area_km2",
    "passage_baseline_temp_mean",
    "passage_baseline_prec_mean",
    "passage_area_km2",
    "seasonal_uncertain_baseline_temp_mean",
    "seasonal_uncertain_baseline_prec_mean",
    "seasonal_uncertain_area_km2",
    "resident_breeding_baseline_temp_mean",
    "resident_breeding_baseline_prec_mean",
    "resident_breeding_area_km2",
    "all_seasons_baseline_temp_mean",
    "all_seasons_baseline_prec_mean",
    "all_seasons_area_km2",
    "resident_breeding_range_year_temp_mean",
    "resident_breeding_range_year_prec_mean",
    "resident_breeding_range_temp_delta_from_baseline",
    "resident_breeding_range_prec_delta_from_baseline",
    "all_seasons_range_year_temp_mean",
    "all_seasons_range_year_prec_mean",
    "all_seasons_range_temp_delta_from_baseline",
    "all_seasons_range_prec_delta_from_baseline",
    "point_baseline_temp",
    "point_baseline_prec",
    "point_year_temp",
    "point_year_prec",
    "point_temp_delta_from_point_baseline",
    "point_prec_delta_from_point_baseline",
    "point_temp_minus_resident_breeding_baseline",
    "point_prec_minus_resident_breeding_baseline",
    "point_temp_minus_all_seasons_baseline",
    "point_prec_minus_all_seasons_baseline",
    "resident_breeding_bearing_from_centroid_deg",
    "resident_breeding_direction_8",
    "resident_breeding_bearing_from_nearest_edge_deg",
    "resident_breeding_direction_8_from_nearest_edge",
    "resident_breeding_range_centroid_lon",
    "resident_breeding_range_centroid_lat",
    "resident_breeding_nearest_edge_lon",
    "resident_breeding_nearest_edge_lat",
    "resident_breeding_centroid_delta_lon_deg",
    "resident_breeding_centroid_delta_lat_deg",
    "resident_breeding_edge_delta_lon_deg",
    "resident_breeding_edge_delta_lat_deg",
    "resident_breeding_distance_to_range_edge_km",
    "resident_breeding_outside_gap_to_range_km",
    "resident_breeding_distance_to_range_centroid_km",
    "resident_breeding_point_inside_historical_range",
    "all_seasons_bearing_from_centroid_deg",
    "all_seasons_direction_8",
    "all_seasons_bearing_from_nearest_edge_deg",
    "all_seasons_direction_8_from_nearest_edge",
    "all_seasons_range_centroid_lon",
    "all_seasons_range_centroid_lat",
    "all_seasons_nearest_edge_lon",
    "all_seasons_nearest_edge_lat",
    "all_seasons_centroid_delta_lon_deg",
    "all_seasons_centroid_delta_lat_deg",
    "all_seasons_edge_delta_lon_deg",
    "all_seasons_edge_delta_lat_deg",
    "all_seasons_distance_to_range_edge_km",
    "all_seasons_outside_gap_to_range_km",
    "all_seasons_distance_to_range_centroid_km",
    "all_seasons_point_inside_historical_range"
  )

  record_metrics <- new_records %>%
    left_join(baseline_wide, by = "species") %>%
    left_join(annual_range_tbl, by = c("species", "year")) %>%
    left_join(point_baseline_tbl, by = "record_id") %>%
    left_join(
      point_year_tbl %>%
        select(record_id, point_year_temp, point_year_prec),
      by = "record_id"
    ) %>%
    left_join(geometry_metrics, by = "record_id") %>%
    mutate(
      point_temp_delta_from_point_baseline = point_year_temp - point_baseline_temp,
      point_prec_delta_from_point_baseline = point_year_prec - point_baseline_prec,
      point_temp_minus_resident_breeding_baseline = point_year_temp - resident_breeding_baseline_temp_mean,
      point_prec_minus_resident_breeding_baseline = point_year_prec - resident_breeding_baseline_prec_mean,
      point_temp_minus_all_seasons_baseline = point_year_temp - all_seasons_baseline_temp_mean,
      point_prec_minus_all_seasons_baseline = point_year_prec - all_seasons_baseline_prec_mean,
      resident_breeding_direction_8 = factor(resident_breeding_direction_8, levels = direction_levels),
      resident_breeding_direction_8_from_nearest_edge = factor(resident_breeding_direction_8_from_nearest_edge, levels = direction_levels),
      all_seasons_direction_8 = factor(all_seasons_direction_8, levels = direction_levels),
      all_seasons_direction_8_from_nearest_edge = factor(all_seasons_direction_8_from_nearest_edge, levels = direction_levels)
    ) %>%
    select(any_of(final_column_order))

  final_flat_table <- record_metrics %>%
    arrange(year, province, species, record_id)

  st_write(
    historical_ranges,
    dsn = file.path(output_dirs$results, "historical_range_polygons_china.gpkg"),
    delete_dsn = TRUE,
    quiet = TRUE
  )

  write_csv(baseline_summary, file.path(output_dirs$results, "species_range_zone_baseline_climate.csv"))
  write_csv(annual_range_tbl, file.path(output_dirs$results, "species_year_historical_range_climate.csv"))
  write_csv(record_metrics, file.path(output_dirs$results, "new_record_range_climate_metrics.csv"))
  write_csv(final_flat_table, file.path(output_dirs$results, "table_new_record_province_year_climate_direction_displacement.csv"))
  write_preview_table(record_metrics, file.path(output_dirs$diagnostics, "new_record_range_climate_metrics_preview_top20.csv"), n = 20)
  write_preview_table(final_flat_table, file.path(output_dirs$diagnostics, "table_new_record_province_year_climate_direction_displacement_preview_top20.csv"), n = 20)

  write_csv(
    final_flat_table %>%
      summarise(
        n_records = n(),
        n_species = n_distinct(species),
        n_nonmissing_point_year_temp = sum(!is.na(point_year_temp)),
        n_nonmissing_resident_breeding_range_year_temp = sum(!is.na(resident_breeding_range_year_temp_mean)),
        n_nonmissing_all_seasons_range_year_temp = sum(!is.na(all_seasons_range_year_temp_mean)),
        n_nonmissing_resident_breeding_direction = sum(!is.na(resident_breeding_direction_8)),
        n_nonmissing_resident_breeding_edge_direction = sum(!is.na(resident_breeding_direction_8_from_nearest_edge)),
        n_nonmissing_all_seasons_direction = sum(!is.na(all_seasons_direction_8)),
        n_nonmissing_all_seasons_edge_direction = sum(!is.na(all_seasons_direction_8_from_nearest_edge)),
        mean_resident_breeding_distance_to_edge_km = mean(resident_breeding_distance_to_range_edge_km, na.rm = TRUE),
        mean_all_seasons_distance_to_edge_km = mean(all_seasons_distance_to_range_edge_km, na.rm = TRUE),
        mean_resident_breeding_distance_to_centroid_km = mean(resident_breeding_distance_to_range_centroid_km, na.rm = TRUE),
        mean_all_seasons_distance_to_centroid_km = mean(all_seasons_distance_to_range_centroid_km, na.rm = TRUE)
      ),
    file.path(output_dirs$results, "table_record_metrics_summary.csv")
  )

  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(
      list(
        new_record_metrics = final_flat_table,
        species_range_baseline = baseline_summary,
        species_year_range_climate = annual_range_tbl
      ),
      path = file.path(output_dirs$results, "bird_range_climate_shift_metrics_bundle.xlsx")
    )
  }

  save_basic_figures(record_metrics, output_dirs$figures)

  resident_breeding_ranges <- historical_ranges %>% filter(zone == "resident_breeding")
  all_seasons_ranges <- historical_ranges %>% filter(zone == "all_seasons")
  missing_species <- setdiff(unique(new_records$species), unique(all_seasons_ranges$species))
  missing_temp_years <- species_year_tbl$year[is.na(match(
    species_year_tbl$year,
    annual_range_tbl$year[
      !is.na(annual_range_tbl$resident_breeding_range_year_temp_mean) |
        !is.na(annual_range_tbl$all_seasons_range_year_temp_mean)
    ]
  ))]
  missing_prec_years <- species_year_tbl$year[is.na(match(
    species_year_tbl$year,
    annual_range_tbl$year[
      !is.na(annual_range_tbl$resident_breeding_range_year_prec_mean) |
        !is.na(annual_range_tbl$all_seasons_range_year_prec_mean)
    ]
  ))]

  write_summary_md(
    file.path(output_dirs$logs, "run_summary.md"),
    meta = list(
      resolution_label = resolution_label,
      n_records = nrow(new_records),
      n_species = n_distinct(new_records$species),
      n_species_with_range = n_distinct(all_seasons_ranges$species),
      n_range_rows = nrow(historical_ranges),
      n_missing_species = length(missing_species),
      missing_species = sort(missing_species),
      missing_temp_years = missing_temp_years,
      missing_prec_years = missing_prec_years,
      notes = c(
        "All output tables are separated into data_clean, diagnostics, results, figures, and logs folders.",
        "If the range map uses different seasonal/presence/origin field names or coding schemes, update the config file explicitly.",
        "Historical range definitions include both resident+breeding and all-seasons unions.",
        "If annual climate files are incomplete, the pipeline still writes partial outputs and records missing years."
      )
    )
  )

  invisible(
    list(
      baseline_summary = baseline_summary,
      annual_range_tbl = annual_range_tbl,
      record_metrics = final_flat_table,
      output_dir = output_root
    )
  )
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    stop("Usage: Rscript code/compute_bird_range_climate_shift_metrics.R <config.csv>")
  }

  run_analysis(args[[1]])
}

if (sys.nframe() == 0) {
  main()
}
