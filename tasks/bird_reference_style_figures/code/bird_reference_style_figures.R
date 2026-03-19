#!/usr/bin/env Rscript

# ============================================================
# Bird new-record reference-style figure set
# 中国鸟类新纪录参考风格图组脚本
#
# Target figures / 目标图表:
# 1) Sankey-style alluvial: Order -> Province -> Year
#    桑基/冲积图：目 -> 省份 -> 年份
# 2) Circular phylogeny-like panel for newly recorded bird species
#    鸟类新纪录环形系统发育/分类树风格图
# 3) Directional radar/polar plot for newly recorded bird directions
#    鸟类新纪录方位雷达图/极坐标图
#
# Notes / 说明:
# - This script is designed to mimic the style, layout, and palette of the
#   mammal reference figures supplied by the user as closely as possible.
#   本脚本尽量复刻用户提供的哺乳动物参考图的样式、布局和配色。
# - A true bird backbone phylogeny is not available in the current local files.
#   Therefore, the circular tree is constructed from the nested taxonomic
#   hierarchy (order-family-genus-species) using ape::as.phylo.formula().
#   当前本地文件中没有现成的鸟类骨干系统发育树，因此环形树基于
#   “目-科-属-种”分类层级构建，为 taxonomic tree-style panel。
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(ggalluvial)
  library(scales)
  library(patchwork)
  library(janitor)
  library(ape)
  library(sf)
  library(geodata)
  library(ggrepel)
  library(cowplot)
  library(png)
  library(jpeg)
  library(grid)
  library(gridExtra)
  library(writexl)
})

set.seed(1234)

# -------------------------------
# 0) Paths and output structure
# 0) 路径与输出目录
# -------------------------------
input_xlsx <- "/Users/dingchenchen/Desktop/鸟类新纪录20260311.xlsx"
sheet_records <- "2000-2025鸟类新记录"
sheet_catalog <- "2025中国生物物种名录"

output_root <- "/Users/dingchenchen/Documents/New project/bird_new_records_R_output"
output_tables <- file.path(output_root, "tables")
output_figures <- file.path(output_root, "figures")

dir.create(output_tables, showWarnings = FALSE, recursive = TRUE)
dir.create(output_figures, showWarnings = FALSE, recursive = TRUE)

# -------------------------------
# 1) Global helpers and style
# 1) 全局辅助函数与风格
# -------------------------------
title_case_order <- function(x) {
  str_to_title(str_to_lower(str_to_upper(x)))
}

save_plot_dual <- function(p, filename_no_ext, width = 12, height = 8, dpi = 420) {
  png_path <- file.path(output_figures, paste0(filename_no_ext, ".png"))
  pdf_path <- file.path(output_figures, paste0(filename_no_ext, ".pdf"))
  ggsave(png_path, p, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(pdf_path, p, width = width, height = height, bg = "white", device = cairo_pdf)
}

theme_ref <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, color = "black"),
      plot.subtitle = element_text(size = base_size, color = "#303030"),
      plot.caption = element_text(size = base_size - 2, color = "#3B3B3B"),
      panel.grid.major = element_line(color = "#CFCFCF", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#202020"),
      axis.title = element_text(face = "bold", color = "#202020"),
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", color = "black")
    )
}

direction_levels <- c("North", "Northeast", "East", "Southeast", "South", "Southwest", "West", "Northwest")

province_cn_to_en <- c(
  "北京市" = "Beijing",
  "天津市" = "Tianjin",
  "上海市" = "Shanghai",
  "重庆市" = "Chongqing",
  "河北省" = "Hebei",
  "山西省" = "Shanxi",
  "辽宁省" = "Liaoning",
  "吉林省" = "Jilin",
  "黑龙江省" = "Heilongjiang",
  "江苏省" = "Jiangsu",
  "浙江省" = "Zhejiang",
  "安徽省" = "Anhui",
  "福建省" = "Fujian",
  "江西省" = "Jiangxi",
  "山东省" = "Shandong",
  "河南省" = "Henan",
  "湖北省" = "Hubei",
  "湖南省" = "Hunan",
  "广东省" = "Guangdong",
  "海南省" = "Hainan",
  "四川省" = "Sichuan",
  "贵州省" = "Guizhou",
  "云南省" = "Yunnan",
  "陕西省" = "Shaanxi",
  "甘肃省" = "Gansu",
  "青海省" = "Qinghai",
  "台湾省" = "Taiwan",
  "内蒙古自治区" = "Inner Mongolia",
  "广西壮族自治区" = "Guangxi",
  "西藏自治区" = "Tibet",
  "宁夏回族自治区" = "Ningxia",
  "新疆维吾尔自治区" = "Xinjiang",
  "香港特别行政区" = "Hong Kong",
  "澳门特别行政区" = "Macao"
)

order_palette <- c(
  "Passeriformes" = "#94A8D1",
  "Charadriiformes" = "#F28C57",
  "Anseriformes" = "#6AC1A5",
  "Accipitriformes" = "#E78AC3",
  "Pelecaniformes" = "#8BC34A",
  "Gruiformes" = "#D8B26E",
  "Columbiformes" = "#B0B0B0",
  "Strigiformes" = "#C9A0DC",
  "Galliformes" = "#F1C40F",
  "Coraciiformes" = "#6FA8DC",
  "Other orders" = "#BFBFBF"
)

iucn_palette <- c(
  "CR" = "#FF0000",
  "EN" = "#0000FF",
  "VU" = "#A020F0",
  "NT" = "#D9D9D9",
  "LC" = "#EFEFEF",
  "DD" = "#BDBDBD",
  "NE" = "#FFFFFF",
  "NA" = "#FFFFFF"
)

# China cartographic projection used for national-scale thematic mapping.
# 中国全国尺度专题图常用 Albers 等面积投影参数（统一在本脚本中固定）。
china_albers_proj4 <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

map_name_to_en <- c(
  "Beijing" = "Beijing",
  "Tianjin" = "Tianjin",
  "Shanghai" = "Shanghai",
  "Chongqing" = "Chongqing",
  "Hebei" = "Hebei",
  "Shanxi" = "Shanxi",
  "Liaoning" = "Liaoning",
  "Jilin" = "Jilin",
  "Heilongjiang" = "Heilongjiang",
  "Jiangsu" = "Jiangsu",
  "Zhejiang" = "Zhejiang",
  "Anhui" = "Anhui",
  "Fujian" = "Fujian",
  "Jiangxi" = "Jiangxi",
  "Shandong" = "Shandong",
  "Henan" = "Henan",
  "Hubei" = "Hubei",
  "Hunan" = "Hunan",
  "Guangdong" = "Guangdong",
  "Hainan" = "Hainan",
  "Sichuan" = "Sichuan",
  "Guizhou" = "Guizhou",
  "Yunnan" = "Yunnan",
  "Shaanxi" = "Shaanxi",
  "Gansu" = "Gansu",
  "Qinghai" = "Qinghai",
  "Taiwan" = "Taiwan",
  "Inner Mongolia" = "Inner Mongolia",
  "Guangxi" = "Guangxi",
  "Tibet" = "Tibet",
  "Ningxia" = "Ningxia",
  "Xinjiang" = "Xinjiang",
  "Hong Kong" = "Hong Kong",
  "Macao" = "Macao",
  "Nei Mongol" = "Inner Mongolia",
  "Ningxia Hui" = "Ningxia",
  "Xinjiang Uygur" = "Xinjiang",
  "Guangxi Zhuang" = "Guangxi",
  "Xizang" = "Tibet",
  "Macau" = "Macao",
  "北京市" = "Beijing",
  "天津市" = "Tianjin",
  "上海市" = "Shanghai",
  "重庆市" = "Chongqing",
  "河北省" = "Hebei",
  "山西省" = "Shanxi",
  "辽宁省" = "Liaoning",
  "吉林省" = "Jilin",
  "黑龙江省" = "Heilongjiang",
  "江苏省" = "Jiangsu",
  "浙江省" = "Zhejiang",
  "安徽省" = "Anhui",
  "福建省" = "Fujian",
  "江西省" = "Jiangxi",
  "山东省" = "Shandong",
  "河南省" = "Henan",
  "湖北省" = "Hubei",
  "湖南省" = "Hunan",
  "广东省" = "Guangdong",
  "海南省" = "Hainan",
  "四川省" = "Sichuan",
  "贵州省" = "Guizhou",
  "云南省" = "Yunnan",
  "陕西省" = "Shaanxi",
  "甘肃省" = "Gansu",
  "青海省" = "Qinghai",
  "台湾省" = "Taiwan",
  "内蒙古自治区" = "Inner Mongolia",
  "广西壮族自治区" = "Guangxi",
  "西藏自治区" = "Tibet",
  "宁夏回族自治区" = "Ningxia",
  "新疆维吾尔自治区" = "Xinjiang",
  "香港特别行政区" = "Hong Kong",
  "澳门特别行政区" = "Macao"
)

read_icon_as_raster <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "png") {
    img <- png::readPNG(path)
  } else if (ext %in% c("jpg", "jpeg")) {
    img <- jpeg::readJPEG(path)
  } else {
    return(NULL)
  }

  if (is.matrix(img)) {
    return(as.raster(img))
  }

  if (length(dim(img)) == 3) {
    ch <- dim(img)[3]
    if (ch == 2) {
      # grayscale + alpha -> RGBA
      g <- img[, , 1]
      a <- img[, , 2]
      img <- array(0, dim = c(dim(img)[1], dim(img)[2], 4))
      img[, , 1] <- g
      img[, , 2] <- g
      img[, , 3] <- g
      img[, , 4] <- a
    } else if (ch == 1) {
      # grayscale -> RGB
      g <- img[, , 1]
      img <- array(0, dim = c(dim(img)[1], dim(img)[2], 3))
      img[, , 1] <- g
      img[, , 2] <- g
      img[, , 3] <- g
    }
  }

  as.raster(img)
}

choose_name_column <- function(x) {
  candidates <- c("province", "Province", "NAME_1", "NAME", "name", "NAME_CHN", "省", "省份")
  candidates[candidates %in% names(x)][1]
}

load_china_province_map <- function(output_root) {
  std_dir <- file.path(output_root, "data_clean", "china_standard_map")
  std_candidates <- c(
    list.files(std_dir, pattern = "\\.(shp|gpkg|geojson)$", full.names = TRUE, ignore.case = TRUE),
    "/Users/dingchenchen/Desktop/china_standard_map/china_province_standard.shp",
    "/Users/dingchenchen/Desktop/china_standard_map/china_province_standard.gpkg",
    "/Users/dingchenchen/Desktop/china_standard_map/china_province_standard.geojson"
  )
  std_candidates <- unique(std_candidates[file.exists(std_candidates)])

  if (length(std_candidates) > 0) {
    sf_obj <- tryCatch(st_read(std_candidates[1], quiet = TRUE), error = function(e) NULL)
    if (!is.null(sf_obj) && inherits(sf_obj, "sf")) {
      name_col <- choose_name_column(sf_obj)
      if (!is.na(name_col) && !is.null(name_col)) {
        sf_obj <- sf_obj %>%
          mutate(province_raw = as.character(.data[[name_col]])) %>%
          mutate(
            province = if_else(
              province_raw %in% names(map_name_to_en),
              unname(map_name_to_en[province_raw]),
              province_raw
            )
          ) %>%
          filter(province %in% unique(unname(province_cn_to_en)) | province == "Macao") %>%
          group_by(province) %>%
          summarise(geometry = st_union(geometry), .groups = "drop")
        return(list(map = sf_obj, source = paste0("Standard map vector: ", std_candidates[1])))
      }
    }
  }

  # Fallback: GADM province boundaries
  # 回退方案：使用 GADM 省级边界
  map_rds <- file.path(output_root, "data_clean", "gadm", "gadm41_CHN_1_pk.rds")
  if (!file.exists(map_rds)) {
    try({
      geodata::gadm(country = "CHN", level = 1, path = file.path(output_root, "data_clean"))
    }, silent = TRUE)
  }
  if (!file.exists(map_rds)) {
    stop("No usable province map found. Place a standard map vector in data_clean/china_standard_map.")
  }

  sf_obj <- readRDS(map_rds) %>%
    st_as_sf() %>%
    transmute(
      province_raw = as.character(NAME_1),
      province = if_else(
        province_raw %in% names(map_name_to_en),
        unname(map_name_to_en[province_raw]),
        province_raw
      ),
      geometry = geometry
    ) %>%
    filter(province %in% unique(unname(province_cn_to_en)) | province == "Macao") %>%
    group_by(province) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")

  list(map = sf_obj, source = "Fallback map: GADM level-1 boundaries")
}

# -------------------------------
# 2) Read and standardize the raw bird new-record data
# 2) 读取并标准化鸟类新纪录原始数据
# -------------------------------
raw <- read_xlsx(input_xlsx, sheet = sheet_records, guess_max = 20000) %>%
  clean_names()

clean <- raw %>%
  transmute(
    record_id = row_number(),
    species = str_squish(scientificname),
    order_raw = str_squish(order_cn),
    order_cn = str_squish(order_la),
    family = str_squish(family_cn),
    genus = str_squish(genus_cn),
    province_cn = str_squish(province_23),
    province_en_raw = str_squish(province_24),
    year = suppressWarnings(as.integer(publicationyear)),
    iucn_raw = str_to_upper(str_squish(iucn_hong_se_ming_lu_deng_ji)),
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude))
  ) %>%
  mutate(
    order = title_case_order(order_raw),
    province = case_when(
      !is.na(province_en_raw) & province_en_raw != "" ~ province_en_raw,
      province_cn %in% names(province_cn_to_en) ~ unname(province_cn_to_en[province_cn]),
      TRUE ~ NA_character_
    ),
    province = str_replace_all(province, " Province$", ""),
    province = str_replace_all(province, " Autonomous Region$", ""),
    province = str_replace_all(province, " Municipality$", ""),
    province = recode(
      province,
      "Juangsu" = "Jiangsu",
      "Tibet Autonomous Region" = "Tibet",
      "Qinghai Province" = "Qinghai",
      "Taiwan Province" = "Taiwan"
    ),
    species = na_if(species, ""),
    family = na_if(family, ""),
    genus = na_if(genus, ""),
    iucn = case_when(
      iucn_raw %in% c("CR", "EN", "VU", "NT", "LC", "DD", "NE") ~ iucn_raw,
      iucn_raw %in% c("LR/LC") ~ "LC",
      iucn_raw %in% c("LR/NT") ~ "NT",
      TRUE ~ "NA"
    )
  ) %>%
  filter(!is.na(species), !is.na(order), !is.na(province), !is.na(year)) %>%
  distinct(species, order, province, year, .keep_all = TRUE) %>%
  arrange(order, species, year, province)

write.csv(
  clean,
  file.path(output_tables, "reference_style_clean_bird_records.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# -------------------------------
# 3) Bird species pool by order from the 2025 checklist
# 3) 基于 2025 名录计算各目物种总数
# -------------------------------
catalog_raw <- read_xlsx(input_xlsx, sheet = sheet_catalog, guess_max = 20000) %>%
  clean_names()

bird_catalog <- bind_rows(
  catalog_raw %>%
    transmute(
      species_lat = wu_zhong_la_ding_ming_1,
      class_lat = gang_la_ding_ming_7,
      order_lat = mu_la_ding_ming_9
    ),
  catalog_raw %>%
    transmute(
      species_lat = wu_zhong_la_ding_ming_20,
      class_lat = gang_la_ding_ming_26,
      order_lat = mu_la_ding_ming_28
    )
) %>%
  filter(class_lat == "Aves", !is.na(species_lat), species_lat != "", !is.na(order_lat), order_lat != "") %>%
  mutate(
    species = str_extract(species_lat, "^[A-Z][A-Za-z-]+\\s+[a-z-]+"),
    order = title_case_order(order_lat)
  ) %>%
  filter(!is.na(species), !is.na(order)) %>%
  distinct(species, order)

order_pool <- bird_catalog %>%
  count(order, name = "n_species_total_order")

# -------------------------------
# 4) Figure 0: Province map using China-wide projection
# 4) 图 0：使用中国全国投影的省级分布图
# -------------------------------
province_counts <- clean %>%
  count(province, name = "n_records")

china_map_info <- load_china_province_map(output_root)
china_map_raw <- china_map_info$map
map_source_note <- china_map_info$source

map_df <- china_map_raw %>%
  left_join(province_counts, by = "province") %>%
  mutate(
    n_records = replace_na(n_records, 0L),
    count_bin = cut(
      n_records,
      breaks = c(-0.1, 5, 10, 15, 20, 25, 30, 35, Inf),
      labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36+"),
      include.lowest = TRUE
    )
  )

map_df_prj <- st_transform(map_df, crs = china_albers_proj4)

label_pts <- map_df_prj %>%
  filter(n_records > 0) %>%
  st_point_on_surface() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

bbox <- st_bbox(map_df_prj)
bw <- as.numeric(bbox["xmax"] - bbox["xmin"])
bh <- as.numeric(bbox["ymax"] - bbox["ymin"])
arrow_x <- as.numeric(bbox["xmax"] - 0.08 * bw)
arrow_y0 <- as.numeric(bbox["ymin"] + 0.78 * bh)
arrow_y1 <- as.numeric(bbox["ymin"] + 0.92 * bh)

p_map <- ggplot(map_df_prj) +
  geom_sf(aes(fill = count_bin), color = "#6C6C6C", linewidth = 0.24) +
  ggrepel::geom_text_repel(
    data = label_pts,
    aes(x = x, y = y, label = province),
    size = 2.7,
    color = "black",
    box.padding = 0.18,
    point.padding = 0.02,
    min.segment.length = 0,
    max.overlaps = Inf,
    segment.color = "#707070",
    seed = 1234
  ) +
  scale_fill_manual(
    values = c(
      "0-5" = "#2B8CBE",
      "6-10" = "#72A7A1",
      "11-15" = "#B9CF7A",
      "16-20" = "#E6ED63",
      "21-25" = "#F5B041",
      "26-30" = "#F27C2B",
      "31-35" = "#E53935",
      "36+" = "#B10026"
    ),
    drop = FALSE,
    name = "Number of new bird records"
  ) +
  annotate("segment", x = arrow_x, xend = arrow_x, y = arrow_y0, yend = arrow_y1, linewidth = 0.7, color = "black") +
  annotate("segment", x = arrow_x, xend = arrow_x - 0.012 * bw, y = arrow_y1, yend = arrow_y1 - 0.045 * bh, linewidth = 0.7, color = "black") +
  annotate("segment", x = arrow_x, xend = arrow_x + 0.012 * bw, y = arrow_y1, yend = arrow_y1 - 0.045 * bh, linewidth = 0.7, color = "black") +
  annotate("text", x = arrow_x, y = arrow_y1 + 0.02 * bh, label = "N", fontface = "bold", size = 5.2) +
  coord_sf(crs = st_crs(china_albers_proj4), expand = FALSE) +
  labs(
    title = "Spatial pattern of new bird records by province",
    subtitle = "China-wide map in Albers equal-area projection",
    caption = paste0(
      "Projection: ", china_albers_proj4, " | Map source: ", map_source_note,
      "\nFor formal publication requiring official national-map compliance code, replace fallback boundaries with certified standard map vectors."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_ref(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.13, 0.22),
    legend.background = element_rect(fill = alpha("white", 0.88), color = NA)
  )

map_df_ll <- st_transform(map_df, 4326)
p_map_inset <- ggplot(map_df_ll) +
  geom_sf(fill = "#F5F5F5", color = "#8A8A8A", linewidth = 0.20) +
  coord_sf(xlim = c(105, 125), ylim = c(2, 26), expand = FALSE) +
  theme_void(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = "#969696", linewidth = 0.35),
    plot.background = element_rect(fill = "white", color = "#969696", linewidth = 0.35)
  )

p_map_final <- cowplot::ggdraw(p_map) +
  cowplot::draw_plot(p_map_inset, x = 0.77, y = 0.08, width = 0.20, height = 0.26)

save_plot_dual(p_map_final, "fig_ref00_china_projection_province_records", width = 11.5, height = 8.2)

# -------------------------------
# 5) Figure A: Sankey / alluvial (Order -> Province -> Year)
# 5) 图 A：桑基/冲积图（目 -> 省份 -> 年份）
# -------------------------------
order_rank <- clean %>%
  distinct(order, species) %>%
  count(order, name = "n_new_species") %>%
  arrange(desc(n_new_species))

province_rank <- clean %>%
  count(province, name = "n_records") %>%
  arrange(desc(n_records))

sankey_df <- clean %>%
  mutate(
    order_grp = order,
    province_grp = province,
    year_f = factor(year, levels = sort(unique(year)))
  ) %>%
  count(order_grp, province_grp, year_f, name = "n_records") %>%
  mutate(
    order_grp = factor(order_grp, levels = order_rank$order),
    province_grp = factor(province_grp, levels = province_rank$province)
  )

write.csv(
  sankey_df,
  file.path(output_tables, "reference_style_sankey_order_province_year.csv"),
  row.names = FALSE
)

all_orders <- levels(sankey_df$order_grp)
extra_orders <- setdiff(all_orders, names(order_palette))
extra_palette <- setNames(grDevices::hcl.colors(length(extra_orders), "Set 3"), extra_orders)
sankey_palette <- c(order_palette[names(order_palette) %in% all_orders], extra_palette)

p_sankey <- ggplot(
  sankey_df,
  aes(axis1 = order_grp, axis2 = province_grp, axis3 = year_f, y = n_records)
) +
  geom_alluvium(aes(fill = order_grp), width = 0.12, alpha = 0.62, knot.pos = 0.38) +
  geom_stratum(width = 0.13, fill = "#E5E5E5", color = "#737373", linewidth = 0.45) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 2.15,
    color = "black"
  ) +
  scale_fill_manual(values = sankey_palette, guide = "none") +
  scale_x_discrete(
    limits = c("Order", "Province", "Year"),
    expand = c(0.03, 0.03)
  ) +
  labs(
    title = "Sankey Diagram: Order -> Province -> Year",
    subtitle = "Bird new records in China (all provinces shown; orders color-coded)",
    x = NULL,
    y = "Number of records"
  ) +
  theme_ref(base_size = 11) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

save_plot_dual(p_sankey, "fig_ref01_sankey_order_province_year", width = 22, height = 12)

# -------------------------------
# 4B) Chord-style network figure (Order <-> Province)
# 4B) 图 B：弦图风格网络图（目 <-> 省份）
# -------------------------------
chord_edges <- clean %>%
  count(order, province, name = "n_records") %>%
  filter(n_records > 0)

order_nodes <- tibble(
  name = order_rank$order,
  group = "Order",
  angle = seq(pi / 2, 3 * pi / 2, length.out = nrow(order_rank) + 1)[- (nrow(order_rank) + 1)]
)

province_nodes <- tibble(
  name = province_rank$province,
  group = "Province",
  angle = seq(-pi / 2, pi / 2, length.out = nrow(province_rank) + 1)[- (nrow(province_rank) + 1)]
)

chord_nodes <- bind_rows(order_nodes, province_nodes) %>%
  mutate(
    x = cos(angle),
    y = sin(angle),
    x_lab = 1.12 * cos(angle),
    y_lab = 1.12 * sin(angle),
    angle_deg = angle * 180 / pi,
    label_angle = if_else(angle_deg < -90 | angle_deg > 90, angle_deg + 180, angle_deg),
    hjust = if_else(angle_deg < -90 | angle_deg > 90, 1, 0),
    node_color = case_when(
      group == "Order" & name %in% names(sankey_palette) ~ sankey_palette[name],
      group == "Province" ~ "#CFCFCF",
      TRUE ~ "#AFAFAF"
    )
  )

chord_edges_plot <- chord_edges %>%
  left_join(chord_nodes %>% select(order = name, x_order = x, y_order = y), by = "order") %>%
  left_join(chord_nodes %>% select(province = name, x_province = x, y_province = y), by = "province") %>%
  mutate(edge_color = sankey_palette[order])

write.csv(
  chord_edges_plot,
  file.path(output_tables, "reference_style_chord_order_province.csv"),
  row.names = FALSE
)

p_chord <- ggplot() +
  geom_curve(
    data = chord_edges_plot,
    aes(
      x = x_order,
      y = y_order,
      xend = x_province,
      yend = y_province,
      color = edge_color,
      linewidth = n_records
    ),
    curvature = 0.22,
    alpha = 0.28,
    lineend = "round"
  ) +
  geom_point(
    data = chord_nodes,
    aes(x = x, y = y, color = node_color),
    size = ifelse(chord_nodes$group == "Order", 2.6, 2.1),
    show.legend = FALSE
  ) +
  geom_text(
    data = chord_nodes,
    aes(x = x_lab, y = y_lab, label = name, angle = label_angle, hjust = hjust),
    size = 2.5,
    color = "black"
  ) +
  scale_color_identity() +
  scale_linewidth(range = c(0.25, 2.4), guide = "none") +
  coord_equal(xlim = c(-1.22, 1.22), ylim = c(-1.18, 1.18), clip = "off") +
  labs(
    title = "Chord-style network: Order and province associations",
    subtitle = "Bird new records in China; links are weighted by record counts"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "black"),
    plot.subtitle = element_text(size = 11, color = "#303030"),
    plot.margin = margin(15, 30, 15, 30)
  )

save_plot_dual(p_chord, "fig_ref01b_chord_order_province", width = 15, height = 15)

# -------------------------------
# 5) Figure B: Circular taxonomic tree styled as a phylogenetic panel
# 5) 图 B：按系统发育图风格绘制的环形分类树
# -------------------------------
# One IUCN value per species:
# choose the most threatened status recorded for the species.
# 每个物种只保留一个 IUCN 等级，这里取最受威胁等级。
iucn_severity <- c("CR" = 1, "EN" = 2, "VU" = 3, "NT" = 4, "DD" = 5, "LC" = 6, "NE" = 7, "NA" = 8)

species_meta <- clean %>%
  mutate(
    iucn_rank = unname(iucn_severity[iucn]),
    family = replace_na(family, "Family_unknown"),
    genus = replace_na(genus, "Genus_unknown")
  ) %>%
  group_by(species, order, family, genus) %>%
  slice_min(iucn_rank, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  distinct(species, order, family, genus, iucn)

order_stats <- species_meta %>%
  count(order, name = "n_new_species") %>%
  left_join(order_pool, by = "order") %>%
  mutate(prop_new_species = n_new_species / n_species_total_order) %>%
  arrange(desc(n_new_species))

order_representative_species <- clean %>%
  count(order, species, name = "n_records") %>%
  group_by(order) %>%
  slice_max(n_records, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(order, representative_species = species)

# Priority icon source: online silhouette database (PhyloPic), with local PPT fallback.
# 图标优先来源：在线剪影库（PhyloPic），缺失时回退到本地 PPT 图片。
icon_manifest_path <- file.path(output_root, "assets", "phylopic_icons", "icon_manifest.csv")
if (file.exists(icon_manifest_path)) {
  icon_manifest <- read.csv(icon_manifest_path, stringsAsFactors = FALSE) %>%
    transmute(
      order = order,
      icon_path = icon_png,
      icon_contributor = contributor,
      icon_license = license_url,
      icon_source = source_page
    )
} else {
  icon_manifest <- tibble(
    order = character(),
    icon_path = character(),
    icon_contributor = character(),
    icon_license = character(),
    icon_source = character()
  )
}

icon_candidates_fallback <- c(
  "/Users/dingchenchen/Documents/New project/ppt_assets_new_records/ppt/media/image14.png",
  "/Users/dingchenchen/Documents/New project/ppt_assets_new_records/ppt/media/image15.png",
  "/Users/dingchenchen/Documents/New project/ppt_assets_new_records/ppt/media/image16.png",
  "/Users/dingchenchen/Documents/New project/ppt_assets_new_records/ppt/media/image23.png",
  "/Users/dingchenchen/Documents/New project/ppt_assets_new_records/ppt/media/image24.jpeg"
)
icon_candidates_fallback <- icon_candidates_fallback[file.exists(icon_candidates_fallback)]

write.csv(
  species_meta,
  file.path(output_tables, "reference_style_phylogeny_tip_metadata.csv"),
  row.names = FALSE
)

tax_tree_input <- species_meta %>%
  transmute(
    order = factor(order),
    family = factor(family),
    genus = factor(genus),
    species = factor(species)
  )

tax_tree <- ape::as.phylo(~ order / family / genus / species, data = tax_tree_input)
tax_tree$tip.label <- as.character(tax_tree$tip.label)

tip_order <- species_meta$order[match(tax_tree$tip.label, species_meta$species)]
tip_iucn <- species_meta$iucn[match(tax_tree$tip.label, species_meta$species)]

descendant_tips <- local({
  n_tip <- ape::Ntip(tax_tree)
  child_map <- split(tax_tree$edge[, 2], tax_tree$edge[, 1])
  cache <- vector("list", n_tip + tax_tree$Nnode)

  get_tips <- function(node) {
    node <- as.integer(node)
    if (!is.null(cache[[node]])) {
      return(cache[[node]])
    }
    if (node <= n_tip) {
      cache[[node]] <<- node
    } else {
      children <- child_map[[as.character(node)]]
      cache[[node]] <<- unlist(lapply(children, get_tips))
    }
    cache[[node]]
  }

  lapply(tax_tree$edge[, 2], get_tips)
})

edge_cols <- vapply(seq_len(nrow(tax_tree$edge)), function(i) {
  ords <- unique(tip_order[descendant_tips[[i]]])
  if (length(ords) == 1 && ords %in% names(order_palette)) {
    order_palette[[ords]]
  } else if (length(ords) == 1) {
    "#9E9E9E"
  } else {
    "#A8A8A8"
  }
}, character(1))

draw_phylo_panel <- function(file, device = c("png", "pdf")) {
  device <- match.arg(device)
  if (device == "png") {
    png(file, width = 2500, height = 2100, res = 300, bg = "white")
  } else {
    pdf(file, width = 11.2, height = 9.0, useDingbats = FALSE)
  }

  par(mar = c(0.8, 0.8, 2.2, 4.8), xpd = NA, bg = "white")

  base_layout <- plot.phylo(
    tax_tree,
    type = "fan",
    use.edge.length = FALSE,
    show.tip.label = FALSE,
    no.margin = TRUE,
    edge.color = edge_cols,
    edge.width = 1.0,
    cex = 0.18,
    plot = FALSE
  )

  plot.phylo(
    tax_tree,
    type = "fan",
    use.edge.length = FALSE,
    show.tip.label = FALSE,
    no.margin = TRUE,
    edge.color = edge_cols,
    edge.width = 1.0,
    cex = 0.18,
    x.lim = base_layout$x.lim * 1.16,
    y.lim = base_layout$y.lim * 1.16
  )

  lp <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  n_tip <- lp$Ntip
  xx <- lp$xx[seq_len(n_tip)]
  yy <- lp$yy[seq_len(n_tip)]
  rr <- sqrt(xx^2 + yy^2)
  theta <- atan2(yy, xx)
  max_r <- max(rr)

  # Order ring / 内侧分类色环
  x_order_ring <- (max_r + 0.05) * cos(theta)
  y_order_ring <- (max_r + 0.05) * sin(theta)
  points(x_order_ring, y_order_ring, pch = 15, cex = 0.62, col = order_palette[tip_order])

  # IUCN ring / 外侧 IUCN 色环
  x_iucn_ring <- (max_r + 0.12) * cos(theta)
  y_iucn_ring <- (max_r + 0.12) * sin(theta)
  points(x_iucn_ring, y_iucn_ring, pch = 15, cex = 0.62, col = iucn_palette[tip_iucn])

  # Order labels and percentage bubbles / 目标签与比例气泡
  tip_layout <- tibble(
    species = tax_tree$tip.label,
    order = tip_order,
    theta = theta
  )

  order_layout <- tip_layout %>%
    group_by(order) %>%
    summarise(
      mean_x = mean(cos(theta)),
      mean_y = mean(sin(theta)),
      .groups = "drop"
    ) %>%
    mutate(angle = atan2(mean_y, mean_x)) %>%
    left_join(order_stats, by = "order") %>%
    left_join(order_representative_species, by = "order") %>%
    arrange(desc(n_new_species))

  label_orders <- order_layout %>%
    slice_head(n = 8)

  label_orders <- label_orders %>%
    left_join(icon_manifest, by = "order")

  if (length(icon_candidates_fallback) > 0) {
    miss_idx <- which(is.na(label_orders$icon_path) | label_orders$icon_path == "" | !file.exists(label_orders$icon_path))
    if (length(miss_idx) > 0) {
      label_orders$icon_path[miss_idx] <- icon_candidates_fallback[((seq_along(miss_idx) - 1) %% length(icon_candidates_fallback)) + 1]
      label_orders$icon_contributor[miss_idx] <- "Local PPT fallback"
      label_orders$icon_license[miss_idx] <- NA_character_
      label_orders$icon_source[miss_idx] <- "User-provided local PPT media"
    }
  }

  label_orders <- label_orders %>%
    mutate(icon_path = if_else(file.exists(icon_path), icon_path, NA_character_))

  if (any(!is.na(label_orders$icon_path))) {
    icon_rasters <- lapply(unique(na.omit(label_orders$icon_path)), read_icon_as_raster)
    names(icon_rasters) <- unique(na.omit(label_orders$icon_path))
  } else {
    icon_rasters <- list()
  }

  for (i in seq_len(nrow(label_orders))) {
    ang <- label_orders$angle[i]
    ord <- label_orders$order[i]
    prop_lab <- percent(label_orders$prop_new_species[i], accuracy = 0.1)
    rad_lab <- ifelse(sin(ang) > 0.82, max_r + 0.21, max_r + 0.27)
    rad_icon <- ifelse(sin(ang) > 0.82, max_r + 0.18, max_r + 0.36)
    x_lab <- rad_lab * cos(ang)
    y_lab <- rad_lab * sin(ang)
    x_bub <- (max_r + 0.16) * cos(ang)
    y_bub <- (max_r + 0.16) * sin(ang)
    x_icon <- rad_icon * cos(ang)
    y_icon <- rad_icon * sin(ang)

    symbols(
      x_bub,
      y_bub,
      circles = 0.06,
      inches = FALSE,
      add = TRUE,
      bg = order_palette[ord],
      fg = NA
    )
    text(x_bub, y_bub, labels = prop_lab, cex = 0.58, col = "black")
    text(x_lab, y_lab, labels = ord, cex = 0.70, col = "black")
    text(x_lab, y_lab - 0.045, labels = label_orders$representative_species[i], cex = 0.46, col = "#303030")

    if (!is.na(label_orders$icon_path[i]) && label_orders$icon_path[i] %in% names(icon_rasters)) {
      this_icon <- icon_rasters[[label_orders$icon_path[i]]]
      if (!is.null(this_icon)) {
        icon_half_w <- ifelse(sin(ang) > 0.82, 0.038, 0.055)
        icon_half_h <- ifelse(sin(ang) > 0.82, 0.028, 0.040)
        y_icon_adj <- ifelse(sin(ang) > 0.82, y_icon - 0.045, y_icon)
        rasterImage(
          this_icon,
          xleft = x_icon - icon_half_w,
          ybottom = y_icon_adj - icon_half_h,
          xright = x_icon + icon_half_w,
          ytop = y_icon_adj + icon_half_h
        )
      }
    }

    segments(
      x0 = (max_r + 0.20) * cos(ang),
      y0 = (max_r + 0.20) * sin(ang),
      x1 = rad_lab * cos(ang),
      y1 = rad_lab * sin(ang),
      col = "#303030",
      lwd = 0.8
    )
  }

  mtext(
    "Circular taxonomic tree of newly recorded bird species",
    side = 3,
    line = 0.2,
    cex = 1.05,
    font = 2
  )

  legend(
    "topright",
    inset = c(0.02, 0.02),
    legend = c("NT", "VU", "EN", "CR"),
    pch = 15,
    pt.cex = 1.3,
    col = c(iucn_palette["NT"], iucn_palette["VU"], iucn_palette["EN"], iucn_palette["CR"]),
    bty = "n",
    title = "IUCN"
  )

  dev.off()
}

draw_phylo_panel(file.path(output_figures, "fig_ref02_phylogeny_style_bird_new_records.png"), device = "png")
draw_phylo_panel(file.path(output_figures, "fig_ref02_phylogeny_style_bird_new_records.pdf"), device = "pdf")

# -------------------------------
# 6) Figure C: Directional radar / polar plot
# 6) 图 C：新纪录方位雷达图 / 极坐标图
# -------------------------------
# Join AVONET centroid coordinates from the existing processed file.
# 利用已整理好的 AVONET 重心经纬度，计算新纪录相对已知分布中心的方位。
species_pool_traits <- read.csv(
  file.path(output_root, "data_clean", "bird_species_pool_with_traits.csv"),
  stringsAsFactors = FALSE
)

direction_df <- clean %>%
  select(species, order, longitude, latitude) %>%
  left_join(
    species_pool_traits %>% select(species, centroid_latitude, centroid_longitude),
    by = "species"
  ) %>%
  filter(!is.na(longitude), !is.na(latitude), !is.na(centroid_latitude), !is.na(centroid_longitude)) %>%
  mutate(
    dx = longitude - centroid_longitude,
    dy = latitude - centroid_latitude,
    angle = (atan2(dx, dy) * 180 / pi + 360) %% 360,
    direction = case_when(
      angle >= 337.5 | angle < 22.5 ~ "North",
      angle < 67.5 ~ "Northeast",
      angle < 112.5 ~ "East",
      angle < 157.5 ~ "Southeast",
      angle < 202.5 ~ "South",
      angle < 247.5 ~ "Southwest",
      angle < 292.5 ~ "West",
      TRUE ~ "Northwest"
    )
  ) %>%
  distinct(species, order, direction)

order_direction_rank <- direction_df %>%
  count(order, name = "n_species") %>%
  arrange(desc(n_species))

direction_orders <- order_direction_rank %>%
  slice_head(n = 6) %>%
  pull(order)

radar_df <- direction_df %>%
  filter(order %in% direction_orders) %>%
  count(order, direction, name = "n_species") %>%
  complete(order, direction = direction_levels, fill = list(n_species = 0)) %>%
  left_join(order_direction_rank, by = "order", suffix = c("", "_total")) %>%
  mutate(
    direction = factor(direction, levels = direction_levels),
    order = factor(order, levels = direction_orders),
    prop_order = n_species / n_species_total
  )

write.csv(
  radar_df,
  file.path(output_tables, "reference_style_direction_radar_data.csv"),
  row.names = FALSE
)

focal_order <- as.character(order_direction_rank$order[1])
focal_total <- as.numeric(order_direction_rank$n_species[1])
focal_max <- radar_df %>%
  filter(order == focal_order) %>%
  summarise(max_n = max(n_species)) %>%
  pull(max_n)
focal_half <- ceiling(focal_max / 2)

radar_palette <- order_palette[direction_orders]

p_direction <- ggplot(radar_df, aes(x = direction, y = n_species, group = order, color = order, fill = order)) +
  geom_polygon(alpha = 0.22, linewidth = 0.7) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_text(
    data = tibble(
      direction = factor("North", levels = direction_levels),
      y = c(focal_half, focal_max),
      label = c(
        paste0(focal_half, "(", percent(focal_half / focal_total, accuracy = 1), ")"),
        paste0(focal_max, "(", percent(focal_max / focal_total, accuracy = 1), ")")
      )
    ),
    aes(x = direction, y = y, label = label),
    inherit.aes = FALSE,
    color = "#404040",
    size = 3.2,
    nudge_x = 0
  ) +
  scale_color_manual(values = radar_palette) +
  scale_fill_manual(values = radar_palette) +
  scale_y_continuous(
    limits = c(0, focal_max),
    breaks = c(focal_half, focal_max),
    labels = NULL
  ) +
  coord_polar(start = -pi / 4) +
  annotate("segment", x = 1:8, xend = 1:8, y = 0, yend = focal_max, color = "#444444", linewidth = 0.35) +
  labs(
    title = "Directions of new bird records relative to known range centroids",
    subtitle = paste0("Top six orders by number of directional bird new-record species; focal radial labels shown for ", focal_order),
    x = NULL,
    y = NULL,
    color = NULL,
    fill = NULL,
    caption = "Directions were assigned to eight 45-degree sectors using AVONET centroid coordinates."
  ) +
  theme_ref(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "#444444", linewidth = 0.55, linetype = "22"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "#303030"),
    legend.position = "right"
  )

save_plot_dual(p_direction, "fig_ref03_direction_radar_bird_new_records", width = 12.8, height = 7.8)

# -------------------------------
# 7) Export a compact metadata workbook
# 7) 导出图组对应的数据清单
# -------------------------------
writexl::write_xlsx(
  list(
    clean_records = clean,
    map_province_data = st_drop_geometry(map_df),
    sankey_data = sankey_df,
    chord_data = chord_edges_plot,
    phylogeny_tip_metadata = species_meta,
    direction_radar_data = radar_df,
    order_species_pool = order_pool,
    icon_attribution = icon_manifest
  ),
  file.path(output_tables, "reference_style_figure_data_bundle.xlsx")
)

writeLines(
  c(
    "# Reference-style bird figure workflow",
    "",
    "## Files",
    paste0("- China projection map data: ", file.path(output_tables, "reference_style_figure_data_bundle.xlsx [sheet: map_province_data]")),
    paste0("- Sankey data: ", file.path(output_tables, "reference_style_sankey_order_province_year.csv")),
    paste0("- Chord data: ", file.path(output_tables, "reference_style_chord_order_province.csv")),
    paste0("- Phylogeny metadata: ", file.path(output_tables, "reference_style_phylogeny_tip_metadata.csv")),
    paste0("- Direction data: ", file.path(output_tables, "reference_style_direction_radar_data.csv")),
    paste0("- Icon attribution: ", icon_manifest_path),
    "",
    "## Methodological notes",
    "- Province map uses an Albers equal-area projection for China-wide cartography, with map source logged below.",
    paste0("- Map source in this run: ", map_source_note),
    "- Sankey figure shows all provinces and all years; high-density labels are retained to match the requested comprehensive display.",
    "- Chord figure summarizes weighted order-province links using the same cleaned event-level records.",
    "- The circular tree is taxonomy-derived (order-family-genus-species), not a downloaded backbone phylogeny.",
    "- Representative bird icon images are assigned by order from local PhyloPic downloads when available.",
    "- The direction figure uses AVONET centroid coordinates already present in the local processed dataset."
  ),
  con = file.path(output_root, "README_reference_style_figures.md")
)

cat("Reference-style bird figure set finished.\n")
cat("Figures saved in:", output_figures, "\n")
cat("Tables saved in:", output_tables, "\n")
