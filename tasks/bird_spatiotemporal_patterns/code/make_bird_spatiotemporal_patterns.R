#!/usr/bin/env Rscript

# ============================================================
# Bird new-record spatiotemporal patterns task bundle
# 鸟类新纪录时空格局任务脚本
# ============================================================
#
# Analysis steps / 分析步骤
# 1. Read the cleaned bird new-record table and the user-provided province-level
#    shapefile base map, provincial boundary line layer, and ten-dash-line layer.
#    读取清洗后的鸟类新纪录表，以及用户提供的省级底图、省界线和十段线图层。
# 2. Transform all spatial layers into a China-suitable projected coordinate
#    system based on CGCS2000 Albers Equal Area.
#    将所有空间图层统一转换到适合中国制图的 CGCS2000 Albers Equal Area 投影。
# 3. Standardize province names, compute provincial counts, area-standardized
#    density, annual counts, and order-level composition tables.
#    标准化省份名称，并计算省级新纪录数量、面积标准化密度、年度记录数及按目组成表。
# 4. Rebuild publication-style spatiotemporal figures, including two choropleth
#    maps, one projected point-distribution map, and stacked bar-line charts.
#    重建投稿风格的时空格局图，包括两张分级设色地图、一张投影后的记录点分布图，以及堆叠柱线图。
# 5. Export all figures in PNG, PDF, and editable vector PPTX formats.
#    将所有图件导出为 PNG、PDF 和可编辑矢量 PPTX 格式。
# 6. Save plot-ready data tables and write bilingual task documentation.
#    保存作图数据表，并输出中英文任务说明与结果摘要。
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sf)
  library(scales)
  library(stringr)
  library(forcats)
  library(tibble)
  library(officer)
  library(rvg)
  library(units)
})

# -------------------------------
# Step 0. Define task paths
# 第 0 步：定义任务路径
# -------------------------------
master_output_root <- "/Users/dingchenchen/Documents/New records/bird_new_records_R_output"
task_dir <- file.path(master_output_root, "tasks", "bird_spatiotemporal_patterns")
figures_dir <- file.path(task_dir, "figures")
code_dir <- file.path(task_dir, "code")
data_dir <- file.path(task_dir, "data")
shape_dir <- file.path(data_dir, "shapefile_base")
results_dir <- file.path(task_dir, "results")

for (dir_path in c(figures_dir, code_dir, data_dir, results_dir, shape_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

clean_path <- file.path(master_output_root, "data_clean", "bird_new_records_clean.csv")
province_shp_path <- file.path(shape_dir, "省.shp")
province_line_path <- file.path(shape_dir, "省_境界线.shp")
ten_dash_path <- file.path(shape_dir, "十段线.shp")

if (!file.exists(clean_path)) stop("Missing clean bird dataset: ", clean_path)
if (!file.exists(province_shp_path)) stop("Missing province shapefile: ", province_shp_path)
if (!file.exists(province_line_path)) stop("Missing province boundary shapefile: ", province_line_path)
if (!file.exists(ten_dash_path)) stop("Missing ten-dash-line shapefile: ", ten_dash_path)

# -------------------------------
# Step 1. Shared constants and helper functions
# 第 1 步：常量与辅助函数
# -------------------------------
china_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +ellps=GRS80 +units=m +no_defs"

province_name_lookup <- tibble(
  province_cn = c(
    "北京市", "天津市", "河北省", "山西省", "内蒙古自治区", "辽宁省", "吉林省", "黑龙江省",
    "上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "河南省",
    "湖北省", "湖南省", "广东省", "广西壮族自治区", "海南省", "重庆市", "四川省", "贵州省",
    "云南省", "西藏自治区", "陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区",
    "香港特别行政区", "澳门特别行政区", "台湾省"
  ),
  province_std = c(
    "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin", "Heilongjiang",
    "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan",
    "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou",
    "Yunnan", "Xizang", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang",
    "Hong Kong", "Macau", "Taiwan"
  ),
  province_label_map = c(
    "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin", "Heilongjiang",
    "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan",
    "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou",
    "Yunnan", "Xizang", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang",
    "Hong Kong", "Macau", "Taiwan"
  ),
  province_label_bar = c(
    "Beijing", "Tianjin", "Hebei", "Shanxi", "Neimenggu", "Liaoning", "Jilin", "Heilongjiang",
    "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan",
    "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou",
    "Yunnan", "Xizang", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang",
    "Hong Kong", "Macau", "Taiwan"
  )
)

province_recode <- c("Tibet" = "Xizang")

count_fill_values <- c(
  "0 - 10" = "#3494C7",
  "11 - 20" = "#84B3B1",
  "21 - 30" = "#C4D88B",
  "31 - 40" = "#FFF95C",
  "41 - 50" = "#FDB84A",
  "51 - 60" = "#FF6D2D",
  "61 - 71" = "#F11313"
)

density_fill_values <- c(
  "0-5" = "#3494C7",
  "6-20" = "#9BC3B1",
  "21-40" = "#DBE97D",
  "41-100" = "#FFD55C",
  "101-200" = "#FF7A28",
  ">200" = "#F31616"
)

bar_palette_base <- c(
  "Accipitriformes" = "#FFF200",
  "Anseriformes" = "#173F74",
  "Caprimulgiformes" = "#8D7BD6",
  "Charadriiformes" = "#12B34B",
  "Ciconiiformes" = "#B7C8E6",
  "Columbiformes" = "#628F45",
  "Coraciiformes" = "#F6C3C5",
  "Cuculiformes" = "#66C3F0",
  "Falconiformes" = "#3C7D74",
  "Galliformes" = "#24DD00",
  "Gaviiformes" = "#6F6A42",
  "Gruiformes" = "#27D7D7",
  "Otidiformes" = "#C79B00",
  "Passeriformes" = "#6D35A5",
  "Pelecaniformes" = "#FF6B00",
  "Phoenicopteriformes" = "#BDBDBD",
  "Piciformes" = "#EFE8A0",
  "Podicipediformes" = "#4A4A4A",
  "Procellariiformes" = "#EADFA5",
  "Pterocliformes" = "#FF1D1D",
  "Strigiformes" = "#176AE6",
  "Suliformes" = "#111111",
  "Trogoniformes" = "#A33D7B",
  "Others" = "#8D8D8D"
)

point_map_palette <- c(
  "Passeriformes" = "#54FF19",
  "Charadriiformes" = "#FFB31A",
  "Anseriformes" = "#38C8FF",
  "Accipitriformes" = "#FF1C1C",
  "Pelecaniformes" = "#C925FF",
  "Others" = "#111111"
)

point_map_shapes <- c(
  "Passeriformes" = 16,
  "Charadriiformes" = 15,
  "Anseriformes" = 17,
  "Accipitriformes" = 18,
  "Pelecaniformes" = 8,
  "Others" = 16
)

build_bbox_from_longlat <- function(xmin, xmax, ymin, ymax, target_crs) {
  bbox_ll <- st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = st_crs(4326)))
  st_bbox(st_transform(bbox_ll, target_crs))
}

slide_dims <- function() {
  ppt <- read_pptx()
  slide_size(ppt)
}

save_chart_bundle <- function(plot_obj, out_dir, filename_no_ext, width, height, dpi = 450) {
  png_path <- file.path(out_dir, paste0(filename_no_ext, ".png"))
  pdf_path <- file.path(out_dir, paste0(filename_no_ext, ".pdf"))
  pptx_path <- file.path(out_dir, paste0(filename_no_ext, ".pptx"))

  ggsave(png_path, plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(pdf_path, plot_obj, width = width, height = height, device = cairo_pdf, bg = "white")

  ppt <- read_pptx()
  dims <- slide_size(ppt)
  ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- ph_with(
    ppt,
    dml(ggobj = plot_obj),
    location = ph_location(left = 0, top = 0, width = dims$width, height = dims$height)
  )
  print(ppt, target = pptx_path)
}

save_map_bundle <- function(main_plot, inset_plot, out_dir, filename_no_ext, width, height, dpi = 450,
                            inset_left = 0.80, inset_bottom = 0.02, inset_right = 0.995, inset_top = 0.31,
                            plot_xlim, plot_ylim) {
  png_path <- file.path(out_dir, paste0(filename_no_ext, ".png"))
  pdf_path <- file.path(out_dir, paste0(filename_no_ext, ".pdf"))
  pptx_path <- file.path(out_dir, paste0(filename_no_ext, ".pptx"))
  emf_main_path <- file.path(out_dir, paste0(filename_no_ext, "_main.emf"))
  emf_inset_path <- file.path(out_dir, paste0(filename_no_ext, "_inset.emf"))

  inset_xmin <- plot_xlim[1] + diff(plot_xlim) * inset_left
  inset_xmax <- plot_xlim[1] + diff(plot_xlim) * inset_right
  inset_ymin <- plot_ylim[1] + diff(plot_ylim) * inset_bottom
  inset_ymax <- plot_ylim[1] + diff(plot_ylim) * inset_top

  combined_plot <- main_plot +
    annotation_custom(
      grob = ggplotGrob(inset_plot),
      xmin = inset_xmin, xmax = inset_xmax, ymin = inset_ymin, ymax = inset_ymax
    )

  ggsave(png_path, combined_plot, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(pdf_path, combined_plot, width = width, height = height, device = cairo_pdf, bg = "white")

  devEMF::emf(file = emf_main_path, width = width, height = height, bg = "white")
  print(main_plot)
  dev.off()

  devEMF::emf(file = emf_inset_path, width = width * (inset_right - inset_left), height = height * (inset_top - inset_bottom), bg = "white")
  print(inset_plot)
  dev.off()

  ppt <- read_pptx()
  dims <- slide_size(ppt)
  slide_w <- dims$width
  slide_h <- dims$height
  ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- ph_with(
    ppt,
    external_img(emf_main_path, width = slide_w, height = slide_h),
    location = ph_location(left = 0, top = 0, width = slide_w, height = slide_h)
  )
  ppt <- ph_with(
    ppt,
    external_img(emf_inset_path, width = slide_w * (inset_right - inset_left), height = slide_h * (inset_top - inset_bottom)),
    location = ph_location(
      left = slide_w * inset_left,
      top = slide_h * (1 - inset_top),
      width = slide_w * (inset_right - inset_left),
      height = slide_h * (inset_top - inset_bottom)
    )
  )
  print(ppt, target = pptx_path)
}

add_north_arrow_projected <- function(plot_obj, xlim, ylim, scale_x = 0.030, scale_y = 0.068) {
  xr <- diff(xlim)
  yr <- diff(ylim)
  x <- xlim[2] - xr * 0.06
  y <- ylim[2] - yr * 0.14
  dx <- xr * scale_x
  dy <- yr * scale_y

  plot_obj +
    annotate("text", x = x, y = y + dy * 1.25, label = "N", size = 11, family = "sans") +
    annotate(
      "polygon",
      x = c(x, x - dx, x, x + dx),
      y = c(y + dy * 0.85, y - dy * 1.35, y - dy * 0.10, y - dy * 1.35),
      fill = "black", color = "black", linewidth = 0.35
    ) +
    annotate(
      "polygon",
      x = c(x, x - dx * 0.42, x, x + dx * 0.42),
      y = c(y + dy * 0.45, y - dy * 0.82, y - dy * 0.02, y - dy * 0.82),
      fill = "white", color = "white"
    )
}

map_theme <- function() {
  theme_void(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75),
      legend.position = c(0.065, 0.135),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
      legend.title = element_text(size = 14.5, face = "bold"),
      legend.text = element_text(size = 12.4),
      legend.key.width = unit(1.2, "cm"),
      legend.key.height = unit(0.78, "cm"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

point_map_theme <- function() {
  theme_void(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75),
      legend.position = c(0.055, 0.115),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = alpha("white", 0.92), color = NA),
      legend.title = element_text(size = 15.5, face = "bold"),
      legend.text = element_text(size = 15.0),
      legend.key.height = unit(0.72, "cm"),
      legend.key.width = unit(0.95, "cm"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

bar_theme <- function() {
  theme_classic(base_family = "sans", base_size = 13) +
    theme(
      panel.grid.major.y = element_line(color = "#D6D6D6", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.6),
      axis.text.x = element_text(size = 11.4, color = "black"),
      axis.text.y = element_text(size = 11.6, color = "black"),
      axis.title.x = element_text(size = 13.8, margin = margin(t = 10)),
      axis.title.y = element_text(size = 13.8),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 11.6),
      legend.key.width = unit(0.85, "cm"),
      legend.key.height = unit(0.32, "cm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 8, 8, 8)
    )
}

make_barline_plot <- function(bar_df, line_df, palette_values, x_var, x_lab, pct_cap = NULL, legend_rows = 4) {
  total_max <- max(line_df$total_records, na.rm = TRUE)
  if (is.null(pct_cap)) {
    pct_cap <- ceiling(max(line_df$percentage, na.rm = TRUE) * 100) / 100
  }
  scale_factor <- total_max / pct_cap

  ggplot(bar_df, aes(x = .data[[x_var]], y = n_records, fill = order_bar)) +
    geom_col(width = 0.72) +
    geom_line(
      data = line_df,
      aes(x = .data[[x_var]], y = percentage * scale_factor, group = 1),
      linewidth = 0.72,
      color = "black",
      linetype = "22",
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    geom_point(
      data = line_df,
      aes(x = .data[[x_var]], y = percentage * scale_factor),
      size = 2.2,
      color = "black",
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = palette_values,
      labels = toupper(names(palette_values)),
      breaks = names(palette_values)
    ) +
    scale_y_continuous(
      name = "Number of new records",
      limits = c(0, pct_cap * scale_factor),
      expand = c(0, 0),
      sec.axis = sec_axis(~ . / scale_factor, name = "Percentage", labels = label_percent(accuracy = 0.1))
    ) +
    labs(x = x_lab) +
    guides(fill = guide_legend(nrow = legend_rows, byrow = TRUE)) +
    bar_theme()
}

prepare_bar_data <- function(records_df, x_col, x_levels, top_n) {
  order_rank <- records_df %>% count(order, sort = TRUE)
  top_orders <- order_rank %>% slice_head(n = top_n) %>% pull(order)
  order_levels <- c(top_orders, "Others")

  palette_values <- bar_palette_base[intersect(names(bar_palette_base), order_levels)]
  if (!"Others" %in% names(palette_values)) {
    palette_values <- c(palette_values, Others = "#8D8D8D")
  }
  palette_values <- palette_values[order_levels]

  bar_df <- records_df %>%
    mutate(order_bar = if_else(order %in% top_orders, order, "Others")) %>%
    count(.data[[x_col]], order_bar, name = "n_records") %>%
    rename(x_value = .data[[x_col]]) %>%
    complete(x_value = x_levels, order_bar = order_levels, fill = list(n_records = 0)) %>%
    mutate(order_bar = factor(order_bar, levels = order_levels))

  total_df <- bar_df %>%
    group_by(x_value) %>%
    summarise(total_records = sum(n_records), .groups = "drop") %>%
    mutate(percentage = total_records / sum(total_records))

  list(bar_df = bar_df, line_df = total_df, palette = palette_values, top_orders = top_orders)
}

# -------------------------------
# Step 2. Read cleaned bird data and shapefiles
# 第 2 步：读取清洗后数据与 shp 图层
# -------------------------------
clean <- read_csv(clean_path, show_col_types = FALSE) %>%
  transmute(
    record_id = record_id,
    species = species,
    order = order,
    province_raw = province,
    year = as.integer(year),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  ) %>%
  mutate(province_std = recode(province_raw, !!!province_recode))

province_sf_ll <- st_read(province_shp_path, quiet = TRUE, options = "ENCODING=UTF-8") %>%
  st_make_valid() %>%
  left_join(province_name_lookup, by = c("省名" = "province_cn"))

province_line_sf_ll <- st_read(province_line_path, quiet = TRUE, options = "ENCODING=UTF-8") %>% st_make_valid()
ten_dash_sf_ll <- st_read(ten_dash_path, quiet = TRUE, options = "ENCODING=UTF-8") %>% st_make_valid()

if (any(is.na(province_sf_ll$province_std))) {
  missing_names <- province_sf_ll %>% st_drop_geometry() %>% filter(is.na(province_std)) %>% pull(省名)
  stop("Missing province mapping for: ", paste(missing_names, collapse = ", "))
}

province_sf <- st_transform(province_sf_ll, china_crs)
province_line_sf <- st_transform(province_line_sf_ll, china_crs)
ten_dash_sf <- st_transform(ten_dash_sf_ll, china_crs)

main_bbox <- st_bbox(province_sf)
main_xlim <- c(main_bbox["xmin"] - 240000, main_bbox["xmax"] + 1380000)
main_ylim <- c(main_bbox["ymin"] - 120000, main_bbox["ymax"] + 230000)
inset_bbox <- build_bbox_from_longlat(104, 125, 2, 26, china_crs)

# -------------------------------
# Step 3. Compute summaries
# 第 3 步：计算统计量
# -------------------------------
province_area <- province_sf %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  select(province_std, area_km2)

province_counts <- clean %>% count(province_std, name = "n_records")

province_summary <- province_name_lookup %>%
  left_join(province_counts, by = "province_std") %>%
  left_join(province_area, by = "province_std") %>%
  mutate(
    n_records = replace_na(n_records, 0L),
    density_100k = if_else(!is.na(area_km2) & area_km2 > 0, n_records / area_km2 * 100000, NA_real_),
    count_class = cut(
      n_records,
      breaks = c(-0.001, 10, 20, 30, 40, 50, 60, 71),
      labels = names(count_fill_values),
      include.lowest = TRUE,
      right = TRUE
    ),
    density_class = cut(
      density_100k,
      breaks = c(-0.001, 5, 20, 40, 100, 200, Inf),
      labels = names(density_fill_values),
      include.lowest = TRUE,
      right = TRUE
    )
  )

province_map_sf <- province_sf %>%
  left_join(select(province_summary, -area_km2), by = c("province_std", "province_label_map", "province_label_bar")) %>%
  left_join(select(province_area, province_std, area_km2), by = "province_std")

label_points <- st_point_on_surface(province_map_sf)
label_coords <- st_coordinates(label_points)
label_df <- province_map_sf %>%
  st_drop_geometry() %>%
  mutate(x = label_coords[, 1], y = label_coords[, 2]) %>%
  select(province_std, province_label_map, x, y)

manual_label_override_ll <- tribble(
  ~province_std,       ~lon,   ~lat,
  "Beijing",          116.08, 41.08,
  "Tianjin",          117.72, 40.10,
  "Hebei",            115.35, 39.15,
  "Liaoning",         122.15, 41.50,
  "Jilin",            125.05, 43.45,
  "Heilongjiang",     127.60, 47.30,
  "Shandong",         118.85, 36.20,
  "Jiangsu",          119.80, 33.05,
  "Shanghai",         122.10, 31.05,
  "Anhui",            117.10, 31.40,
  "Zhejiang",         120.65, 28.70,
  "Fujian",           118.75, 25.00,
  "Jiangxi",          116.25, 27.60,
  "Guangdong",        112.70, 22.85,
  "Hong Kong",        114.50, 22.20,
  "Hainan",           109.35, 18.90,
  "Chongqing",        106.95, 29.92,
  "Inner Mongolia",   111.10, 44.05,
  "Ningxia",          106.10, 37.45,
  "Shanxi",           112.00, 37.62,
  "Shaanxi",          108.55, 35.08,
  "Henan",            113.35, 34.10,
  "Hubei",            112.35, 30.50,
  "Hunan",            111.55, 27.72,
  "Xizang",            87.95, 31.00,
  "Taiwan",           121.15, 23.45,
  "Macau",            113.58, 22.13
)

manual_label_override <- st_as_sf(manual_label_override_ll, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(china_crs)
manual_coords <- st_coordinates(manual_label_override)
manual_label_df <- manual_label_override %>%
  st_drop_geometry() %>%
  mutate(x = manual_coords[, 1], y = manual_coords[, 2]) %>%
  select(province_std, x, y)

label_df <- label_df %>%
  rows_update(manual_label_df, by = "province_std") %>%
  filter(province_std != "Macau") %>%
  mutate(hjust = 0.5, vjust = 0.5)

label_just_df <- tribble(
  ~province_std,      ~hjust, ~vjust,
  "Beijing",          0.15,   0.15,
  "Tianjin",          0.05,   0.05,
  "Hebei",            0.50,   0.65,
  "Liaoning",         0.55,   0.55,
  "Jilin",            0.55,   0.50,
  "Heilongjiang",     0.55,   0.50,
  "Shandong",         0.45,   0.60,
  "Jiangsu",          0.30,   0.55,
  "Shanghai",         0.00,   0.45,
  "Anhui",            0.45,   0.45,
  "Zhejiang",         0.25,   0.45,
  "Fujian",           0.45,   0.60,
  "Jiangxi",          0.50,   0.55,
  "Guangdong",        0.20,   0.70,
  "Hong Kong",        0.00,   0.55,
  "Hainan",           0.50,   0.50,
  "Chongqing",        0.45,   0.55,
  "Inner Mongolia",   0.50,   0.55,
  "Ningxia",          0.50,   0.50,
  "Shanxi",           0.50,   0.55,
  "Shaanxi",          0.45,   0.55,
  "Henan",            0.45,   0.55,
  "Hubei",            0.45,   0.55,
  "Hunan",            0.45,   0.55,
  "Taiwan",           0.50,   0.55
)

label_df <- label_df %>%
  rows_update(label_just_df, by = "province_std")

point_orders <- c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes")
point_map_df <- clean %>%
  mutate(order_group = if_else(order %in% point_orders, order, "Others")) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(china_crs)
point_coords <- st_coordinates(point_map_df)
point_map_df <- point_map_df %>% mutate(x = point_coords[, 1], y = point_coords[, 2])

province_order_full <- prepare_bar_data(clean, "province_std", province_name_lookup$province_std, top_n = 20)
year_order_full <- prepare_bar_data(clean, "year", sort(unique(clean$year)), top_n = 20)
province_order_top10 <- prepare_bar_data(clean, "province_std", province_name_lookup$province_std, top_n = 10)
year_order_top10 <- prepare_bar_data(clean, "year", sort(unique(clean$year)), top_n = 10)

province_order_full$bar_df <- province_order_full$bar_df %>%
  left_join(select(province_summary, province_std, province_label_bar), by = c("x_value" = "province_std")) %>%
  mutate(province_label_bar = factor(province_label_bar,
    levels = province_summary %>% arrange(desc(n_records)) %>% pull(province_label_bar)
  ))
province_order_full$line_df <- province_order_full$line_df %>%
  left_join(select(province_summary, province_std, province_label_bar), by = c("x_value" = "province_std")) %>%
  mutate(province_label_bar = factor(province_label_bar,
    levels = province_summary %>% arrange(desc(n_records)) %>% pull(province_label_bar)
  ))

province_order_top10$bar_df <- province_order_top10$bar_df %>%
  left_join(select(province_summary, province_std, province_label_bar), by = c("x_value" = "province_std")) %>%
  mutate(province_label_bar = factor(province_label_bar,
    levels = province_summary %>% arrange(desc(n_records)) %>% pull(province_label_bar)
  ))
province_order_top10$line_df <- province_order_top10$line_df %>%
  left_join(select(province_summary, province_std, province_label_bar), by = c("x_value" = "province_std")) %>%
  mutate(province_label_bar = factor(province_label_bar,
    levels = province_summary %>% arrange(desc(n_records)) %>% pull(province_label_bar)
  ))

year_order_full$bar_df <- year_order_full$bar_df %>% mutate(year = factor(x_value, levels = sort(unique(clean$year))))
year_order_full$line_df <- year_order_full$line_df %>% mutate(year = factor(x_value, levels = sort(unique(clean$year))))
year_order_top10$bar_df <- year_order_top10$bar_df %>% mutate(year = factor(x_value, levels = sort(unique(clean$year))))
year_order_top10$line_df <- year_order_top10$line_df %>% mutate(year = factor(x_value, levels = sort(unique(clean$year))))

# -------------------------------
# Step 4. Save standardized data tables
# 第 4 步：保存标准化数据表
# -------------------------------
write_csv(province_summary, file.path(data_dir, "province_spatiotemporal_summary.csv"))
write_csv(st_drop_geometry(point_map_df), file.path(data_dir, "point_map_records_by_order_group.csv"))
write_csv(label_df, file.path(data_dir, "province_label_positions.csv"))
write_csv(province_order_full$bar_df %>% select(-x_value), file.path(data_dir, "province_order_stacked_bar_data.csv"))
write_csv(province_order_full$line_df %>% select(-x_value), file.path(data_dir, "province_percentage_line_data.csv"))
write_csv(year_order_full$bar_df %>% select(-x_value), file.path(data_dir, "year_order_stacked_bar_data.csv"))
write_csv(year_order_full$line_df %>% select(-x_value), file.path(data_dir, "year_percentage_line_data.csv"))
write_csv(province_order_top10$bar_df %>% select(-x_value), file.path(data_dir, "province_order_stacked_bar_top10_data.csv"))
write_csv(province_order_top10$line_df %>% select(-x_value), file.path(data_dir, "province_percentage_line_top10_data.csv"))
write_csv(year_order_top10$bar_df %>% select(-x_value), file.path(data_dir, "year_order_stacked_bar_top10_data.csv"))
write_csv(year_order_top10$line_df %>% select(-x_value), file.path(data_dir, "year_percentage_line_top10_data.csv"))
write_csv(tibble(order = names(province_order_full$palette), color = unname(province_order_full$palette)), file.path(data_dir, "spatiotemporal_order_palette.csv"))
write_csv(tibble(order = names(province_order_top10$palette), color = unname(province_order_top10$palette)), file.path(data_dir, "spatiotemporal_order_palette_top10.csv"))

# -------------------------------
# Step 5. Build projected maps
# 第 5 步：绘制投影地图
# -------------------------------
count_fill_scale <- scale_fill_manual(values = count_fill_values, drop = FALSE, name = "Number of new records")
density_fill_scale <- scale_fill_manual(values = density_fill_values, drop = FALSE, name = "Number of provincial-level\nnew records/100.000km2")

count_map_main <- ggplot() +
  geom_sf(data = province_map_sf, aes(fill = count_class), color = "#9A9A9A", linewidth = 0.24) +
  geom_sf(data = province_line_sf, color = "#777777", linewidth = 0.20, fill = NA) +
  geom_sf(data = ten_dash_sf, color = "#272727", linewidth = 0.22, fill = NA) +
  geom_text(data = label_df, aes(x = x, y = y, label = province_label_map, hjust = hjust, vjust = vjust), family = "sans", size = 3.85, lineheight = 0.90) +
  count_fill_scale +
  coord_sf(xlim = main_xlim, ylim = main_ylim, expand = FALSE, crs = china_crs) +
  map_theme()
count_map_main <- add_north_arrow_projected(count_map_main, main_xlim, main_ylim)

count_map_inset <- ggplot() +
  geom_sf(data = st_crop(province_map_sf, inset_bbox), aes(fill = count_class), color = "#8A8A8A", linewidth = 0.18) +
  geom_sf(data = st_crop(province_line_sf, inset_bbox), color = "#777777", linewidth = 0.18, fill = NA) +
  geom_sf(data = st_crop(ten_dash_sf, inset_bbox), color = "#272727", linewidth = 0.24, fill = NA) +
  count_fill_scale +
  coord_sf(xlim = c(inset_bbox["xmin"], inset_bbox["xmax"]), ylim = c(inset_bbox["ymin"], inset_bbox["ymax"]), expand = FALSE, crs = china_crs) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7), legend.position = "none")

density_map_main <- ggplot() +
  geom_sf(data = province_map_sf, aes(fill = density_class), color = "#9A9A9A", linewidth = 0.24) +
  geom_sf(data = province_line_sf, color = "#777777", linewidth = 0.20, fill = NA) +
  geom_sf(data = ten_dash_sf, color = "#272727", linewidth = 0.22, fill = NA) +
  geom_text(data = label_df, aes(x = x, y = y, label = province_label_map, hjust = hjust, vjust = vjust), family = "sans", size = 3.85, lineheight = 0.90) +
  density_fill_scale +
  coord_sf(xlim = main_xlim, ylim = main_ylim, expand = FALSE, crs = china_crs) +
  map_theme()
density_map_main <- add_north_arrow_projected(density_map_main, main_xlim, main_ylim)

density_map_inset <- ggplot() +
  geom_sf(data = st_crop(province_map_sf, inset_bbox), aes(fill = density_class), color = "#8A8A8A", linewidth = 0.18) +
  geom_sf(data = st_crop(province_line_sf, inset_bbox), color = "#777777", linewidth = 0.18, fill = NA) +
  geom_sf(data = st_crop(ten_dash_sf, inset_bbox), color = "#272727", linewidth = 0.24, fill = NA) +
  density_fill_scale +
  coord_sf(xlim = c(inset_bbox["xmin"], inset_bbox["xmax"]), ylim = c(inset_bbox["ymin"], inset_bbox["ymax"]), expand = FALSE, crs = china_crs) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7), legend.position = "none")

point_map_main <- ggplot() +
  geom_sf(data = province_sf, fill = "white", color = "#D0D0D0", linewidth = 0.20) +
  geom_sf(data = province_line_sf, color = "#BEBEBE", linewidth = 0.24, fill = NA) +
  geom_sf(data = ten_dash_sf, color = "#5C5C5C", linewidth = 0.26, fill = NA) +
  geom_point(
    data = point_map_df,
    aes(x = x, y = y, color = order_group, shape = order_group),
    size = 2.85, stroke = 0.22, alpha = 0.95
  ) +
  scale_color_manual(
    values = point_map_palette,
    breaks = c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others"),
    labels = c("PASSERIFORMES", "CHARADRIIFORMES", "ANSERIFORMES", "ACCIPITRIFORMES", "PELECANIFORMES", "Others"),
    name = "New records across orders"
  ) +
  scale_shape_manual(
    values = point_map_shapes,
    breaks = c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others"),
    labels = c("PASSERIFORMES", "CHARADRIIFORMES", "ANSERIFORMES", "ACCIPITRIFORMES", "PELECANIFORMES", "Others"),
    name = "New records across orders"
  ) +
  coord_sf(xlim = main_xlim, ylim = main_ylim, expand = FALSE, crs = china_crs) +
  point_map_theme() +
  guides(
    color = guide_legend(
      ncol = 1,
      byrow = TRUE,
      override.aes = list(
        shape = unname(point_map_shapes[c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others")]),
        color = unname(point_map_palette[c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others")]),
        size = 4.2, alpha = 1
      )
    ),
    shape = "none"
  )
point_map_main <- add_north_arrow_projected(point_map_main, main_xlim, main_ylim)

point_map_inset <- ggplot() +
  geom_sf(data = st_crop(province_line_sf, inset_bbox), color = "#2A2A2A", linewidth = 0.22, fill = NA) +
  geom_sf(data = st_crop(ten_dash_sf, inset_bbox), color = "#222222", linewidth = 0.24, fill = NA) +
  coord_sf(xlim = c(inset_bbox["xmin"], inset_bbox["xmax"]), ylim = c(inset_bbox["ymin"], inset_bbox["ymax"]), expand = FALSE, crs = china_crs) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7), legend.position = "none")

# -------------------------------
# Step 6. Build stacked bar-line charts
# 第 6 步：绘制堆叠柱线图
# -------------------------------
p_province_barline <- make_barline_plot(
  bar_df = province_order_full$bar_df,
  line_df = province_order_full$line_df,
  palette_values = province_order_full$palette,
  x_var = "province_label_bar",
  x_lab = "Province",
  pct_cap = 0.07,
  legend_rows = 5
) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p_year_barline <- make_barline_plot(
  bar_df = year_order_full$bar_df,
  line_df = year_order_full$line_df,
  palette_values = year_order_full$palette,
  x_var = "year",
  x_lab = "Year",
  pct_cap = 0.09,
  legend_rows = 5
) + theme(axis.text.x = element_text(angle = 35, hjust = 1))

p_province_barline_top10 <- make_barline_plot(
  bar_df = province_order_top10$bar_df,
  line_df = province_order_top10$line_df,
  palette_values = province_order_top10$palette,
  x_var = "province_label_bar",
  x_lab = "Province",
  pct_cap = 0.07,
  legend_rows = 3
) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p_year_barline_top10 <- make_barline_plot(
  bar_df = year_order_top10$bar_df,
  line_df = year_order_top10$line_df,
  palette_values = year_order_top10$palette,
  x_var = "year",
  x_lab = "Year",
  pct_cap = 0.09,
  legend_rows = 3
) + theme(axis.text.x = element_text(angle = 35, hjust = 1))

# -------------------------------
# Step 7. Export figures
# 第 7 步：导出图件
# -------------------------------
save_map_bundle(count_map_main, count_map_inset, figures_dir, "fig_sp01_province_new_record_count_map", width = 13.6, height = 9.4, plot_xlim = main_xlim, plot_ylim = main_ylim)
save_map_bundle(density_map_main, density_map_inset, figures_dir, "fig_sp02_province_new_record_density_map", width = 13.6, height = 9.4, plot_xlim = main_xlim, plot_ylim = main_ylim)
save_map_bundle(point_map_main, point_map_inset, figures_dir, "fig_sp03_across_order_point_map", width = 16.0, height = 11.0, plot_xlim = main_xlim, plot_ylim = main_ylim)
save_chart_bundle(p_province_barline, figures_dir, "fig_sp04_province_stacked_barline", width = 14.2, height = 9.1)
save_chart_bundle(p_year_barline, figures_dir, "fig_sp05_year_stacked_barline", width = 14.2, height = 8.7)
save_chart_bundle(p_province_barline_top10, figures_dir, "fig_sp06_province_stacked_barline_top10", width = 14.2, height = 9.1)
save_chart_bundle(p_year_barline_top10, figures_dir, "fig_sp07_year_stacked_barline_top10", width = 14.2, height = 8.7)

# -------------------------------
# Step 8. Documentation and summary
# 第 8 步：任务说明与结果摘要
# -------------------------------
readme_lines <- c(
  "# Bird Spatiotemporal Patterns",
  "",
  "## Overview | 任务概述",
  "This task rebuilds the spatiotemporal figure set for bird new-distribution records using the user-provided shapefile base map and a China-suitable projected coordinate system.",
  "本任务基于用户提供的 shp 底图和适合中国制图的投影方式，重建鸟类新纪录的时空格局图组。",
  "",
  "## Key Outputs | 主要输出",
  "- `figures/fig_sp01_province_new_record_count_map.png`",
  "- `figures/fig_sp02_province_new_record_density_map.png`",
  "- `figures/fig_sp03_across_order_point_map.png`",
  "- `figures/fig_sp04_province_stacked_barline.png`",
  "- `figures/fig_sp05_year_stacked_barline.png`",
  "- `figures/fig_sp06_province_stacked_barline_top10.png`",
  "- `figures/fig_sp07_year_stacked_barline_top10.png`"
)
writeLines(readme_lines, file.path(task_dir, "README.md"))

methods_lines <- c(
  "# Methods | 方法说明",
  "",
  "## Projection | 投影方式",
  "All spatial layers are transformed into a CGCS2000-based Albers Equal Area projection suitable for China-wide thematic mapping.",
  "所有空间图层统一转换到基于 CGCS2000 的 Albers Equal Area 投影，以适配中国尺度专题制图。",
  "",
  "## Workflow | 分析流程",
  "1. Read the cleaned bird record table and the province-level shapefile base map.",
  "2. Standardize province names and derive province/year/order summary tables.",
  "3. Build projected choropleth maps, a projected point-distribution map, and two levels of stacked bar-line charts.",
  "4. Export PNG, PDF, and editable vector PPTX outputs.",
  "",
  "1. 读取清洗后的鸟类记录表和省级 shp 底图。",
  "2. 标准化省份名称，并生成年份、省份和目的统计表。",
  "3. 构建投影地图、投影点位图，以及完整和 top10 两套堆叠柱线图。",
  "4. 导出 PNG、PDF 和可编辑矢量 PPTX。"
)
writeLines(methods_lines, file.path(task_dir, "METHODS.md"))

province_top <- province_summary %>% arrange(desc(n_records)) %>% slice_head(n = 5)
density_top <- province_summary %>% arrange(desc(density_100k)) %>% slice_head(n = 5)
year_top <- year_order_full$line_df %>% arrange(desc(total_records)) %>% slice_head(n = 5)

summary_lines <- c(
  "# Bird spatiotemporal patterns: result summary",
  "# 鸟类新纪录时空格局：结果摘要",
  "",
  paste0("- Total bird new records included: ", nrow(clean), "."),
  paste0("- 本任务纳入的新纪录总数：", nrow(clean), "。"),
  paste0("- Highest-count provinces: ", paste0(province_top$province_label_bar, " (", province_top$n_records, ")", collapse = "; "), "."),
  paste0("- 新纪录数量最高的省份：", paste0(province_top$province_label_bar, "（", province_top$n_records, "）", collapse = "；"), "。"),
  paste0("- Highest density provinces: ", paste0(density_top$province_label_bar, " (", round(density_top$density_100k, 1), ")", collapse = "; "), "."),
  paste0("- 面积标准化密度最高的省份：", paste0(density_top$province_label_bar, "（", round(density_top$density_100k, 1), "）", collapse = "；"), "。"),
  paste0("- Peak years: ", paste0(year_top$x_value, " (", year_top$total_records, ")", collapse = "; "), "."),
  paste0("- 年度高值：", paste0(year_top$x_value, "（", year_top$total_records, "）", collapse = "；"), "。")
)
writeLines(summary_lines, file.path(results_dir, "task_summary.md"))

cat("Spatiotemporal task completed:\n")
cat("  Figures: ", figures_dir, "\n")
