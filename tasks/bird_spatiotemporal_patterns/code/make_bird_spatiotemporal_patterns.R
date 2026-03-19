#!/usr/bin/env Rscript

# ============================================================
# Bird new-record spatiotemporal patterns task bundle
# 鸟类新纪录时空格局任务脚本
# ============================================================
#
# Analysis steps / 分析步骤
# 1. Create a dedicated task directory with figures, code, data, and results.
#    建立独立任务目录，包括 figures、code、data、results 四个子文件夹。
# 2. Read the cleaned bird new-record table and the user-provided province-level
#    shapefile base map, provincial boundary line layer, and ten-dash-line layer.
#    读取清洗后的鸟类新纪录表，以及用户提供的省级底图、省界线和十段线图层。
# 3. Standardize Chinese-English province names, compute provincial counts,
#    provincial densities, annual counts, and order-level composition tables.
#    标准化中英文省份名称，计算省级新纪录数量、面积标准化密度、年度记录数以及按目组成表。
# 4. Rebuild manuscript-style spatiotemporal figures following the supplied
#    reference style: two choropleth maps, one across-order point map, and two
#    stacked bar-line charts.
#    参照用户提供的参考样式，重建论文风格的时空格局图，包括两张分级设色地图、一张跨目点位图，以及两张堆叠柱线图。
# 5. Export all figures in PNG, PDF, and PPTX formats, and save plot-ready data.
#    将所有图件导出为 PNG、PDF、PPTX 三种格式，并保存作图数据。
# 6. Write bilingual task documentation and a concise result summary.
#    输出中英文任务说明与简要结果摘要。
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
  library(patchwork)
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
# Step 1. Helper functions
# 第 1 步：辅助函数
# -------------------------------
save_plot_bundle <- function(plot_obj, out_dir, filename_no_ext, width, height, dpi = 450) {
  png_path <- file.path(out_dir, paste0(filename_no_ext, ".png"))
  pdf_path <- file.path(out_dir, paste0(filename_no_ext, ".pdf"))
  pptx_path <- file.path(out_dir, paste0(filename_no_ext, ".pptx"))

  ggsave(png_path, plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(pdf_path, plot_obj, width = width, height = height, device = cairo_pdf, bg = "white")

  ppt <- read_pptx()
  ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- ph_with(ppt, external_img(png_path, width = width, height = height), location = ph_location_fullsize())
  print(ppt, target = pptx_path)
}

add_north_arrow <- function(plot_obj, x = 129.8, y = 51.8, scale = 1.0) {
  plot_obj +
    annotate("text", x = x, y = y + 2.4 * scale, label = "N",
             size = 11 * scale, family = "sans", fontface = "plain") +
    annotate(
      "polygon",
      x = c(x, x - 0.9 * scale, x, x + 0.9 * scale),
      y = c(y + 1.2 * scale, y - 2.2 * scale, y - 0.4 * scale, y - 2.2 * scale),
      fill = "black", color = "black", linewidth = 0.4
    ) +
    annotate(
      "polygon",
      x = c(x, x - 0.36 * scale, x, x + 0.36 * scale),
      y = c(y + 0.6 * scale, y - 1.4 * scale, y - 0.2 * scale, y - 1.4 * scale),
      fill = "white", color = "white"
    )
}

build_inset_plot <- function(fill_sf, fill_var, fill_scale, fill_title = NULL) {
  inset_bbox <- st_bbox(c(xmin = 104, xmax = 125, ymin = 2, ymax = 26), crs = st_crs(fill_sf))

  ggplot() +
    geom_sf(data = st_crop(fill_sf, inset_bbox), aes(fill = .data[[fill_var]]), color = "#8A8A8A", linewidth = 0.22) +
    geom_sf(data = st_crop(province_line_sf, inset_bbox), color = "#6A6A6A", linewidth = 0.18, fill = NA) +
    geom_sf(data = st_crop(ten_dash_sf, inset_bbox), color = "#1C1C1C", linewidth = 0.28, fill = NA) +
    fill_scale +
    coord_sf(xlim = c(104, 125), ylim = c(2, 26), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

map_theme <- function() {
  theme_void(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75),
      legend.position = c(0.11, 0.17),
      legend.justification = c(0, 0),
      legend.title = element_text(size = 13.8, face = "bold"),
      legend.text = element_text(size = 12.2),
      legend.key.width = unit(1.25, "cm"),
      legend.key.height = unit(0.8, "cm"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

bar_theme <- function() {
  theme_classic(base_family = "sans", base_size = 13) +
    theme(
      panel.grid.major.y = element_line(color = "#D7D7D7", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 11.6, color = "black"),
      axis.text.y = element_text(size = 11.6, color = "black"),
      axis.title.x = element_text(size = 13.5, margin = margin(t = 10)),
      axis.title.y = element_text(size = 13.5),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(size = 11.8),
      legend.key.width = unit(0.9, "cm"),
      legend.key.height = unit(0.35, "cm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 8, 8, 8)
    )
}

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
  "Others" = "#808080"
)

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

point_map_palette <- c(
  "Passeriformes" = "#54FF19",
  "Charadriiformes" = "#FFB31A",
  "Anseriformes" = "#38C8FF",
  "Accipitriformes" = "#FF1C1C",
  "Pelecaniformes" = "#C925FF",
  "Others" = "#111111"
)

# -------------------------------
# Step 2. Read datasets and shapefiles
# 第 2 步：读取数据与底图图层
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
  mutate(
    province_std = recode(province_raw, !!!province_recode)
  )

province_sf <- st_read(province_shp_path, quiet = TRUE, options = "ENCODING=UTF-8") %>%
  st_make_valid() %>%
  left_join(province_name_lookup, by = c("省名" = "province_cn"))

province_line_sf <- st_read(province_line_path, quiet = TRUE, options = "ENCODING=UTF-8") %>%
  st_make_valid()

ten_dash_sf <- st_read(ten_dash_path, quiet = TRUE, options = "ENCODING=UTF-8") %>%
  st_make_valid()

if (any(is.na(province_sf$province_std))) {
  missing_names <- province_sf %>% st_drop_geometry() %>% filter(is.na(province_std)) %>% pull(省名)
  stop("Missing province mapping for: ", paste(missing_names, collapse = ", "))
}

# -------------------------------
# Step 3. Derive summaries for maps and bar-line figures
# 第 3 步：生成地图与柱线图统计表
# -------------------------------
province_area <- province_sf %>%
  st_transform(3857) %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  select(province_std, area_km2)

province_counts <- clean %>%
  count(province_std, name = "n_records")

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

order_rank <- clean %>%
  count(order, sort = TRUE)

bar_orders <- c(order_rank %>% slice_head(n = 20) %>% pull(order), "Others")
point_orders <- c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes")

fallback_orders <- setdiff(bar_orders, names(bar_palette_base))
if (length(fallback_orders) > 0) {
  fallback_cols <- setNames(hue_pal()(length(fallback_orders)), fallback_orders)
  bar_palette <- c(bar_palette_base, fallback_cols)
} else {
  bar_palette <- bar_palette_base
}
bar_palette <- bar_palette[unique(bar_orders)]

province_bar_df <- clean %>%
  mutate(order_bar = if_else(order %in% bar_orders, order, "Others")) %>%
  count(province_std, order_bar, name = "n_records") %>%
  complete(province_std = province_name_lookup$province_std, order_bar = bar_orders, fill = list(n_records = 0)) %>%
  left_join(select(province_summary, province_std, province_label_bar, province_total = n_records), by = "province_std") %>%
  mutate(
    province_total = replace_na(province_total, 0L),
    order_bar = factor(order_bar, levels = bar_orders),
    province_label_bar = fct_reorder(province_label_bar, province_total, .desc = TRUE)
  )

province_line_df <- province_summary %>%
  mutate(
    province_label_bar = factor(province_label_bar,
      levels = province_bar_df %>%
        distinct(province_label_bar, province_total) %>%
        arrange(desc(province_total), province_label_bar) %>%
        pull(province_label_bar)
    ),
    percentage = n_records / sum(n_records)
  )

year_bar_df <- clean %>%
  mutate(order_bar = if_else(order %in% bar_orders, order, "Others")) %>%
  count(year, order_bar, name = "n_records") %>%
  complete(year = seq(min(clean$year, na.rm = TRUE), max(clean$year, na.rm = TRUE), by = 1), order_bar = bar_orders, fill = list(n_records = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(n_records)) %>%
  ungroup() %>%
  mutate(
    order_bar = factor(order_bar, levels = bar_orders),
    year = factor(year, levels = sort(unique(year)))
  )

year_line_df <- year_bar_df %>%
  distinct(year, year_total) %>%
  mutate(percentage = year_total / sum(year_total))

point_map_df <- clean %>%
  mutate(order_group = if_else(order %in% point_orders, order, "Others")) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  select(record_id, species, order, order_group, longitude, latitude, province_std, year)

# -------------------------------
# Step 4. Create label positions and save processed data
# 第 4 步：构建标注位置并保存数据
# -------------------------------
label_points <- st_point_on_surface(province_map_sf)
label_coords <- st_coordinates(label_points)
label_df <- province_map_sf %>%
  st_drop_geometry() %>%
  mutate(x = label_coords[, 1], y = label_coords[, 2]) %>%
  select(province_std, province_label_map, x, y)

manual_label_override <- tribble(
  ~province_std,       ~x,    ~y,
  "Beijing",          116.9, 40.8,
  "Tianjin",          117.8, 39.5,
  "Shanghai",         121.5, 31.2,
  "Hong Kong",        114.6, 22.6,
  "Macau",            113.8, 22.3,
  "Chongqing",        107.5, 30.2,
  "Inner Mongolia",   111.2, 43.5,
  "Ningxia",          106.5, 37.5,
  "Xizang",            88.0, 31.2,
  "Hainan",           109.6, 19.4,
  "Taiwan",           121.0, 23.8,
  "Jiangsu",          119.3, 33.1,
  "Anhui",            117.2, 31.7,
  "Guangdong",        113.6, 23.2,
  "Fujian",           118.4, 25.7,
  "Zhejiang",         120.2, 29.0
)

label_df <- label_df %>%
  rows_update(manual_label_override, by = "province_std") %>%
  filter(province_std != "Macau")

write_csv(province_summary, file.path(data_dir, "province_spatiotemporal_summary.csv"))
write_csv(province_bar_df, file.path(data_dir, "province_order_stacked_bar_data.csv"))
write_csv(province_line_df, file.path(data_dir, "province_percentage_line_data.csv"))
write_csv(year_bar_df, file.path(data_dir, "year_order_stacked_bar_data.csv"))
write_csv(year_line_df, file.path(data_dir, "year_percentage_line_data.csv"))
write_csv(point_map_df, file.path(data_dir, "point_map_records_by_order_group.csv"))
write_csv(label_df, file.path(data_dir, "province_label_positions.csv"))
write_csv(
  tibble(order = names(bar_palette), color = unname(bar_palette)),
  file.path(data_dir, "spatiotemporal_order_palette.csv")
)

# -------------------------------
# Step 5. Build choropleth maps
# 第 5 步：绘制两类分级设色地图
# -------------------------------
count_fill_scale <- scale_fill_manual(
  values = count_fill_values,
  drop = FALSE,
  name = "Number of new records"
)

density_fill_scale <- scale_fill_manual(
  values = density_fill_values,
  drop = FALSE,
  name = "Number of provincial-level\nnew records/100.000km2"
)

count_map_core <- ggplot() +
  geom_sf(data = province_map_sf, aes(fill = count_class), color = "#8A8A8A", linewidth = 0.28) +
  geom_sf(data = province_line_sf, color = "#777777", linewidth = 0.22, fill = NA) +
  geom_text(data = label_df, aes(x = x, y = y, label = province_label_map), family = "sans", size = 4.6) +
  count_fill_scale +
  coord_sf(xlim = c(73, 136), ylim = c(16, 55), expand = FALSE) +
  map_theme()

count_inset <- build_inset_plot(province_map_sf, "count_class", count_fill_scale)

p_map_count <- count_map_core +
  inset_element(count_inset, left = 0.80, bottom = 0.02, right = 0.995, top = 0.31, align_to = "full")

p_map_count <- add_north_arrow(p_map_count)


density_map_core <- ggplot() +
  geom_sf(data = province_map_sf, aes(fill = density_class), color = "#8A8A8A", linewidth = 0.28) +
  geom_sf(data = province_line_sf, color = "#777777", linewidth = 0.22, fill = NA) +
  geom_text(data = label_df, aes(x = x, y = y, label = province_label_map), family = "sans", size = 4.6) +
  density_fill_scale +
  coord_sf(xlim = c(73, 136), ylim = c(16, 55), expand = FALSE) +
  map_theme()

density_inset <- build_inset_plot(province_map_sf, "density_class", density_fill_scale)

p_map_density <- density_map_core +
  inset_element(density_inset, left = 0.80, bottom = 0.02, right = 0.995, top = 0.31, align_to = "full")

p_map_density <- add_north_arrow(p_map_density)

# -------------------------------
# Step 6. Build across-order point map
# 第 6 步：绘制跨目点位图
# -------------------------------
point_map_core <- ggplot() +
  geom_sf(data = province_line_sf, color = "#2D2D2D", linewidth = 0.24, fill = NA) +
  geom_point(
    data = point_map_df,
    aes(x = longitude, y = latitude, fill = order_group),
    shape = 21, size = 2.9, stroke = 0.35, color = "black", alpha = 0.98
  ) +
  scale_fill_manual(
    values = point_map_palette,
    breaks = c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others"),
    labels = c("PASSERIFORMES", "CHARADRIIFORMES", "ANSERIFORMES", "ACCIPITRIFORMES", "PELECANIFORMES", "Others"),
    name = "New records across orders"
  ) +
  coord_sf(xlim = c(73, 136), ylim = c(16, 55), expand = FALSE) +
  theme_void(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75),
    legend.position = c(0.05, 0.11),
    legend.justification = c(0, 0),
    legend.title = element_text(size = 15.5, face = "bold"),
    legend.text = element_text(size = 15.0),
    legend.key.height = unit(0.72, "cm"),
    legend.key.width = unit(0.9, "cm"),
    plot.margin = margin(8, 8, 8, 8)
  )

point_inset <- ggplot() +
  geom_sf(data = st_crop(province_line_sf, st_bbox(c(xmin = 104, xmax = 125, ymin = 2, ymax = 26), crs = st_crs(province_line_sf))),
          color = "#2D2D2D", linewidth = 0.22, fill = NA) +
  geom_sf(data = st_crop(ten_dash_sf, st_bbox(c(xmin = 104, xmax = 125, ymin = 2, ymax = 26), crs = st_crs(ten_dash_sf))),
          color = "#1C1C1C", linewidth = 0.28, fill = NA) +
  coord_sf(xlim = c(104, 125), ylim = c(2, 26), expand = FALSE) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    plot.background = element_rect(fill = "white", color = NA)
  )

p_point_map <- point_map_core +
  inset_element(point_inset, left = 0.80, bottom = 0.02, right = 0.995, top = 0.31, align_to = "full")

p_point_map <- add_north_arrow(p_point_map, x = 129.8, y = 51.5, scale = 1.05)

# -------------------------------
# Step 7. Build stacked bar-line figures
# 第 7 步：绘制省份与年份堆叠柱线图
# -------------------------------
province_max_count <- max(province_line_df$n_records, na.rm = TRUE)
province_pct_cap <- ceiling(max(province_line_df$percentage, na.rm = TRUE) * 100 / 1) / 100
province_pct_cap <- max(province_pct_cap, 0.07)
province_scale_factor <- province_max_count / province_pct_cap

p_province_barline <- ggplot(province_bar_df, aes(x = province_label_bar, y = n_records, fill = order_bar)) +
  geom_col(width = 0.72) +
  geom_line(
    data = province_line_df,
    aes(x = province_label_bar, y = percentage * province_scale_factor, group = 1, color = "Percentage", linetype = "Percentage"),
    linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = province_line_df,
    aes(x = province_label_bar, y = percentage * province_scale_factor, color = "Percentage", shape = "Percentage"),
    size = 2.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = bar_palette,
    labels = toupper(names(bar_palette)),
    breaks = names(bar_palette)
  ) +
  scale_color_manual(values = c("Percentage" = "black"), breaks = "Percentage") +
  scale_linetype_manual(values = c("Percentage" = "22"), breaks = "Percentage") +
  scale_shape_manual(values = c("Percentage" = 16), breaks = "Percentage") +
  scale_y_continuous(
    name = "Number of new records",
    limits = c(0, province_pct_cap * province_scale_factor),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . / province_scale_factor, name = "Percentage", labels = label_percent(accuracy = 0.1))
  ) +
  labs(x = "Province") +
  guides(
    fill = guide_legend(nrow = 5, byrow = TRUE, order = 1),
    color = guide_legend(order = 2, override.aes = list(linetype = "22", shape = 16, linewidth = 0.7)),
    linetype = "none",
    shape = "none"
  ) +
  bar_theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

year_max_count <- max(year_line_df$year_total, na.rm = TRUE)
year_pct_cap <- ceiling(max(year_line_df$percentage, na.rm = TRUE) * 100 / 1) / 100
year_pct_cap <- max(year_pct_cap, 0.09)
year_scale_factor <- year_max_count / year_pct_cap

p_year_barline <- ggplot(year_bar_df, aes(x = year, y = n_records, fill = order_bar)) +
  geom_col(width = 0.72) +
  geom_line(
    data = year_line_df,
    aes(x = year, y = percentage * year_scale_factor, group = 1, color = "Percentage", linetype = "Percentage"),
    linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = year_line_df,
    aes(x = year, y = percentage * year_scale_factor, color = "Percentage", shape = "Percentage"),
    size = 2.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = bar_palette,
    labels = toupper(names(bar_palette)),
    breaks = names(bar_palette)
  ) +
  scale_color_manual(values = c("Percentage" = "black"), breaks = "Percentage") +
  scale_linetype_manual(values = c("Percentage" = "22"), breaks = "Percentage") +
  scale_shape_manual(values = c("Percentage" = 16), breaks = "Percentage") +
  scale_y_continuous(
    name = "Number of new records",
    limits = c(0, year_pct_cap * year_scale_factor),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . / year_scale_factor, name = "Percentage", labels = label_percent(accuracy = 0.1))
  ) +
  labs(x = "Year") +
  guides(
    fill = guide_legend(nrow = 5, byrow = TRUE, order = 1),
    color = guide_legend(order = 2, override.aes = list(linetype = "22", shape = 16, linewidth = 0.7)),
    linetype = "none",
    shape = "none"
  ) +
  bar_theme() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# -------------------------------
# Step 8. Export all figures
# 第 8 步：导出全部图件
# -------------------------------
save_plot_bundle(p_map_count, figures_dir, "fig_sp01_province_new_record_count_map", width = 13.6, height = 9.4)
save_plot_bundle(p_map_density, figures_dir, "fig_sp02_province_new_record_density_map", width = 13.6, height = 9.4)
save_plot_bundle(p_point_map, figures_dir, "fig_sp03_across_order_point_map", width = 16.0, height = 11.0)
save_plot_bundle(p_province_barline, figures_dir, "fig_sp04_province_stacked_barline", width = 14.2, height = 9.1)
save_plot_bundle(p_year_barline, figures_dir, "fig_sp05_year_stacked_barline", width = 14.2, height = 8.7)

# -------------------------------
# Step 9. Write task documentation and results
# 第 9 步：输出任务文档与结果摘要
# -------------------------------
readme_lines <- c(
  "# Bird Spatiotemporal Patterns",
  "",
  "## Overview | 任务概述",
  "This task rebuilds the spatiotemporal figure set for bird new-distribution records using the user-provided shapefile base map and manuscript-style stacked bar-line charts.",
  "本任务基于用户提供的 shp 底图，重建鸟类新纪录的时空格局图，包括地图和论文风格堆叠柱线图。",
  "",
  "## Folder Structure | 文件夹结构",
  "- `figures`: choropleth maps, point map, and stacked bar-line figures",
  "- `code`: bilingual R workflow script with step-by-step comments",
  "- `data`: plot-ready tables and the copied shapefile base map",
  "- `results`: concise result summary",
  "",
  "## Main Script | 主脚本",
  "- `code/make_bird_spatiotemporal_patterns.R`",
  "",
  "## Key Outputs | 主要输出",
  "- `figures/fig_sp01_province_new_record_count_map.png`",
  "- `figures/fig_sp02_province_new_record_density_map.png`",
  "- `figures/fig_sp03_across_order_point_map.png`",
  "- `figures/fig_sp04_province_stacked_barline.png`",
  "- `figures/fig_sp05_year_stacked_barline.png`"
)
writeLines(readme_lines, file.path(task_dir, "README.md"))

methods_lines <- c(
  "# Methods | 方法说明",
  "",
  "## Objective | 目标",
  "Reconstruct the spatial and temporal pattern figures of bird new-distribution records with a standardized province-level shapefile base and manuscript-style layout.",
  "利用标准化省级 shp 底图与论文风格版式，重建鸟类新纪录的空间与时间格局图件。",
  "",
  "## Workflow | 分析流程",
  "1. Read the cleaned bird new-record table and the user-provided province-level shapefile base.",
  "2. Standardize province names, compute provincial counts, area-standardized density, annual counts, and order composition.",
  "3. Build two choropleth maps, one point distribution map across orders, and two stacked bar-line figures.",
  "4. Export all plots in PNG, PDF, and PPTX formats together with the processed data tables.",
  "",
  "1. 读取清洗后的鸟类新纪录表和用户提供的省级 shp 底图。",
  "2. 标准化省份名称，并计算省级记录数、面积标准化密度、年度记录数和按目组成。",
  "3. 绘制两张分级设色地图、一张跨目点位图以及两张堆叠柱线图。",
  "4. 同时导出 PNG、PDF、PPTX 图件及整理后的作图数据。"
)
writeLines(methods_lines, file.path(task_dir, "METHODS.md"))

province_top <- province_summary %>% arrange(desc(n_records)) %>% slice_head(n = 5)
density_top <- province_summary %>% arrange(desc(density_100k)) %>% slice_head(n = 5)
year_top <- year_line_df %>% mutate(year = as.character(year)) %>% arrange(desc(year_total)) %>% slice_head(n = 5)

summary_lines <- c(
  "# Bird spatiotemporal patterns: result summary",
  "# 鸟类新纪录时空格局：结果摘要",
  "",
  "## Overview / 总体概况",
  paste0("- Total bird new records used in this task: ", nrow(clean), "."),
  paste0("- 本任务纳入的鸟类新纪录总数：", nrow(clean), "。"),
  paste0("- Orders shown in stacked bar-line figures: ", length(bar_orders) - 1, " major orders plus Others."),
  paste0("- 堆叠柱线图展示：", length(bar_orders) - 1, " 个主要目加 Others。"),
  "",
  "## Top provinces by counts / 新纪录数量最高的省份",
  paste0("- ", paste0(province_top$province_label_bar, " (", province_top$n_records, ")", collapse = "; "), "."),
  paste0("- ", paste0(province_top$province_label_bar, "（", province_top$n_records, "）", collapse = "；"), "。"),
  "",
  "## Top provinces by density / 面积标准化密度最高的省份",
  paste0("- ", paste0(density_top$province_label_bar, " (", round(density_top$density_100k, 1), ")", collapse = "; "), "."),
  paste0("- ", paste0(density_top$province_label_bar, "（", round(density_top$density_100k, 1), "）", collapse = "；"), "。"),
  "",
  "## Peak years / 年度高值",
  paste0("- ", paste0(year_top$year, " (", year_top$year_total, ")", collapse = "; "), "."),
  paste0("- ", paste0(year_top$year, "（", year_top$year_total, "）", collapse = "；"), "。")
)
writeLines(summary_lines, file.path(results_dir, "task_summary.md"))

cat("Spatiotemporal task completed:\n")
cat("  ", figures_dir, "\n")
