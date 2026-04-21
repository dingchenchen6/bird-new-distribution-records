#!/usr/bin/env Rscript

# ============================================================
# SVG-backed editable PPT export for two key bird maps
# 鸟类两张核心地图的 SVG 支撑可编辑 PPT 导出脚本
# ============================================================
#
# Scientific problem / 科学问题
# The current PPT exports of the province-level new-record count map and the
# record-point distribution map are not editable enough inside PowerPoint.
# Direct officer + rvg export was explicitly tested for these maps, but complex
# map geometries trigger a low-level graphics-engine crash on this machine.
# We therefore need a safer vector-preserving route that still improves downstream
# editability compared with the current EMF/PNG workflow.
# 当前省级新纪录数量图和经纬点分布图的 PPT 导出在 PowerPoint 中可编辑性不足。
# 我们已经实测过 officer + rvg 直接导出，但复杂地图几何在本机上会触发底层图形引擎崩溃。
# 因此需要改用更稳妥的矢量保留路线，同时尽量提升后续在 PowerPoint 中的编辑能力。
#
# Objectives / 研究目标
# 1. Rebuild the two target maps as simplified ggplot layers that are stable for
#    vector-file export.
#    把两张目标地图重构为更稳定的 ggplot 基础图层，便于输出矢量文件。
# 2. Export separate main-map and inset SVG files.
#    分别导出主图与鹰眼图的 SVG 文件。
# 3. Assemble new PowerPoint files with officer by placing SVG vector graphics
#    at the exact map and inset positions.
#    使用 officer 将 SVG 矢量图置入 PowerPoint，并保持主图与鹰眼图的版式位置。
# 4. Keep the workflow transparent and reproducible, and preserve preview PNGs
#    for quick checking.
#    保持流程透明可复现，并输出预览 PNG 以便快速检查。
#
# Important note on editability / 关于“可编辑性”的重要说明
# These PPT files embed SVG vector graphics instead of raster images. In modern
# PowerPoint, SVG content usually remains vector and can often be converted to
# shapes or ungrouped for further editing. This is more editable than the current
# image-like exports, although it is not identical to fully native DrawingML shapes.
# 这些 PPT 文件嵌入的是 SVG 矢量图而不是栅格图片。在较新的 PowerPoint 中，SVG
# 通常可以保持矢量属性，并经“转换为形状/取消组合”等操作继续编辑；这会比当前的
# 图像式导出更接近你需要的可编辑效果，但它并不等同于完全原生的 DrawingML 图形。
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(officer)
  library(tibble)
  library(scales)
  library(svglite)
})

repo_task_dir <- "/Users/dingchenchen/Documents/New records/bird-new-distribution-records/tasks/bird_spatiotemporal_patterns"
mirror_task_dir <- "/Users/dingchenchen/Documents/New records/bird_new_records_R_output/tasks/bird_spatiotemporal_patterns"
data_dir <- file.path(repo_task_dir, "data")
figures_dir <- file.path(repo_task_dir, "figures")
mirror_figures_dir <- file.path(mirror_task_dir, "figures")
shape_dir <- file.path(data_dir, "shapefile_base")

province_shape_path <- file.path(shape_dir, "省.shp")
province_line_path <- file.path(shape_dir, "省_境界线.shp")
ten_dash_path <- file.path(shape_dir, "十段线.shp")
province_summary_path <- file.path(data_dir, "province_spatiotemporal_summary.csv")
point_map_path <- file.path(data_dir, "point_map_records_by_order_group.csv")
label_path <- file.path(data_dir, "province_label_positions.csv")

for (dir_path in c(figures_dir, mirror_figures_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

china_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +ellps=GRS80 +units=m +no_defs"

count_fill_values <- c(
  "0 - 10" = "#3494C7",
  "11 - 20" = "#84B3B1",
  "21 - 30" = "#C4D88B",
  "31 - 40" = "#FFF95C",
  "41 - 50" = "#FDB84A",
  "51 - 60" = "#FF6D2D",
  "61 - 71" = "#F11313"
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

sf_to_polygon_df <- function(sf_obj, keep_cols = character()) {
  obj <- st_cast(sf_obj, "MULTIPOLYGON", warn = FALSE)
  coords <- as_tibble(st_coordinates(obj))
  attrs <- st_drop_geometry(obj)
  coords <- coords %>% mutate(feature_id = L3, piece_id = paste(L3, L2, L1, sep = "_"))
  bind_cols(coords, attrs[coords$feature_id, keep_cols, drop = FALSE])
}

sf_to_path_df <- function(sf_obj, keep_cols = character()) {
  obj <- st_cast(sf_obj, "MULTILINESTRING", warn = FALSE)
  coords <- as_tibble(st_coordinates(obj))
  attrs <- st_drop_geometry(obj)
  coords <- coords %>% mutate(feature_id = L2, piece_id = paste(L2, L1, sep = "_"))
  bind_cols(coords, attrs[coords$feature_id, keep_cols, drop = FALSE])
}

add_north_arrow_projected <- function(plot_obj, xlim, ylim, scale_x = 0.018, scale_y = 0.040) {
  xr <- diff(xlim)
  yr <- diff(ylim)
  x <- xlim[2] - xr * 0.055
  y <- ylim[2] - yr * 0.085
  dx <- xr * scale_x
  dy <- yr * scale_y

  plot_obj +
    annotate("text", x = x, y = y + dy * 1.14, label = "N", size = 8.2, family = "sans") +
    annotate("polygon", x = c(x, x - dx * 0.55, x, x + dx * 0.55), y = c(y + dy * 0.78, y - dy * 0.95, y - dy * 0.02, y - dy * 0.95), fill = "black", color = "black", linewidth = 0.26) +
    annotate("polygon", x = c(x, x - dx * 0.17, x, x + dx * 0.17), y = c(y + dy * 0.49, y - dy * 0.62, y + dy * 0.02, y - dy * 0.62), fill = "white", color = "white") +
    annotate("segment", x = x, xend = x, y = y - dy * 0.01, yend = y + dy * 0.53, linewidth = 0.15, color = "white")
}

map_theme <- function() {
  theme_void(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75),
      legend.position = c(0.022, 0.018),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
      legend.title = element_text(size = 14.5, face = "bold"),
      legend.text = element_text(size = 12.4),
      legend.key.width = unit(1.2, "cm"),
      legend.key.height = unit(0.60, "cm"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

point_map_theme <- function() {
  theme_void(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75),
      legend.position = c(0.021, 0.016),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = alpha("white", 0.92), color = NA),
      legend.title = element_text(size = 15.5, face = "bold"),
      legend.text = element_text(size = 15.0),
      legend.key.height = unit(0.68, "cm"),
      legend.key.width = unit(0.95, "cm"),
      plot.margin = margin(8, 8, 8, 8)
    )
}

save_svg <- function(plot_obj, target_path, width, height) {
  svglite::svglite(file = target_path, width = width, height = height, bg = "white")
  print(plot_obj)
  dev.off()
}

export_svg_backed_ppt <- function(main_svg, inset_svg, target_pptx,
                                  slide_width = 13.333, slide_height = 7.5,
                                  inset_left = 0.842, inset_bottom = 0.0005,
                                  inset_right = 0.998, inset_top = 0.232) {
  ppt <- read_pptx()
  ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- ph_with(
    ppt,
    external_img(main_svg, width = slide_width, height = slide_height),
    location = ph_location(left = 0, top = 0, width = slide_width, height = slide_height)
  )
  ppt <- ph_with(
    ppt,
    external_img(inset_svg, width = slide_width * (inset_right - inset_left), height = slide_height * (inset_top - inset_bottom)),
    location = ph_location(
      left = slide_width * inset_left,
      top = slide_height * (1 - inset_top),
      width = slide_width * (inset_right - inset_left),
      height = slide_height * (inset_top - inset_bottom)
    )
  )
  print(ppt, target = target_pptx)
}

province_sf_ll <- st_read(province_shape_path, quiet = TRUE, options = "ENCODING=UTF-8")
province_line_sf_ll <- st_read(province_line_path, quiet = TRUE, options = "ENCODING=UTF-8")
ten_dash_sf_ll <- st_read(ten_dash_path, quiet = TRUE, options = "ENCODING=UTF-8")
province_summary <- read_csv(province_summary_path, show_col_types = FALSE)
point_map_df <- read_csv(point_map_path, show_col_types = FALSE)
label_df <- read_csv(label_path, show_col_types = FALSE)

province_sf <- st_transform(province_sf_ll, china_crs)
province_line_sf <- st_transform(province_line_sf_ll, china_crs)
ten_dash_sf <- st_transform(ten_dash_sf_ll, china_crs)
province_map_sf <- province_sf %>% left_join(select(province_summary, province_cn, province_std, count_class), by = c("省名" = "province_cn"))
if (any(is.na(province_map_sf$count_class))) stop("Province join failed for count-class map.")

main_bbox <- st_bbox(province_sf)
main_xlim <- c(main_bbox["xmin"] - 760000, main_bbox["xmax"] + 560000)
main_ylim <- c(main_bbox["ymin"] + 840000, main_bbox["ymax"] + 140000)
ten_dash_main_bbox <- c(xmin = unname(as.numeric(main_xlim[1])), xmax = unname(as.numeric(main_xlim[2])), ymin = unname(as.numeric(main_ylim[1])), ymax = unname(as.numeric(main_ylim[1] + diff(main_ylim) * 0.18)))
ten_dash_main_sf <- st_crop(ten_dash_sf, ten_dash_main_bbox)
inset_bbox <- build_bbox_from_longlat(104, 125, 2, 26, china_crs)

province_poly_df <- sf_to_polygon_df(province_map_sf, keep_cols = c("count_class"))
province_line_df <- sf_to_path_df(province_line_sf)
ten_dash_main_df <- sf_to_path_df(ten_dash_main_sf)
count_inset_poly_df <- sf_to_polygon_df(st_crop(province_map_sf, inset_bbox), keep_cols = c("count_class"))
count_inset_line_df <- sf_to_path_df(st_crop(province_line_sf, inset_bbox))
count_inset_dash_df <- sf_to_path_df(st_crop(ten_dash_sf, inset_bbox))
point_main_poly_df <- sf_to_polygon_df(province_sf)
point_inset_poly_df <- sf_to_polygon_df(st_crop(province_sf, inset_bbox))
point_inset_line_df <- sf_to_path_df(st_crop(province_line_sf, inset_bbox))
point_inset_dash_df <- sf_to_path_df(st_crop(ten_dash_sf, inset_bbox))
point_inset_df <- point_map_df %>% filter(x >= inset_bbox["xmin"], x <= inset_bbox["xmax"], y >= inset_bbox["ymin"], y <= inset_bbox["ymax"])

count_map_main <- ggplot() +
  geom_polygon(data = province_poly_df, aes(x = X, y = Y, group = piece_id, fill = count_class), color = "#9A9A9A", linewidth = 0.24) +
  geom_path(data = province_line_df, aes(x = X, y = Y, group = piece_id), color = "#777777", linewidth = 0.20, lineend = "round") +
  geom_path(data = ten_dash_main_df, aes(x = X, y = Y, group = piece_id), color = "#272727", linewidth = 0.22, lineend = "round") +
  geom_text(data = label_df, aes(x = x, y = y, label = province_label_map, hjust = hjust, vjust = vjust), family = "sans", size = 3.85, lineheight = 0.90) +
  scale_fill_manual(values = count_fill_values, drop = FALSE, name = "Number of new records") +
  coord_equal(xlim = main_xlim, ylim = main_ylim, expand = FALSE) +
  map_theme()
count_map_main <- add_north_arrow_projected(count_map_main, main_xlim, main_ylim)

count_map_inset <- ggplot() +
  geom_polygon(data = count_inset_poly_df, aes(x = X, y = Y, group = piece_id, fill = count_class), color = "#8A8A8A", linewidth = 0.18) +
  geom_path(data = count_inset_line_df, aes(x = X, y = Y, group = piece_id), color = "#777777", linewidth = 0.18, lineend = "round") +
  geom_path(data = count_inset_dash_df, aes(x = X, y = Y, group = piece_id), color = "#272727", linewidth = 0.24, lineend = "round") +
  scale_fill_manual(values = count_fill_values, drop = FALSE, guide = "none") +
  coord_equal(xlim = c(inset_bbox["xmin"], inset_bbox["xmax"]), ylim = c(inset_bbox["ymin"], inset_bbox["ymax"]), expand = FALSE) +
  theme_void() + theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

point_map_main <- ggplot() +
  geom_polygon(data = point_main_poly_df, aes(x = X, y = Y, group = piece_id), fill = "white", color = "#8C8C8C", linewidth = 0.26) +
  geom_path(data = province_line_df, aes(x = X, y = Y, group = piece_id), color = "#5A5A5A", linewidth = 0.36, lineend = "round") +
  geom_path(data = ten_dash_main_df, aes(x = X, y = Y, group = piece_id), color = "#4A4A4A", linewidth = 0.28, lineend = "round") +
  geom_point(data = point_map_df, aes(x = x, y = y, color = order_group, shape = order_group), size = 2.85, stroke = 0.22, alpha = 0.95) +
  scale_color_manual(values = point_map_palette, breaks = c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others"), labels = c("PASSERIFORMES", "CHARADRIIFORMES", "ANSERIFORMES", "ACCIPITRIFORMES", "PELECANIFORMES", "Others"), name = "New records across orders") +
  scale_shape_manual(values = point_map_shapes, breaks = c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others"), labels = c("PASSERIFORMES", "CHARADRIIFORMES", "ANSERIFORMES", "ACCIPITRIFORMES", "PELECANIFORMES", "Others"), name = "New records across orders") +
  coord_equal(xlim = main_xlim, ylim = main_ylim, expand = FALSE) +
  point_map_theme() +
  guides(color = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(shape = unname(point_map_shapes[c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others")]), color = unname(point_map_palette[c("Passeriformes", "Charadriiformes", "Anseriformes", "Accipitriformes", "Pelecaniformes", "Others")]), size = 4.2, alpha = 1)), shape = "none")
point_map_main <- add_north_arrow_projected(point_map_main, main_xlim, main_ylim)

point_map_inset <- ggplot() +
  geom_polygon(data = point_inset_poly_df, aes(x = X, y = Y, group = piece_id), fill = "white", color = "#B8B8B8", linewidth = 0.18) +
  geom_path(data = point_inset_line_df, aes(x = X, y = Y, group = piece_id), color = "#6A6A6A", linewidth = 0.22, lineend = "round") +
  geom_path(data = point_inset_dash_df, aes(x = X, y = Y, group = piece_id), color = "#222222", linewidth = 0.28, lineend = "round") +
  geom_point(data = point_inset_df, aes(x = x, y = y, color = order_group, shape = order_group), size = 1.8, stroke = 0.18, alpha = 0.92, show.legend = FALSE) +
  scale_color_manual(values = point_map_palette, guide = "none") +
  scale_shape_manual(values = point_map_shapes, guide = "none") +
  coord_equal(xlim = c(inset_bbox["xmin"], inset_bbox["xmax"]), ylim = c(inset_bbox["ymin"], inset_bbox["ymax"]), expand = FALSE) +
  theme_void() + theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

count_main_svg <- file.path(figures_dir, "fig_sp01_province_new_record_count_map_editable_v4_main.svg")
count_inset_svg <- file.path(figures_dir, "fig_sp01_province_new_record_count_map_editable_v4_inset.svg")
count_preview <- file.path(figures_dir, "fig_sp01_province_new_record_count_map_editable_v4_preview.png")
count_pptx <- file.path(figures_dir, "fig_sp01_province_new_record_count_map_editable_v4.pptx")
point_main_svg <- file.path(figures_dir, "fig_sp03_across_order_point_map_editable_v4_main.svg")
point_inset_svg <- file.path(figures_dir, "fig_sp03_across_order_point_map_editable_v4_inset.svg")
point_preview <- file.path(figures_dir, "fig_sp03_across_order_point_map_editable_v4_preview.png")
point_pptx <- file.path(figures_dir, "fig_sp03_across_order_point_map_editable_v4.pptx")

save_svg(count_map_main, count_main_svg, width = 13.6, height = 9.4)
save_svg(count_map_inset, count_inset_svg, width = 13.6 * (0.998 - 0.842), height = 9.4 * (0.232 - 0.0005))
ggsave(count_preview, count_map_main, width = 13.6, height = 9.4, dpi = 360, bg = "white")

save_svg(point_map_main, point_main_svg, width = 16.0, height = 11.0)
save_svg(point_map_inset, point_inset_svg, width = 16.0 * (0.998 - 0.842), height = 11.0 * (0.232 - 0.0005))
ggsave(point_preview, point_map_main, width = 16.0, height = 11.0, dpi = 360, bg = "white")

for (f in c(count_main_svg, count_inset_svg, count_preview, point_main_svg, point_inset_svg, point_preview)) {
  file.copy(f, file.path(mirror_figures_dir, basename(f)), overwrite = TRUE)
}

cat('SVG source files and preview PNGs exported successfully.\n')
cat('  ', count_main_svg, '\n')
cat('  ', point_main_svg, '\n')
