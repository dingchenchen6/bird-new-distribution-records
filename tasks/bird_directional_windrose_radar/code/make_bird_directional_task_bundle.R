#!/usr/bin/env Rscript

# ============================================================
# Bird new-record directional plots task bundle
# 鸟类新纪录方向图任务打包脚本
# ============================================================
#
# Analysis steps / 分析步骤
# 1. Define a task-specific output directory with four subfolders:
#    figures, code, data, and results.
#    定义任务专属输出目录，并建立 figures、code、data、results 四个子文件夹。
# 2. Read the cleaned bird new-record dataset and species centroid traits.
#    读取清洗后的鸟类新纪录数据和物种分布中心性状数据。
# 3. Compute the relative direction of each new record compared with the
#    known distribution centroid of each species, then classify it into
#    eight compass sectors.
#    计算每条新纪录相对于物种已知分布中心的方向，并划分为八个方位。
# 4. Summarize directional counts and proportions for all bird new records
#    and for each order.
#    统计全部鸟类新纪录以及各目在八个方向上的数量和比例。
# 5. Recreate publication-style directional figures in two forms:
#    radar-style plots and wind-rose-style plots.
#    以两种形式重建发表风格方向图：雷达图和风玫瑰图。
# 6. Export main 4x4 order-panel figures, top-order overlay figures, and
#    individual order-specific plots in PNG, PDF, and PPTX formats.
#    导出 4x4 主图、主要目叠加图及各目单独图，统一输出为 PNG、PDF、PPTX。
# 7. Save cleaned plotting tables and write a bilingual result summary.
#    保存整理后的作图数据表，并输出中英文结果描述摘要。
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(ggradar)
  library(patchwork)
  library(tibble)
  library(officer)
  library(rvg)
  library(stringr)
})

# -------------------------------
# Step 0. Define task directories
# 第 0 步：定义任务目录
# -------------------------------
master_output_root <- "/Users/dingchenchen/Documents/New records/bird_new_records_R_output"
task_dir <- file.path(master_output_root, "tasks", "bird_directional_windrose_radar")

figures_dir <- file.path(task_dir, "figures")
figures_overall_dir <- file.path(figures_dir, "overall")
figures_combined_dir <- file.path(figures_dir, "combined")
figures_radar_order_dir <- file.path(figures_dir, "radar_by_order")
figures_windrose_order_dir <- file.path(figures_dir, "windrose_by_order")
code_dir <- file.path(task_dir, "code")
data_dir <- file.path(task_dir, "data")
results_dir <- file.path(task_dir, "results")

clean_path <- file.path(master_output_root, "data_clean", "bird_new_records_clean.csv")
traits_path <- file.path(master_output_root, "data_clean", "bird_species_pool_with_traits.csv")

if (!file.exists(clean_path)) stop("Missing clean bird dataset: ", clean_path)
if (!file.exists(traits_path)) stop("Missing bird trait dataset: ", traits_path)

direction_levels <- c(
  "North", "Northeast", "East", "Southeast",
  "South", "Southwest", "West", "Northwest"
)

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
  ppt <- ph_with(ppt, dml(ggobj = plot_obj), location = ph_location_fullsize())
  print(ppt, target = pptx_path)
}

sanitize_filename <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "") %>%
    tolower()
}

build_header_strip <- function(title_text) {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "#D9D9D9", color = "#6B6B6B", linewidth = 0.5) +
    annotate("text", x = 0.5, y = 0.5, label = title_text,
             family = "sans", fontface = "bold", size = 4.1, color = "#222222") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void()
}

compose_with_strip <- function(plot_obj, title_text, strip_height = 0.12) {
  build_header_strip(title_text) / plot_obj + plot_layout(heights = c(strip_height, 1))
}

repo_six_colors <- c("#00bfc4", "#be84db", "#f8766d", "#7ad151", "#f1b722", "#619cff")
extended_colors <- c(
  "#F29FB7", "#F4B183", "#C4A46B", "#B7C36B", "#9DCC8A",
  "#78C8A0", "#5FCFCF", "#7FB3FF", "#C3A4FF", "#E2A9E5"
)

build_radar_core <- function(radar_df, color_value, alpha_fill = 0.55) {
  ggradar(
    radar_df,
    grid.min = 0,
    grid.mid = 50,
    grid.max = 100,
    values.radar = c("0%", "50%", "100%"),
    group.line.width = 1.15,
    group.point.size = 2.15,
    background.circle.colour = "#F7FBFD",
    gridline.mid.colour = "#79CBE3",
    gridline.max.colour = "#79CBE3",
    gridline.min.colour = "#79CBE3",
    axis.label.size = 2.8,
    grid.label.size = 2.1,
    legend.position = "none",
    group.colours = alpha(color_value, alpha_fill)
  ) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(3, 3, 3, 3)
    )
}

build_windrose_core <- function(plot_df, color_value, show_axis_labels = TRUE) {
  ymax <- max(plot_df$count, na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1

  ggplot(plot_df, aes(x = direction, y = count, group = 1)) +
    geom_polygon(fill = alpha(color_value, 0.26), color = color_value, linewidth = 0.92) +
    geom_line(color = color_value, linewidth = 0.92) +
    geom_point(color = color_value, size = 2.0) +
    annotate("segment", x = 1:8, xend = 1:8, y = 0, yend = ymax,
             color = "#707070", linewidth = 0.32) +
    scale_y_continuous(
      limits = c(0, ymax),
      breaks = c(ymax * 0.5, ymax),
      labels = function(x) paste0(round(100 * x / ymax), "%")
    ) +
    coord_polar(start = -pi / 8) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "#79CBE3", linewidth = 0.55, linetype = "22"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 7.0, color = "#6B6B6B"),
      axis.text.x = if (show_axis_labels) element_text(size = 8.5, color = "#222222") else element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(3, 3, 3, 3)
    )
}

# -------------------------------
# Step 2. Read source datasets
# 第 2 步：读取源数据
# -------------------------------
clean <- read_csv(clean_path, show_col_types = FALSE) %>%
  transmute(
    species = species,
    order = order,
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

traits <- read_csv(traits_path, show_col_types = FALSE) %>%
  transmute(
    species = species,
    centroid_latitude = as.numeric(centroid_latitude),
    centroid_longitude = as.numeric(centroid_longitude)
  )

# -------------------------------
# Step 3. Compute direction for each species-level new record
# 第 3 步：计算每个物种层面新纪录的方向
# -------------------------------
direction_species <- clean %>%
  left_join(traits, by = "species") %>%
  filter(
    !is.na(species),
    !is.na(order),
    !is.na(longitude),
    !is.na(latitude),
    !is.na(centroid_latitude),
    !is.na(centroid_longitude)
  ) %>%
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

# -------------------------------
# Step 4. Summarize overall and order-level direction data
# 第 4 步：汇总总体与按目方向数据
# -------------------------------
overall_counts <- direction_species %>%
  count(direction, name = "count") %>%
  complete(direction = direction_levels, fill = list(count = 0)) %>%
  mutate(
    proportion = count / sum(count),
    direction = factor(direction, levels = direction_levels)
  )

order_totals <- direction_species %>%
  count(order, name = "n_total") %>%
  arrange(desc(n_total))

order_counts <- direction_species %>%
  count(order, direction, name = "count") %>%
  complete(order, direction = direction_levels, fill = list(count = 0)) %>%
  left_join(order_totals, by = "order") %>%
  mutate(
    proportion = if_else(n_total > 0, count / n_total, 0),
    direction = factor(direction, levels = direction_levels)
  )

selected_orders <- order_totals %>%
  slice_head(n = 16) %>%
  pull(order)

top_orders <- order_totals %>%
  slice_head(n = 6) %>%
  pull(order)

main_palette <- c(repo_six_colors, extended_colors)[seq_along(selected_orders)]
order_palette <- setNames(main_palette, selected_orders)

# -------------------------------
# Step 5. Save cleaned task data tables
# 第 5 步：保存任务数据表
# -------------------------------
write_csv(direction_species, file.path(data_dir, "bird_direction_species_level.csv"))
write_csv(overall_counts, file.path(data_dir, "bird_direction_overall_counts.csv"))
write_csv(order_counts, file.path(data_dir, "bird_direction_order_counts.csv"))
write_csv(
  tibble(order = selected_orders, color = unname(order_palette[selected_orders])),
  file.path(data_dir, "bird_direction_order_palette.csv")
)
write_csv(
  tibble(order = selected_orders, rank = seq_along(selected_orders)),
  file.path(data_dir, "bird_direction_selected_orders.csv")
)

top_direction_by_order <- order_counts %>%
  group_by(order) %>%
  slice_max(order_by = count, n = 1, with_ties = TRUE) %>%
  ungroup() %>%
  arrange(desc(n_total), order, direction)

write_csv(top_direction_by_order, file.path(data_dir, "bird_direction_top_direction_by_order.csv"))

# -------------------------------
# Step 6. Build and export main 4x4 order-panel figures
# 第 6 步：构建并导出 4x4 按目主图
# -------------------------------
radar_panels_main <- lapply(selected_orders, function(ord) {
  radar_df <- order_counts %>%
    filter(order == ord) %>%
    select(direction, proportion) %>%
    pivot_wider(names_from = direction, values_from = proportion, values_fill = 0) %>%
    mutate(across(where(is.numeric), ~ . * 100)) %>%
    mutate(group = ord) %>%
    select(group, all_of(direction_levels))

  p <- build_radar_core(radar_df, color_value = order_palette[[ord]], alpha_fill = 0.52)
  compose_with_strip(p, ord, strip_height = 0.15)
})

windrose_panels_main <- lapply(selected_orders, function(ord) {
  windrose_df <- order_counts %>% filter(order == ord)
  p <- build_windrose_core(windrose_df, color_value = order_palette[[ord]])
  compose_with_strip(p, ord, strip_height = 0.15)
})

main_radar_4x4 <- wrap_plots(radar_panels_main, ncol = 4)
main_windrose_4x4 <- wrap_plots(windrose_panels_main, ncol = 4)

save_plot_bundle(main_radar_4x4, figures_overall_dir, "overall_direction_radar", width = 14.6, height = 14.6)
save_plot_bundle(main_windrose_4x4, figures_overall_dir, "overall_direction_windrose", width = 14.6, height = 14.6)

# -------------------------------
# Step 7. Build and export top-order overlay figures
# 第 7 步：构建并导出主要目叠加图
# -------------------------------
top6_radar_df <- order_counts %>%
  filter(order %in% top_orders) %>%
  select(order, direction, proportion) %>%
  pivot_wider(names_from = direction, values_from = proportion, values_fill = 0) %>%
  mutate(across(where(is.numeric), ~ . * 100)) %>%
  rename(group = order) %>%
  select(group, all_of(direction_levels))

p_top6_radar <- ggradar(
  top6_radar_df,
  grid.min = 0,
  grid.mid = 50,
  grid.max = 100,
  values.radar = c("0%", "50%", "100%"),
  group.line.width = 1.2,
  group.point.size = 2.4,
  background.circle.colour = "white",
  gridline.mid.colour = "#79CBE3",
  gridline.max.colour = "#79CBE3",
  gridline.min.colour = "#79CBE3",
  axis.label.size = 3.2,
  grid.label.size = 2.5,
  legend.position = "right",
  group.colours = alpha(order_palette[top_orders], 0.72)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 11.5),
    plot.margin = margin(8, 8, 8, 8)
  )

overlay_df <- order_counts %>%
  filter(order %in% top_orders) %>%
  mutate(order = factor(order, levels = top_orders))

overlay_ymax <- max(overlay_df$count) * 1.06

p_top6_windrose <- ggplot(
  overlay_df,
  aes(x = direction, y = count, group = order, color = order, fill = order)
) +
  geom_polygon(alpha = 0.24, linewidth = 0.82) +
  geom_line(linewidth = 0.92) +
  geom_point(size = 2.3) +
  annotate("segment", x = 1:8, xend = 1:8, y = 0, yend = overlay_ymax,
           color = "#4E4E4E", linewidth = 0.42) +
  scale_color_manual(values = order_palette[top_orders]) +
  scale_fill_manual(values = order_palette[top_orders]) +
  coord_polar(start = -pi / 8) +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "#79CBE3", linewidth = 0.55, linetype = "22"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 11.2, color = "#222222"),
    legend.position = "right",
    legend.text = element_text(size = 11.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

save_plot_bundle(p_top6_radar, figures_combined_dir, "top6_direction_radar_overlay", width = 12.8, height = 8.6)
save_plot_bundle(p_top6_windrose, figures_combined_dir, "top6_direction_windrose_overlay", width = 12.8, height = 8.6)

# -------------------------------
# Step 8. Export individual order-specific radar and wind-rose plots
# 第 8 步：导出各目单独雷达图与风玫瑰图
# -------------------------------
for (ord in selected_orders) {
  ord_slug <- sanitize_filename(ord)

  radar_df <- order_counts %>%
    filter(order == ord) %>%
    select(direction, proportion) %>%
    pivot_wider(names_from = direction, values_from = proportion, values_fill = 0) %>%
    mutate(across(where(is.numeric), ~ . * 100)) %>%
    mutate(group = ord) %>%
    select(group, all_of(direction_levels))

  radar_plot <- build_radar_core(radar_df, color_value = order_palette[[ord]], alpha_fill = 0.52)
  radar_plot <- compose_with_strip(radar_plot, ord, strip_height = 0.15)
  save_plot_bundle(radar_plot, figures_radar_order_dir, paste0(ord_slug, "_direction_radar"), width = 4.7, height = 4.4)

  windrose_df <- order_counts %>% filter(order == ord)
  windrose_plot <- build_windrose_core(windrose_df, color_value = order_palette[[ord]])
  windrose_plot <- compose_with_strip(windrose_plot, ord, strip_height = 0.15)
  save_plot_bundle(windrose_plot, figures_windrose_order_dir, paste0(ord_slug, "_direction_windrose"), width = 4.7, height = 4.6)
}

# -------------------------------
# Step 9. Write a bilingual result summary
# 第 9 步：输出中英文结果摘要
# -------------------------------
overall_total <- sum(overall_counts$count)
overall_top <- overall_counts %>% arrange(desc(count)) %>% slice_head(n = 3)
dropped_orders <- setdiff(order_totals$order, selected_orders)

summary_lines <- c(
  "# Bird directional plots: result summary",
  "# 鸟类新纪录方向图：结果摘要",
  "",
  "## Overview / 总体概况",
  paste0("- Total bird new-record species used in directional analysis: ", overall_total, "."),
  paste0("- 纳入方向分析的鸟类新纪录物种总数：", overall_total, "。"),
  paste0("- Main 4x4 figures include the top ", length(selected_orders), " orders ranked by species counts."),
  paste0("- 4x4 主图纳入新纪录物种数排名前 ", length(selected_orders), " 的鸟类目。"),
  "",
  "## Overall dominant directions / 总体优势方向",
  paste0("- Top overall directions: ",
         paste0(overall_top$direction, " (", overall_top$count, ", ",
                percent(overall_top$proportion, accuracy = 0.1), ")", collapse = "; "),
         "."),
  paste0("- 总体上占比最高的方向为：",
         paste0(overall_top$direction, "（", overall_top$count, "，",
                percent(overall_top$proportion, accuracy = 0.1), "）", collapse = "；"),
         "。"),
  "",
  "## Included and omitted orders / 纳入与舍弃的目",
  paste0("- Included orders: ", paste(selected_orders, collapse = ", "), "."),
  paste0("- Dropped small-count orders: ", if (length(dropped_orders) > 0) paste(dropped_orders, collapse = ", ") else "None", "."),
  paste0("- 纳入的目：", paste(selected_orders, collapse = "、"), "。"),
  paste0("- 舍弃的小样本目：", if (length(dropped_orders) > 0) paste(dropped_orders, collapse = "、") else "无", "。"),
  "",
  "## Output structure / 输出结构",
  "- `figures/overall`: main 4x4 order-panel figures used as the primary directional summary.",
  "- `figures/combined`: top-order overlay comparison figures.",
  "- `figures/radar_by_order` and `figures/windrose_by_order`: individual order-specific plots.",
  "- `figures/overall`：4x4 按目主图。",
  "- `figures/combined`：主要目叠加对比图。",
  "- `figures/radar_by_order` 和 `figures/windrose_by_order`：各目单独图。"
)

writeLines(summary_lines, file.path(results_dir, "directional_results_summary.md"))

cat("Directional task bundle exported successfully.\n")
cat("Task folder: ", task_dir, "\n", sep = "")
