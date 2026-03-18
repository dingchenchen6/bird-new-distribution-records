#!/usr/bin/env Rscript

# ============================================================
# Figure (a): Binomial expectation plot for bird new-record orders
# 图 (a)：鸟类新纪录各目非随机性二项分布期望图
# ============================================================
#
# Analysis steps / 分析步骤
# 1. Read the standardized bird order summary table.
#    读取标准化后的鸟类按目汇总表。
# 2. Calculate the overall expected proportion of newly recorded species
#    among the full national bird species pool.
#    计算全国鸟类物种池中新纪录物种的总体期望比例。
# 3. Build exact 95% binomial confidence envelopes across different
#    order sizes (number of species in each order).
#    根据不同目的总物种数，构建精确二项分布 95% 置信区间包络线。
# 4. Classify each order as above, below, or within the 95% CI.
#    判定每个目落在 95% CI 之上、之下或区间内。
# 5. Recreate the publication-style panel with shaded significance regions
#    and labeled order points.
#    复刻带显著性阴影区和目标签的发表风格图版。
# 6. Export the figure as PNG, PDF, and PPTX, and save plotting data as CSV.
#    同步导出 PNG、PDF、PPTX，并保存作图数据为 CSV。
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(stringr)
  library(officer)
  library(rvg)
})

# -------------------------------
# Step 0. Define input and output paths
# 第 0 步：定义输入输出路径
# -------------------------------
project_root <- "/Users/dingchenchen/Documents/New project"
output_root <- file.path(project_root, "bird_new_records_R_output")
tables_dir <- file.path(output_root, "tables")
figures_dir <- file.path(output_root, "figures")

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

summary_path <- file.path(tables_dir, "table_order_summary_bird_new_records_numeric.csv")
if (!file.exists(summary_path)) {
  stop("Required order summary file not found: ", summary_path)
}

# -------------------------------
# Step 1. Read order-level summary data
# 第 1 步：读取按目汇总数据
# -------------------------------
order_df <- readr::read_csv(summary_path, show_col_types = FALSE) %>%
  filter(order != "Total") %>%
  transmute(
    order = order,
    n_new_species = as.numeric(n_new_species),
    n_species_total_order = as.numeric(n_species_total_order),
    prop_new = as.numeric(prop_to_total_species_order)
  ) %>%
  filter(!is.na(order), !is.na(n_new_species), !is.na(n_species_total_order), n_species_total_order > 0)

total_new_species <- sum(order_df$n_new_species, na.rm = TRUE)
total_species_pool <- sum(order_df$n_species_total_order, na.rm = TRUE)
expected_prop <- total_new_species / total_species_pool

# -------------------------------
# Step 2. Build 95% exact binomial envelopes
# 第 2 步：构建 95% 精确二项分布包络
# -------------------------------
max_species <- max(order_df$n_species_total_order, na.rm = TRUE)

envelope_df <- tibble(
  n_species_total_order = 1:max_species
) %>%
  mutate(
    lower_count = qbinom(0.025, n_species_total_order, expected_prop),
    upper_count = qbinom(0.975, n_species_total_order, expected_prop),
    lower_prop = lower_count / n_species_total_order,
    upper_prop = upper_count / n_species_total_order
  )

# -------------------------------
# Step 3. Classify each order relative to the envelope
# 第 3 步：判断各目相对包络区间的位置
# -------------------------------
order_plot_df <- order_df %>%
  left_join(
    envelope_df %>% select(n_species_total_order, lower_prop, upper_prop),
    by = "n_species_total_order"
  ) %>%
  mutate(
    position_status = case_when(
      prop_new >= upper_prop ~ "Above 95% CI",
      prop_new <= lower_prop ~ "Below 95% CI",
      TRUE ~ "Within 95% CI"
    ),
    label = str_to_upper(order)
  )

write_csv(order_plot_df, file.path(tables_dir, "bird_fig_a_order_binomial_plot_data.csv"))
write_csv(envelope_df, file.path(tables_dir, "bird_fig_a_order_binomial_envelope.csv"))

# -------------------------------
# Step 4. Draw the publication-style figure
# 第 4 步：绘制发表风格图件
# -------------------------------
point_palette <- c(
  "Above 95% CI" = "black",
  "Within 95% CI" = "#7A7A7A",
  "Below 95% CI" = "#7A7A7A"
)

fill_palette <- c(
  "Above 95% CI" = "#E53935",
  "Below 95% CI" = "#87A6D0"
)

p <- ggplot() +
  geom_ribbon(
    data = envelope_df,
    aes(x = n_species_total_order, ymin = upper_prop, ymax = 1, fill = "Above 95% CI"),
    alpha = 0.60
  ) +
  geom_ribbon(
    data = envelope_df,
    aes(x = n_species_total_order, ymin = 0, ymax = lower_prop, fill = "Below 95% CI"),
    alpha = 0.78
  ) +
  geom_hline(yintercept = expected_prop, linewidth = 0.8, color = "black") +
  geom_point(
    data = order_plot_df,
    aes(x = n_species_total_order, y = prop_new, color = position_status),
    size = 3.3
  ) +
  ggrepel::geom_text_repel(
    data = order_plot_df,
    aes(x = n_species_total_order, y = prop_new, label = label, color = position_status),
    size = 5.2,
    family = "sans",
    fontface = "plain",
    box.padding = 0.24,
    point.padding = 0.08,
    segment.color = NA,
    max.overlaps = Inf,
    seed = 1234
  ) +
  annotate("text", x = 4, y = 1.08, label = "(a)", hjust = 0, vjust = 1, size = 10) +
  scale_color_manual(values = point_palette, breaks = c("Above 95% CI", "Within 95% CI")) +
  scale_fill_manual(values = fill_palette, breaks = c("Above 95% CI", "Below 95% CI")) +
  scale_x_continuous(
    limits = c(0, max_species * 1.05),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1.1),
    labels = number_format(accuracy = 0.01)
  ) +
  guides(
    color = guide_legend(title = NULL, override.aes = list(size = 4)),
    fill = guide_legend(title = NULL, override.aes = list(color = NA))
  ) +
  labs(
    x = "Number of species in order",
    y = "Proportion of newly recorded species"
  ) +
  theme_classic(base_size = 14, base_family = "sans") +
  theme(
    legend.position = c(0.60, 0.93),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 15),
    axis.text = element_text(color = "black", size = 13),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    plot.margin = margin(12, 20, 12, 12)
  )

# -------------------------------
# Step 5. Export figure bundle
# 第 5 步：多格式导出图件
# -------------------------------
save_plot_bundle <- function(plot_obj, filename_no_ext, width = 8.6, height = 8.2, dpi = 420) {
  png_path <- file.path(figures_dir, paste0(filename_no_ext, ".png"))
  pdf_path <- file.path(figures_dir, paste0(filename_no_ext, ".pdf"))
  pptx_path <- file.path(figures_dir, paste0(filename_no_ext, ".pptx"))

  ggsave(
    filename = png_path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )

  ggsave(
    filename = pdf_path,
    plot = plot_obj,
    width = width,
    height = height,
    bg = "white",
    device = cairo_pdf
  )

  ppt <- officer::read_pptx()
  ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- officer::ph_with(
    x = ppt,
    value = rvg::dml(ggobj = plot_obj),
    location = officer::ph_location_fullsize()
  )
  print(ppt, target = pptx_path)
}

save_plot_bundle(
  plot_obj = p,
  filename_no_ext = "fig_ref04_order_binomial_pattern_a",
  width = 8.8,
  height = 8.4
)

cat("Figure (a) exported for bird order binomial pattern.\n")
