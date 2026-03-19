#!/usr/bin/env Rscript

# ============================================================
# Bird new-record Sankey diagram (English version)
# 鸟类新纪录桑基图（英文版）：Order -> Province -> Year
# ============================================================
#
# Analysis steps / 分析步骤
# 1. Read the cleaned bird new-record dataset.
#    读取已经标准化清洗好的鸟类新纪录数据。
# 2. Keep the four fields required for the Sankey workflow:
#    species, order, province, and year.
#    保留桑基图分析所需的四个核心字段：物种、目、省份和年份。
# 3. Remove incomplete records and deduplicate repeated species-order-
#    province-year combinations.
#    去除缺失记录，并对重复的“物种-目-省份-年份”组合去重。
# 4. Summarize the data into Order -> Province -> Year flows.
#    将数据汇总为“目 -> 省份 -> 年份”的流向结构。
# 5. Rank orders and provinces so the Sankey layout is more readable.
#    按记录数对目和省份排序，以提高桑基图可读性。
# 6. Draw a publication-style English Sankey figure with enlarged labels.
#    绘制英文发表风格桑基图，并放大目/省份/年份标签字体。
# 7. Export the figure in PNG, PDF, and PPTX formats.
#    同步导出 PNG、PDF 和 PPTX 三种格式，便于论文和汇报使用。
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(ggalluvial)
  library(stringr)
  library(officer)
  library(rvg)
})

# -------------------------------
# Step 0. Define paths and outputs
# 第 0 步：定义输入输出路径
# -------------------------------
output_root <- "/Users/dingchenchen/Documents/New project/bird_new_records_R_output"
tables_dir <- file.path(output_root, "tables")
figures_dir <- file.path(output_root, "figures")

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

clean_path <- file.path(output_root, "data_clean", "bird_new_records_clean.csv")
if (!file.exists(clean_path)) {
  stop("Clean bird dataset not found: ", clean_path)
}

# -------------------------------
# Step 1. Read cleaned bird data
# 第 1 步：读取清洗后的鸟类数据
# -------------------------------
clean <- readr::read_csv(clean_path, show_col_types = FALSE) %>%
  transmute(
    species = species,
    order = order,
    province = province,
    year = as.integer(year)
  ) %>%
  filter(!is.na(species), !is.na(order), !is.na(province), !is.na(year)) %>%
  distinct(species, order, province, year)

# -------------------------------
# Step 2. Rank orders and provinces
# 第 2 步：按记录数排序目和省份
# -------------------------------
order_rank <- clean %>%
  count(order, name = "n_records") %>%
  arrange(desc(n_records))

province_rank <- clean %>%
  count(province, name = "n_records") %>%
  arrange(desc(n_records))

# -------------------------------
# Step 3. Summarize Sankey flow table
# 第 3 步：汇总桑基图流向数据表
# -------------------------------
sankey_df <- clean %>%
  count(order, province, year, name = "n_records") %>%
  mutate(
    order = factor(order, levels = order_rank$order),
    province = factor(province, levels = province_rank$province),
    year = factor(year, levels = sort(unique(year)))
  )

write_csv(
  sankey_df,
  file.path(tables_dir, "bird_sankey_order_province_year_en_v2.csv")
)

# -------------------------------
# Step 4. Define a publication palette
# 第 4 步：定义发表风格配色方案
# -------------------------------
base_palette <- c(
  "Passeriformes" = "#8FA8D6",
  "Charadriiformes" = "#F28E5B",
  "Anseriformes" = "#67C1B3",
  "Accipitriformes" = "#E78AC3",
  "Pelecaniformes" = "#8BC34A",
  "Gruiformes" = "#D9B26F",
  "Columbiformes" = "#9E9E9E",
  "Galliformes" = "#F1C40F",
  "Strigiformes" = "#B497D6",
  "Coraciiformes" = "#6FA8DC"
)

all_orders <- levels(sankey_df$order)
missing_orders <- setdiff(all_orders, names(base_palette))
if (length(missing_orders) > 0) {
  extra_palette <- setNames(grDevices::hcl.colors(length(missing_orders), "Set 3"), missing_orders)
  sankey_palette <- c(base_palette, extra_palette)
} else {
  sankey_palette <- base_palette
}

# -------------------------------
# Step 5. Build the Sankey figure
# 第 5 步：绘制桑基图
# -------------------------------
# Note:
# Increase stratum label size by about two points compared with the previous version.
# 说明：
# 相比上一版，将“目-省份-年份”节点标签整体放大约两号。
p <- ggplot(
  sankey_df,
  aes(axis1 = order, axis2 = province, axis3 = year, y = n_records)
) +
  geom_alluvium(aes(fill = order), width = 0.14, alpha = 0.68, knot.pos = 0.38) +
  geom_stratum(width = 0.14, fill = "#E9E9E9", color = "#707070", linewidth = 0.42) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 4.2,
    color = "black",
    family = "sans"
  ) +
  scale_fill_manual(values = sankey_palette, guide = "none") +
  scale_x_discrete(
    limits = c("Order", "Province", "Year"),
    expand = c(0.035, 0.035)
  ) +
  labs(
    x = NULL,
    y = "Number of records"
  ) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12.5, color = "#303030"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold", size = 12.5),
    panel.grid = element_blank(),
    plot.margin = margin(12, 18, 12, 18)
  )

# -------------------------------
# Step 6. Export figure in multiple formats
# 第 6 步：多格式导出图件
# -------------------------------
save_plot_bundle <- function(plot_obj, filename_no_ext, width = 22, height = 12, dpi = 420) {
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
  filename_no_ext = "fig_ref01_sankey_order_province_year_en_v2",
  width = 22,
  height = 12
)

cat("English Sankey figure exported in PNG, PDF, and PPTX formats.\n")
