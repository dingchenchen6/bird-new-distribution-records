#!/usr/bin/env Rscript

# ============================================================
# Bird new-record Sankey diagram (compact English version)
# 鸟类新纪录桑基图（紧凑英文版）：Order -> Province -> Year
# ============================================================
#
# Scientific question / 科学问题
# How can the bird new-record Sankey diagram be made more compact in the
# horizontal direction while retaining all orders, all provinces, and all years?
# 如何在保留全部目、全部省份和全部年份的前提下，让鸟类新纪录桑基图在横向上更紧凑？
#
# Objective / 研究目标
# 1. Rebuild a narrower Sankey layout for Order -> Province -> Year.
#    重建更紧凑的“目 -> 省份 -> 年份”桑基图。
# 2. Preserve the previous version and export the compact version as a new file set.
#    保留旧版本，并将紧凑版输出为全新的文件集。
# 3. Save the plot-ready data and editable PPTX for downstream revision.
#    保存作图数据和可编辑 PPTX，便于后续修改。
#
# Analytical idea / 分析思路
# 1. Read the cleaned bird new-record table.
#    读取标准化清洗后的鸟类新纪录数据表。
# 2. Keep the variables needed by the Sankey diagram and remove incomplete rows.
#    保留桑基图所需字段，并去除缺失记录。
# 3. Deduplicate repeated species-order-province-year combinations.
#    对重复的物种-目-省份-年份组合进行去重。
# 4. Aggregate Order -> Province -> Year flows and rank nodes by abundance.
#    汇总“目 -> 省份 -> 年份”流向，并按记录数排序节点。
# 5. Tighten the horizontal visual layout by reducing exported width, reducing
#    x-axis expansion, and slightly widening strata to shorten perceived gaps.
#    通过减小导出画布宽度、减小 x 轴扩展、略微增大节点宽度，压缩三层之间的视觉间距。
# 6. Export PNG, PDF, and PPTX while keeping the previous version unchanged.
#    在不覆盖旧版本的前提下导出 PNG、PDF 和 PPTX。
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(ggalluvial)
  library(officer)
  library(export)
})

# -------------------------------
# Step 0. Define task paths
# 第 0 步：定义任务路径
# -------------------------------
master_output_root <- "/Users/dingchenchen/Documents/New records/bird_new_records_R_output"
task_dir <- file.path(master_output_root, "tasks", "bird_sankey_order_province_year")
figures_dir <- file.path(task_dir, "figures")
data_dir <- file.path(task_dir, "data")
code_dir <- file.path(task_dir, "code")
results_dir <- file.path(task_dir, "results")
clean_path <- file.path(master_output_root, "data_clean", "bird_new_records_clean.csv")

for (dir_path in c(figures_dir, data_dir, code_dir, results_dir)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

if (!file.exists(clean_path)) {
  stop("Clean bird dataset not found: ", clean_path)
}

# -------------------------------
# Step 1. Read and screen the cleaned dataset
# 第 1 步：读取并筛查清洗后的数据
# -------------------------------
# Keep only the fields needed by the Sankey workflow and remove incomplete rows.
# 仅保留桑基图所需字段，并删除不完整记录。
clean <- read_csv(clean_path, show_col_types = FALSE) %>%
  transmute(
    species = species,
    order = order,
    province = province,
    year = as.integer(year)
  ) %>%
  filter(!is.na(species), !is.na(order), !is.na(province), !is.na(year)) %>%
  distinct(species, order, province, year)

# -------------------------------
# Step 2. Data checking and ranking
# 第 2 步：数据检查与节点排序
# -------------------------------
# Record a small diagnostic summary so the compact version has a traceable data basis.
# 保存简要诊断信息，确保紧凑版也有可追踪的数据基础。
diagnostic_summary <- tibble::tibble(
  metric = c("n_unique_species_order_province_year", "n_orders", "n_provinces", "year_min", "year_max"),
  value = c(
    nrow(clean),
    dplyr::n_distinct(clean$order),
    dplyr::n_distinct(clean$province),
    min(clean$year, na.rm = TRUE),
    max(clean$year, na.rm = TRUE)
  )
)

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

write_csv(sankey_df, file.path(data_dir, "bird_sankey_order_province_year_en_compact_v3.csv"))
write_csv(diagnostic_summary, file.path(data_dir, "bird_sankey_order_province_year_en_compact_v3_diagnostics.csv"))

# -------------------------------
# Step 4. Define the order palette
# 第 4 步：定义目级配色
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
# Step 5. Build a more compact Sankey figure
# 第 5 步：绘制更紧凑的桑基图
# -------------------------------
# Key compacting choices:
# 1. Reduce x-axis outer expansion.
# 2. Slightly increase stratum/alluvium width.
# 3. Export using a narrower figure width while preserving height.
# 紧凑化的关键设置：
# 1. 减小 x 轴两侧留白；
# 2. 略微增大节点与流带宽度；
# 3. 在保持高度的同时缩小导出宽度。
p_compact <- ggplot(
  sankey_df,
  aes(axis1 = order, axis2 = province, axis3 = year, y = n_records)
) +
  geom_alluvium(aes(fill = order), width = 0.18, alpha = 0.70, knot.pos = 0.42) +
  geom_stratum(width = 0.18, fill = "#E9E9E9", color = "#707070", linewidth = 0.42) +
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
    expand = c(0.008, 0.008)
  ) +
  labs(x = NULL, y = "Number of records") +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12.5, color = "#303030"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold", size = 12.5),
    panel.grid = element_blank(),
    plot.margin = margin(12, 8, 12, 8)
  )

# -------------------------------
# Step 6. Export figure in multiple formats
# 第 6 步：多格式导出图件
# -------------------------------
save_plot_bundle <- function(plot_obj, filename_no_ext, width = 17.5, height = 12, dpi = 420) {
  png_path <- file.path(figures_dir, paste0(filename_no_ext, ".png"))
  pdf_path <- file.path(figures_dir, paste0(filename_no_ext, ".pdf"))
  pptx_base <- file.path(figures_dir, filename_no_ext)

  ggsave(filename = png_path, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(filename = pdf_path, plot = plot_obj, width = width, height = height, bg = "white", device = cairo_pdf)
  export::graph2ppt(x = plot_obj, file = pptx_base, width = width, height = height, append = FALSE, vector.graphic = TRUE)
}

save_plot_bundle(
  plot_obj = p_compact,
  filename_no_ext = "fig_ref01_sankey_order_province_year_en_compact_v3",
  width = 17.5,
  height = 12
)

# -------------------------------
# Step 7. Write a short task summary
# 第 7 步：输出简要结果说明
# -------------------------------
summary_lines <- c(
  "# Compact Sankey Summary",
  "",
  "This compact version keeps all orders, provinces, and years unchanged.",
  "Compared with the previous version, the horizontal layout was tightened by reducing export width and outer x-padding.",
  "",
  paste0("Total flows: ", nrow(sankey_df)),
  paste0("Orders: ", dplyr::n_distinct(sankey_df$order)),
  paste0("Provinces: ", dplyr::n_distinct(sankey_df$province)),
  paste0("Years: ", dplyr::n_distinct(sankey_df$year))
)
writeLines(summary_lines, file.path(results_dir, "task_summary_compact_v3.md"))

cat("Compact English Sankey figure exported in PNG, PDF, and PPTX formats.\n")
