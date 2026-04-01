#!/usr/bin/env Rscript

# ============================================================
# Corrected Sankey diagram: Order -> Province -> Year
# 校正后桑基图：Order -> Province -> Year
# ============================================================
#
# Objective / 研究目标
# Rebuild the publication-style Sankey diagram using the corrected canonical
# event table after synonym resolution and province-level duplicate removal.
# 用完成同物异名校正和省级重复去重后的标准事件表重建发表风格桑基图。
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(ggalluvial)
  library(stringr)
  library(officer)
  library(rvg)
})

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    candidate <- sub("^--file=", "", file_arg[1])
    if (file.exists(candidate)) return(normalizePath(candidate))
  }
  normalizePath(getwd())
}

script_path <- get_script_path()
code_dir <- if (dir.exists(script_path)) script_path else dirname(script_path)
task_root <- Sys.getenv(
  "BIRD_REANALYSIS_TASK_ROOT",
  unset = "/Users/dingchenchen/Documents/New records/bird-new-distribution-records/tasks/bird_identity_synonym_dedup_reanalysis"
)
subtask_root <- file.path(task_root, "sankey_corrected")
data_dir <- file.path(subtask_root, "data")
fig_dir <- file.path(subtask_root, "figures")
results_dir <- file.path(subtask_root, "results")
for (dir_path in c(data_dir, fig_dir, results_dir)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

clean_path <- file.path(task_root, "data", "bird_new_records_clean_corrected.csv")
if (!file.exists(clean_path)) stop("Missing corrected clean dataset: ", clean_path)

save_plot_bundle <- function(plot_obj, filename_no_ext, width = 18, height = 10, dpi = 420) {
  png_path <- file.path(fig_dir, paste0(filename_no_ext, ".png"))
  pdf_path <- file.path(fig_dir, paste0(filename_no_ext, ".pdf"))
  pptx_path <- file.path(fig_dir, paste0(filename_no_ext, ".pptx"))

  ggsave(filename = png_path, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(filename = pdf_path, plot = plot_obj, width = width, height = height, bg = "white", device = grDevices::pdf)

  ppt <- officer::read_pptx()
  ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
  ppt <- officer::ph_with(ppt, value = rvg::dml(ggobj = plot_obj), location = officer::ph_location_fullsize())
  print(ppt, target = pptx_path)
}

clean <- readr::read_csv(clean_path, show_col_types = FALSE) %>%
  transmute(
    species = species,
    order = order,
    province = province,
    year = as.integer(year)
  ) %>%
  filter(!is.na(species), !is.na(order), !is.na(province), !is.na(year)) %>%
  distinct(species, order, province, year)

order_rank <- clean %>% count(order, name = "n_records") %>% arrange(desc(n_records))
province_rank <- clean %>% count(province, name = "n_records") %>% arrange(desc(n_records))

sankey_df <- clean %>%
  count(order, province, year, name = "n_records") %>%
  mutate(
    order = factor(order, levels = order_rank$order),
    province = factor(province, levels = province_rank$province),
    year = factor(year, levels = sort(unique(year)))
  )

write_csv(sankey_df, file.path(data_dir, "bird_sankey_order_province_year_corrected.csv"))

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
sankey_palette <- c(base_palette, setNames(grDevices::hcl.colors(length(missing_orders), "Set 3"), missing_orders))

p <- ggplot(sankey_df, aes(axis1 = order, axis2 = province, axis3 = year, y = n_records)) +
  geom_alluvium(aes(fill = order), width = 0.13, alpha = 0.72, knot.pos = 0.38) +
  geom_stratum(width = 0.13, fill = "#ECECEC", color = "#737373", linewidth = 0.42) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4.0, color = "black", family = "sans") +
  scale_fill_manual(values = sankey_palette, guide = "none") +
  scale_x_discrete(limits = c("Order", "Province", "Year"), expand = c(0.02, 0.02)) +
  labs(x = NULL, y = "Number of records") +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12.5, color = "#303030"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold", size = 12.5),
    panel.grid = element_blank(),
    plot.margin = margin(12, 16, 12, 16)
  )

save_plot_bundle(p, "fig_sankey_order_province_year_corrected", width = 18, height = 10)

summary_lines <- c(
  "# Corrected Sankey summary / 校正后桑基图摘要",
  "",
  "## English",
  paste0("- Flow records retained in the corrected Sankey dataset: ", sum(sankey_df$n_records)),
  paste0("- Orders represented: ", n_distinct(clean$order)),
  paste0("- Provinces represented: ", n_distinct(clean$province)),
  paste0("- Year span: ", min(clean$year), "-", max(clean$year)),
  "",
  "## 中文",
  paste0("- 校正后用于桑基图的记录数：", sum(sankey_df$n_records)),
  paste0("- 涉及的目数：", n_distinct(clean$order)),
  paste0("- 涉及的省份数：", n_distinct(clean$province)),
  paste0("- 年份跨度：", min(clean$year), "-", max(clean$year))
)
writeLines(summary_lines, file.path(results_dir, "task_summary_bilingual.md"))

cat("Corrected Sankey figure exported successfully.\n")
