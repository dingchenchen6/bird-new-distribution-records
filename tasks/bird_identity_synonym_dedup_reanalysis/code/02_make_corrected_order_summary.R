#!/usr/bin/env Rscript

# ============================================================
# Corrected bird new-record summary table by order
# 校正后鸟类新纪录按目汇总表
# ============================================================
#
# Scientific question / 科学问题
# After resolving species identity and same-species same-province duplicates,
# how do the taxonomic totals by order change?
# 在完成物种身份校正和同物种同省重复去重后，按目的新纪录统计量如何变化？
#
# Objective / 研究目标
# 1. Count newly recorded bird species by order from the corrected event table.
# 2. Count unique papers by order after duplicate resolution.
# 3. Join the national bird checklist to quantify the proportion of new-record
#    species relative to the total species richness of each order.
# 1. 基于校正后的事件表统计各目新纪录物种数。
# 2. 在去重后统计各目对应论文数。
# 3. 连接全国鸟类名录，计算新纪录物种占该目总物种数的比例。
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(writexl)
  library(gridExtra)
  library(grid)
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
subtask_root <- file.path(task_root, "order_summary_corrected")
data_dir <- file.path(subtask_root, "data")
fig_dir <- file.path(subtask_root, "figures")
results_dir <- file.path(subtask_root, "results")
for (dir_path in c(data_dir, fig_dir, results_dir)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

corrected_clean_csv <- file.path(task_root, "data", "bird_new_records_clean_corrected.csv")
master_xlsx <- "/Users/dingchenchen/Desktop/鸟类新纪录20260311.xlsx"
sheet_catalog <- "2025中国生物物种名录"

if (!file.exists(corrected_clean_csv)) stop("Missing corrected clean dataset: ", corrected_clean_csv)

title_case_order <- function(x) {
  stringr::str_to_title(stringr::str_to_lower(as.character(x)))
}

format_pct <- function(x) sprintf("%.1f%%", 100 * x)

records_clean <- read.csv(corrected_clean_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8") %>%
  tibble::as_tibble() %>%
  transmute(
    species = stringr::str_squish(species),
    order = title_case_order(stringr::str_squish(order)),
    paper_id = stringr::str_squish(paper_id)
  ) %>%
  filter(!is.na(species), species != "", !is.na(order), order != "")

catalog_raw <- read_xlsx(master_xlsx, sheet = sheet_catalog, guess_max = 20000) %>%
  clean_names()

bird_catalog <- bind_rows(
  catalog_raw %>%
    transmute(species_lat = wu_zhong_la_ding_ming_1, class_lat = gang_la_ding_ming_7, order_lat = mu_la_ding_ming_9),
  catalog_raw %>%
    transmute(species_lat = wu_zhong_la_ding_ming_20, class_lat = gang_la_ding_ming_26, order_lat = mu_la_ding_ming_28)
) %>%
  filter(class_lat == "Aves", !is.na(species_lat), species_lat != "", !is.na(order_lat), order_lat != "") %>%
  mutate(
    species = str_extract(species_lat, "^[A-Z][A-Za-z-]+\\s+[a-z-]+"),
    order = title_case_order(order_lat)
  ) %>%
  filter(!is.na(species), !is.na(order)) %>%
  distinct(species, order)

order_pool <- bird_catalog %>% count(order, name = "n_species_total_order")
order_species <- records_clean %>% distinct(order, species) %>% count(order, name = "n_new_species")
order_papers <- records_clean %>% filter(!is.na(paper_id), paper_id != "") %>% distinct(order, paper_id) %>% count(order, name = "n_papers")

total_new_species <- sum(order_species$n_new_species, na.rm = TRUE)
total_species_pool <- sum(order_pool$n_species_total_order, na.rm = TRUE)

order_summary <- order_species %>%
  left_join(order_papers, by = "order") %>%
  left_join(order_pool, by = "order") %>%
  mutate(
    n_papers = if_else(is.na(n_papers), 0L, n_papers),
    prop_new_species_all = n_new_species / total_new_species,
    prop_to_total_species_order = n_new_species / n_species_total_order
  ) %>%
  arrange(desc(n_new_species), desc(n_papers), order)

order_summary_total <- bind_rows(
  order_summary,
  tibble(
    order = "Total",
    n_new_species = sum(order_summary$n_new_species, na.rm = TRUE),
    n_papers = dplyr::n_distinct(records_clean$paper_id[!is.na(records_clean$paper_id) & records_clean$paper_id != ""]),
    n_species_total_order = total_species_pool,
    prop_new_species_all = 1,
    prop_to_total_species_order = sum(order_summary$n_new_species, na.rm = TRUE) / total_species_pool
  )
)

display_tbl <- order_summary_total %>%
  transmute(
    Order = order,
    `Number of newly recorded bird species` = n_new_species,
    `Number of papers` = n_papers,
    `Proportion of newly recorded bird species` = format_pct(prop_new_species_all),
    `Proportion to total species in the order` = format_pct(prop_to_total_species_order)
  )

write.csv(order_summary_total, file.path(data_dir, "table_order_summary_corrected_numeric.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(display_tbl, file.path(data_dir, "table_order_summary_corrected_display.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writexl::write_xlsx(list(order_summary_numeric = order_summary_total, order_summary_display = display_tbl), file.path(data_dir, "table_order_summary_corrected.xlsx"))

table_plot_df <- display_tbl
table_grob <- tableGrob(table_plot_df, rows = NULL, theme = ttheme_minimal(base_size = 10))
png_path <- file.path(fig_dir, "table_order_summary_corrected.png")
pdf_path <- file.path(fig_dir, "table_order_summary_corrected.pdf")

png(png_path, width = 2600, height = 1800, res = 300, bg = "white")
grid.newpage()
grid.draw(table_grob)
dev.off()

pdf(pdf_path, width = 14, height = 9, bg = "white")
grid.newpage()
grid.draw(table_grob)
dev.off()

summary_lines <- c(
  "# Corrected order summary / 校正后按目汇总",
  "",
  "## English",
  paste0("- Orders represented after correction: ", n_distinct(order_summary$order)),
  paste0("- Total newly recorded species after correction: ", sum(order_summary$n_new_species)),
  paste0("- Top order by corrected newly recorded species: ", order_summary$order[1], " (", order_summary$n_new_species[1], ")."),
  "",
  "## 中文",
  paste0("- 校正后涉及的目数：", n_distinct(order_summary$order)),
  paste0("- 校正后新纪录物种总数：", sum(order_summary$n_new_species)),
  paste0("- 校正后新纪录物种数最多的目：", order_summary$order[1], "（", order_summary$n_new_species[1], "）。")
)
writeLines(summary_lines, file.path(results_dir, "task_summary_bilingual.md"))

cat("Corrected order summary exported successfully.\n")
