#!/usr/bin/env Rscript

# ============================================================
# Bird new-record summary table by order
# Reproducible workflow:
# 1) standardize bird new-record data
# 2) count newly recorded species and papers by order
# 3) join the 2025 national bird checklist to get total species per order
# 4) export numeric and publication-ready table versions
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

input_xlsx <- "/Users/dingchenchen/Desktop/鸟类新纪录20260311.xlsx"
sheet_records <- "2000-2025鸟类新记录"
sheet_catalog <- "2025中国生物物种名录"
output_root <- "/Users/dingchenchen/Documents/New project/bird_new_records_R_output"
output_tables <- file.path(output_root, "tables")
output_figures <- file.path(output_root, "figures")

dir.create(output_tables, showWarnings = FALSE, recursive = TRUE)
dir.create(output_figures, showWarnings = FALSE, recursive = TRUE)

title_case_order <- function(x) {
  str_to_title(str_to_lower(str_to_upper(x)))
}

format_pct <- function(x) {
  sprintf("%.1f%%", x * 100)
}

# -------------------------------
# 1) Read and standardize new-record data
# -------------------------------
records_raw <- read_xlsx(input_xlsx, sheet = sheet_records, guess_max = 20000) %>%
  clean_names()

records_clean <- records_raw %>%
  transmute(
    species = str_squish(scientificname),
    order = title_case_order(str_squish(order_cn)),
    year = suppressWarnings(as.integer(publicationyear)),
    title = str_squish(title),
    title_en = str_squish(title_en),
    quote = str_squish(quote),
    authors = str_squish(authors),
    journal = str_squish(journal)
  ) %>%
  mutate(
    order = na_if(order, ""),
    species = na_if(species, ""),
    paper_id = case_when(
      !is.na(title) & title != "" ~ title,
      !is.na(title_en) & title_en != "" ~ title_en,
      !is.na(quote) & quote != "" ~ quote,
      !is.na(authors) & authors != "" & !is.na(journal) & journal != "" & !is.na(year) ~
        paste(authors, journal, year, sep = " | "),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(order), !is.na(species)) %>%
  distinct(species, order, paper_id, .keep_all = TRUE)

# -------------------------------
# 2) Build total bird species pool by order
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
# 3) Summarize by order
# -------------------------------
order_species <- records_clean %>%
  distinct(order, species) %>%
  count(order, name = "n_new_species")

order_papers <- records_clean %>%
  filter(!is.na(paper_id), paper_id != "") %>%
  distinct(order, paper_id) %>%
  count(order, name = "n_papers")

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

order_summary_display <- order_summary_total %>%
  transmute(
    Order = order,
    `Number of newly recorded bird species` = n_new_species,
    `Number of papers` = n_papers,
    `Proportion of newly recorded bird species` = format_pct(prop_new_species_all),
    `Proportion of newly recorded bird species to total species in the order` = format_pct(prop_to_total_species_order)
  )

order_summary_display_fig <- order_summary_total %>%
  transmute(
    Order = order,
    `Newly recorded\nbird species` = n_new_species,
    `Papers` = n_papers,
    `Proportion of all newly\nrecorded bird species` = format_pct(prop_new_species_all),
    `Proportion to total\nspecies in the order` = format_pct(prop_to_total_species_order)
  )

# -------------------------------
# 4) Export numeric and formatted tables
# -------------------------------
write.csv(
  order_summary_total,
  file.path(output_tables, "table_order_summary_bird_new_records_numeric.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  order_summary_display,
  file.path(output_tables, "table_order_summary_bird_new_records_display.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

writexl::write_xlsx(
  list(
    order_summary_numeric = order_summary_total,
    order_summary_display = order_summary_display
  ),
  file.path(output_tables, "table_order_summary_bird_new_records.xlsx")
)

writeLines(
  c(
    "# Table Sx. Summary statistics of newly recorded bird species in China by order",
    "",
    paste0("- Total newly recorded bird species: ", sum(order_summary$n_new_species, na.rm = TRUE)),
    paste0("- Total papers: ", dplyr::n_distinct(records_clean$paper_id[!is.na(records_clean$paper_id) & records_clean$paper_id != ""])),
    paste0("- Orders included: ", nrow(order_summary)),
    "",
    paste(capture.output(print(order_summary_display, row.names = FALSE)), collapse = "\n")
  ),
  con = file.path(output_tables, "table_order_summary_bird_new_records.md")
)

# -------------------------------
# 5) Create a publication-style table figure
# -------------------------------
table_theme <- ttheme_minimal(
  base_size = 9.2,
  core = list(
    fg_params = list(hjust = 0, x = 0.02, fontsize = 8.8),
    bg_params = list(fill = c(rep("white", nrow(order_summary_display_fig) - 1), "#F7F7F7"), col = NA)
  ),
  colhead = list(
    fg_params = list(fontface = "bold", fontsize = 8.8),
    bg_params = list(fill = "#F0F0F0", col = NA)
  )
)

table_grob <- tableGrob(order_summary_display_fig, rows = NULL, theme = table_theme)

for (j in seq_len(ncol(order_summary_display))) {
  table_grob$widths[j] <- unit(c(1.45, 1.15, 0.85, 1.8, 1.8)[j], "in")
}

png(
  filename = file.path(output_figures, "table_order_summary_bird_new_records.png"),
  width = 3400,
  height = 2200,
  res = 300,
  bg = "white"
)
grid.newpage()
grid.text(
  "Table Sx. Summary statistics of newly recorded bird species in China by order",
  x = 0.02,
  y = 0.985,
  just = c("left", "top"),
  gp = gpar(fontsize = 13, fontface = "bold")
)
grid.draw(editGrob(table_grob, vp = viewport(x = 0.5, y = 0.455, width = 0.97, height = 0.86)))
dev.off()

pdf(
  file = file.path(output_figures, "table_order_summary_bird_new_records.pdf"),
  width = 16,
  height = 10,
  useDingbats = FALSE
)
grid.newpage()
grid.text(
  "Table Sx. Summary statistics of newly recorded bird species in China by order",
  x = 0.02,
  y = 0.985,
  just = c("left", "top"),
  gp = gpar(fontsize = 13, fontface = "bold")
)
grid.draw(editGrob(table_grob, vp = viewport(x = 0.5, y = 0.455, width = 0.97, height = 0.86)))
dev.off()

cat("Order summary table finished.\n")
cat("Output table:", file.path(output_tables, "table_order_summary_bird_new_records.xlsx"), "\n")
