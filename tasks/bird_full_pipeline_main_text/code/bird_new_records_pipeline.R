#!/usr/bin/env Rscript

# ============================================================
# Bird New Distribution Records in China (2000-2025)
# Reproducible pipeline: data cleaning -> summaries -> figures
# Author: Codex (for Ding Chenchen project workflow)
# Date: 2026-03-12
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(ggalluvial)
  library(scales)
  library(patchwork)
  library(writexl)
  library(janitor)
  library(sf)
  library(terra)
  library(ggrepel)
  library(cowplot)
})

set.seed(1234)

# -------------------------------
# 0) Paths and global style
# -------------------------------
input_xlsx <- "/Users/dingchenchen/Desktop/鸟类新纪录20260311.xlsx"
sheet_name <- "2000-2025鸟类新记录"

output_root <- "/Users/dingchenchen/Documents/New project/bird_new_records_R_output"
dir.create(output_root, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_root, "data_clean"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_root, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_root, "figures"), showWarnings = FALSE, recursive = TRUE)

fig_w <- 12
fig_h <- 7
dpi_out <- 420

title_case_order <- function(x) {
  str_to_title(str_to_lower(str_to_upper(x)))
}

theme_pub <- function(base_size = 12, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "#2B2B2B"),
      plot.caption = element_text(color = "grey30", size = base_size - 2),
      panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#1A1A1A"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

save_plot_dual <- function(p, filename_no_ext, width = fig_w, height = fig_h) {
  png_path <- file.path(output_root, "figures", paste0(filename_no_ext, ".png"))
  pdf_path <- file.path(output_root, "figures", paste0(filename_no_ext, ".pdf"))
  ggsave(png_path, p, width = width, height = height, dpi = dpi_out, bg = "white")
  ggsave(pdf_path, p, width = width, height = height, dpi = dpi_out, bg = "white", device = cairo_pdf)
}

classify_discovery_reason <- function(x) {
  case_when(
    str_detect(x, "新发现|描述并命名") ~ "New species or formal description",
    str_detect(x, "分类变动|独立成种|重新认定|误认") &
      str_detect(x, "分布变化|调查") ~ "Mixed: taxonomy + distribution/survey",
    str_detect(x, "分类变动|独立成种|重新认定|误认") ~ "Taxonomic revision",
    str_detect(x, "分布变化|游荡|最北端|北端|扩展|扩散") &
      str_detect(x, "技术|调查不足|缺乏调查|调查扩大") ~ "Mixed: range change + survey/technology",
    str_detect(x, "分布变化|游荡|最北端|北端|扩展|扩散") ~ "Range shift or distribution change",
    str_detect(x, "技术|整理照片|公众科学|卫星") ~ "Technology or improved detection",
    str_detect(x, "调查不足|缺乏调查|调查扩大") ~ "Survey gap or under-sampling",
    is.na(x) | x == "" ~ "Unclear",
    TRUE ~ "Other"
  )
}

classify_discovery_method <- function(x) {
  case_when(
    str_detect(x, "相机|红外|camera") ~ "Camera trapping / imaging",
    str_detect(x, "卫星") ~ "Satellite tracking",
    str_detect(x, "录音|鸣声|鸣叫|声") ~ "Acoustic detection",
    str_detect(x, "标本|尸体|捡到|采集") ~ "Specimen or carcass collection",
    str_detect(x, "网捕|捕获|迷网") ~ "Capture or mist-netting",
    str_detect(x, "救助|救护|市民") ~ "Rescue or citizen report",
    str_detect(x, "观察|拍摄|望远镜|照相机") ~ "Direct observation / photography",
    str_detect(x, "整理照片|查阅|总结") ~ "Archive or record review",
    is.na(x) | x == "" | str_detect(x, "不明|\\?\\?") ~ "Unclear",
    TRUE ~ "Other"
  )
}

# -------------------------------
# 1) Read and standardize data
# -------------------------------
raw <- read_xlsx(input_xlsx, sheet = sheet_name, guess_max = 10000) %>%
  clean_names()

province_cn_to_en <- c(
  "北京市" = "Beijing",
  "天津市" = "Tianjin",
  "上海市" = "Shanghai",
  "重庆市" = "Chongqing",
  "河北省" = "Hebei",
  "山西省" = "Shanxi",
  "辽宁省" = "Liaoning",
  "吉林省" = "Jilin",
  "黑龙江省" = "Heilongjiang",
  "江苏省" = "Jiangsu",
  "浙江省" = "Zhejiang",
  "安徽省" = "Anhui",
  "福建省" = "Fujian",
  "江西省" = "Jiangxi",
  "山东省" = "Shandong",
  "河南省" = "Henan",
  "湖北省" = "Hubei",
  "湖南省" = "Hunan",
  "广东省" = "Guangdong",
  "海南省" = "Hainan",
  "四川省" = "Sichuan",
  "贵州省" = "Guizhou",
  "云南省" = "Yunnan",
  "陕西省" = "Shaanxi",
  "甘肃省" = "Gansu",
  "青海省" = "Qinghai",
  "台湾省" = "Taiwan",
  "内蒙古自治区" = "Inner Mongolia",
  "广西壮族自治区" = "Guangxi",
  "西藏自治区" = "Tibet",
  "宁夏回族自治区" = "Ningxia",
  "新疆维吾尔自治区" = "Xinjiang",
  "香港特别行政区" = "Hong Kong",
  "澳门特别行政区" = "Macao"
)

iucn_levels <- c("CR", "EN", "VU", "NT", "LC", "DD", "NE", "NA")

clean0 <- raw %>%
  transmute(
    record_id = row_number(),
    species = str_squish(scientificname),
    order_raw = str_squish(order_cn),
    order_cn = str_squish(order_la),
    province_cn = str_squish(province_23),
    province_en_raw = str_squish(province_24),
    year = suppressWarnings(as.integer(publicationyear)),
    iucn_raw = str_squish(iucn_hong_se_ming_lu_deng_ji),
    discover_cause_raw = str_squish(discovercause),
    discovery_method_raw = str_squish(discoverymethod),
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude))
  )

clean1 <- clean0 %>%
  mutate(
    # Normalize order names to publication-ready Latin case style.
    order = title_case_order(order_raw),
    order = case_when(
      is.na(order) | order == "" ~ NA_character_,
      TRUE ~ order
    ),
    # Province: prioritize provided English field, then fallback to mapped Chinese names.
    province = case_when(
      !is.na(province_en_raw) & province_en_raw != "" ~ province_en_raw,
      province_cn %in% names(province_cn_to_en) ~ unname(province_cn_to_en[province_cn]),
      TRUE ~ NA_character_
    ),
    province = str_replace_all(province, " Province$", ""),
    province = str_replace_all(province, " Autonomous Region$", ""),
    province = str_replace_all(province, " Municipality$", ""),
    province = recode(
      province,
      "Juangsu" = "Jiangsu",
      "Qinghai Province" = "Qinghai",
      "Taiwan Province" = "Taiwan",
      "Tibet Autonomous Region" = "Tibet"
    ),
    # IUCN category normalization.
    iucn = str_to_upper(iucn_raw),
    iucn = na_if(iucn, ""),
    iucn = recode(iucn, "LR/LC" = "LC", "LR/NT" = "NT", .default = iucn),
    iucn = if_else(is.na(iucn) | !iucn %in% iucn_levels, "NA", iucn),
    discover_reason = classify_discovery_reason(discover_cause_raw),
    discovery_method = classify_discovery_method(discovery_method_raw),
    # Keep plausible publication years.
    year = if_else(!is.na(year) & year >= 1900 & year <= 2100, year, NA_integer_)
  ) %>%
  filter(!is.na(order), !is.na(province), !is.na(year)) %>%
  mutate(
    iucn = factor(iucn, levels = iucn_levels, ordered = TRUE)
  )

# Deduplicate to event-level records.
clean <- clean1 %>%
  distinct(species, order, province, year, iucn, .keep_all = TRUE) %>%
  arrange(year, order, province, species)

# -------------------------------
# 2) Export standardized datasets
# -------------------------------
clean_csv <- file.path(output_root, "data_clean", "bird_new_records_clean.csv")
clean_xlsx <- file.path(output_root, "data_clean", "bird_new_records_clean.xlsx")
write.csv(clean, clean_csv, row.names = FALSE, fileEncoding = "UTF-8")
writexl::write_xlsx(list(clean_data = clean), clean_xlsx)

# QA summary table.
qa_tbl <- tibble(
  metric = c(
    "Raw rows",
    "Rows after filtering key fields (order/province/year)",
    "Rows after deduplication",
    "Unique species",
    "Unique orders",
    "Unique provinces",
    "Year min",
    "Year max"
  ),
  value = c(
    nrow(raw),
    nrow(clean1),
    nrow(clean),
    n_distinct(clean$species),
    n_distinct(clean$order),
    n_distinct(clean$province),
    min(clean$year, na.rm = TRUE),
    max(clean$year, na.rm = TRUE)
  )
)
write.csv(qa_tbl, file.path(output_root, "tables", "table_qa_summary.csv"), row.names = FALSE)

# -------------------------------
# 3) Summary tables for manuscript
# -------------------------------
tbl_year <- clean %>% count(year, name = "n_records") %>% arrange(desc(n_records))
tbl_province <- clean %>% count(province, name = "n_records") %>% arrange(desc(n_records))
tbl_order <- clean %>% count(order, name = "n_records") %>% arrange(desc(n_records))
tbl_iucn <- clean %>% count(iucn, name = "n_records") %>% arrange(iucn)
tbl_flow <- clean %>% count(order, province, year, iucn, name = "n_records")
tbl_reason <- clean %>% count(discover_reason, name = "n_records") %>% arrange(desc(n_records))
tbl_method <- clean %>% count(discovery_method, name = "n_records") %>% arrange(desc(n_records))

write.csv(tbl_year, file.path(output_root, "tables", "table_year_counts.csv"), row.names = FALSE)
write.csv(tbl_province, file.path(output_root, "tables", "table_province_counts.csv"), row.names = FALSE)
write.csv(tbl_order, file.path(output_root, "tables", "table_order_counts.csv"), row.names = FALSE)
write.csv(tbl_iucn, file.path(output_root, "tables", "table_iucn_counts.csv"), row.names = FALSE)
write.csv(tbl_flow, file.path(output_root, "tables", "table_flow_order_province_year_iucn.csv"), row.names = FALSE)
write.csv(tbl_reason, file.path(output_root, "tables", "table_discovery_reason_counts.csv"), row.names = FALSE)
write.csv(tbl_method, file.path(output_root, "tables", "table_discovery_method_counts.csv"), row.names = FALSE)

# -------------------------------
# 4) Reference-style figure set
# -------------------------------
top8_orders <- tbl_order %>% slice_head(n = 8) %>% pull(order)
order_palette_main <- c(
  setNames(RColorBrewer::brewer.pal(8, "Set2"), top8_orders),
  "Other orders" = "#BDBDBD"
)

# 4.1 Province map styled after the mammal-paper spatial panel.
map_rds <- file.path(output_root, "data_clean", "gadm", "gadm41_CHN_1_pk.rds")
if (!file.exists(map_rds)) {
  try({
    geodata::gadm(country = "CHN", level = 1, path = file.path(output_root, "data_clean"))
  }, silent = TRUE)
}
if (!file.exists(map_rds)) {
  stop("Province boundary data not found. Please ensure gadm41_CHN_1_pk.rds is available.")
}

china_map <- readRDS(map_rds) %>%
  st_as_sf() %>%
  transmute(
    province = recode(
      NAME_1,
      "Nei Mongol" = "Inner Mongolia",
      "Ningxia Hui" = "Ningxia",
      "Xinjiang Uygur" = "Xinjiang",
      "Xizang" = "Tibet",
      "Guangxi Zhuang" = "Guangxi",
      "Macau" = "Macao",
      .default = NAME_1
    ),
    geometry = geometry
  ) %>%
  filter(province %in% c(unname(province_cn_to_en), "Macao")) %>%
  group_by(province) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

map_df <- china_map %>%
  left_join(tbl_province, by = "province") %>%
  mutate(
    n_records = replace_na(n_records, 0),
    count_bin = cut(
      n_records,
      breaks = c(-0.1, 10, 20, 30, 40, 50, 60, Inf),
      labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61+"),
      include.lowest = TRUE
    )
  )

map_labels <- map_df %>%
  filter(n_records > 0) %>%
  st_transform(3857) %>%
  st_point_on_surface() %>%
  st_transform(4326) %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

p_map <- ggplot(map_df) +
  geom_sf(aes(fill = count_bin), color = "#666666", linewidth = 0.22) +
  ggrepel::geom_text_repel(
    data = map_labels,
    aes(x = x, y = y, label = province),
    size = 2.6,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 1234,
    box.padding = 0.18,
    point.padding = 0.02,
    segment.color = "grey55"
  ) +
  scale_fill_manual(
    values = c(
      "0-10" = "#2B8CBE",
      "11-20" = "#7FB8B0",
      "21-30" = "#B9CF7A",
      "31-40" = "#E6ED63",
      "41-50" = "#F5B041",
      "51-60" = "#F27C2B",
      "61+" = "#E53935"
    ),
    drop = FALSE,
    name = "Number of records"
  ) +
  coord_sf(xlim = c(73, 136), ylim = c(18, 54), expand = FALSE) +
  labs(
    title = "Spatial distribution of new bird records across provinces",
    subtitle = "Event-level record counts in China",
    x = NULL,
    y = NULL
  ) +
  theme_pub(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.12, 0.22),
    legend.background = element_rect(fill = alpha("white", 0.85), color = NA)
  )

save_plot_dual(p_map, "fig08_province_choropleth_records", width = 10.8, height = 7.8)

# 4.2 Province-by-order and year-by-order stacked bars with percentage lines.
province_totals <- clean %>%
  count(province, name = "total_records") %>%
  mutate(
    province = fct_reorder(province, total_records, .desc = FALSE),
    pct = total_records / sum(total_records) * 100
  )

province_order <- clean %>%
  mutate(order_grp = if_else(order %in% top8_orders, order, "Other orders")) %>%
  count(province, order_grp, name = "n_records") %>%
  left_join(province_totals %>% mutate(province_chr = as.character(province)), by = c("province" = "province_chr")) %>%
  mutate(
    province = factor(province, levels = levels(province_totals$province)),
    order_grp = factor(order_grp, levels = c(top8_orders, "Other orders"))
  )

province_scale <- max(province_totals$total_records) / max(province_totals$pct)

p_province_stack <- ggplot(province_order, aes(x = province, y = n_records, fill = order_grp)) +
  geom_col(width = 0.82, color = "white", linewidth = 0.08) +
  geom_line(
    data = province_totals,
    aes(x = province, y = pct * province_scale, group = 1),
    inherit.aes = FALSE,
    color = "#6B7A8F",
    linewidth = 0.8,
    linetype = "22"
  ) +
  geom_point(
    data = province_totals,
    aes(x = province, y = pct * province_scale),
    inherit.aes = FALSE,
    color = "#6B7A8F",
    size = 1.2
  ) +
  scale_fill_manual(values = order_palette_main) +
  scale_y_continuous(
    name = "Number of records",
    sec.axis = sec_axis(~ . / province_scale, name = "Percentage (%)")
  ) +
  labs(
    title = "New bird records across provinces",
    subtitle = "Stacked by taxonomic order",
    x = NULL,
    fill = "Order"
  ) +
  theme_pub(base_size = 10.5) +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1),
    legend.position = "right"
  )

save_plot_dual(p_province_stack, "fig09_province_stacked_by_order_percentage", width = 13.5, height = 7.6)

year_totals <- clean %>%
  count(year, name = "total_records") %>%
  mutate(pct = total_records / sum(total_records) * 100)

year_order <- clean %>%
  mutate(order_grp = if_else(order %in% top8_orders, order, "Other orders")) %>%
  count(year, order_grp, name = "n_records") %>%
  left_join(year_totals, by = "year") %>%
  mutate(order_grp = factor(order_grp, levels = c(top8_orders, "Other orders")))

year_scale <- max(year_totals$total_records) / max(year_totals$pct)

p_year_stack <- ggplot(year_order, aes(x = year, y = n_records, fill = order_grp)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.08) +
  geom_line(
    data = year_totals,
    aes(x = year, y = pct * year_scale, group = 1),
    inherit.aes = FALSE,
    color = "#6B7A8F",
    linewidth = 0.8,
    linetype = "22"
  ) +
  geom_point(
    data = year_totals,
    aes(x = year, y = pct * year_scale),
    inherit.aes = FALSE,
    color = "#6B7A8F",
    size = 1.2
  ) +
  scale_fill_manual(values = order_palette_main) +
  scale_x_continuous(breaks = seq(min(clean$year), max(clean$year), by = 1)) +
  scale_y_continuous(
    name = "Number of records",
    sec.axis = sec_axis(~ . / year_scale, name = "Percentage (%)")
  ) +
  labs(
    title = "Annual numbers of new bird records",
    subtitle = "Stacked by taxonomic order",
    x = "Year",
    fill = "Order"
  ) +
  theme_pub(base_size = 10.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot_dual(p_year_stack, "fig10_year_stacked_by_order_percentage", width = 13.5, height = 7.6)

p_main_panel <- p_map + (p_province_stack / p_year_stack) +
  plot_layout(widths = c(1.1, 1.25)) +
  plot_annotation(
    title = "Taxonomic and spatiotemporal patterns of new bird records in China"
  )

save_plot_dual(p_main_panel, "fig11_main_panel_map_province_year", width = 18, height = 11)

# 4.3 Order-level representativeness using the 2025 species catalogue.
catalog_raw <- read_xlsx(input_xlsx, sheet = "2025中国生物物种名录", guess_max = 20000) %>%
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

tbl_order_pool <- bird_catalog %>%
  count(order, name = "n_species_pool")
write.csv(tbl_order_pool, file.path(output_root, "tables", "table_order_species_pool_2025_catalogue.csv"), row.names = FALSE)

order_repr <- clean %>%
  distinct(species, order) %>%
  count(order, name = "n_new_species") %>%
  left_join(tbl_order, by = "order") %>%
  left_join(tbl_order_pool, by = "order") %>%
  filter(!is.na(n_species_pool), n_species_pool > 0) %>%
  mutate(
    prop_new_species = n_new_species / n_species_pool,
    size_class = if_else(n_species_pool >= median(n_species_pool, na.rm = TRUE), "Species-rich order", "Species-poor order")
  )

write.csv(order_repr, file.path(output_root, "tables", "table_order_representativeness.csv"), row.names = FALSE)

p_order_repr <- ggplot(order_repr, aes(x = n_species_pool, y = prop_new_species)) +
  geom_hline(yintercept = median(order_repr$prop_new_species, na.rm = TRUE), linetype = "22", color = "#8C8C8C") +
  geom_vline(xintercept = median(order_repr$n_species_pool, na.rm = TRUE), linetype = "22", color = "#8C8C8C") +
  geom_point(aes(size = n_new_species, color = size_class), alpha = 0.92) +
  ggrepel::geom_text_repel(
    aes(label = order),
    size = 3.2,
    min.segment.length = 0,
    box.padding = 0.25,
    max.overlaps = Inf,
    seed = 1234
  ) +
  scale_size_continuous(name = "Newly recorded species") +
  scale_color_manual(values = c("Species-rich order" = "#D95F02", "Species-poor order" = "#4C78A8")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Order-level representativeness of newly recorded bird species",
    subtitle = "Proportion of newly recorded species relative to the 2025 national species pool",
    x = "Number of bird species in the 2025 Chinese catalogue",
    y = "Proportion of newly recorded species",
    color = NULL
  ) +
  theme_pub(base_size = 11.5)

save_plot_dual(p_order_repr, "fig12_order_representativeness_scatter", width = 11.5, height = 7.5)

# 4.4 Discovery reasons and methods (supplementary descriptive panels).
reason_plot_df <- tbl_reason %>%
  mutate(
    pct = n_records / sum(n_records),
    discover_reason = fct_reorder(discover_reason, n_records)
  )

method_plot_df <- tbl_method %>%
  mutate(
    pct = n_records / sum(n_records),
    discovery_method = fct_reorder(discovery_method, n_records)
  )

p_reason <- ggplot(reason_plot_df, aes(x = pct, y = discover_reason)) +
  geom_col(fill = "#3C7D9E", width = 0.72) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), hjust = -0.08, size = 3.1) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Inferred reasons for new bird records",
    x = "Proportion of records",
    y = NULL
  ) +
  theme_pub(base_size = 11)

p_method <- ggplot(method_plot_df, aes(x = pct, y = discovery_method)) +
  geom_col(fill = "#B85C38", width = 0.72) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), hjust = -0.08, size = 3.1) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Discovery methods for new bird records",
    x = "Proportion of records",
    y = NULL
  ) +
  theme_pub(base_size = 11)

p_reason_method <- p_reason / p_method +
  plot_annotation(title = "Attributed causes and evidence streams of new bird records")

save_plot_dual(p_reason_method, "fig13_discovery_reason_and_method", width = 11.5, height = 10.5)

# -------------------------------
# 5) Species-level exploratory mechanism figures
# -------------------------------
trait_av <- read_xlsx(input_xlsx, sheet = "AVONET traits", guess_max = 20000) %>%
  clean_names() %>%
  transmute(
    species = species1,
    mass = as.numeric(mass),
    migration_av = as.character(migration),
    habitat_av = habitat,
    trophic_level_av = trophic_level,
    trophic_niche_av = trophic_niche,
    lifestyle_av = primary_lifestyle,
    centroid_latitude = suppressWarnings(as.numeric(centroid_latitude)),
    centroid_longitude = suppressWarnings(as.numeric(centroid_longitude))
  ) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)

trait_cn <- read_xlsx(input_xlsx, sheet = "中国鸟类生态学特征", guess_max = 5000) %>%
  clean_names() %>%
  transmute(
    species = zhong_la_ding_ming,
    migration_cn = qian_xi_zhuang_tai_liu_niao_r_xia_hou_niao_s_dong_hou_niao_w_lu_niao_p_mi_niao_v,
    diet_cn = shi_xing,
    range_size_provinces = suppressWarnings(as.numeric(fen_bu_sheng_fen_shu))
  ) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)

trait_bb <- read_xlsx(input_xlsx, sheet = "BIRDBASE traits", guess_max = 20000) %>%
  clean_names() %>%
  transmute(
    species = x3,
    iucn_pool = conservation_status
  ) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)

species_response <- clean %>%
  count(species, name = "n_new_records") %>%
  mutate(new_record = 1L)

species_pool <- bird_catalog %>%
  left_join(species_response, by = "species") %>%
  mutate(
    new_record = replace_na(new_record, 0L),
    n_new_records = replace_na(n_new_records, 0L)
  ) %>%
  left_join(trait_av, by = "species") %>%
  left_join(trait_cn, by = "species") %>%
  left_join(trait_bb, by = "species") %>%
  mutate(
    log_mass = log10(mass),
    migration_class = recode(
      migration_av,
      "1" = "Resident/low mobility",
      "2" = "Partial migrant",
      "3" = "Full migrant",
      .default = "Unknown"
    ),
    migration_class = factor(migration_class, levels = c("Resident/low mobility", "Partial migrant", "Full migrant", "Unknown")),
    iucn_group = case_when(
      iucn_pool %in% c("CR", "EN", "VU") ~ "Threatened",
      iucn_pool == "NT" ~ "NT",
      iucn_pool == "DD" ~ "DD",
      iucn_pool == "LC" ~ "LC",
      iucn_pool == "NE" ~ "NE",
      TRUE ~ "Unknown"
    ),
    iucn_group = factor(iucn_group, levels = c("LC", "NT", "DD", "Threatened", "NE", "Unknown")),
    risk_group = case_when(
      iucn_group %in% c("Threatened", "DD") ~ "Threatened/DD",
      iucn_group == "NT" ~ "NT",
      iucn_group %in% c("LC", "NE") ~ "LC/NE",
      TRUE ~ "Unknown"
    ),
    risk_group = factor(risk_group, levels = c("LC/NE", "NT", "Threatened/DD", "Unknown")),
    trophic_group = case_when(
      trophic_level_av %in% c("Carnivore", "Herbivore", "Omnivore", "Scavenger") ~ trophic_level_av,
      TRUE ~ "Other"
    ),
    trophic_group = factor(trophic_group, levels = c("Omnivore", "Carnivore", "Herbivore", "Scavenger", "Other"))
  )

write.csv(species_pool, file.path(output_root, "data_clean", "bird_species_pool_with_traits.csv"), row.names = FALSE)

species_model_df <- species_pool %>%
  filter(
    !is.na(log_mass),
    !is.na(range_size_provinces),
    migration_class != "Unknown",
    risk_group != "Unknown"
  )

glm_bin <- glm(
  new_record ~ scale(log_mass) + scale(range_size_provinces) + migration_class + risk_group,
  data = species_model_df,
  family = binomial(),
  control = glm.control(maxit = 100)
)

glm_nb <- glm(
  n_new_records ~ scale(log_mass) + scale(range_size_provinces) + migration_class + risk_group,
  data = species_model_df,
  family = quasipoisson(),
  control = glm.control(maxit = 100)
)

coef_df <- bind_rows(
  broom::tidy(glm_bin, conf.int = TRUE) %>%
    mutate(model = "Binary occurrence model"),
  broom::tidy(glm_nb, conf.int = TRUE) %>%
    mutate(model = "Quasi-Poisson count model")
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_label = term,
    term_label = str_replace(term_label, "scale\\(log_mass\\)", "Body mass (log10)"),
    term_label = str_replace(term_label, "scale\\(range_size_provinces\\)", "Range size (no. provinces)"),
    term_label = str_replace(term_label, "migration_class", "Migration: "),
    term_label = str_replace(term_label, "risk_group", "Risk group: ")
  )

write.csv(coef_df, file.path(output_root, "tables", "table_species_trait_model_coefficients.csv"), row.names = FALSE)

p_species_coef <- ggplot(coef_df, aes(x = estimate, y = forcats::fct_rev(factor(term_label)), color = model)) +
  geom_vline(xintercept = 0, linetype = "22", color = "#8B8B8B") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.18, orientation = "y", position = position_dodge(width = 0.55)) +
  geom_point(size = 2.4, position = position_dodge(width = 0.55)) +
  scale_color_manual(values = c("Binary occurrence model" = "#2C7FB8", "Quasi-Poisson count model" = "#D95F0E")) +
  labs(
    title = "Species-level trait correlates of newly reported bird records",
    subtitle = "Exploratory coefficient plots from binary and quasi-Poisson models",
    x = "Coefficient estimate",
    y = NULL,
    color = NULL
  ) +
  theme_pub(base_size = 11)

save_plot_dual(p_species_coef, "fig14_species_trait_coefficient_models", width = 12.5, height = 8.5)

mig_assoc <- species_pool %>%
  filter(migration_class != "Unknown") %>%
  count(migration_class, new_record, name = "n") %>%
  group_by(migration_class) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(new_record = factor(new_record, levels = c(0, 1), labels = c("No new record", "New record")))

iucn_assoc <- species_pool %>%
  filter(risk_group != "Unknown") %>%
  count(risk_group, new_record, name = "n") %>%
  group_by(risk_group) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(new_record = factor(new_record, levels = c(0, 1), labels = c("No new record", "New record")))

mig_test_df <- species_pool %>%
  filter(!is.na(migration_class), migration_class != "Unknown", !is.na(new_record)) %>%
  mutate(migration_class = droplevels(migration_class))

iucn_test_df <- species_pool %>%
  filter(!is.na(risk_group), risk_group != "Unknown", !is.na(new_record)) %>%
  mutate(risk_group = droplevels(risk_group))

mig_p <- suppressWarnings(stats::chisq.test(
  table(mig_test_df$migration_class, mig_test_df$new_record),
  simulate.p.value = TRUE,
  B = 5000
)$p.value)

iucn_p <- suppressWarnings(stats::chisq.test(
  table(iucn_test_df$risk_group, iucn_test_df$new_record),
  simulate.p.value = TRUE,
  B = 5000
)$p.value)

p_mig_assoc <- ggplot(mig_assoc, aes(x = migration_class, y = prop, fill = new_record)) +
  geom_col(position = "fill", color = "white", linewidth = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("No new record" = "#C7D4E2", "New record" = "#D95F5F")) +
  labs(
    title = paste0("Migration status (chi-square p = ", formatC(mig_p, format = "e", digits = 2), ")"),
    x = NULL,
    y = "Within-category proportion",
    fill = NULL
  ) +
  theme_pub(base_size = 10.5)

p_iucn_assoc <- ggplot(iucn_assoc, aes(x = risk_group, y = prop, fill = new_record)) +
  geom_col(position = "fill", color = "white", linewidth = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("No new record" = "#C7D4E2", "New record" = "#D95F5F")) +
  labs(
    title = paste0("IUCN group (chi-square p = ", formatC(iucn_p, format = "e", digits = 2), ")"),
    x = NULL,
    y = "Within-category proportion",
    fill = NULL
  ) +
  theme_pub(base_size = 10.5)

p_species_assoc <- p_mig_assoc + p_iucn_assoc +
  plot_annotation(title = "Categorical trait associations with new bird records")

save_plot_dual(p_species_assoc, "fig15_species_categorical_associations", width = 12.5, height = 5.8)
# Export a versioned alias as well, which helps avoid stale preview caching in some viewers.
save_plot_dual(p_species_assoc, "fig15_species_categorical_associations_v2", width = 12.5, height = 5.8)

# -------------------------------
# 6) Directionality based on AVONET centroids
# -------------------------------
direction_df <- clean %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  left_join(trait_av %>% select(species, centroid_latitude, centroid_longitude), by = "species") %>%
  filter(!is.na(centroid_latitude), !is.na(centroid_longitude)) %>%
  mutate(
    dx = longitude - centroid_longitude,
    dy = latitude - centroid_latitude,
    angle = (atan2(dx, dy) * 180 / pi + 360) %% 360,
    direction = case_when(
      angle >= 337.5 | angle < 22.5 ~ "N",
      angle < 67.5 ~ "NE",
      angle < 112.5 ~ "E",
      angle < 157.5 ~ "SE",
      angle < 202.5 ~ "S",
      angle < 247.5 ~ "SW",
      angle < 292.5 ~ "W",
      TRUE ~ "NW"
    ),
    direction = factor(direction, levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")),
    order_grp = if_else(order %in% top8_orders, order, "Other orders"),
    order_grp = factor(order_grp, levels = c(top8_orders, "Other orders"))
  )

tbl_direction <- direction_df %>%
  count(direction, order_grp, name = "n_records")
write.csv(tbl_direction, file.path(output_root, "tables", "table_directionality_by_order.csv"), row.names = FALSE)

dir_totals <- direction_df %>%
  count(direction, name = "n_total") %>%
  mutate(
    pct = n_total / sum(n_total),
    label_y = n_total + max(n_total) * 0.04
  )

p_direction <- ggplot(tbl_direction, aes(x = direction, y = n_records, fill = order_grp)) +
  geom_col(width = 0.82, color = "white", linewidth = 0.12) +
  geom_text(
    data = dir_totals,
    aes(x = direction, y = label_y, label = paste0(n_total, " (", percent(pct, accuracy = 0.1), ")")),
    inherit.aes = FALSE,
    size = 3.1
  ) +
  scale_fill_manual(values = order_palette_main) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Directional pattern of new bird records relative to AVONET centroids",
    subtitle = "Exploratory centroid-to-record bearing across eight directional sectors",
    x = "Direction sector",
    y = "Number of records",
    fill = "Order"
  ) +
  theme_pub(base_size = 11)

save_plot_dual(p_direction, "fig16_directionality_by_order", width = 12.5, height = 7.2)

# -------------------------------
# 7) Figure 1: 4-level alluvial (full)
# -------------------------------
flow_full <- clean %>%
  mutate(year_f = factor(year)) %>%
  count(order, province, year_f, iucn, name = "n")

order_levels <- tbl_order$order
province_levels <- tbl_province$province
year_levels <- sort(unique(clean$year))

flow_full <- flow_full %>%
  mutate(
    order = factor(order, levels = order_levels),
    province = factor(province, levels = province_levels),
    year_f = factor(year_f, levels = year_levels),
    iucn = factor(iucn, levels = iucn_levels)
  )

pal_order <- setNames(colorRampPalette(c("#164B7A", "#2B8CBE", "#41B6C4", "#A1DAB4"))(length(order_levels)), order_levels)

p1 <- ggplot(
  flow_full,
  aes(axis1 = order, axis2 = province, axis3 = year_f, axis4 = iucn, y = n)
) +
  geom_alluvium(aes(fill = order), width = 0.08, alpha = 0.35, knot.pos = 0.35) +
  geom_stratum(width = 0.1, fill = "grey95", color = "grey65", linewidth = 0.22) +
  scale_fill_manual(values = pal_order, guide = "none") +
  scale_x_discrete(
    limits = c("Order", "Province", "Year", "IUCN Status"),
    expand = c(0.03, 0.03)
  ) +
  labs(
    title = "Alluvial Pattern of New Bird Records in China",
    subtitle = "Order -> Province -> Publication Year -> IUCN Red List status",
    x = NULL,
    y = "Number of records",
    caption = "Data source: Bird new-record database (2000-2025); event-level deduplicated records."
  ) +
  theme_pub(base_size = 11) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

save_plot_dual(p1, "fig01_alluvial_order_province_year_iucn_full", width = 18, height = 10)

# -------------------------------
# 5) Figure 2: Compact alluvial (publication-focused readability)
# -------------------------------
top_orders <- tbl_order %>% slice_head(n = 10) %>% pull(order)
top_provinces <- tbl_province %>% slice_head(n = 12) %>% pull(province)

flow_compact <- clean %>%
  mutate(
    order_grp = if_else(order %in% top_orders, order, "Other orders"),
    province_grp = if_else(province %in% top_provinces, province, "Other provinces"),
    year_bin = cut(
      year,
      breaks = c(1999, 2004, 2009, 2014, 2019, 2025),
      labels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2025"),
      include.lowest = TRUE
    )
  ) %>%
  count(order_grp, province_grp, year_bin, iucn, name = "n")

flow_compact <- flow_compact %>%
  mutate(
    order_grp = fct_reorder(order_grp, n, .fun = sum, .desc = TRUE),
    province_grp = fct_reorder(province_grp, n, .fun = sum, .desc = TRUE),
    iucn = factor(iucn, levels = iucn_levels)
  )

p2 <- ggplot(
  flow_compact,
  aes(axis1 = order_grp, axis2 = province_grp, axis3 = year_bin, axis4 = iucn, y = n)
) +
  geom_alluvium(aes(fill = order_grp), width = 0.08, alpha = 0.55, knot.pos = 0.45) +
  geom_stratum(width = 0.1, fill = "grey96", color = "grey60", linewidth = 0.25) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3,
    color = "#1F1F1F"
  ) +
  scale_fill_manual(values = c(setNames(RColorBrewer::brewer.pal(11, "Spectral"), levels(flow_compact$order_grp)[1:11])), guide = "none") +
  scale_x_discrete(limits = c("Order", "Province", "Year Bin", "IUCN Status"), expand = c(0.04, 0.04)) +
  labs(
    title = "Compact Alluvial Summary of New Bird Records",
    subtitle = "Top orders and provinces grouped for publication readability",
    x = NULL,
    y = "Number of records"
  ) +
  theme_pub(base_size = 11)

save_plot_dual(p2, "fig02_alluvial_compact_top_orders_provinces", width = 15, height = 8.8)

# -------------------------------
# 6) Figure 3: Annual trend
# -------------------------------
annual <- clean %>%
  count(year, name = "n_records") %>%
  arrange(year) %>%
  mutate(roll3 = stats::filter(n_records, rep(1 / 3, 3), sides = 1))

top_year_labels <- annual %>% slice_max(order_by = n_records, n = 3)

p3 <- ggplot(annual, aes(x = year, y = n_records)) +
  geom_col(fill = "#DCE8F6", color = "#4C78A8", linewidth = 0.2, width = 0.8) +
  geom_line(aes(y = roll3), color = "#E45756", linewidth = 1.1, na.rm = TRUE) +
  geom_point(color = "#4C78A8", size = 1.8) +
  geom_text(
    data = top_year_labels,
    aes(label = paste0(year, " (", n_records, ")")),
    nudge_y = 3,
    size = 3.1,
    color = "#1A1A1A"
  ) +
  scale_x_continuous(breaks = seq(min(annual$year), max(annual$year), by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Temporal Trend of New Bird Records (2000-2025)",
    subtitle = "Bars: annual records; Red line: trailing 3-year moving average",
    x = "Publication year",
    y = "Number of records"
  ) +
  theme_pub(base_size = 12)

save_plot_dual(p3, "fig03_temporal_trend_annual_records", width = 13, height = 7.5)

# -------------------------------
# 7) Figure 4: Province-year heatmap (top 20 provinces)
# -------------------------------
top20_prov <- tbl_province %>% slice_head(n = 20) %>% pull(province)
heat_df <- clean %>%
  filter(province %in% top20_prov) %>%
  count(province, year, name = "n_records") %>%
  mutate(province = fct_reorder(province, n_records, .fun = sum, .desc = TRUE))

p4 <- ggplot(heat_df, aes(x = year, y = province, fill = n_records)) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(option = "C", end = 0.95, name = "Records") +
  scale_x_continuous(breaks = seq(min(clean$year), max(clean$year), by = 2)) +
  labs(
    title = "Spatiotemporal Heatmap of New Bird Records",
    subtitle = "Top 20 provinces by total number of records",
    x = "Publication year",
    y = "Province"
  ) +
  theme_pub(base_size = 11) +
  theme(panel.grid = element_blank())

save_plot_dual(p4, "fig04_spatiotemporal_heatmap_top20_provinces", width = 13, height = 9.2)

# -------------------------------
# 8) Figure 5: Order composition by period
# -------------------------------
comp_order <- clean %>%
  mutate(
    period = cut(
      year,
      breaks = c(1999, 2004, 2009, 2014, 2019, 2025),
      labels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2025"),
      include.lowest = TRUE
    ),
    order_grp = if_else(order %in% top_orders, order, "Other orders")
  ) %>%
  count(period, order_grp, name = "n_records") %>%
  group_by(period) %>%
  mutate(prop = n_records / sum(n_records)) %>%
  ungroup() %>%
  mutate(order_grp = fct_reorder(order_grp, n_records, .fun = sum, .desc = TRUE))

p5 <- ggplot(comp_order, aes(x = period, y = prop, fill = order_grp)) +
  geom_col(position = "fill", color = "white", linewidth = 0.15) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Taxonomic Composition Shift Across Time Periods",
    subtitle = "Proportional contribution of orders to newly reported bird records",
    x = "Period",
    y = "Proportion of records",
    fill = "Order"
  ) +
  theme_pub(base_size = 12) +
  theme(legend.position = "right")

save_plot_dual(p5, "fig05_order_composition_by_period", width = 13, height = 7.5)

# -------------------------------
# 9) Figure 6: IUCN status dynamics
# -------------------------------
iucn_ts <- clean %>%
  count(year, iucn, name = "n_records") %>%
  complete(year = seq(min(clean$year), max(clean$year), by = 1), iucn = iucn_levels, fill = list(n_records = 0)) %>%
  mutate(iucn = factor(iucn, levels = iucn_levels))

iucn_palette <- c(
  CR = "#8B0000",
  EN = "#C62828",
  VU = "#F57C00",
  NT = "#FBC02D",
  LC = "#2E7D32",
  DD = "#616161",
  NE = "#7CB342",
  "NA" = "#B0BEC5"
)

p6 <- ggplot(iucn_ts, aes(x = year, y = n_records, fill = iucn)) +
  geom_area(position = "stack", alpha = 0.92, color = "white", linewidth = 0.08) +
  scale_fill_manual(values = iucn_palette, drop = FALSE) +
  scale_x_continuous(breaks = seq(min(clean$year), max(clean$year), by = 2)) +
  labs(
    title = "IUCN Status Composition Through Time",
    subtitle = "Annual counts of new records by threat category",
    x = "Publication year",
    y = "Number of records",
    fill = "IUCN"
  ) +
  theme_pub(base_size = 12)

save_plot_dual(p6, "fig06_iucn_status_dynamics", width = 13, height = 7.5)

# -------------------------------
# 10) Figure 7: Provincial hotspots (Top 15)
# -------------------------------
hotspot15 <- tbl_province %>%
  slice_head(n = 15) %>%
  mutate(
    share = n_records / sum(tbl_province$n_records)
  ) %>%
  arrange(n_records) %>%
  mutate(province = factor(province, levels = province))

p7 <- ggplot(hotspot15, aes(x = n_records, y = province)) +
  geom_segment(aes(x = 0, xend = n_records, y = province, yend = province), color = "#AFC4DE", linewidth = 1) +
  geom_point(size = 3.3, color = "#1F5A99") +
  geom_text(
    aes(label = paste0(n_records, " (", percent(share, accuracy = 0.1), ")")),
    hjust = -0.08,
    size = 3.4
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Provincial Hotspots of New Bird Records",
    subtitle = "Top 15 provinces ranked by event-level record counts",
    x = "Number of records",
    y = "Province"
  ) +
  theme_pub(base_size = 12)

save_plot_dual(p7, "fig07_provincial_hotspots_top15", width = 12, height = 8)

# -------------------------------
# 11) Write concise report
# -------------------------------
top_year_text <- tbl_year %>% slice_head(n = 5)
top_prov_text <- tbl_province %>% slice_head(n = 10)
top_order_text <- tbl_order %>% slice_head(n = 8)
iucn_text <- tbl_iucn %>% mutate(pct = n_records / sum(n_records))
top_reason_text <- tbl_reason %>% slice_head(n = 3)
top_method_text <- tbl_method %>% slice_head(n = 3)
top_dir_text <- dir_totals %>% arrange(desc(n_total)) %>% slice_head(n = 3)

report_path <- file.path(output_root, "Bird_New_Records_Analytical_Summary.md")
report_lines <- c(
  "# Bird New Distribution Records in China: Analytical Summary",
  "",
  "## Data and QC",
  paste0("- Source file: `", input_xlsx, "` (sheet: `", sheet_name, "`)"),
  paste0("- Raw rows: **", nrow(raw), "**"),
  paste0("- Rows with valid key fields (order, province, year): **", nrow(clean1), "**"),
  paste0("- Event-level records after deduplication: **", nrow(clean), "**"),
  paste0("- Coverage: **", n_distinct(clean$order), "** orders, **", n_distinct(clean$province), "** provinces, years **", min(clean$year), "-", max(clean$year), "**"),
  "",
  "## IUCN structure",
  paste0("- ", iucn_text$iucn, ": ", iucn_text$n_records, " (", percent(iucn_text$pct, accuracy = 0.1), ")"),
  "",
  "## Top years (record counts)",
  paste0("- ", top_year_text$year, ": ", top_year_text$n_records),
  "",
  "## Top provinces (record counts)",
  paste0("- ", top_prov_text$province, ": ", top_prov_text$n_records),
  "",
  "## Top orders (record counts)",
  paste0("- ", top_order_text$order, ": ", top_order_text$n_records),
  "",
  "## Discovery reasons and methods",
  paste0("- Top inferred reasons: ", paste0(top_reason_text$discover_reason, " (", top_reason_text$n_records, ")", collapse = "; ")),
  paste0("- Top discovery methods: ", paste0(top_method_text$discovery_method, " (", top_method_text$n_records, ")", collapse = "; ")),
  "",
  "## Directionality",
  paste0("- Dominant sectors: ", paste0(top_dir_text$direction, " (", top_dir_text$n_total, ", ", percent(top_dir_text$pct, accuracy = 0.1), ")", collapse = "; ")),
  "",
  "## Interpretation (brief)",
  "- New records increase markedly in recent years, with strong inter-annual fluctuations.",
  "- Spatial hotspots are concentrated in western and southwestern provinces, alongside selected central/eastern provinces.",
  "- The record structure is dominated by LC categories, but EN/CR records indicate continued conservation relevance.",
  "- Exploratory trait models suggest broader-range and migratory species contribute disproportionately to new records.",
  "- Exploratory directionality based on AVONET centroids shows a strong eastward and northeastward signal."
)
writeLines(report_lines, report_path, useBytes = TRUE)

cat("Pipeline finished.\n")
cat("Output root:", output_root, "\n")
cat("Clean data:", clean_csv, "\n")
cat("Summary report:", report_path, "\n")
