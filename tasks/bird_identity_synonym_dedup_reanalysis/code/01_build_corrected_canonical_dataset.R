#!/usr/bin/env Rscript

# ============================================================
# Bird new-distribution records in China:
# canonical species-identity correction and province-level
# duplicate resolution workflow
# 中国鸟类新纪录：同物异名校正与同省重复记录去重流程
# ============================================================
#
# Scientific background / 研究背景
# Bird new-distribution records are often sensitive to two kinds of data issues:
# (1) taxonomic identity inconsistency, including synonymous usage, spelling
# variation, inconsistent Chinese/English names, or formatting noise in the
# scientific name field; and (2) repeated reporting of the same species in the
# same province, where later papers may re-report an already published provincial
# record. If these issues are not handled explicitly, downstream taxonomic,
# spatiotemporal, directional, and driver analyses can overestimate both the
# number of events and the number of species involved.
# 鸟类新纪录数据最容易受到两类问题影响：
# （1）物种身份不一致，包括同物异名、拼写差异、中英文名不统一以及学名
#     字段中的格式噪声；
# （2）同一物种在同一省份被后续论文重复报道，导致事件数和物种-省份组合
#     被重复计入。
# 如果不对这两类问题进行显式处理，后续分类格局、时空格局、方向格局以及
# 驱动因素分析都可能高估新纪录事件和涉及物种的数量。
#
# Scientific questions / 科学问题
# 1. How can we convert row-level audit tables on suspected name mismatches and
#    same-species same-province duplicates into a transparent canonical-event table?
# 2. After applying species-identity correction and earliest-publication retention,
#    how much do the main descriptive denominators change?
# 1. 如何把逐行的命名异常审计表和“同物种同省重复”审计表转化为一张透明、
#    可追踪的标准事件表？
# 2. 在实施物种身份校正和“同省同物种保留最早发表记录”规则后，主要描述性
#    分母会发生多大变化？
#
# Objectives / 研究目标
# 1. Read the master bird record workbook plus the two user-provided audit files.
# 2. Build a row-level canonical scientific-name map with explicit source priority
#    and conflict checks.
# 3. Standardize province, order, year, IUCN, and coordinate fields into a clean
#    event table that remains compatible with existing downstream scripts.
# 4. Resolve duplicate species-province records by retaining the earliest
#    publication year (and the smallest row id as a deterministic tiebreaker).
# 5. Export corrected event data, trait-ready species pool updates, resolution logs,
#    diagnostic plots, and bilingual summaries.
# 1. 读取主鸟类新纪录工作簿以及用户提供的两份审计文件。
# 2. 构建具有明确优先级和冲突检查机制的逐行标准学名映射表。
# 3. 标准化省份、目、年份、IUCN 和经纬度字段，形成与既有下游脚本兼容的
#    清洗事件表。
# 4. 对“同物种-同省份”重复记录，保留最早发表年份；若年份相同，则以最小
#    序号作为可重复的确定性择优规则。
# 5. 导出校正后的事件数据、更新后的物种池性状表、校正规则日志、诊断图和
#    双语结果摘要。
#
# Analytical strategy / 分析思路
# 1. Use the master workbook as the raw event source and attach a stable row id
#    (`record_id`) equal to the workbook row order.
# 2. Convert the two audit workbooks into row-level correction candidates keyed by
#    `xu_hao`, harmonize scientific-name formatting, and prioritize sources as
#    follows: true mismatch > format mismatch > duplicate workbook > raw table.
# 3. Normalize scientific names to species-level binomials so that synonyms and
#    formatting noise can be resolved onto a comparable taxonomic identity field.
# 4. Screen missingness, conflicts, duplicate combinations, coordinate ranges,
#    and before/after summary statistics before exporting the corrected base table.
# 5. Update the existing species-trait pool by replacing the new-record counts with
#    counts derived from the corrected canonical event table.
# 1. 以主工作簿为原始事件源，并附加一个稳定的逐行标识 `record_id`，其数值
#    与工作簿行序一致。
# 2. 将两份审计表转成以 `xu_hao` 为键的逐行校正候选表，并统一学名格式；
#    校正优先级设定为：真正错配 > 格式不统一 > 重复记录表 > 原始主表。
# 3. 将学名归一到物种层 binomial，以便把同物异名和格式差异压缩到同一可比
#    的物种身份字段上。
# 4. 在导出校正底表前，显式检查缺失值、冲突、重复组合、坐标范围以及前后
#    统计量变化。
# 5. 用校正后的标准事件表重算已有物种性状池中的新纪录计数，从而为方向分析
#    和驱动分析提供一致的数据基础。
#
# Diagnostics and validation / 诊断与验证
# - Row-id coverage checks for both audit files.
# - Conflict checks when the same row receives more than one canonical species.
# - Coordinate-range checks and key-field missingness summaries.
# - Before/after comparisons for records, species, species-province combinations,
#   orders, provinces, and year span.
# - Trait-pool matching audit to quantify how many corrected species are matched to
#   the existing trait table and how many remain unmatched.
# - 对两份审计文件做序号覆盖检查。
# - 检查同一条记录是否被赋予多个标准学名。
# - 检查坐标范围与关键字段缺失值。
# - 比较校正前后在记录数、物种数、物种-省份组合数、目数、省份数和年份跨度
#   上的变化。
# - 对校正物种与既有性状池的匹配情况做审计，量化匹配和未匹配物种数。
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(janitor)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(writexl)
  library(ggplot2)
  library(patchwork)
  library(tibble)
  library(scales)
})

# -------------------------------
# Step 0. Task paths and reusable constants
# 第 0 步：任务路径与常量
# -------------------------------
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
data_dir <- file.path(task_root, "data")
fig_dir <- file.path(task_root, "figures")
results_dir <- file.path(task_root, "results")
for (dir_path in c(data_dir, fig_dir, results_dir)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

master_output_root <- "/Users/dingchenchen/Documents/New records/bird_new_records_R_output"
master_data_clean_dir <- file.path(master_output_root, "data_clean")
dir.create(master_data_clean_dir, recursive = TRUE, showWarnings = FALSE)

bird_xlsx <- "/Users/dingchenchen/Desktop/鸟类新纪录20260311.xlsx"
duplicate_xlsx <- "/Users/dingchenchen/Desktop/鸟类新纪录20260209_同物种同省重复记录.xlsx"
mismatch_xlsx <- "/Users/dingchenchen/Desktop/鸟类新纪录20260209_中文名英文名学名匹配异常.xlsx"
trait_pool_csv <- file.path(master_data_clean_dir, "bird_species_pool_with_traits.csv")

sheet_records <- "2000-2025鸟类新记录"

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
  "西藏自治区" = "Xizang",
  "宁夏回族自治区" = "Ningxia",
  "新疆维吾尔自治区" = "Xinjiang",
  "香港特别行政区" = "Hong Kong",
  "澳门特别行政区" = "Macau"
)

# -------------------------------
# Step 1. Helper functions
# 第 1 步：辅助函数
# -------------------------------
normalize_missing <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_replace_all(x, "_", " ")
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null", "-", "--")] <- NA_character_
  x
}

canonize_scientific <- function(x) {
  x <- normalize_missing(x)
  out <- str_extract(x, "[A-Za-z][A-Za-z-]+\\s+[A-Za-z-]+")
  out <- ifelse(is.na(out), x, out)
  pieces <- str_split_fixed(out, " ", 2)
  genus <- str_to_sentence(str_to_lower(pieces[, 1]))
  species <- str_to_lower(pieces[, 2])
  out <- str_squish(paste(genus, species))
  out[out %in% c("Na", "Na na")] <- NA_character_
  out
}

title_case_order <- function(x) {
  x <- normalize_missing(x)
  str_to_title(str_to_lower(x))
}

standardize_province <- function(province_cn, province_en) {
  province_cn <- normalize_missing(province_cn)
  province_en <- normalize_missing(province_en)
  province <- case_when(
    !is.na(province_cn) & province_cn %in% names(province_cn_to_en) ~ unname(province_cn_to_en[province_cn]),
    !is.na(province_en) ~ province_en,
    TRUE ~ NA_character_
  )
  recode(
    province,
    "Juangsu" = "Jiangsu",
    "Qinghai Province" = "Qinghai",
    "Taiwan Province" = "Taiwan",
    "Tibet Autonomous Region" = "Xizang",
    "Macao SAR" = "Macau",
    "Hong Kong SAR" = "Hong Kong"
  )
}

standardize_iucn <- function(x) {
  x <- normalize_missing(x)
  code <- str_extract(str_to_upper(x), "CR|EN|VU|NT|LC|DD|NE|NA")
  ifelse(is.na(code), x, code)
}

classify_discovery_reason <- function(x) {
  x <- normalize_missing(x)
  case_when(
    str_detect(x, "新发现|描述并命名") ~ "New species or formal description",
    str_detect(x, "分类变动|独立成种|重新认定|误认") & str_detect(x, "分布变化|调查") ~ "Mixed: taxonomy + distribution/survey",
    str_detect(x, "分类变动|独立成种|重新认定|误认") ~ "Taxonomic revision",
    str_detect(x, "分布变化|游荡|最北端|北端|扩展|扩散") & str_detect(x, "技术|调查不足|缺乏调查|调查扩大") ~ "Mixed: range change + survey/technology",
    str_detect(x, "分布变化|游荡|最北端|北端|扩展|扩散") ~ "Range shift or distribution change",
    str_detect(x, "技术|整理照片|公众科学|卫星") ~ "Technology or improved detection",
    str_detect(x, "调查不足|缺乏调查|调查扩大") ~ "Survey gap or under-sampling",
    is.na(x) ~ "Unclear",
    TRUE ~ "Other"
  )
}

build_paper_id <- function(title, title_en, quote, authors, journal, year) {
  case_when(
    !is.na(title) & title != "" ~ title,
    !is.na(title_en) & title_en != "" ~ title_en,
    !is.na(quote) & quote != "" ~ quote,
    !is.na(authors) & authors != "" & !is.na(journal) & journal != "" & !is.na(year) ~
      paste(authors, journal, year, sep = " | "),
    TRUE ~ NA_character_
  )
}

# -------------------------------
# Step 2. Read the master workbook and build the raw event table
# 第 2 步：读取主工作簿并构建原始事件表
# -------------------------------
raw_records <- read_xlsx(bird_xlsx, sheet = sheet_records, guess_max = 20000) %>%
  clean_names()

bird_raw <- raw_records %>%
  transmute(
    record_id = row_number(),
    workbook_seq = suppressWarnings(as.integer(xu_hao)),
    species_cn = normalize_missing(species),
    english_name = normalize_missing(englishname),
    scientific_name_raw = normalize_missing(coalesce(scientificname, scientificname_draft)),
    scientific_name_field = normalize_missing(scientificname),
    scientific_name_draft = normalize_missing(scientificname_draft),
    naming_time = suppressWarnings(as.integer(naming_time)),
    order_raw = normalize_missing(order_cn),
    order_latin = normalize_missing(order_la),
    family_raw = normalize_missing(family_la),
    genus_raw = normalize_missing(genus_la),
    title = normalize_missing(title),
    title_en = normalize_missing(title_en),
    newdistribution_province_text = normalize_missing(newdistribution_province),
    province_cn = normalize_missing(province_23),
    province_en_raw = normalize_missing(province_24),
    iucn_raw = normalize_missing(iucn_hong_se_ming_lu_deng_ji),
    discover_cause_raw = normalize_missing(discovercause),
    discovery_method_raw = normalize_missing(discoverymethod),
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude)),
    recordingtime = normalize_missing(recordingtime),
    year = suppressWarnings(as.integer(publicationyear)),
    quote = normalize_missing(quote),
    authors = normalize_missing(authors),
    journal = normalize_missing(journal),
    migratory_status = normalize_missing(migratory_status)
  ) %>%
  mutate(
    workbook_seq = if_else(is.na(workbook_seq), record_id, workbook_seq),
    species_raw_binomial = canonize_scientific(scientific_name_raw),
    order = title_case_order(order_raw),
    province = standardize_province(province_cn, province_en_raw),
    iucn = standardize_iucn(iucn_raw),
    discover_reason = classify_discovery_reason(discover_cause_raw),
    discovery_method = discovery_method_raw,
    paper_id = build_paper_id(title, title_en, quote, authors, journal, year),
    year = if_else(!is.na(year) & year >= 1900 & year <= 2100, year, NA_integer_)
  )

# -------------------------------
# Step 3. Read audit tables and build correction candidates
# 第 3 步：读取审计表并构建校正候选
# -------------------------------
read_audit_sheet <- function(path, sheet, source_name, source_priority) {
  read_xlsx(path, sheet = sheet, guess_max = 20000) %>%
    clean_names() %>%
    transmute(
      record_id = suppressWarnings(as.integer(xu_hao)),
      source_name = source_name,
      source_priority = source_priority,
      issue_group = if ("wen_ti_gui_lei" %in% names(.)) normalize_missing(wen_ti_gui_lei) else source_name,
      issue_type = if ("cuo_wu_lei_xing" %in% names(.)) normalize_missing(cuo_wu_lei_xing) else NA_character_,
      corrected_species = canonize_scientific(coalesce(scientificname, scientificname_draft)),
      corrected_species_cn = if ("species" %in% names(.)) normalize_missing(species) else NA_character_,
      corrected_english_name = if ("englishname" %in% names(.)) normalize_missing(englishname) else NA_character_
    ) %>%
    filter(!is.na(record_id))
}

audit_candidates <- bind_rows(
  read_audit_sheet(mismatch_xlsx, "疑似真正错配", "true_mismatch", 1L),
  read_audit_sheet(mismatch_xlsx, "大小写等格式不统一", "format_mismatch", 2L),
  read_audit_sheet(duplicate_xlsx, "重复记录", "duplicate_audit", 3L)
) %>%
  distinct(record_id, source_name, corrected_species, .keep_all = TRUE)

audit_coverage <- audit_candidates %>%
  summarise(
    n_candidate_rows = n(),
    n_unique_record_ids = n_distinct(record_id),
    record_id_min = min(record_id, na.rm = TRUE),
    record_id_max = max(record_id, na.rm = TRUE)
  )

correction_conflicts <- audit_candidates %>%
  filter(!is.na(corrected_species)) %>%
  group_by(record_id) %>%
  summarise(
    n_distinct_species = n_distinct(corrected_species),
    candidate_species = paste(sort(unique(corrected_species)), collapse = " | "),
    candidate_sources = paste(sort(unique(source_name)), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(n_distinct_species > 1)

correction_map <- audit_candidates %>%
  arrange(record_id, source_priority) %>%
  group_by(record_id) %>%
  summarise(
    correction_sources = paste(source_name[!is.na(corrected_species)], collapse = " | "),
    correction_source_primary = source_name[which(!is.na(corrected_species))[1]],
    canonical_species = corrected_species[which(!is.na(corrected_species))[1]],
    canonical_species_cn = corrected_species_cn[which(!is.na(corrected_species))[1]],
    canonical_english_name = corrected_english_name[which(!is.na(corrected_species))[1]],
    .groups = "drop"
  )

# -------------------------------
# Step 4. Apply row-level identity correction
# 第 4 步：应用逐行物种身份校正
# -------------------------------
bird_corrected_pre_dedup <- bird_raw %>%
  left_join(correction_map, by = "record_id") %>%
  mutate(
    species = coalesce(canonical_species, species_raw_binomial),
    species_cn = coalesce(canonical_species_cn, species_cn),
    english_name = coalesce(canonical_english_name, english_name),
    identity_change_flag = !is.na(canonical_species) & canonical_species != species_raw_binomial,
    identity_source = coalesce(correction_source_primary, "raw_table"),
    identity_notes = case_when(
      record_id %in% correction_conflicts$record_id ~ "Multiple candidate canonical names flagged; highest-priority source retained.",
      identity_change_flag ~ "Canonical species updated from audit tables.",
      !is.na(canonical_species) ~ "Canonical species confirmed by audit tables without changing the binomial.",
      TRUE ~ "No external row-level identity correction applied."
    )
  )

# -------------------------------
# Step 5. Resolve same-species same-province duplicates
# 第 5 步：按“同物种-同省份保留最早发表记录”规则去重
# -------------------------------
valid_for_resolution <- bird_corrected_pre_dedup %>%
  filter(!is.na(species), species != "", !is.na(province), province != "", !is.na(year))

duplicate_group_table <- valid_for_resolution %>%
  count(species, province, name = "n_records_same_species_province") %>%
  filter(n_records_same_species_province > 1)

duplicate_resolution_log <- valid_for_resolution %>%
  group_by(species, province) %>%
  arrange(year, record_id, .by_group = TRUE) %>%
  mutate(
    duplicate_rank = row_number(),
    earliest_year_in_group = min(year, na.rm = TRUE),
    keep_record = duplicate_rank == 1L,
    duplicate_group_size = n(),
    duplicate_rule = case_when(
      duplicate_group_size == 1 ~ "Unique species-province combination; retained.",
      keep_record ~ "Retained as the earliest publication for this species-province combination.",
      TRUE ~ "Dropped because an earlier publication already established this provincial record."
    )
  ) %>%
  ungroup()

bird_corrected <- duplicate_resolution_log %>%
  filter(keep_record) %>%
  bind_rows(
    bird_corrected_pre_dedup %>%
      filter(is.na(species) | species == "" | is.na(province) | province == "" | is.na(year)) %>%
      mutate(
        duplicate_rank = NA_integer_,
        earliest_year_in_group = NA_integer_,
        keep_record = TRUE,
        duplicate_group_size = NA_integer_,
        duplicate_rule = "Retained but excluded from duplicate-resolution groups because key fields were missing."
      )
  ) %>%
  arrange(record_id)

# -------------------------------
# Step 6. Build compatible clean dataset and updated trait pool
# 第 6 步：构建兼容既有脚本的 clean dataset 与更新后的性状池
# -------------------------------
clean_corrected <- bird_corrected %>%
  transmute(
    record_id = record_id,
    species = species,
    order_raw = order_raw,
    order_cn = order_latin,
    province_cn = province_cn,
    province_en_raw = province_en_raw,
    year = year,
    iucn_raw = iucn_raw,
    discover_cause_raw = discover_cause_raw,
    discovery_method_raw = discovery_method_raw,
    longitude = longitude,
    latitude = latitude,
    order = order,
    province = province,
    iucn = iucn,
    discover_reason = discover_reason,
    discovery_method = discovery_method,
    paper_id = paper_id,
    species_cn = species_cn,
    english_name = english_name,
    naming_time = naming_time,
    identity_source = identity_source,
    identity_change_flag = identity_change_flag,
    duplicate_group_size = duplicate_group_size,
    keep_record = keep_record
  ) %>%
  arrange(year, province, order, species, record_id)

species_counts_corrected <- clean_corrected %>%
  filter(!is.na(species), species != "") %>%
  count(species, order, name = "n_new_records") %>%
  mutate(new_record = 1L)

trait_pool_existing <- read_csv(trait_pool_csv, show_col_types = FALSE) %>%
  mutate(species = canonize_scientific(species)) %>%
  distinct(species, .keep_all = TRUE)

trait_pool_corrected <- trait_pool_existing %>%
  select(-any_of(c("n_new_records", "new_record"))) %>%
  full_join(species_counts_corrected, by = c("species", "order")) %>%
  mutate(
    n_new_records = replace_na(n_new_records, 0L),
    new_record = replace_na(new_record, 0L)
  ) %>%
  arrange(desc(new_record), desc(n_new_records), order, species)

trait_match_audit <- tibble(
  metric = c(
    "Corrected unique species",
    "Species matched to existing trait pool",
    "Species unmatched in trait pool",
    "Trait-pool rows after update"
  ),
  value = c(
    n_distinct(species_counts_corrected$species),
    sum(species_counts_corrected$species %in% trait_pool_existing$species),
    sum(!(species_counts_corrected$species %in% trait_pool_existing$species)),
    nrow(trait_pool_corrected)
  )
)

# -------------------------------
# Step 7. Diagnostics, before/after summaries, and QA tables
# 第 7 步：诊断、前后对比和 QA 表
# -------------------------------
key_field_missing_summary <- clean_corrected %>%
  summarise(
    n_missing_species = sum(is.na(species) | species == ""),
    n_missing_order = sum(is.na(order) | order == ""),
    n_missing_province = sum(is.na(province) | province == ""),
    n_missing_year = sum(is.na(year)),
    n_missing_longitude = sum(is.na(longitude)),
    n_missing_latitude = sum(is.na(latitude))
  )

coordinate_screen <- clean_corrected %>%
  transmute(
    record_id,
    species,
    province,
    longitude,
    latitude,
    longitude_out_of_range = !is.na(longitude) & (longitude < 70 | longitude > 140),
    latitude_out_of_range = !is.na(latitude) & (latitude < 0 | latitude > 60)
  )

before_after_summary <- bind_rows(
  tibble(
    stage = "Before species-province deduplication",
    n_records = nrow(valid_for_resolution),
    n_species = n_distinct(valid_for_resolution$species),
    n_species_province = n_distinct(paste(valid_for_resolution$species, valid_for_resolution$province, sep = " | ")),
    n_orders = n_distinct(valid_for_resolution$order),
    n_provinces = n_distinct(valid_for_resolution$province),
    year_min = min(valid_for_resolution$year, na.rm = TRUE),
    year_max = max(valid_for_resolution$year, na.rm = TRUE)
  ),
  {
    valid_after <- clean_corrected %>% filter(!is.na(species), species != "", !is.na(province), province != "", !is.na(year))
    tibble(
    stage = "After earliest-publication retention",
    n_records = nrow(valid_after),
    n_species = n_distinct(valid_after$species),
    n_species_province = n_distinct(paste(valid_after$species, valid_after$province, sep = " | ")),
    n_orders = n_distinct(valid_after$order),
    n_provinces = n_distinct(valid_after$province),
    year_min = min(valid_after$year, na.rm = TRUE),
    year_max = max(valid_after$year, na.rm = TRUE)
  )}
)

identity_source_summary <- bird_corrected_pre_dedup %>%
  count(identity_source, identity_change_flag, name = "n_records") %>%
  mutate(identity_change_flag = if_else(identity_change_flag, "Name changed", "No binomial change"))

duplicate_drop_by_province <- duplicate_resolution_log %>%
  filter(!keep_record, !is.na(province)) %>%
  count(province, name = "n_dropped") %>%
  arrange(desc(n_dropped)) %>%
  slice_head(n = 12)

summary_long <- before_after_summary %>%
  pivot_longer(cols = c(n_records, n_species, n_species_province), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, n_records = "Records", n_species = "Species", n_species_province = "Species-province combinations"))

p_a <- ggplot(summary_long, aes(x = metric, y = value, fill = stage)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_fill_manual(values = c("Before species-province deduplication" = "#7FA2D9", "After earliest-publication retention" = "#F29F67")) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Count", title = "Before-versus-after denominator changes") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p_b <- ggplot(identity_source_summary, aes(x = identity_source, y = n_records, fill = identity_change_flag)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Name changed" = "#D55E00", "No binomial change" = "#56B4E9")) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Rows", title = "Identity-audit contribution") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

p_c <- ggplot(duplicate_drop_by_province, aes(x = reorder(province, n_dropped), y = n_dropped)) +
  geom_col(fill = "#6BA292", width = 0.68) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Dropped later duplicate records", title = "Top provinces affected by duplicate removal") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

qa_plot <- (p_a | p_b) / p_c + plot_annotation(title = "Canonical-identity and duplicate-resolution diagnostics")

ggsave(file.path(fig_dir, "fig_qa_identity_synonym_duplicate_reanalysis.png"), qa_plot, width = 13.5, height = 9, dpi = 420, bg = "white")
ggsave(file.path(fig_dir, "fig_qa_identity_synonym_duplicate_reanalysis.pdf"), qa_plot, width = 13.5, height = 9, device = grDevices::pdf, bg = "white")

# -------------------------------
# Step 8. Export datasets, logs, and bilingual summary
# 第 8 步：导出数据、日志和双语摘要
# -------------------------------
write_csv(clean_corrected, file.path(data_dir, "bird_new_records_clean_corrected.csv"))
write_csv(trait_pool_corrected, file.path(data_dir, "bird_species_pool_with_traits_corrected.csv"))
write_csv(before_after_summary, file.path(data_dir, "before_after_summary.csv"))
write_csv(audit_candidates, file.path(data_dir, "identity_audit_candidates.csv"))
write_csv(correction_map, file.path(data_dir, "identity_correction_map.csv"))
write_csv(correction_conflicts, file.path(data_dir, "identity_correction_conflicts.csv"))
write_csv(duplicate_group_table, file.path(data_dir, "duplicate_species_province_groups.csv"))
write_csv(duplicate_resolution_log, file.path(data_dir, "duplicate_resolution_log.csv"))
write_csv(key_field_missing_summary, file.path(data_dir, "key_field_missing_summary.csv"))
write_csv(coordinate_screen, file.path(data_dir, "coordinate_screening.csv"))
write_csv(trait_match_audit, file.path(data_dir, "trait_pool_matching_audit.csv"))
write_csv(audit_coverage, file.path(data_dir, "audit_coverage_summary.csv"))

writexl::write_xlsx(
  list(
    corrected_clean = clean_corrected,
    correction_map = correction_map,
    correction_conflicts = correction_conflicts,
    duplicate_resolution_log = duplicate_resolution_log,
    before_after_summary = before_after_summary,
    trait_pool_matching_audit = trait_match_audit
  ),
  file.path(data_dir, "bird_identity_synonym_dedup_reanalysis_bundle.xlsx")
)

write_csv(clean_corrected, file.path(master_data_clean_dir, "bird_new_records_clean_corrected_identity_dedup.csv"))
write_csv(trait_pool_corrected, file.path(master_data_clean_dir, "bird_species_pool_with_traits_corrected_identity_dedup.csv"))
writexl::write_xlsx(clean_corrected, file.path(master_data_clean_dir, "bird_new_records_clean_corrected_identity_dedup.xlsx"))

summary_lines <- c(
  "# Bird identity/synonym and duplicate reanalysis summary / 鸟类同物异名与重复记录重分析摘要",
  "",
  "## English",
  paste0("- Raw master rows read: ", nrow(bird_raw)),
  paste0("- Rows with usable species-province-year fields before deduplication: ", before_after_summary$n_records[1]),
  paste0("- Rows retained after earliest-publication retention: ", before_after_summary$n_records[2]),
  paste0("- Unique species after correction: ", before_after_summary$n_species[2]),
  paste0("- Species-province duplicate groups identified: ", nrow(duplicate_group_table)),
  paste0("- Rows whose canonical species binomial changed through audit tables: ", sum(bird_corrected_pre_dedup$identity_change_flag, na.rm = TRUE)),
  paste0("- Rows with conflicting candidate canonical names requiring priority resolution: ", nrow(correction_conflicts)),
  paste0("- Corrected species matched to the existing trait pool: ", trait_match_audit$value[2], " / ", trait_match_audit$value[1]),
  "",
  "## 中文",
  paste0("- 主表读取原始行数：", nrow(bird_raw)),
  paste0("- 去重前具有可用 物种-省份-年份 字段的行数：", before_after_summary$n_records[1]),
  paste0("- 按最早发表规则保留后的行数：", before_after_summary$n_records[2]),
  paste0("- 校正后的唯一物种数：", before_after_summary$n_species[2]),
  paste0("- 识别出的同物种-同省份重复组合数：", nrow(duplicate_group_table)),
  paste0("- 通过审计表导致标准物种双名发生变化的行数：", sum(bird_corrected_pre_dedup$identity_change_flag, na.rm = TRUE)),
  paste0("- 出现多个候选标准学名、需按优先级裁决的记录数：", nrow(correction_conflicts)),
  paste0("- 能与既有性状池匹配的校正物种数：", trait_match_audit$value[2], " / ", trait_match_audit$value[1])
)
writeLines(summary_lines, file.path(results_dir, "task_summary_bilingual.md"))

cat("Canonical corrected dataset exported successfully.\n")
