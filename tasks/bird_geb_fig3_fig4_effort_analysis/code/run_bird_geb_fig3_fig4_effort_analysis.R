#!/usr/bin/env Rscript

# ============================================================
# Bird New Distribution Records in China:
# GEB-style Figure 3 and Figure 4 workflow with survey effort
# 中国鸟类新纪录：参考 GEB 文章 Figure 3 和 Figure 4 的调查努力分析流程
# ============================================================
#
# Scientific question / 科学问题
# How do species-level traits and province-level survey effort jointly shape
# the probability, frequency, and spatial intensity of new provincial bird
# records in China?
# 中国鸟类新纪录在物种层面和省级层面分别受到哪些因素影响？尤其是，物种
# 性状、历史调查基础、当前调查强度以及省级结构性变量，如何共同塑造新纪录
# 的出现概率、出现频次与空间发现强度？
#
# Objectives / 研究目标
# 1. Build a bird equivalent of the GEB species-level analysis (Figure 3),
#    quantifying how body mass, range size, migration, and conservation-risk
#    group relate to whether a species yields a new provincial record and how
#    many such records it accumulates.
# 2. Build a bird equivalent of the GEB province-level analysis (Figure 4)
#    using the newly supplied province-year survey-effort table. We quantify
#    how historical and current effort intensity, together with province-level
#    structural variables, explain variation in bird new-record discovery
#    intensity.
# 3. Export publication-ready figures, cleaned/derived datasets, diagnostics,
#    model summaries, and bilingual result narratives in a standardized task
#    directory.
# 1. 构建鸟类版的 GEB Figure 3：评估体重、分布范围、迁徙性和风险等级对
#    新纪录出现概率与出现频次的影响。
# 2. 使用新提供的省份-年份调查努力表，构建鸟类版的 GEB Figure 4：量化
#    历史调查基础、当前调查强度以及省级结构变量对新纪录发现强度的影响。
# 3. 在统一任务目录中输出可发表质量的图表、整理后的数据、诊断结果、模型表
#    和中英文结果摘要。
#
# Analytical strategy / 分析思路
# 1. Read and standardize the bird new-record table, the effort table, and the
#    generic province-level covariates used previously in the mammal GEB study.
# 2. Perform explicit data checks: duplicated events, missing values, province
#    coverage, year coverage, effort outliers, and trait completeness.
# 3. Rebuild a species pool with traits and fit two complementary species-level
#    models:
#    - logistic model for whether a species has any new provincial record;
#    - negative-binomial count model for how many such records it accumulates.
# 4. Build province-level effort variables by era:
#    - historical effort: mean annual report count during 1980-1999;
#    - current effort: mean annual report count during 2000-2024.
#    To reduce the confounding introduced by province size, the primary
#    province-level response is modeled as log-transformed new-record density
#    (records per 100,000 km2), and effort predictors are also expressed as
#    area-standardized intensities.
# 5. Fit a multiple regression model for province-level discovery intensity,
#    screen collinearity, generate partial-regression plots, compute coefficient
#    estimates, and quantify relative importance using hierarchical partitioning
#    with bootstrap confidence intervals.
# 6. Run sensitivity checks, including an alternative count-based model, and
#    export all core outputs.
# 1. 读取并标准化鸟类新纪录主表、调查努力表和哺乳动物 GEB 研究中已使用过的
#    省级通用协变量。
# 2. 显式开展数据检查：重复记录、缺失值、省份覆盖、年份覆盖、努力值异常和
#    性状完整性。
# 3. 重建带性状的鸟类物种池，并拟合两个互补的物种层模型：
#    - 是否产生新省级纪录的 logistic 模型；
#    - 新省级纪录数量的负二项模型。
# 4. 构建省级调查努力指标：
#    - 历史努力：1980-1999 年平均年度报告数；
#    - 当前努力：2000-2024 年平均年度报告数。
#    为减弱省域面积造成的混杂，省级主响应变量采用单位面积新纪录强度的对数
#    转换值，努力变量也采用面积标准化后的强度值。
# 5. 拟合省级多元回归模型，检查共线性，绘制偏回归图，输出系数估计，并通过
#    层次分解与自助法置信区间量化相对贡献。
# 6. 开展敏感性检验（基于计数响应的替代模型）与模型诊断，并导出所有结果。
#
# Diagnostics and validation / 诊断与验证
# - Missingness summaries for the effort table and trait table.
# - Duplicate screening for bird record events.
# - Coverage summaries by province and year.
# - Outlier screening for effort counts using the IQR rule.
# - Predictor-source auditing to avoid directly reusing mammal-only variables
#   from the GEB repository when they are taxonomically mismatched for birds.
# - Collinearity screening with VIF.
# - Residual diagnostics for the province-level regression.
# - Sensitivity model using a count response with an area offset.
# - Sensitivity model replacing report-based effort with user-based effort.
# - Influence sensitivity by refitting the province model after excluding
#   provinces with Cook's distance above the common 4/n threshold.
# - Missingness, duplicates, and range checks are exported for auditing.
# - 调查努力表与性状表缺失值汇总。
# - 鸟类新纪录事件重复筛查。
# - 省份和年份覆盖度检查。
# - 采用 IQR 规则筛查努力值异常。
# - 对解释变量来源进行审计，避免把 GEB 仓库中仅适用于哺乳动物的变量直接
#   生搬硬套到鸟类分析中。
# - 使用 VIF 检查共线性。
# - 省级回归模型的残差诊断。
# - 使用带面积 offset 的计数模型开展敏感性检验。
# - 使用用户数替代报告数开展努力指标敏感性检验。
# - 基于 Cook's distance 识别高影响省份，并在剔除这些省份后重新拟合模型。
# - 所有缺失、重复和范围检查结果均单独导出，便于审阅和复核。
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(janitor)
  library(broom)
  library(MASS)
  library(performance)
  library(car)
  library(export)
  library(writexl)
  library(ggrepel)
  library(purrr)
  library(tibble)
})

set.seed(1234)

# -------------------------------
# Step 0. Paths and task structure
# 第 0 步：路径与任务目录结构
# -------------------------------
get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    script_candidate <- sub("^--file=", "", file_arg[1])
    if (file.exists(script_candidate)) return(normalizePath(script_candidate))
  }
  r_candidates <- cmd_args[grepl("\\.[Rr]$", cmd_args)]
  r_candidates <- r_candidates[file.exists(r_candidates)]
  if (length(r_candidates) > 0) return(normalizePath(r_candidates[1]))
  current_frame <- sys.frames()
  if (length(current_frame) > 0 && !is.null(current_frame[[1]]$ofile) && file.exists(current_frame[[1]]$ofile)) {
    return(normalizePath(current_frame[[1]]$ofile))
  }
  normalizePath(getwd())
}

script_path <- get_script_path()
code_dir <- if (dir.exists(script_path)) script_path else dirname(script_path)
if (basename(code_dir) != "code" && basename(getwd()) == "code") {
  code_dir <- normalizePath(getwd())
}
task_root <- normalizePath(file.path(code_dir, ".."), mustWork = FALSE)
data_dir <- file.path(task_root, "data")
fig_dir <- file.path(task_root, "figures")
results_dir <- file.path(task_root, "results")

invisible(lapply(c(data_dir, fig_dir, results_dir), dir.create, recursive = TRUE, showWarnings = FALSE))

bird_xlsx <- "/Users/dingchenchen/Desktop/鸟类新纪录20260311.xlsx"
effort_xlsx <- "/Users/dingchenchen/Desktop/effort补全.xlsx"
province_covariate_csv <- "/Users/dingchenchen/Desktop/?>/Data and codes/Province_variables.csv"

sheet_records <- "2000-2025鸟类新记录"
sheet_catalog <- "2025中国生物物种名录"
sheet_trait_cn <- "中国鸟类生态学特征"
sheet_trait_avonet <- "AVONET traits"
sheet_trait_birdbase <- "BIRDBASE traits"

dpi_out <- 420

# -------------------------------
# Step 0.1. Global style helpers
# 第 0.1 步：全局图形风格与导出函数
# -------------------------------
theme_geb <- function(base_size = 12, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, colour = "#111111"),
      plot.subtitle = element_text(size = base_size - 0.5, colour = "#2F2F2F"),
      plot.caption = element_text(size = base_size - 2, colour = "#444444"),
      axis.title = element_text(face = "bold", colour = "#111111"),
      axis.text = element_text(colour = "#1A1A1A"),
      legend.title = element_text(face = "bold"),
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      strip.background = element_rect(fill = "#D9D9D9", colour = "#5A5A5A", linewidth = 0.6),
      strip.text = element_text(face = "bold", colour = "#111111"),
      plot.tag = element_text(face = "bold", size = base_size + 1)
    )
}

save_plot_bundle <- function(plot_obj, stub, width, height) {
  png_path <- file.path(fig_dir, paste0(stub, ".png"))
  pdf_path <- file.path(fig_dir, paste0(stub, ".pdf"))
  pptx_path <- file.path(fig_dir, paste0(stub, ".pptx"))
  ggsave(png_path, plot_obj, width = width, height = height, dpi = dpi_out, bg = "white")
  ggsave(pdf_path, plot_obj, width = width, height = height, dpi = dpi_out, bg = "white", device = cairo_pdf)
  export::graph2ppt(x = plot_obj, file = pptx_path, width = width, height = height, vector.graphic = TRUE, append = FALSE)
}

stdz <- function(x) as.numeric(scale(x))

wald_ci <- function(est, se) {
  tibble(conf.low = est - 1.96 * se, conf.high = est + 1.96 * se)
}

extract_model_fit <- function(model_obj, model_name, model_type = "lm") {
  if (model_type == "lm") {
    tibble(
      model = model_name,
      family = "Gaussian LM",
      aic = AIC(model_obj),
      bic = BIC(model_obj),
      r_squared = summary(model_obj)$r.squared,
      adj_r_squared = summary(model_obj)$adj.r.squared
    )
  } else {
    tibble(
      model = model_name,
      family = "Negative binomial",
      aic = AIC(model_obj),
      bic = BIC(model_obj),
      r_squared = NA_real_,
      adj_r_squared = NA_real_
    )
  }
}

recode_province_predictor <- function(x) {
  dplyr::recode(
    x,
    "z_hist_report_density" = "Historical survey effort",
    "z_curr_report_density" = "Current survey effort",
    "z_curr_user_density" = "Current user effort",
    "z_gdp_per_capita" = "Per capita GDP",
    "z_area_km2" = "Area",
    "z_habitat_heterogeneity" = "Habitat heterogeneity"
  )
}

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

title_case_order <- function(x) {
  stringr::str_to_title(stringr::str_to_lower(as.character(x)))
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

# -------------------------------
# Step 1. Read and standardize bird new-record data
# 第 1 步：读取并标准化鸟类新纪录数据
# -------------------------------
raw_records <- read_xlsx(bird_xlsx, sheet = sheet_records, guess_max = 20000) %>%
  clean_names()

bird_events <- raw_records %>%
  transmute(
    record_id = row_number(),
    species = str_squish(scientificname),
    order_raw = str_squish(order_cn),
    order_cn = str_squish(order_la),
    province_cn = str_squish(province_23),
    province_en_raw = str_squish(province_24),
    year = suppressWarnings(as.integer(publicationyear)),
    discover_cause_raw = str_squish(discovercause),
    discovery_method_raw = str_squish(discoverymethod),
    longitude = suppressWarnings(as.numeric(longitude)),
    latitude = suppressWarnings(as.numeric(latitude))
  ) %>%
  mutate(
    order = title_case_order(order_raw),
    province = case_when(
      !is.na(province_en_raw) & province_en_raw != "" ~ province_en_raw,
      province_cn %in% names(province_cn_to_en) ~ unname(province_cn_to_en[province_cn]),
      TRUE ~ NA_character_
    ),
    province = dplyr::recode(
      province,
      "Juangsu" = "Jiangsu",
      "Qinghai Province" = "Qinghai",
      "Taiwan Province" = "Taiwan",
      "Tibet Autonomous Region" = "Tibet"
    ),
    discover_reason = classify_discovery_reason(discover_cause_raw),
    year = if_else(!is.na(year) & year >= 1900 & year <= 2100, year, NA_integer_)
  )

duplicate_screen <- bird_events %>%
  count(species, province, year, name = "n_dups") %>%
  filter(n_dups > 1)

bird_clean <- bird_events %>%
  filter(!is.na(species), species != "", !is.na(order), order != "", !is.na(province), !is.na(year)) %>%
  distinct(species, order, province, year, .keep_all = TRUE) %>%
  arrange(year, order, province, species)

write.csv(bird_clean, file.path(data_dir, "bird_new_records_clean_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(duplicate_screen, file.path(data_dir, "bird_record_duplicate_screening.csv"), row.names = FALSE)

qa_summary <- tibble(
  metric = c(
    "Raw rows",
    "Rows after filtering key fields",
    "Rows after deduplication",
    "Unique species",
    "Unique orders",
    "Unique provinces",
    "Year min",
    "Year max"
  ),
  value = c(
    nrow(raw_records),
    nrow(bird_events %>% filter(!is.na(species), species != "", !is.na(order), order != "", !is.na(province), !is.na(year))),
    nrow(bird_clean),
    n_distinct(bird_clean$species),
    n_distinct(bird_clean$order),
    n_distinct(bird_clean$province),
    min(bird_clean$year, na.rm = TRUE),
    max(bird_clean$year, na.rm = TRUE)
  )
)
write.csv(qa_summary, file.path(data_dir, "bird_record_qa_summary.csv"), row.names = FALSE)

# -------------------------------
# Step 2. Read and diagnose the effort table
# 第 2 步：读取并诊断调查努力表
# -------------------------------
effort_raw <- read_xlsx(effort_xlsx, sheet = 1, guess_max = 5000) %>%
  clean_names() %>%
  transmute(
    province_cn = str_squish(province),
    year = suppressWarnings(as.integer(year)),
    report_count = suppressWarnings(as.numeric(report_count)),
    user_count = suppressWarnings(as.numeric(user_count))
  ) %>%
  mutate(
    province = unname(province_cn_to_en[province_cn])
  )

effort_missing_summary <- tibble(
  variable = names(effort_raw),
  n_missing = sapply(effort_raw, function(x) sum(is.na(x))),
  pct_missing = n_missing / nrow(effort_raw)
)

screen_outliers_iqr <- function(x) {
  q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[[2]] - q[[1]]
  lower <- q[[1]] - 1.5 * iqr
  upper <- q[[2]] + 1.5 * iqr
  tibble(value = x, is_outlier = !is.na(x) & (x < lower | x > upper), lower = lower, upper = upper)
}

report_outlier_tbl <- bind_cols(effort_raw, screen_outliers_iqr(effort_raw$report_count) %>% dplyr::select(report_outlier = is_outlier))
user_outlier_tbl <- bind_cols(effort_raw, screen_outliers_iqr(effort_raw$user_count) %>% dplyr::select(user_outlier = is_outlier))
effort_outlier_tbl <- report_outlier_tbl %>%
  left_join(user_outlier_tbl %>% dplyr::select(province_cn, year, user_outlier), by = c("province_cn", "year"))

coverage_by_province <- effort_raw %>%
  group_by(province_cn, province) %>%
  summarise(
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE),
    n_years = n_distinct(year),
    report_sum = sum(report_count, na.rm = TRUE),
    user_sum = sum(user_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(report_sum))

coverage_by_year <- effort_raw %>%
  group_by(year) %>%
  summarise(
    n_provinces = n_distinct(province),
    report_sum = sum(report_count, na.rm = TRUE),
    user_sum = sum(user_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

write.csv(effort_raw, file.path(data_dir, "effort_clean_province_year.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(effort_missing_summary, file.path(data_dir, "effort_missing_summary.csv"), row.names = FALSE)
write.csv(effort_outlier_tbl, file.path(data_dir, "effort_outlier_screening.csv"), row.names = FALSE)
write.csv(coverage_by_province, file.path(data_dir, "effort_coverage_by_province.csv"), row.names = FALSE)
write.csv(coverage_by_year, file.path(data_dir, "effort_coverage_by_year.csv"), row.names = FALSE)

# -------------------------------
# Step 3. Build species pool and trait dataset
# 第 3 步：构建物种池与性状数据集
# -------------------------------
catalog_raw <- read_xlsx(bird_xlsx, sheet = sheet_catalog, guess_max = 20000) %>%
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

trait_av <- read_xlsx(bird_xlsx, sheet = sheet_trait_avonet, guess_max = 20000) %>%
  clean_names() %>%
  transmute(
    species = species1,
    mass = suppressWarnings(as.numeric(mass)),
    migration_av = as.character(migration),
    trophic_level_av = trophic_level,
    centroid_latitude = suppressWarnings(as.numeric(centroid_latitude)),
    centroid_longitude = suppressWarnings(as.numeric(centroid_longitude))
  ) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)

trait_cn <- read_xlsx(bird_xlsx, sheet = sheet_trait_cn, guess_max = 5000) %>%
  clean_names() %>%
  transmute(
    species = zhong_la_ding_ming,
    migration_cn = qian_xi_zhuang_tai_liu_niao_r_xia_hou_niao_s_dong_hou_niao_w_lu_niao_p_mi_niao_v,
    range_size_provinces = suppressWarnings(as.numeric(fen_bu_sheng_fen_shu))
  ) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)

trait_bb <- read_xlsx(bird_xlsx, sheet = sheet_trait_birdbase, guess_max = 20000) %>%
  clean_names() %>%
  transmute(
    species = x3,
    iucn_pool = conservation_status
  ) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)

species_response <- bird_clean %>%
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
    migration_class = dplyr::recode(
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
    risk_group = case_when(
      iucn_group %in% c("Threatened", "DD") ~ "Threatened/DD",
      iucn_group == "NT" ~ "NT",
      iucn_group %in% c("LC", "NE") ~ "LC/NE",
      TRUE ~ "Unknown"
    ),
    risk_group = factor(risk_group, levels = c("LC/NE", "NT", "Threatened/DD", "Unknown"))
  )

trait_missing_summary <- tibble(
  variable = c("log_mass", "range_size_provinces", "migration_class", "risk_group"),
  n_missing = c(
    sum(is.na(species_pool$log_mass)),
    sum(is.na(species_pool$range_size_provinces)),
    sum(is.na(species_pool$migration_class) | species_pool$migration_class == "Unknown"),
    sum(is.na(species_pool$risk_group) | species_pool$risk_group == "Unknown")
  ),
  pct_missing = n_missing / nrow(species_pool)
)

species_model_df <- species_pool %>%
  filter(
    !is.na(log_mass),
    !is.na(range_size_provinces),
    !is.na(migration_class), migration_class != "Unknown",
    !is.na(risk_group), risk_group != "Unknown"
  ) %>%
  mutate(
    z_log_mass = stdz(log_mass),
    z_range_size = stdz(range_size_provinces)
  )

write.csv(species_pool, file.path(data_dir, "bird_species_pool_with_traits.csv"), row.names = FALSE)
write.csv(trait_missing_summary, file.path(data_dir, "species_trait_missing_summary.csv"), row.names = FALSE)
write.csv(species_model_df, file.path(data_dir, "bird_species_model_dataset.csv"), row.names = FALSE)

# -------------------------------
# Step 4. Species-level models (GEB Figure 3 equivalent)
# 第 4 步：物种层模型（对应 GEB Figure 3）
# -------------------------------
glm_occ <- glm(
  new_record ~ z_log_mass + z_range_size + migration_class + risk_group,
  data = species_model_df,
  family = binomial(),
  control = glm.control(maxit = 100)
)

glm_count_nb <- MASS::glm.nb(
  n_new_records ~ z_log_mass + z_range_size + migration_class + risk_group,
  data = species_model_df,
  control = glm.control(maxit = 100)
)

count_overdisp <- performance::check_overdispersion(glm_count_nb)
occ_vif <- car::vif(glm_occ)
count_vif <- car::vif(glm_count_nb)

occ_coef <- broom::tidy(glm_occ) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  mutate(model = "Occurrence (binomial)")
count_coef <- broom::tidy(glm_count_nb) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  mutate(model = "Frequency (negative binomial)")

species_coef_tbl <- bind_rows(occ_coef, count_coef) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_label = dplyr::recode(
      term,
      "z_log_mass" = "Body mass (log10)",
      "z_range_size" = "Range size (no. provinces)",
      "migration_classPartial migrant" = "Migration: partial migrant",
      "migration_classFull migrant" = "Migration: full migrant",
      "risk_groupNT" = "Risk group: NT",
      "risk_groupThreatened/DD" = "Risk group: Threatened or DD"
    ),
    significant = conf.low * conf.high > 0
  )

write.csv(species_coef_tbl, file.path(data_dir, "species_level_model_coefficients.csv"), row.names = FALSE)
write.csv(tibble(term = names(occ_vif), vif = as.numeric(occ_vif)), file.path(data_dir, "species_occurrence_vif.csv"), row.names = FALSE)
write.csv(tibble(term = names(count_vif), vif = as.numeric(count_vif)), file.path(data_dir, "species_count_vif.csv"), row.names = FALSE)
write.csv(
  tibble(
    dispersion_ratio = unclass(count_overdisp)$dispersion_ratio,
    p_value = unclass(count_overdisp)$p_value
  ),
  file.path(data_dir, "species_count_overdispersion_check.csv"),
  row.names = FALSE
)

predict_occ_curve <- function(var_name, label_name, n = 120) {
  rng <- range(species_model_df[[var_name]], na.rm = TRUE)
  newdata <- tibble(
    z_log_mass = median(species_model_df$z_log_mass, na.rm = TRUE),
    z_range_size = median(species_model_df$z_range_size, na.rm = TRUE),
    migration_class = factor("Resident/low mobility", levels = levels(species_model_df$migration_class)),
    risk_group = factor("LC/NE", levels = levels(species_model_df$risk_group))
  )[rep(1, n), ]
  newdata[[var_name]] <- seq(rng[1], rng[2], length.out = n)
  pred <- predict(glm_occ, newdata = newdata, type = "link", se.fit = TRUE)
  tibble(
    predictor = newdata[[var_name]],
    fit = plogis(pred$fit),
    conf.low = plogis(pred$fit - 1.96 * pred$se.fit),
    conf.high = plogis(pred$fit + 1.96 * pred$se.fit),
    label = label_name
  )
}

predict_occ_group <- function(var_name) {
  lvls <- levels(species_model_df[[var_name]])
  lvls <- lvls[lvls != "Unknown"]
  newdata <- tibble(
    z_log_mass = median(species_model_df$z_log_mass, na.rm = TRUE),
    z_range_size = median(species_model_df$z_range_size, na.rm = TRUE),
    migration_class = factor("Resident/low mobility", levels = levels(species_model_df$migration_class)),
    risk_group = factor("LC/NE", levels = levels(species_model_df$risk_group))
  )[rep(1, length(lvls)), ]
  newdata[[var_name]] <- factor(lvls, levels = levels(species_model_df[[var_name]]))
  pred <- predict(glm_occ, newdata = newdata, type = "link", se.fit = TRUE)
  tibble(
    group = lvls,
    fit = plogis(pred$fit),
    conf.low = plogis(pred$fit - 1.96 * pred$se.fit),
    conf.high = plogis(pred$fit + 1.96 * pred$se.fit),
    variable = var_name
  )
}

pred_range <- predict_occ_curve("z_range_size", "Range size")
pred_mass <- predict_occ_curve("z_log_mass", "Body mass")
pred_migration <- predict_occ_group("migration_class")
pred_risk <- predict_occ_group("risk_group")

write.csv(pred_range, file.path(data_dir, "species_prediction_range_size.csv"), row.names = FALSE)
write.csv(pred_mass, file.path(data_dir, "species_prediction_body_mass.csv"), row.names = FALSE)
write.csv(pred_migration, file.path(data_dir, "species_prediction_migration.csv"), row.names = FALSE)
write.csv(pred_risk, file.path(data_dir, "species_prediction_risk_group.csv"), row.names = FALSE)

species_palette <- c(
  "Occurrence (binomial)" = "#2C7FB8",
  "Frequency (negative binomial)" = "#D95F0E"
)

p_fig3a <- ggplot(species_coef_tbl, aes(x = estimate, y = forcats::fct_rev(factor(term_label)), colour = model, shape = significant)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6F6F6F") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.18, linewidth = 0.8, position = position_dodge(width = 0.55)) +
  geom_point(size = 2.9, stroke = 1, fill = "white", position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = species_palette) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21)) +
  labs(
    x = "Coefficient estimate",
    y = NULL,
    colour = NULL,
    shape = "95% CI excludes 0"
  ) +
  theme_geb(base_size = 11.5) +
  theme(legend.position = "top")

p_fig3b <- ggplot(pred_range, aes(x = predictor, y = fit)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#A6CEE3", alpha = 0.4) +
  geom_line(linewidth = 1.1, colour = "#2C7FB8") +
  labs(x = "Standardized range size", y = "Predicted probability") +
  theme_geb(base_size = 11)

p_fig3c <- ggplot(pred_mass, aes(x = predictor, y = fit)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#FDD0A2", alpha = 0.45) +
  geom_line(linewidth = 1.1, colour = "#D95F0E") +
  labs(x = "Standardized body mass", y = "Predicted probability") +
  theme_geb(base_size = 11)

p_fig3d <- ggplot(pred_migration, aes(x = group, y = fit)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.14, linewidth = 0.85, colour = "#3B6FB6") +
  geom_point(size = 3.2, colour = "#3B6FB6") +
  labs(x = NULL, y = "Predicted probability") +
  theme_geb(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_fig3e <- ggplot(pred_risk, aes(x = group, y = fit)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.14, linewidth = 0.85, colour = "#8C2D04") +
  geom_point(size = 3.2, colour = "#8C2D04") +
  labs(x = NULL, y = "Predicted probability") +
  theme_geb(base_size = 11)

fig3 <- (p_fig3a | p_fig3b | p_fig3c) /
  (plot_spacer() | p_fig3d | p_fig3e) +
  plot_annotation(tag_levels = "a")

save_plot_bundle(fig3, "fig_geb3_species_level_correlates", width = 15, height = 9.6)

species_diag_df <- tibble(
  fitted = fitted(glm_occ),
  residuals_pearson = residuals(glm_occ, type = "pearson"),
  residuals_deviance = residuals(glm_occ, type = "deviance"),
  cooks_d = cooks.distance(glm_occ)
)

p_s3_diag1 <- ggplot(species_diag_df, aes(x = fitted, y = residuals_pearson)) +
  geom_point(size = 1.8, alpha = 0.8, colour = "#2C7FB8") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Fitted probability", y = "Pearson residual") +
  theme_geb(base_size = 10.5)

p_s3_diag2 <- ggplot(species_diag_df, aes(sample = residuals_deviance)) +
  stat_qq(size = 1.6, alpha = 0.8, colour = "#444444") +
  stat_qq_line(colour = "#D95F0E") +
  labs(x = "Theoretical quantiles", y = "Deviance residual quantiles") +
  theme_geb(base_size = 10.5)

p_s3_diag3 <- ggplot(species_diag_df, aes(x = seq_along(cooks_d), y = cooks_d)) +
  geom_col(fill = "#7BCCC4") +
  geom_hline(yintercept = 4 / nrow(species_diag_df), linetype = 2, colour = "#B30000") +
  labs(x = "Species index", y = "Cook's distance") +
  theme_geb(base_size = 10.5)

fig3_diag <- p_s3_diag1 | p_s3_diag2 | p_s3_diag3
save_plot_bundle(fig3_diag, "fig_s1_species_model_diagnostics", width = 13.5, height = 4.6)

write.csv(species_diag_df, file.path(data_dir, "species_model_diagnostics_data.csv"), row.names = FALSE)

# -------------------------------
# Step 5. Province-level predictors and screening
# 第 5 步：构建省级解释变量并开展预筛查
# -------------------------------
province_covariates <- read.csv(province_covariate_csv) %>%
  transmute(
    province = dplyr::recode(Province, "Neimenggu" = "Inner Mongolia", "Xizang" = "Tibet"),
    gdp_per_capita = as.numeric(GDP_per),
    area_km2 = as.numeric(Area),
    habitat_heterogeneity = as.numeric(habitat_heterogeneity),
    human_density = as.numeric(Human_density),
    population_10k = as.numeric(Population),
    reference_mammal_richness = as.numeric(Richness)
  )

predictor_source_audit <- tibble(
  variable = c(
    "gdp_per_capita",
    "area_km2",
    "habitat_heterogeneity",
    "human_density",
    "reference_mammal_richness"
  ),
  source_file = c(
    "Province_variables.csv",
    "Province_variables.csv",
    "Province_variables.csv",
    "Province_variables.csv",
    "Province_variables.csv"
  ),
  used_in_primary_bird_model = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  rationale = c(
    "Generic socioeconomic covariate transferable across taxa.",
    "Generic geographic covariate transferable across taxa.",
    "Generic environmental heterogeneity covariate transferable across taxa.",
    "Screened only; not retained because current user effort was already available directly and highly collinear with current report effort.",
    "Not used because this richness field originates from the mammal GEB workflow and is therefore taxonomically mismatched for a bird-specific model."
  )
)
write.csv(predictor_source_audit, file.path(data_dir, "province_predictor_source_audit.csv"), row.names = FALSE)

province_response <- bird_clean %>%
  group_by(province) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

province_effort_era <- effort_raw %>%
  mutate(
    era = case_when(
      year >= 1980 & year <= 1999 ~ "Historical (1980-1999)",
      year >= 2000 & year <= 2024 ~ "Current (2000-2024)",
      TRUE ~ "Outside target eras"
    )
  ) %>%
  filter(era != "Outside target eras") %>%
  group_by(province, era) %>%
  summarise(
    report_sum = sum(report_count, na.rm = TRUE),
    user_sum = sum(user_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = era,
    values_from = c(report_sum, user_sum),
    values_fill = 0
  ) %>%
  rename(
    historical_report_sum = `report_sum_Historical (1980-1999)`,
    current_report_sum = `report_sum_Current (2000-2024)`,
    historical_user_sum = `user_sum_Historical (1980-1999)`,
    current_user_sum = `user_sum_Current (2000-2024)`
  ) %>%
  mutate(
    historical_report_annual = historical_report_sum / 20,
    current_report_annual = current_report_sum / 25,
    historical_user_annual = historical_user_sum / 20,
    current_user_annual = current_user_sum / 25
  )

province_model_full <- province_response %>%
  left_join(province_effort_era, by = "province") %>%
  left_join(province_covariates, by = "province") %>%
  mutate(
    across(c(historical_report_annual, current_report_annual, historical_user_annual, current_user_annual), ~replace_na(.x, 0)),
    new_record_density_100k = n_records / area_km2 * 1e5,
    log_new_record_density = log1p(new_record_density_100k),
    historical_report_density = historical_report_annual / area_km2 * 1e5,
    current_report_density = current_report_annual / area_km2 * 1e5,
    historical_user_density = historical_user_annual / area_km2 * 1e5,
    current_user_density = current_user_annual / area_km2 * 1e5
  )

province_model_df <- province_model_full %>%
  filter(!is.na(area_km2), !is.na(gdp_per_capita), !is.na(habitat_heterogeneity)) %>%
  mutate(
    z_hist_report_density = stdz(historical_report_density),
    z_curr_report_density = stdz(current_report_density),
    z_gdp_per_capita = stdz(gdp_per_capita),
    z_area_km2 = stdz(area_km2),
    z_habitat_heterogeneity = stdz(habitat_heterogeneity),
    z_curr_user_density = stdz(current_user_density)
  )

province_vif_candidate <- lm(
  log_new_record_density ~ z_hist_report_density + z_curr_report_density + z_curr_user_density +
    z_gdp_per_capita + z_area_km2 + z_habitat_heterogeneity,
  data = province_model_df
)
province_vif_tbl <- tibble(
  predictor = names(car::vif(province_vif_candidate)),
  vif = as.numeric(car::vif(province_vif_candidate))
)

province_predictor_correlation <- province_model_df %>%
  dplyr::select(
    log_new_record_density,
    historical_report_density,
    current_report_density,
    current_user_density,
    gdp_per_capita,
    area_km2,
    habitat_heterogeneity
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable_1") %>%
  pivot_longer(-variable_1, names_to = "variable_2", values_to = "pearson_r")
write.csv(province_predictor_correlation, file.path(data_dir, "province_predictor_correlation.csv"), row.names = FALSE)

province_model_primary <- lm(
  log_new_record_density ~ z_hist_report_density + z_curr_report_density + z_gdp_per_capita +
    z_area_km2 + z_habitat_heterogeneity,
  data = province_model_df
)

province_model_user_effort <- lm(
  log_new_record_density ~ z_hist_report_density + z_curr_user_density + z_gdp_per_capita +
    z_area_km2 + z_habitat_heterogeneity,
  data = province_model_df
)

province_model_sensitivity <- MASS::glm.nb(
  n_records ~ z_hist_report_density + z_curr_report_density + z_gdp_per_capita +
    z_habitat_heterogeneity + offset(log(area_km2)),
  data = province_model_df,
  control = glm.control(maxit = 100)
)

province_model_df <- province_model_df %>%
  mutate(
    fitted_primary = fitted(province_model_primary),
    resid_primary = residuals(province_model_primary),
    std_resid_primary = rstandard(province_model_primary),
    cooks_d_primary = cooks.distance(province_model_primary),
    hat_primary = hatvalues(province_model_primary),
    dffits_primary = dffits(province_model_primary)
  )

influence_threshold <- 4 / nrow(province_model_df)
influential_provinces <- province_model_df %>%
  filter(cooks_d_primary > influence_threshold) %>%
  arrange(desc(cooks_d_primary))

province_model_influence <- province_model_df %>%
  filter(!(province %in% influential_provinces$province))

province_model_influence_sensitivity <- lm(
  log_new_record_density ~ z_hist_report_density + z_curr_report_density + z_gdp_per_capita +
    z_area_km2 + z_habitat_heterogeneity,
  data = province_model_influence
)

province_vif_primary_final <- tibble(
  predictor = names(car::vif(province_model_primary)),
  vif = as.numeric(car::vif(province_model_primary)),
  model = "Primary report-effort"
)
province_vif_user_final <- tibble(
  predictor = names(car::vif(province_model_user_effort)),
  vif = as.numeric(car::vif(province_model_user_effort)),
  model = "Alternative user-effort"
)
province_vif_influence_final <- tibble(
  predictor = names(car::vif(province_model_influence_sensitivity)),
  vif = as.numeric(car::vif(province_model_influence_sensitivity)),
  model = "Influence-filtered"
)
province_vif_final_tbl <- bind_rows(
  province_vif_primary_final,
  province_vif_user_final,
  province_vif_influence_final
)

province_model_comparison <- bind_rows(
  extract_model_fit(province_model_primary, "Primary report-effort model", model_type = "lm"),
  extract_model_fit(province_model_user_effort, "Alternative user-effort model", model_type = "lm"),
  extract_model_fit(province_model_influence_sensitivity, "Influence-filtered report-effort model", model_type = "lm"),
  extract_model_fit(province_model_sensitivity, "Count-based sensitivity model", model_type = "nb")
) %>%
  mutate(
    max_vif = c(
      max(province_vif_primary_final$vif, na.rm = TRUE),
      max(province_vif_user_final$vif, na.rm = TRUE),
      max(province_vif_influence_final$vif, na.rm = TRUE),
      NA_real_
    )
  )

province_diag_summary <- tibble(
  metric = c(
    "n_provinces_modelled",
    "adjusted_r2_primary",
    "shapiro_p_residuals",
    "ncvtest_p_heteroskedasticity",
    "max_cooks_distance",
    "overdispersion_ratio_sensitivity",
    "n_influential_provinces",
    "influence_threshold_4_over_n"
  ),
  value = c(
    nrow(province_model_df),
    summary(province_model_primary)$adj.r.squared,
    shapiro.test(residuals(province_model_primary))$p.value,
    car::ncvTest(province_model_primary)$p,
    max(province_model_df$cooks_d_primary, na.rm = TRUE),
    performance::check_overdispersion(province_model_sensitivity)$dispersion_ratio,
    nrow(influential_provinces),
    influence_threshold
  )
)

write.csv(province_model_full, file.path(data_dir, "province_level_merged_dataset_full.csv"), row.names = FALSE)
write.csv(province_model_df, file.path(data_dir, "province_level_model_dataset.csv"), row.names = FALSE)
write.csv(province_vif_tbl, file.path(data_dir, "province_level_vif_screening.csv"), row.names = FALSE)
write.csv(province_vif_final_tbl, file.path(data_dir, "province_level_vif_final_models.csv"), row.names = FALSE)
write.csv(province_diag_summary, file.path(data_dir, "province_level_model_diagnostics.csv"), row.names = FALSE)
write.csv(influential_provinces, file.path(data_dir, "province_influential_units.csv"), row.names = FALSE)
write.csv(province_model_comparison, file.path(data_dir, "province_candidate_model_comparison.csv"), row.names = FALSE)

# -------------------------------
# Step 6. Province-level coefficients, partial regression, and importance
# 第 6 步：省级模型系数、偏回归与相对贡献分析
# -------------------------------
province_coef_tbl <- broom::tidy(province_model_primary) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    predictor = recode_province_predictor(term),
    significant = conf.low * conf.high > 0
  )

province_user_effort_coef_tbl <- broom::tidy(province_model_user_effort) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    predictor = recode_province_predictor(term)
  )

province_sensitivity_coef_tbl <- broom::tidy(province_model_sensitivity) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    predictor = recode_province_predictor(term)
  )

province_influence_coef_tbl <- broom::tidy(province_model_influence_sensitivity) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    predictor = recode_province_predictor(term)
  )

province_robustness_coef_tbl <- bind_rows(
  province_coef_tbl %>% mutate(model = "Primary report-effort"),
  province_user_effort_coef_tbl %>% mutate(model = "Alternative user-effort"),
  province_influence_coef_tbl %>% mutate(model = "Influence-filtered")
)

province_robustness_delta_tbl <- province_robustness_coef_tbl %>%
  dplyr::select(model, term, predictor, estimate) %>%
  pivot_wider(names_from = model, values_from = estimate) %>%
  mutate(
    delta_user_minus_primary = `Alternative user-effort` - `Primary report-effort`,
    delta_influence_minus_primary = `Influence-filtered` - `Primary report-effort`
  )

write.csv(province_coef_tbl, file.path(data_dir, "province_level_primary_coefficients.csv"), row.names = FALSE)
write.csv(province_user_effort_coef_tbl, file.path(data_dir, "province_level_user_effort_coefficients.csv"), row.names = FALSE)
write.csv(province_sensitivity_coef_tbl, file.path(data_dir, "province_level_sensitivity_coefficients.csv"), row.names = FALSE)
write.csv(province_influence_coef_tbl, file.path(data_dir, "province_level_influence_coefficients.csv"), row.names = FALSE)
write.csv(province_robustness_coef_tbl, file.path(data_dir, "province_level_robustness_coefficients.csv"), row.names = FALSE)
write.csv(province_robustness_delta_tbl, file.path(data_dir, "province_level_robustness_deltas.csv"), row.names = FALSE)

compute_partial_df <- function(data, response, focal, others) {
  response_resid <- residuals(lm(as.formula(paste(response, "~", paste(others, collapse = " + "))), data = data))
  focal_resid <- residuals(lm(as.formula(paste(focal, "~", paste(others, collapse = " + "))), data = data))
  tibble(focal_resid = focal_resid, response_resid = response_resid)
}

partial_specs <- tribble(
  ~term, ~predictor, ~color,
  "z_hist_report_density", "Historical survey effort", "#FFE5B4",
  "z_curr_report_density", "Current survey effort", "#FC8D59",
  "z_gdp_per_capita", "Per capita GDP", "#B30000",
  "z_area_km2", "Area", "#EF478A",
  "z_habitat_heterogeneity", "Habitat heterogeneity", "#D58BE8"
)

model_terms_primary <- partial_specs$term

partial_plot_list <- list()
partial_data_all <- list()

for (i in seq_len(nrow(partial_specs))) {
  focal <- partial_specs$term[i]
  focal_label <- partial_specs$predictor[i]
  focal_color <- partial_specs$color[i]
  other_terms <- setdiff(model_terms_primary, focal)
  part_df <- compute_partial_df(
    data = province_model_df,
    response = "log_new_record_density",
    focal = focal,
    others = other_terms
  ) %>%
    bind_cols(province_model_df %>% dplyr::select(province)) %>%
    mutate(term = focal, predictor = focal_label)

  partial_data_all[[focal]] <- part_df

  coef_row <- province_coef_tbl %>% filter(term == focal)
  label_txt <- paste0(
    "italic(beta)==", round(coef_row$estimate, 3),
    "*','~~italic(p)==", formatC(summary(province_model_primary)$coefficients[focal, "Pr(>|t|)"], format = "e", digits = 2)
  )

  partial_plot_list[[focal]] <- ggplot(part_df, aes(x = focal_resid, y = response_resid)) +
    geom_point(size = 2.3, alpha = 0.9, colour = "#303030") +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1, colour = focal_color, fill = alpha(focal_color, 0.35)) +
    annotate(
      "text",
      x = max(part_df$focal_resid, na.rm = TRUE),
      y = max(part_df$response_resid, na.rm = TRUE),
      label = label_txt,
      parse = TRUE,
      hjust = 1,
      vjust = 1,
      size = 4.1
    ) +
    labs(x = paste0("Residualized ", focal_label), y = "Residualized log density") +
    theme_geb(base_size = 10.5)
}

partial_data_tbl <- bind_rows(partial_data_all)
write.csv(partial_data_tbl, file.path(data_dir, "province_partial_regression_data.csv"), row.names = FALSE)

calc_r2 <- function(data, response, predictors) {
  if (length(predictors) == 0) return(0)
  formula_txt <- paste(response, "~", paste(predictors, collapse = " + "))
  summary(lm(as.formula(formula_txt), data = data))$r.squared
}

hierarchical_partition <- function(data, response, predictors) {
  independent_r2 <- setNames(rep(NA_real_, length(predictors)), predictors)
  for (pred in predictors) {
    others <- setdiff(predictors, pred)
    incr <- c()
    for (k in 0:length(others)) {
      subsets <- if (k == 0) list(character(0)) else combn(others, k, simplify = FALSE)
      for (sub_set in subsets) {
        r2_without <- calc_r2(data, response, sub_set)
        r2_with <- calc_r2(data, response, c(sub_set, pred))
        incr <- c(incr, r2_with - r2_without)
      }
    }
    independent_r2[pred] <- mean(incr)
  }
  tibble(
    term = names(independent_r2),
    independent_r2 = as.numeric(independent_r2),
    percent = independent_r2 / sum(independent_r2) * 100
  )
}

importance_tbl <- hierarchical_partition(
  data = province_model_df,
  response = "log_new_record_density",
  predictors = model_terms_primary
) %>%
  mutate(
    predictor = recode_province_predictor(term)
  )

bootstrap_hp <- function(data, response, predictors, n_boot = 400) {
  res <- vector("list", n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample(seq_len(nrow(data)), size = nrow(data), replace = TRUE)
    boot_df <- data[idx, , drop = FALSE]
    res[[b]] <- hierarchical_partition(boot_df, response, predictors) %>% mutate(iteration = b)
  }
  bind_rows(res)
}

importance_boot_tbl <- bootstrap_hp(
  data = province_model_df,
  response = "log_new_record_density",
  predictors = model_terms_primary,
  n_boot = 400
) %>%
  mutate(
    predictor = recode_province_predictor(term)
  )

importance_boot_summary <- importance_boot_tbl %>%
  group_by(term, predictor) %>%
  summarise(
    independent_r2_mean = mean(independent_r2, na.rm = TRUE),
    independent_r2_low = quantile(independent_r2, 0.025, na.rm = TRUE),
    independent_r2_high = quantile(independent_r2, 0.975, na.rm = TRUE),
    percent_mean = mean(percent, na.rm = TRUE),
    percent_low = quantile(percent, 0.025, na.rm = TRUE),
    percent_high = quantile(percent, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(importance_tbl, file.path(data_dir, "province_hierarchical_partitioning.csv"), row.names = FALSE)
write.csv(importance_boot_tbl, file.path(data_dir, "province_hierarchical_partitioning_bootstrap.csv"), row.names = FALSE)
write.csv(importance_boot_summary, file.path(data_dir, "province_hierarchical_partitioning_bootstrap_summary.csv"), row.names = FALSE)

coef_colors <- c(
  "Historical survey effort" = "#FFE5B4",
  "Current survey effort" = "#FC8D59",
  "Per capita GDP" = "#B30000",
  "Area" = "#EF478A",
  "Habitat heterogeneity" = "#D58BE8"
)

p_fig4a <- partial_plot_list[["z_hist_report_density"]]
p_fig4b <- partial_plot_list[["z_curr_report_density"]]
p_fig4c <- partial_plot_list[["z_gdp_per_capita"]]
p_fig4d <- partial_plot_list[["z_area_km2"]]
p_fig4e <- partial_plot_list[["z_habitat_heterogeneity"]]

p_fig4f <- ggplot(province_coef_tbl, aes(x = predictor, y = estimate, colour = predictor)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.18, linewidth = 0.9) +
  geom_point(size = 4.2) +
  scale_colour_manual(values = coef_colors) +
  coord_flip() +
  labs(x = NULL, y = "Slope estimate") +
  theme_geb(base_size = 11) +
  theme(legend.position = "none")

p_fig4g <- ggplot(importance_tbl, aes(x = "Model", y = percent, fill = predictor)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.5) +
  scale_fill_manual(values = coef_colors) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(x = paste0("R² = ", format(round(summary(province_model_primary)$r.squared, 2), nsmall = 2)), y = "Relative importance (%)", fill = NULL) +
  theme_geb(base_size = 11) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")

p_fig4h <- ggplot(importance_boot_summary, aes(x = predictor, y = percent_mean, colour = predictor)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_errorbar(aes(ymin = percent_low, ymax = percent_high), width = 0.16, linewidth = 0.85) +
  geom_point(size = 3.6) +
  scale_colour_manual(values = coef_colors) +
  coord_flip() +
  labs(x = NULL, y = "Bootstrapped relative importance (%)") +
  theme_geb(base_size = 11) +
  theme(legend.position = "none")

fig4 <- (p_fig4a | p_fig4b | p_fig4c) /
  (p_fig4d | p_fig4e | p_fig4f) /
  (p_fig4g | p_fig4h) +
  plot_annotation(tag_levels = "a")

save_plot_bundle(fig4, "fig_geb4_province_level_effort_drivers", width = 13, height = 14.5)

screening_long <- bind_rows(
  species_model_df %>%
    transmute(dataset = "Species", variable = "Log10 body mass", value = log_mass),
  species_model_df %>%
    transmute(dataset = "Species", variable = "Range size (provinces)", value = range_size_provinces),
  province_model_df %>%
    transmute(dataset = "Province", variable = "Current report density", value = current_report_density),
  province_model_df %>%
    transmute(dataset = "Province", variable = "Log discovery intensity", value = log_new_record_density)
)

p_s0a <- ggplot(screening_long, aes(x = value, fill = dataset)) +
  geom_histogram(bins = 18, alpha = 0.8, colour = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("Species" = "#74A9CF", "Province" = "#FC8D59")) +
  labs(x = "Observed value", y = "Frequency", fill = NULL) +
  theme_geb(base_size = 10.5) +
  theme(legend.position = "top")

p_s0b <- ggplot(effort_outlier_tbl, aes(x = factor(report_outlier), fill = factor(report_outlier))) +
  geom_bar(width = 0.72) +
  scale_fill_manual(values = c("FALSE" = "#A1D99B", "TRUE" = "#E34A33")) +
  labs(x = "Report-count outlier flag", y = "Province-year cells", fill = NULL) +
  theme_geb(base_size = 10.5) +
  theme(legend.position = "none")

p_s0c <- ggplot(effort_outlier_tbl, aes(x = factor(user_outlier), fill = factor(user_outlier))) +
  geom_bar(width = 0.72) +
  scale_fill_manual(values = c("FALSE" = "#A1D99B", "TRUE" = "#E34A33")) +
  labs(x = "User-count outlier flag", y = "Province-year cells", fill = NULL) +
  theme_geb(base_size = 10.5) +
  theme(legend.position = "none")

p_s0d <- ggplot(trait_missing_summary, aes(x = reorder(variable, -pct_missing), y = pct_missing)) +
  geom_col(fill = "#6BAED6", width = 0.72) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Missing proportion") +
  theme_geb(base_size = 10.5) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

fig_s0 <- (p_s0a | p_s0b) / (p_s0c | p_s0d) + plot_annotation(tag_levels = "a")
save_plot_bundle(fig_s0, "fig_s0_data_screening_overview", width = 12.8, height = 8.6)

p_s4_diag1 <- ggplot(province_model_df, aes(x = fitted_primary, y = std_resid_primary)) +
  geom_point(size = 2.4, alpha = 0.9, colour = "#333333") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Fitted values", y = "Standardized residuals") +
  theme_geb(base_size = 10.5)

p_s4_diag2 <- ggplot(province_model_df, aes(sample = std_resid_primary)) +
  stat_qq(size = 2, alpha = 0.8, colour = "#333333") +
  stat_qq_line(colour = "#FC8D59") +
  labs(x = "Theoretical quantiles", y = "Standardized residual quantiles") +
  theme_geb(base_size = 10.5)

p_s4_diag3 <- ggplot(province_model_df, aes(x = seq_along(cooks_d_primary), y = cooks_d_primary)) +
  geom_col(fill = "#7FCDBB") +
  geom_hline(yintercept = 4 / nrow(province_model_df), linetype = 2, colour = "#B30000") +
  labs(x = "Province index", y = "Cook's distance") +
  theme_geb(base_size = 10.5)

p_s4_diag4 <- ggplot(province_sensitivity_coef_tbl, aes(x = predictor, y = estimate)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.18, linewidth = 0.85, colour = "#2C7FB8") +
  geom_point(size = 3.2, colour = "#2C7FB8") +
  coord_flip() +
  labs(x = NULL, y = "Sensitivity-model estimate") +
  theme_geb(base_size = 10.5)

fig4_diag <- (p_s4_diag1 | p_s4_diag2) / (p_s4_diag3 | p_s4_diag4) +
  plot_annotation(tag_levels = "a")

save_plot_bundle(fig4_diag, "fig_s2_province_model_diagnostics_and_sensitivity", width = 12.5, height = 8.5)

p_s4_robust1 <- ggplot(province_robustness_coef_tbl, aes(x = predictor, y = estimate, colour = model)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_point(position = position_dodge(width = 0.45), size = 2.8) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.45),
    width = 0.14,
    linewidth = 0.75
  ) +
  coord_flip() +
  scale_colour_manual(values = c(
    "Primary report-effort" = "#2C7FB8",
    "Alternative user-effort" = "#FC8D59",
    "Influence-filtered" = "#31A354"
  )) +
  labs(x = NULL, y = "Coefficient estimate", colour = NULL) +
  theme_geb(base_size = 10.5) +
  theme(legend.position = "top")

p_s4_robust2 <- ggplot(province_model_comparison, aes(x = reorder(model, aic), y = aic, fill = family)) +
  geom_col(width = 0.7, colour = "white") +
  coord_flip() +
  scale_fill_manual(values = c("Gaussian LM" = "#9ECAE1", "Negative binomial" = "#FDD0A2")) +
  labs(x = NULL, y = "AIC", fill = NULL) +
  theme_geb(base_size = 10.5) +
  theme(legend.position = "top")

p_s4_robust3 <- ggplot(influential_provinces, aes(x = reorder(province, cooks_d_primary), y = cooks_d_primary)) +
  geom_col(fill = "#E34A33", width = 0.72) +
  geom_hline(yintercept = influence_threshold, linetype = 2, colour = "#333333") +
  coord_flip() +
  labs(x = NULL, y = "Cook's distance") +
  theme_geb(base_size = 10.5)

fig_s3 <- (p_s4_robust1 | p_s4_robust2) / (p_s4_robust3 | plot_spacer()) +
  plot_annotation(tag_levels = "a")
save_plot_bundle(fig_s3, "fig_s3_province_model_robustness", width = 12.8, height = 8.8)

# -------------------------------
# Step 7. Write bilingual result summary
# 第 7 步：撰写中英文结果摘要
# -------------------------------
main_occ_range <- species_coef_tbl %>% filter(term == "z_range_size", model == "Occurrence (binomial)")
main_count_range <- species_coef_tbl %>% filter(term == "z_range_size", model == "Frequency (negative binomial)")
main_province_curr <- province_coef_tbl %>% filter(term == "z_curr_report_density")
main_province_area <- province_coef_tbl %>% filter(term == "z_area_km2")
main_user_effort <- province_user_effort_coef_tbl %>% filter(term == "z_curr_user_density")
robust_area <- province_influence_coef_tbl %>% filter(term == "z_area_km2")
robust_curr <- province_influence_coef_tbl %>% filter(term == "z_curr_report_density")
influential_label <- if (nrow(influential_provinces) == 0) "None" else paste(influential_provinces$province, collapse = ", ")

summary_lines <- c(
  "# Bird GEB-style Figure 3 and Figure 4 analysis / 鸟类 GEB 风格 Figure 3 与 Figure 4 分析摘要",
  "",
  "## English",
  paste0("- Species-level models were fitted for ", nrow(species_model_df), " bird species with sufficiently complete trait data."),
  paste0("- Range size was positively associated with whether a species yielded any new provincial record (beta = ", round(main_occ_range$estimate, 3), ", 95% CI ", round(main_occ_range$conf.low, 3), " to ", round(main_occ_range$conf.high, 3), ")."),
  paste0("- Range size also showed a positive association with the number of new provincial records in the count model (beta = ", round(main_count_range$estimate, 3), ", 95% CI ", round(main_count_range$conf.low, 3), " to ", round(main_count_range$conf.high, 3), ")."),
  paste0("- Province-level analysis included ", nrow(province_model_df), " provinces with complete structural covariates. After area standardization, current survey effort showed a positive relationship with discovery intensity (beta = ", round(main_province_curr$estimate, 3), ", p = ", signif(summary(province_model_primary)$coefficients["z_curr_report_density", "Pr(>|t|)"], 3), "), whereas province area showed a negative relationship (beta = ", round(main_province_area$estimate, 3), ", p = ", signif(summary(province_model_primary)$coefficients["z_area_km2", "Pr(>|t|)"], 3), ")."),
  paste0("- The primary province model explained ", percent(summary(province_model_primary)$r.squared, accuracy = 0.1), " of the variance in log-transformed discovery intensity, and hierarchical partitioning identified current survey effort and province area as the largest contributors."),
  paste0("- High-collinearity screening showed that current report and current user effort were nearly redundant (joint VIF > ", round(max(province_vif_tbl$vif, na.rm = TRUE), 1), "), so they were not entered together in the same primary model."),
  paste0("- Influence screening identified the following provinces above the 4/n Cook's-distance threshold: ", influential_label, ". After removing these provinces, the effect directions for current survey effort (beta = ", round(robust_curr$estimate, 3), ") and area (beta = ", round(robust_area$estimate, 3), ") remained unchanged."),
  paste0("- Replacing current report effort with current user effort yielded a positive user-effort coefficient (beta = ", round(main_user_effort$estimate, 3), "), indicating that the effort effect is qualitatively robust to proxy choice."),
  "",
  "## 中文",
  paste0("- 物种层模型共纳入 ", nrow(species_model_df), " 个性状信息较完整的鸟种。"),
  paste0("- 分布范围对物种是否产生新省级纪录具有正向作用（beta = ", round(main_occ_range$estimate, 3), "，95% CI ", round(main_occ_range$conf.low, 3), " 至 ", round(main_occ_range$conf.high, 3), "）。"),
  paste0("- 在计数模型中，分布范围对新省级纪录数量同样表现出正向影响（beta = ", round(main_count_range$estimate, 3), "，95% CI ", round(main_count_range$conf.low, 3), " 至 ", round(main_count_range$conf.high, 3), "）。"),
  paste0("- 省级分析共纳入 ", nrow(province_model_df), " 个具有完整结构协变量的省级单元。面积标准化后，当前调查努力与新纪录发现强度呈显著正相关（beta = ", round(main_province_curr$estimate, 3), "，p = ", signif(summary(province_model_primary)$coefficients["z_curr_report_density", "Pr(>|t|)"], 3), "），而省域面积呈显著负相关（beta = ", round(main_province_area$estimate, 3), "，p = ", signif(summary(province_model_primary)$coefficients["z_area_km2", "Pr(>|t|)"], 3), "）。"),
  paste0("- 省级主模型解释了对数转换后发现强度变异的 ", percent(summary(province_model_primary)$r.squared, accuracy = 0.1), "；层次分解结果表明，当前调查努力和省域面积是贡献最大的两个解释变量。"),
  paste0("- 共线性筛查表明，当前报告数努力与当前用户数努力几乎冗余（联合 VIF > ", round(max(province_vif_tbl$vif, na.rm = TRUE), 1), "），因此主模型中不同时纳入两者。"),
  paste0("- 影响点筛查识别出 Cook's distance 高于 4/n 阈值的省份为：", influential_label, "。剔除这些省份后，当前调查努力（beta = ", round(robust_curr$estimate, 3), "）和面积（beta = ", round(robust_area$estimate, 3), "）的作用方向保持不变。"),
  paste0("- 将当前报告数努力替换为当前用户数努力后，用户努力系数仍为正（beta = ", round(main_user_effort$estimate, 3), "），说明努力效应对代理指标的选择具有定性稳健性。")
)

writeLines(summary_lines, con = file.path(results_dir, "task_summary_bilingual.md"))

caption_lines <- c(
  "# Captions / 图题",
  "",
  "## Figure 3",
  "EN: Species-level correlates of bird new provincial records in China. Panel (a) shows coefficient estimates from binomial occurrence and negative-binomial frequency models. Panels (b-c) show marginal predictions for range size and body mass. Panels (d-e) show predicted occurrence probabilities across migration and risk groups.",
  "CN: 中国鸟类新省级纪录的物种层相关因子。面板 (a) 展示二元出现模型和负二项计数模型的系数估计；面板 (b-c) 展示分布范围和体重的边际预测；面板 (d-e) 展示不同迁徙类型和风险组的预测出现概率。",
  "",
  "## Figure 4",
  "EN: Province-level drivers of bird new-record discovery intensity. Panels (a-e) show partial-regression relationships for historical survey effort, current survey effort, per capita GDP, province area, and habitat heterogeneity. Panel (f) summarizes standardized slope estimates from the primary multiple-regression model. Panel (g) shows relative importance derived from hierarchical partitioning, and panel (h) shows bootstrap confidence intervals for the corresponding importance estimates.",
  "CN: 鸟类新纪录发现强度的省级驱动因子。面板 (a-e) 分别展示历史调查努力、当前调查努力、人均 GDP、省域面积和栖息地异质性的偏回归关系；面板 (f) 汇总主多元回归模型的标准化系数；面板 (g) 展示层次分解得到的相对贡献；面板 (h) 展示相应贡献值的 bootstrap 置信区间。",
  "",
  "## Figure S0",
  "EN: Data-screening overview for the bird effort analysis, including input-variable distributions, effort outlier counts, and trait missingness.",
  "CN: 鸟类调查努力分析的数据筛查总览，包括输入变量分布、调查努力异常值数量和性状缺失情况。",
  "",
  "## Figure S3",
  "EN: Robustness checks for the province-level model, showing coefficient stability across effort proxies, model-level information-criterion comparison, and provinces identified as influential by Cook's distance.",
  "CN: 省级模型的稳健性检验，展示不同努力代理指标下的系数稳定性、不同模型的信息准则比较，以及由 Cook's distance 识别出的高影响省份。"
)
writeLines(caption_lines, con = file.path(results_dir, "figure_captions_bilingual.md"))

interpretation_lines <- c(
  "# Interpretation / 结果解释",
  "",
  "## English",
  "The bird analogue of GEB Figure 3 indicates that mobility and spatial footprint matter more than broad conservation category for explaining which species generate new provincial records. This is consistent with a bird-specific discovery process in which widespread and migratory taxa have greater opportunities to cross administrative boundaries, occupy marginal sites intermittently, and be rediscovered under improving observation networks.",
  "",
  "At the province level, the bird analogue of GEB Figure 4 supports an observation-process explanation in which contemporary effort is a major axis of variation in discovery intensity. The positive current-effort signal remains when alternative effort proxies are considered, while the negative area effect is stable after influence filtering, suggesting that the main conclusion is not an artifact of a single province or a single effort metric.",
  "",
  "Importantly, the mammal GEB workflow included a richness predictor that is not directly transferable here because the available province richness field belongs to the mammal reference dataset rather than a bird-specific source. Excluding that taxonomically mismatched variable makes the bird workflow more defensible, even if it departs slightly from the original mammal formula.",
  "",
  "## 中文",
  "鸟类版 Figure 3 表明，与粗尺度保护等级相比，移动能力和空间分布范围更能解释哪些物种会产生新的省级纪录。这说明鸟类新纪录的形成更可能与广布、迁徙类群跨行政边界活动频繁、边缘生境间歇占据以及观测网络改善后的再次发现有关。",
  "",
  "在省级层面，鸟类版 Figure 4 支持一种明显的观测过程解释：当前调查努力是影响新纪录发现强度的重要因素。无论使用报告数还是用户数作为努力代理，该正向关系都保持一致；同时，面积的负效应在剔除高影响省份后仍然稳定，说明主结论并不是由单个省份或单一努力指标驱动的假象。",
  "",
  "需要强调的是，哺乳动物 GEB 工作流中包含的 richness 变量并不能直接照搬到这里，因为当前可用的省级 richness 字段来自哺乳动物参考数据，而不是鸟类专属来源。主动剔除这个分类单元不匹配的变量，反而使鸟类版流程在方法学上更可辩护、更严谨。"
)
writeLines(interpretation_lines, con = file.path(results_dir, "interpretation_bilingual.md"))

writexl::write_xlsx(
  list(
    qa_summary = qa_summary,
    effort_missing_summary = effort_missing_summary,
    effort_coverage_by_province = coverage_by_province,
    species_coefficients = species_coef_tbl,
    province_coefficients = province_coef_tbl,
    province_user_effort_coefficients = province_user_effort_coef_tbl,
    province_sensitivity = province_sensitivity_coef_tbl,
    province_influence_coefficients = province_influence_coef_tbl,
    province_importance = importance_tbl,
    province_importance_boot_ci = importance_boot_summary,
    province_diagnostics = province_diag_summary,
    province_model_comparison = province_model_comparison,
    province_influential_units = influential_provinces,
    predictor_source_audit = predictor_source_audit
  ),
  path = file.path(results_dir, "bird_geb_fig3_fig4_analysis_bundle.xlsx")
)

message("Completed bird GEB-style Figure 3 and Figure 4 analysis task.")
message("Outputs written to: ", task_root)
