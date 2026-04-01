#!/usr/bin/env Rscript

# ============================================================
# Bird New Distribution Records in China:
# Upgraded GEB-style Figure 3 and Figure 4 workflow with
# expanded species traits, phylogenetic non-independence,
# survey effort, diagnostics, and publication-ready outputs
# 中国鸟类新纪录：升级版 GEB Figure 3 与 Figure 4 分析流程
# （扩展物种性状、控制系统发育非独立性、整合调查努力、
#  完整诊断与高质量图表输出）
# ============================================================
#
# Scientific background / 研究背景
# New bird distribution records emerge from the joint action of biological
# properties, taxonomic context, historical under-sampling, and contemporary
# survey intensity. In the GEB-style framework used previously for mammals,
# two linked scientific questions are central: (1) which species are more
# likely to generate new records, and (2) which provinces are more likely to
# yield those records under different combinations of effort and structural
# constraints. For birds, these questions are especially interesting because
# mobility, diet, endemicity, clutch size, and habitat association may alter
# detectability and true range occupancy simultaneously.
# 鸟类新纪录的形成并不是单一过程，而是由生物学属性、分类学背景、
# 历史调查不足以及当代调查强度共同驱动的。在我们前面参照 GEB 论文
# 构建的分析框架中，核心科学问题包括两层：
# （1）哪些鸟类物种更容易产生新纪录；
# （2）哪些省份在何种调查努力与结构条件组合下更容易发现这些新纪录。
# 对鸟类而言，这些问题尤其关键，因为迁徙能力、食性、特有性、窝卵数
# 和栖息地关联性既可能影响真实分布格局，也可能影响被观察和记录到的
# 概率。
#
# Scientific questions / 科学问题
# 1. After accounting for phylogenetic non-independence among bird species,
#    which continuous traits and categorical ecological attributes are
#    associated with the probability that a species generates at least one
#    new provincial distribution record in China?
# 2. How do province-level historical effort, current effort, area, GDP, and
#    habitat heterogeneity shape the spatial intensity of bird new-record
#    discovery, and how robust are these relationships to effort proxies and
#    influential provinces?
# 1. 在控制鸟类物种之间系统发育非独立性之后，哪些连续性状和分类生态
#    属性与“一个物种是否在中国产生至少一个新的省级分布纪录”显著相关？
# 2. 在省级尺度上，历史调查努力、当前调查努力、面积、经济水平和生境
#    异质性如何共同影响鸟类新纪录发现强度？这些关系对不同努力代理变量
#    和高影响省份是否稳健？
#
# Objectives / 研究目标
# 1. Rebuild the species-level Figure 3 analysis using bird trait data drawn
#    primarily from AVONET and Wang Yanping et al.'s bird ecological dataset,
#    while explicitly auditing which requested variables are and are not
#    supportable from these two sources.
# 2. Fit phylogenetic Bernoulli models separately for continuous traits and
#    categorical traits, then visualize posterior distributions with a
#    top-journal-style ridge-plot layout.
# 3. Rebuild the province-level Figure 4 analysis in a more horizontal,
#    publication-friendly multi-panel arrangement.
# 4. Export cleaned datasets, variable audits, model summaries, diagnostics,
#    figure captions, bilingual interpretations, and bundled Excel outputs in
#    the standardized task directory.
# 1. 以 AVONET 和王彦平等鸟类生态学数据为主要性状来源，重建物种层
#    Figure 3 分析，同时显式审计用户提出的候选变量中哪些可以由这两套
#    数据稳定支持，哪些目前尚缺乏稳健来源。
# 2. 分别针对连续性状与分类性状拟合系统发育 Bernoulli 模型，并用更适合
#    顶刊论文的山脊图版式展示后验分布。
# 3. 以更横向、更紧凑、更适合投稿排版的方式重建省级 Figure 4。
# 4. 在统一任务目录中导出整理后的数据、变量审计、模型汇总、诊断结果、
#    双语图题、双语结果解释以及汇总 Excel 文件。
#
# Analytical strategy / 分析思路
# 1. Standardize the bird record table and the province-year effort table.
# 2. Reconstruct a taxonomically complete bird species pool from the 2025
#    national checklist, then attach AVONET traits, Wang-trait fields,
#    checklist-derived congener counts, and species-level response variables.
# 3. Build a variable-audit table so that requested traits such as HWI,
#    clutch size, endemicity, and diet are transparently distinguished from
#    variables that are currently not stably available from the specified
#    sources (e.g., generation length, description year, direct habitat
#    breadth).
# 4. Fit two phylogenetic Bernoulli models using the Hackett bird phylogeny:
#    one for continuous traits and one for categorical traits. Use cmdstanr
#    backend for efficient and reproducible Bayesian estimation.
# 5. Conduct supplementary non-phylogenetic negative-binomial sensitivity
#    analysis for the number of new records per species.
# 6. Reuse and upgrade the province-level GEB-style regression framework for
#    effort and structural drivers, but redesign the multi-panel figure into
#    a wider layout.
# 7. Perform diagnostics at each stage: missingness, duplicate screening,
#    trait distributions, rare-category screening, collinearity, posterior
#    diagnostics, posterior predictive checks, residual diagnostics, and
#    sensitivity analyses.
# 1. 标准化鸟类新纪录事件表和省份-年份调查努力表。
# 2. 基于 2025 中国生物物种名录重建完整鸟类物种池，并连接 AVONET 性状、
#    王彦平等鸟类生态学字段、名录推导的同属物种数以及物种层响应变量。
# 3. 建立候选变量审计表，明确 HWI、窝卵数、特有性、食性等变量可以稳定支撑，
#    而世代长度、命名时间、直接 habitat breadth 等变量在当前指定来源下
#    不能稳健获取，从而避免不严谨地强行拼接外部数据。
# 4. 基于 Hackett 鸟类系统发育树分别拟合连续性状模型和分类性状模型，
#    使用 cmdstanr 后端提高 Bayesian 估计效率与可复现性。
# 5. 对“新纪录条数”额外开展非系统发育负二项敏感性分析。
# 6. 复用并升级省级 GEB 风格的 effort-driver 回归框架，同时把图的布局改为
#    更适合正文排版的横向多面板形式。
# 7. 在每一步都实施诊断：缺失值、重复记录、性状分布、稀有类别、共线性、
#    后验诊断、后验预测检验、残差诊断以及敏感性分析。
#
# Diagnostics and validation / 诊断与验证
# - Duplicate-event screening and key-field QA for bird records.
# - Missingness summaries and outlier screening for province-year effort.
# - Candidate-variable availability audit and source traceability table.
# - Distributional checks for continuous traits and frequency checks for
#   categorical traits before modelling.
# - Tree matching audit: how many modelled species are retained in the
#   phylogeny and how many are dropped.
# - Posterior diagnostics: Rhat, ESS, posterior predictive checks, Bayes R2,
#   and estimated phylogenetic signal.
# - Predictor collinearity checks for non-phylogenetic sensitivity models.
# - Province-model residual diagnostics, Cook's distance screening, and
#   alternative effort-proxy robustness checks.
# - All major intermediate objects are exported for transparent review.
# - 鸟类新纪录事件的重复筛查和关键字段 QA 检查。
# - 省份-年份努力表的缺失汇总与异常值筛查。
# - 候选变量可用性审计与变量来源可追溯表。
# - 建模前对连续性状做分布检查、对分类变量做频数检查。
# - 系统发育匹配审计：记录每个模型能匹配到系统发育树中的物种数与被排除数。
# - 后验诊断：Rhat、ESS、后验预测检验、Bayes R2 以及系统发育信号估计。
# - 对非系统发育敏感性模型进行共线性检查。
# - 省级模型残差诊断、Cook's distance 高影响点筛查以及不同 effort 代理的
#   稳健性检验。
# - 所有关键中间结果均导出，便于审阅、复核和写作调用。
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(ggridges)
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
  library(ape)
  library(brms)
  library(cmdstanr)
  library(tidybayes)
  library(posterior)
  library(bayesplot)
})

set.seed(20260322)
options(mc.cores = min(4, parallel::detectCores()))

# -------------------------------
# Step 0. Paths, task structure, and global options
# 第 0 步：路径、任务目录结构与全局参数
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
task_root_default <- normalizePath(file.path(code_dir, ".."), mustWork = FALSE)
task_root <- Sys.getenv("BIRD_TASK_DIR", unset = task_root_default)
data_dir <- file.path(task_root, "data")
fig_dir <- file.path(task_root, "figures")
results_dir <- file.path(task_root, "results")
invisible(lapply(c(data_dir, fig_dir, results_dir), dir.create, recursive = TRUE, showWarnings = FALSE))

bird_xlsx <- Sys.getenv("BIRD_MASTER_XLSX", unset = "/Users/dingchenchen/Desktop/鸟类新纪录20260311.xlsx")
effort_xlsx <- Sys.getenv("BIRD_EFFORT_XLSX", unset = "/Users/dingchenchen/Desktop/effort补全.xlsx")
province_covariate_csv <- Sys.getenv("BIRD_PROVINCE_COVARIATE_CSV", unset = "/Users/dingchenchen/Desktop/?>/Data and codes/Province_variables.csv")
phylo_nexus <- Sys.getenv("BIRD_PHYLO_NEXUS", unset = "/Users/dingchenchen/lucc/AVONET bird traits/ELEData/PhylogeneticData/HackettStage1_0001_1000_MCCTreeTargetHeights.nex")
corrected_events_csv <- Sys.getenv("BIRD_CORRECTED_EVENTS_CSV", unset = "")

sheet_records <- "2000-2025鸟类新记录"
sheet_catalog <- "2025中国生物物种名录"
sheet_trait_cn <- "中国鸟类生态学特征"
sheet_trait_avonet <- "AVONET traits"

dpi_out <- 420
stan_seed <- 20260322
stan_chains <- 4
stan_iter <- 2000
stan_warmup <- 1000
stan_adapt_delta <- 0.96
stan_max_treedepth <- 12

cmdstanr::set_cmdstan_path(cmdstanr::cmdstan_path())

theme_geb <- function(base_size = 12, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, colour = "#111111"),
      plot.subtitle = element_text(size = base_size - 0.2, colour = "#2F2F2F"),
      plot.caption = element_text(size = base_size - 2, colour = "#444444"),
      axis.title = element_text(face = "bold", colour = "#111111"),
      axis.text = element_text(colour = "#1A1A1A"),
      legend.title = element_text(face = "bold"),
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      strip.background = element_rect(fill = "#D9D9D9", colour = "#5A5A5A", linewidth = 0.6),
      strip.text = element_text(face = "bold", colour = "#111111"),
      panel.grid.major.x = element_line(colour = alpha("#B0B0B0", 0.35), linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.tag = element_text(face = "bold", size = base_size + 1)
    )
}

save_plot_bundle <- function(plot_obj, stub, width, height) {
  png_path <- file.path(fig_dir, paste0(stub, ".png"))
  pdf_path <- file.path(fig_dir, paste0(stub, ".pdf"))
  pptx_path <- file.path(fig_dir, paste0(stub, ".pptx"))
  ggsave(png_path, plot_obj, width = width, height = height, dpi = dpi_out, bg = "white")
  ggsave(pdf_path, plot_obj, width = width, height = height, dpi = dpi_out, bg = "white", device = cairo_pdf)
  tryCatch(
    export::graph2ppt(x = plot_obj, file = pptx_path, width = width, height = height, vector.graphic = TRUE, append = FALSE),
    error = function(e) message("PPTX export failed for ", stub, ": ", e$message)
  )
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
    str_detect(x, "分类变动|独立成种|重新认定|误认") & str_detect(x, "分布变化|调查") ~ "Mixed: taxonomy + distribution/survey",
    str_detect(x, "分类变动|独立成种|重新认定|误认") ~ "Taxonomic revision",
    str_detect(x, "分布变化|游荡|最北端|北端|扩展|扩散") & str_detect(x, "技术|调查不足|缺乏调查|调查扩大") ~ "Mixed: range change + survey/technology",
    str_detect(x, "分布变化|游荡|最北端|北端|扩展|扩散") ~ "Range shift or distribution change",
    str_detect(x, "技术|整理照片|公众科学|卫星") ~ "Technology or improved detection",
    str_detect(x, "调查不足|缺乏调查|调查扩大") ~ "Survey gap or under-sampling",
    is.na(x) | x == "" ~ "Unclear",
    TRUE ~ "Other"
  )
}

parse_midpoint_numeric <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "（.*?）|\\(.*?\\)", "")
  x <- str_replace_all(x, "[~～—–−]", "-")
  x <- str_replace_all(x, "[^0-9\\.-]", "")
  out <- suppressWarnings(as.numeric(x))
  pieces <- str_split_fixed(x, "-", 2)
  use_mid <- is.na(out) & pieces[, 1] != "" & pieces[, 2] != ""
  out[use_mid] <- rowMeans(
    cbind(
      suppressWarnings(as.numeric(pieces[use_mid, 1])),
      suppressWarnings(as.numeric(pieces[use_mid, 2]))
    ),
    na.rm = TRUE
  )
  out
}

make_cn_trait_sheet <- function(path, sheet) {
  raw <- read_xlsx(path, sheet = sheet, col_names = FALSE, guess_max = 10000)
  h1 <- as.character(unlist(raw[1, ]))
  h2 <- as.character(unlist(raw[2, ]))
  nm <- ifelse(is.na(h2) | h2 == "NA" | h2 == "", h1, paste0(h1, "_", h2))
  nm[is.na(nm) | nm == ""] <- paste0("col_", seq_along(nm))[is.na(nm) | nm == ""]
  nm <- make.unique(nm)
  dat <- raw[-c(1, 2), ]
  names(dat) <- nm
  dat
}

collapse_diet_avonet <- function(x) {
  case_when(
    x %in% c("Invertivore") ~ "Invertebrate",
    x %in% c("Omnivore") ~ "Omnivore",
    x %in% c("Frugivore", "Nectarivore") ~ "Fruit_nectar",
    x %in% c("Granivore", "Herbivore terrestrial", "Herbivore aquatic") ~ "Plant_seed",
    x %in% c("Aquatic predator", "Vertivore", "Scavenger") ~ "Vertebrate_aquatic",
    TRUE ~ NA_character_
  )
}

migration_av_class <- function(x) {
  case_when(
    x == "1" ~ "Resident_low",
    x == "2" ~ "Partial_migrant",
    x == "3" ~ "Long_distance_migrant",
    TRUE ~ NA_character_
  )
}

forest_association_from_habitat <- function(x) {
  case_when(
    x %in% c("Forest", "Woodland") ~ "Forest_primary",
    !is.na(x) ~ "Non_forest",
    TRUE ~ NA_character_
  )
}

extract_phylo_lambda <- function(brms_fit, group_pattern = "phylo_species") {
  vc <- as.data.frame(VarCorr(brms_fit))
  est_col <- names(vc)[str_detect(names(vc), paste0(group_pattern, ".*Estimate"))][1]
  if (length(est_col) == 0 || is.na(est_col)) return(NA_real_)
  phylo_sd <- suppressWarnings(as.numeric(vc[[est_col]][1]))
  if (is.na(phylo_sd)) return(NA_real_)
  phylo_var <- phylo_sd^2
  phylo_var / (phylo_var + (pi^2 / 3))
}

summarise_brms_draws <- function(fit_obj, term_map, panel_group) {
  draw_names <- paste0("b_", names(term_map))
  posterior::as_draws_df(fit_obj) %>%
    dplyr::select(any_of(draw_names)) %>%
    pivot_longer(everything(), names_to = "draw_term", values_to = "estimate") %>%
    mutate(
      term = str_remove(draw_term, "^b_"),
      term_label = unname(term_map[term]),
      panel_group = panel_group
    )
}

summarise_effect_intervals <- function(draw_df) {
  draw_df %>%
    group_by(term, term_label, panel_group) %>%
    summarise(
      median = median(estimate),
      mean = mean(estimate),
      conf.low = quantile(estimate, 0.025),
      conf.high = quantile(estimate, 0.975),
      prob_positive = mean(estimate > 0),
      .groups = "drop"
    )
}

make_ridge_panel <- function(draw_df, summary_df, fill_values, title_text, subtitle_text) {
  draw_df <- draw_df %>%
    mutate(term_label = factor(term_label, levels = summary_df$term_label))
  summary_df <- summary_df %>%
    mutate(term_label = factor(term_label, levels = levels(draw_df$term_label)))

  ggplot(draw_df, aes(x = estimate, y = forcats::fct_rev(term_label), fill = term_label)) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.75, colour = "#5A5A5A") +
    ggridges::geom_density_ridges(
      scale = 1.05,
      rel_min_height = 0.004,
      alpha = 0.88,
      colour = "white",
      linewidth = 0.28
    ) +
    geom_errorbar(
      data = summary_df,
      aes(x = median, ymin = as.numeric(forcats::fct_rev(term_label)) - 0.18, ymax = as.numeric(forcats::fct_rev(term_label)) + 0.18, colour = term_label),
      inherit.aes = FALSE,
      linewidth = 0.0
    ) +
    geom_segment(
      data = summary_df,
      aes(x = conf.low, xend = conf.high, y = forcats::fct_rev(term_label), yend = forcats::fct_rev(term_label), colour = term_label),
      inherit.aes = FALSE,
      linewidth = 1.05,
      lineend = "round"
    ) +
    geom_point(
      data = summary_df,
      aes(x = median, y = forcats::fct_rev(term_label), colour = term_label),
      inherit.aes = FALSE,
      size = 2.6
    ) +
    scale_fill_manual(values = fill_values) +
    scale_colour_manual(values = fill_values) +
    labs(x = "Posterior effect size (log-odds)", y = NULL, title = title_text, subtitle = subtitle_text) +
    theme_geb(base_size = 11.8) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      axis.title.x = element_text(margin = margin(t = 8))
    )
}

make_summary_table_plot <- function(df, title_text) {
  ggplot(df, aes(x = predictor, y = estimate, colour = predictor)) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.18, linewidth = 0.9) +
    geom_point(size = 3.8) +
    coord_flip() +
    labs(x = NULL, y = "Slope estimate", title = title_text) +
    theme_geb(base_size = 10.8) +
    theme(legend.position = "none")
}

# -------------------------------
# Step 1. Read and standardize bird new-record data
# 第 1 步：读取并标准化鸟类新纪录事件数据
# -------------------------------
if (nzchar(corrected_events_csv) && file.exists(corrected_events_csv)) {
  raw_records <- read.csv(corrected_events_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8") %>%
    tibble::as_tibble()

  bird_events <- raw_records %>%
    mutate(
      record_id = if ("record_id" %in% names(.)) suppressWarnings(as.integer(record_id)) else row_number(),
      species = str_squish(species),
      order_raw = if ("order_raw" %in% names(.)) str_squish(order_raw) else str_squish(order),
      order_cn = if ("order_cn" %in% names(.)) str_squish(order_cn) else NA_character_,
      province_cn = if ("province_cn" %in% names(.)) str_squish(province_cn) else NA_character_,
      province_en_raw = if ("province_en_raw" %in% names(.)) str_squish(province_en_raw) else str_squish(province),
      year = suppressWarnings(as.integer(year)),
      discover_cause_raw = if ("discover_cause_raw" %in% names(.)) str_squish(discover_cause_raw) else NA_character_,
      discovery_method_raw = if ("discovery_method_raw" %in% names(.)) str_squish(discovery_method_raw) else NA_character_,
      longitude = suppressWarnings(as.numeric(longitude)),
      latitude = suppressWarnings(as.numeric(latitude)),
      order = if ("order" %in% names(.)) str_squish(order) else title_case_order(order_raw),
      province = if ("province" %in% names(.)) str_squish(province) else province_en_raw,
      discover_reason = if ("discover_reason" %in% names(.)) str_squish(discover_reason) else classify_discovery_reason(discover_cause_raw)
    ) %>%
    mutate(
      province = dplyr::recode(
        province,
        "Juangsu" = "Jiangsu",
        "Qinghai Province" = "Qinghai",
        "Taiwan Province" = "Taiwan",
        "Tibet Autonomous Region" = "Tibet"
      ),
      year = if_else(!is.na(year) & year >= 1900 & year <= 2100, year, NA_integer_)
    )
} else {
  raw_records <- read_xlsx(bird_xlsx, sheet = sheet_records, guess_max = 20000) %>% clean_names()

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
}

duplicate_screen <- bird_events %>% count(species, province, year, name = "n_dups") %>% filter(n_dups > 1)

bird_clean <- bird_events %>%
  filter(!is.na(species), species != "", !is.na(order), order != "", !is.na(province), !is.na(year)) %>%
  distinct(species, order, province, year, .keep_all = TRUE) %>%
  arrange(year, order, province, species)

qa_summary <- tibble(
  metric = c("Raw rows", "Rows after filtering key fields", "Rows after deduplication", "Unique species", "Unique orders", "Unique provinces", "Year min", "Year max"),
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

write.csv(bird_clean, file.path(data_dir, "bird_new_records_clean_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(duplicate_screen, file.path(data_dir, "bird_record_duplicate_screening.csv"), row.names = FALSE)
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
  mutate(province = unname(province_cn_to_en[province_cn]))

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
# Step 3. Build species pool, trait audit, and derived candidate variables
# 第 3 步：构建物种池、性状审计表与派生候选变量
# -------------------------------
# 这里的关键原则是“先做变量审计，再做模型”。这样能清楚说明哪些性状
# 来自用户指定的主要数据源（AVONET + 王彦平等数据），哪些只能作为目前
# 不可稳健获取的候选变量列出，而不是在主模型里硬塞进去。

catalog_raw <- read_xlsx(bird_xlsx, sheet = sheet_catalog, guess_max = 20000) %>% clean_names()

bird_catalog <- bind_rows(
  catalog_raw %>%
    transmute(
      species_lat = wu_zhong_la_ding_ming_1,
      class_lat = gang_la_ding_ming_7,
      order_lat = mu_la_ding_ming_9,
      family_lat = ke_la_ding_ming_11,
      genus_lat = shu_la_ding_ming_13
    ),
  catalog_raw %>%
    transmute(
      species_lat = wu_zhong_la_ding_ming_20,
      class_lat = gang_la_ding_ming_26,
      order_lat = mu_la_ding_ming_28,
      family_lat = ke_la_ding_ming_30,
      genus_lat = shu_la_ding_ming_32
    )
) %>%
  filter(class_lat == "Aves", !is.na(species_lat), species_lat != "", !is.na(order_lat), order_lat != "") %>%
  mutate(
    species = str_extract(species_lat, "^[A-Z][A-Za-z-]+\\s+[a-z-]+"),
    order = title_case_order(order_lat),
    family = family_lat,
    genus = genus_lat
  ) %>%
  filter(!is.na(species), !is.na(order)) %>%
  distinct(species, .keep_all = TRUE)

genus_tbl <- bird_catalog %>% count(genus, name = "genus_size") %>% mutate(n_congeners = pmax(genus_size - 1, 0))

species_response <- bird_clean %>% count(species, name = "n_new_records") %>% mutate(new_record = 1L)

trait_avonet <- read_xlsx(bird_xlsx, sheet = sheet_trait_avonet, guess_max = 20000) %>%
  clean_names() %>%
  transmute(
    species = str_replace_all(species1, "_", " "),
    mass = suppressWarnings(as.numeric(mass)),
    hwi = suppressWarnings(as.numeric(hand_wing_index)),
    habitat_av = habitat,
    habitat_density_av = habitat_density,
    migration_av_raw = as.character(migration),
    trophic_level_av = trophic_level,
    trophic_niche_av = trophic_niche,
    range_size_av = suppressWarnings(as.numeric(range_size)),
    centroid_latitude = suppressWarnings(as.numeric(centroid_latitude)),
    centroid_longitude = suppressWarnings(as.numeric(centroid_longitude))
  ) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)

trait_cn_raw <- make_cn_trait_sheet(bird_xlsx, sheet_trait_cn)
trait_cn <- trait_cn_raw %>%
  transmute(
    species = `种拉丁名`,
    endemic_cn = `是否特有种`,
    diet_cn = `食性`,
    clutch_raw = `窝卵数`,
    migration_cn_raw = `迁徙状态（留鸟(R)、夏候鸟(S)、冬候鸟(W)、旅鸟(P)、迷鸟(V)）`,
    range_size_provinces = suppressWarnings(as.numeric(`分布省份数`))
  ) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)

species_pool <- bird_catalog %>%
  left_join(genus_tbl, by = "genus") %>%
  left_join(species_response, by = "species") %>%
  mutate(
    new_record = replace_na(new_record, 0L),
    n_new_records = replace_na(n_new_records, 0L)
  ) %>%
  left_join(trait_avonet, by = "species") %>%
  left_join(trait_cn, by = "species") %>%
  mutate(
    clutch_size = parse_midpoint_numeric(clutch_raw),
    endemic_status = case_when(
      endemic_cn == "是" ~ "Endemic",
      endemic_cn == "否" ~ "Non_endemic",
      TRUE ~ NA_character_
    ),
    migration_class_av = migration_av_class(migration_av_raw),
    diet_group_av = collapse_diet_avonet(trophic_niche_av),
    forest_association = forest_association_from_habitat(habitat_av),
    log_mass = log10(mass),
    log_hwi = log1p(hwi),
    log_range_provinces = log1p(range_size_provinces),
    log_clutch_size = log1p(clutch_size),
    log_n_congeners = log1p(n_congeners),
    tree_label = str_replace_all(species, " ", "_")
  )

candidate_trait_audit <- tibble(
  requested_variable = c(
    "Body mass",
    "Hand-wing index (HWI)",
    "Range size",
    "Clutch size",
    "Number of congeners",
    "Endemicity",
    "Migration strategy",
    "Diet type",
    "Forest dependence / forest association",
    "Habitat breadth",
    "Generation length",
    "Naming time / description year"
  ),
  primary_source = c(
    "AVONET",
    "AVONET",
    "Wang bird ecological dataset",
    "Wang bird ecological dataset",
    "2025 national bird checklist",
    "Wang bird ecological dataset",
    "AVONET (primary); Wang used for cross-checking",
    "AVONET (primary); Wang used for contextual checking",
    "AVONET primary habitat used as a proxy",
    "Not stably available from AVONET + Wang in this workbook",
    "Not stably available from AVONET + Wang in this workbook",
    "Not stably available from AVONET + Wang in this workbook"
  ),
  derived_field = c(
    "log_mass",
    "log_hwi",
    "log_range_provinces",
    "log_clutch_size",
    "log_n_congeners",
    "endemic_status",
    "migration_class_av",
    "diet_group_av",
    "forest_association",
    NA,
    NA,
    NA
  ),
  used_in_primary_phylo_model = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  status = c(
    "available",
    "available",
    "available",
    "available",
    "available",
    "available",
    "available",
    "available",
    "proxy_available",
    "not_available_from_requested_primary_sources",
    "not_available_from_requested_primary_sources",
    "not_available_from_requested_primary_sources"
  ),
  note = c(
    "Continuous trait used in phylogenetic continuous-trait model.",
    "Explicitly added per user request.",
    "Province-count range size from Wang data is preferred here because the user requested Wang as a primary source.",
    "Mid-point parsed from clutch-size ranges when needed.",
    "Derived from the 2025 checklist rather than a trait workbook.",
    "Binary endemic vs non-endemic contrast.",
    "AVONET migration codes 1/2/3 collapsed into standardized mobility classes.",
    "AVONET trophic niche collapsed into broad ecological diet guilds.",
    "Operationalized as forest/woodland versus non-forest primary habitat; interpreted as a proxy rather than a direct dependency metric.",
    "AVONET habitat field is primary habitat, not habitat breadth, so habitat breadth is not claimed directly.",
    "Generation length was requested but is not robustly available from the two user-specified primary sources in the current workbook.",
    "Description year was requested but is not robustly available from the two user-specified primary sources in the current workbook."
  )
)

trait_availability <- candidate_trait_audit %>%
  mutate(
    n_nonmissing = c(
      sum(!is.na(species_pool$log_mass)),
      sum(!is.na(species_pool$log_hwi)),
      sum(!is.na(species_pool$log_range_provinces)),
      sum(!is.na(species_pool$log_clutch_size)),
      sum(!is.na(species_pool$log_n_congeners)),
      sum(!is.na(species_pool$endemic_status)),
      sum(!is.na(species_pool$migration_class_av)),
      sum(!is.na(species_pool$diet_group_av)),
      sum(!is.na(species_pool$forest_association)),
      NA_real_,
      NA_real_,
      NA_real_
    ),
    pct_nonmissing = n_nonmissing / nrow(species_pool)
  )

migration_crosswalk <- species_pool %>%
  filter(!is.na(migration_av_raw), !is.na(migration_cn_raw)) %>%
  count(migration_av_raw, migration_cn_raw, name = "n_species") %>%
  arrange(desc(n_species))

range_size_concordance <- species_pool %>%
  filter(!is.na(range_size_av), !is.na(range_size_provinces)) %>%
  summarise(
    n_species = n(),
    pearson_r = cor(range_size_av, range_size_provinces, use = "complete.obs"),
    spearman_rho = cor(range_size_av, range_size_provinces, method = "spearman", use = "complete.obs")
  )

write.csv(candidate_trait_audit, file.path(data_dir, "species_candidate_variable_audit.csv"), row.names = FALSE)
write.csv(trait_availability, file.path(data_dir, "species_candidate_variable_availability.csv"), row.names = FALSE)
write.csv(migration_crosswalk, file.path(data_dir, "species_migration_source_crosswalk.csv"), row.names = FALSE)
write.csv(range_size_concordance, file.path(data_dir, "species_range_size_source_concordance.csv"), row.names = FALSE)
write.csv(species_pool, file.path(data_dir, "bird_species_pool_with_traits.csv"), row.names = FALSE)

# -------------------------------
# Step 4. Build modelling datasets and check distributions
# 第 4 步：构建建模数据集并检查变量分布与完整性
# -------------------------------
continuous_model_df <- species_pool %>%
  filter(
    !is.na(log_mass),
    !is.na(log_hwi),
    !is.na(log_range_provinces),
    !is.na(log_clutch_size),
    !is.na(log_n_congeners)
  ) %>%
  mutate(
    z_log_mass = stdz(log_mass),
    z_log_hwi = stdz(log_hwi),
    z_log_range_provinces = stdz(log_range_provinces),
    z_log_clutch_size = stdz(log_clutch_size),
    z_log_n_congeners = stdz(log_n_congeners)
  )

categorical_model_df <- species_pool %>%
  filter(
    !is.na(endemic_status),
    !is.na(migration_class_av),
    !is.na(diet_group_av),
    !is.na(forest_association)
  ) %>%
  mutate(
    endemic_status = factor(endemic_status, levels = c("Non_endemic", "Endemic")),
    migration_class_av = factor(migration_class_av, levels = c("Resident_low", "Partial_migrant", "Long_distance_migrant")),
    diet_group_av = factor(diet_group_av, levels = c("Invertebrate", "Omnivore", "Fruit_nectar", "Plant_seed", "Vertebrate_aquatic")),
    forest_association = factor(forest_association, levels = c("Non_forest", "Forest_primary"))
  )

continuous_trait_distribution <- continuous_model_df %>%
  transmute(
    `Body mass (log10 g)` = log_mass,
    `HWI (log1p)` = log_hwi,
    `Range size (log1p provinces)` = log_range_provinces,
    `Clutch size (log1p)` = log_clutch_size,
    `Congeners (log1p)` = log_n_congeners
  ) %>%
  pivot_longer(everything(), names_to = "trait", values_to = "value")

categorical_trait_distribution <- bind_rows(
  categorical_model_df %>% count(variable = "Endemicity", level = endemic_status, name = "n"),
  categorical_model_df %>% count(variable = "Migration", level = migration_class_av, name = "n"),
  categorical_model_df %>% count(variable = "Diet", level = diet_group_av, name = "n"),
  categorical_model_df %>% count(variable = "Forest association", level = forest_association, name = "n")
)

trait_missing_summary <- tibble(
  variable = c("Body mass", "HWI", "Range size (provinces)", "Clutch size", "Congeners", "Endemicity", "Migration class", "Diet guild", "Forest association"),
  n_missing = c(
    sum(is.na(species_pool$log_mass)),
    sum(is.na(species_pool$log_hwi)),
    sum(is.na(species_pool$log_range_provinces)),
    sum(is.na(species_pool$log_clutch_size)),
    sum(is.na(species_pool$log_n_congeners)),
    sum(is.na(species_pool$endemic_status)),
    sum(is.na(species_pool$migration_class_av)),
    sum(is.na(species_pool$diet_group_av)),
    sum(is.na(species_pool$forest_association))
  )
) %>% mutate(pct_missing = n_missing / nrow(species_pool))

write.csv(continuous_model_df, file.path(data_dir, "bird_species_continuous_model_dataset.csv"), row.names = FALSE)
write.csv(categorical_model_df, file.path(data_dir, "bird_species_categorical_model_dataset.csv"), row.names = FALSE)
write.csv(trait_missing_summary, file.path(data_dir, "species_trait_missing_summary.csv"), row.names = FALSE)

# -------------------------------
# Step 5. Match species to the phylogeny and build covariance matrices
# 第 5 步：将物种匹配到系统发育树并构建协方差矩阵
# -------------------------------
# 这一部分直接回答“系统发育非独立性如何处理”。我们把每个模型的数据集
# 分别与鸟类系统发育树匹配，并导出匹配成功/失败的审计结果。

tree_full <- read.nexus(phylo_nexus)

audit_tree_match <- function(dat, dataset_name) {
  tibble(
    dataset = dataset_name,
    n_species_before_tree_match = nrow(dat),
    n_species_in_tree = sum(dat$tree_label %in% tree_full$tip.label),
    n_species_not_in_tree = sum(!(dat$tree_label %in% tree_full$tip.label))
  )
}

tree_match_audit <- bind_rows(
  audit_tree_match(continuous_model_df, "Continuous phylogenetic model"),
  audit_tree_match(categorical_model_df, "Categorical phylogenetic model")
)

continuous_model_tree <- continuous_model_df %>%
  filter(tree_label %in% tree_full$tip.label) %>%
  mutate(phylo_species = factor(tree_label))

categorical_model_tree <- categorical_model_df %>%
  filter(tree_label %in% tree_full$tip.label) %>%
  mutate(phylo_species = factor(tree_label))

tree_cont <- drop.tip(tree_full, setdiff(tree_full$tip.label, continuous_model_tree$tree_label))
A_cont <- ape::vcv.phylo(tree_cont, corr = TRUE)
continuous_model_tree <- continuous_model_tree %>%
  filter(tree_label %in% rownames(A_cont)) %>%
  mutate(phylo_species = factor(tree_label, levels = rownames(A_cont)))

tree_cat <- drop.tip(tree_full, setdiff(tree_full$tip.label, categorical_model_tree$tree_label))
A_cat <- ape::vcv.phylo(tree_cat, corr = TRUE)
categorical_model_tree <- categorical_model_tree %>%
  filter(tree_label %in% rownames(A_cat)) %>%
  mutate(phylo_species = factor(tree_label, levels = rownames(A_cat)))

write.csv(tree_match_audit, file.path(data_dir, "species_phylogeny_matching_audit.csv"), row.names = FALSE)
write.csv(continuous_model_tree, file.path(data_dir, "bird_species_continuous_model_dataset_tree_matched.csv"), row.names = FALSE)
write.csv(categorical_model_tree, file.path(data_dir, "bird_species_categorical_model_dataset_tree_matched.csv"), row.names = FALSE)

# -------------------------------
# Step 6. Fit upgraded species-level models (Figure 3)
# 第 6 步：拟合升级版物种层模型（Figure 3）
# -------------------------------
# 连续性状与分类性状分开建模，是为了避免两类变量在解释上互相遮蔽，
# 也更贴近 GEB 风格下“连续性状”和“类别属性”分开展示的思路。
# 这里的主响应变量是：物种是否产生至少一个新省级纪录（0/1）。

priors_phylo <- c(
  set_prior("normal(0, 0.8)", class = "b"),
  set_prior("student_t(3, 0, 1.5)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sd")
)

brms_control <- list(adapt_delta = stan_adapt_delta, max_treedepth = stan_max_treedepth)

fit_continuous_phylo <- brm(
  formula = new_record ~ z_log_mass + z_log_hwi + z_log_range_provinces + z_log_clutch_size + z_log_n_congeners +
    (1 | gr(phylo_species, cov = A_cont)),
  data = continuous_model_tree,
  data2 = list(A_cont = A_cont),
  family = bernoulli(link = "logit"),
  prior = priors_phylo,
  chains = stan_chains,
  iter = stan_iter,
  warmup = stan_warmup,
  backend = "cmdstanr",
  seed = stan_seed,
  control = brms_control,
  refresh = 0,
  file = file.path(results_dir, "brms_species_continuous_phylo"),
  file_refit = "on_change"
)

fit_categorical_phylo <- brm(
  formula = new_record ~ endemic_status + migration_class_av + diet_group_av + forest_association +
    (1 | gr(phylo_species, cov = A_cat)),
  data = categorical_model_tree,
  data2 = list(A_cat = A_cat),
  family = bernoulli(link = "logit"),
  prior = priors_phylo,
  chains = stan_chains,
  iter = stan_iter,
  warmup = stan_warmup,
  backend = "cmdstanr",
  seed = stan_seed + 1,
  control = brms_control,
  refresh = 0,
  file = file.path(results_dir, "brms_species_categorical_phylo"),
  file_refit = "on_change"
)

# 非系统发育计数敏感性模型：用于查看“物种积累了多少新纪录”时，
# 连续性状方向是否与主分析一致。它不是主结论来源，但能提高结果稳健性。
nb_species_count <- MASS::glm.nb(
  n_new_records ~ z_log_mass + z_log_hwi + z_log_range_provinces + z_log_clutch_size + z_log_n_congeners,
  data = continuous_model_tree,
  control = glm.control(maxit = 100)
)

species_count_vif <- car::vif(nb_species_count)
write.csv(tibble(term = names(species_count_vif), vif = as.numeric(species_count_vif)), file.path(data_dir, "species_count_vif.csv"), row.names = FALSE)
write.csv(
  broom::tidy(nb_species_count) %>% bind_cols(wald_ci(.$estimate, .$std.error)),
  file.path(data_dir, "species_count_sensitivity_coefficients.csv"),
  row.names = FALSE
)

continuous_term_map <- c(
  z_log_mass = "Body mass",
  z_log_hwi = "Hand-wing index (HWI)",
  z_log_range_provinces = "Range size",
  z_log_clutch_size = "Clutch size",
  z_log_n_congeners = "Number of congeners"
)

categorical_term_map <- c(
  endemic_statusEndemic = "Endemic species",
  migration_class_avPartial_migrant = "Partial migrant",
  migration_class_avLong_distance_migrant = "Long-distance migrant",
  diet_group_avOmnivore = "Diet: omnivore",
  diet_group_avFruit_nectar = "Diet: fruit/nectar",
  diet_group_avPlant_seed = "Diet: plant/seed",
  diet_group_avVertebrate_aquatic = "Diet: vertebrate/aquatic",
  forest_associationForest_primary = "Forest-associated primary habitat"
)

continuous_draws <- summarise_brms_draws(fit_continuous_phylo, continuous_term_map, panel_group = "Continuous traits")
categorical_draws <- summarise_brms_draws(fit_categorical_phylo, categorical_term_map, panel_group = "Categorical traits")

continuous_effects <- summarise_effect_intervals(continuous_draws) %>%
  mutate(term_label = factor(term_label, levels = rev(unname(continuous_term_map))))

categorical_effects <- summarise_effect_intervals(categorical_draws) %>%
  mutate(term_label = factor(term_label, levels = rev(unname(categorical_term_map))))

write.csv(continuous_effects, file.path(data_dir, "species_phylo_continuous_effects.csv"), row.names = FALSE)
write.csv(categorical_effects, file.path(data_dir, "species_phylo_categorical_effects.csv"), row.names = FALSE)

continuous_diag <- posterior::summarise_draws(as_draws_matrix(fit_continuous_phylo)) %>%
  as_tibble() %>%
  filter(str_detect(variable, "^b_|^sd_")) %>%
  mutate(model = "Continuous phylogenetic")

categorical_diag <- posterior::summarise_draws(as_draws_matrix(fit_categorical_phylo)) %>%
  as_tibble() %>%
  filter(str_detect(variable, "^b_|^sd_")) %>%
  mutate(model = "Categorical phylogenetic")

species_phylo_model_fit <- tibble(
  model = c("Continuous phylogenetic", "Categorical phylogenetic"),
  n_species = c(nrow(continuous_model_tree), nrow(categorical_model_tree)),
  n_new_record_species = c(sum(continuous_model_tree$new_record), sum(categorical_model_tree$new_record)),
  bayes_r2 = c(as.numeric(bayes_R2(fit_continuous_phylo)[1, 1]), as.numeric(bayes_R2(fit_categorical_phylo)[1, 1])),
  phylogenetic_lambda = c(extract_phylo_lambda(fit_continuous_phylo), extract_phylo_lambda(fit_categorical_phylo)),
  max_rhat = c(max(continuous_diag$rhat, na.rm = TRUE), max(categorical_diag$rhat, na.rm = TRUE)),
  min_bulk_ess = c(min(continuous_diag$ess_bulk, na.rm = TRUE), min(categorical_diag$ess_bulk, na.rm = TRUE)),
  min_tail_ess = c(min(continuous_diag$ess_tail, na.rm = TRUE), min(categorical_diag$ess_tail, na.rm = TRUE))
)

write.csv(bind_rows(continuous_diag, categorical_diag), file.path(data_dir, "species_phylo_model_parameter_diagnostics.csv"), row.names = FALSE)
write.csv(species_phylo_model_fit, file.path(data_dir, "species_phylo_model_fit_summary.csv"), row.names = FALSE)

continuous_palette <- c(
  "Body mass" = "#0E7490",
  "Hand-wing index (HWI)" = "#14B8A6",
  "Range size" = "#2563EB",
  "Clutch size" = "#F59E0B",
  "Number of congeners" = "#D97706"
)

categorical_palette <- c(
  "Endemic species" = "#C2410C",
  "Partial migrant" = "#4F46E5",
  "Long-distance migrant" = "#7C3AED",
  "Diet: omnivore" = "#15803D",
  "Diet: fruit/nectar" = "#65A30D",
  "Diet: plant/seed" = "#84CC16",
  "Diet: vertebrate/aquatic" = "#BE123C",
  "Forest-associated primary habitat" = "#166534"
)

p_fig3a <- make_ridge_panel(
  continuous_draws,
  continuous_effects %>% arrange(term_label),
  fill_values = continuous_palette,
  title_text = "Continuous traits with phylogenetic control",
  subtitle_text = paste0("Bernoulli model; n = ", nrow(continuous_model_tree), " species; posterior medians and 95% credible intervals")
)

p_fig3b <- make_ridge_panel(
  categorical_draws,
  categorical_effects %>% arrange(term_label),
  fill_values = categorical_palette,
  title_text = "Categorical ecological traits with phylogenetic control",
  subtitle_text = paste0("Bernoulli model; n = ", nrow(categorical_model_tree), " species; contrasts relative to the baseline levels")
)

fig3 <- (p_fig3a | p_fig3b) + plot_annotation(tag_levels = "a")
save_plot_bundle(fig3, "fig_geb3_species_level_correlates", width = 17.2, height = 7.6)

pp_cont <- pp_check(fit_continuous_phylo, type = "bars", ndraws = 100) + theme_geb(base_size = 10.2) + labs(title = "Posterior predictive check: continuous-trait model")
pp_cat <- pp_check(fit_categorical_phylo, type = "bars", ndraws = 100) + theme_geb(base_size = 10.2) + labs(title = "Posterior predictive check: categorical-trait model")

p_diag_cont <- ggplot(continuous_diag, aes(x = rhat, y = forcats::fct_reorder(variable, rhat), colour = rhat > 1.01)) +
  geom_vline(xintercept = 1.01, linetype = 2, linewidth = 0.7, colour = "#8C2D04") +
  geom_point(size = 2.4) +
  scale_colour_manual(values = c(`FALSE` = "#2C7FB8", `TRUE` = "#B30000")) +
  labs(x = "Rhat", y = NULL, title = "Convergence diagnostic: continuous model") +
  theme_geb(base_size = 10.2) +
  theme(legend.position = "none")

p_diag_cat <- ggplot(categorical_diag, aes(x = rhat, y = forcats::fct_reorder(variable, rhat), colour = rhat > 1.01)) +
  geom_vline(xintercept = 1.01, linetype = 2, linewidth = 0.7, colour = "#8C2D04") +
  geom_point(size = 2.4) +
  scale_colour_manual(values = c(`FALSE` = "#2C7FB8", `TRUE` = "#B30000")) +
  labs(x = "Rhat", y = NULL, title = "Convergence diagnostic: categorical model") +
  theme_geb(base_size = 10.2) +
  theme(legend.position = "none")

fig_s1 <- (pp_cont | pp_cat) / (p_diag_cont | p_diag_cat) + plot_annotation(tag_levels = "a")
save_plot_bundle(fig_s1, "fig_s1_species_model_diagnostics", width = 15.5, height = 9.2)

# -------------------------------
# Step 7. Province-level predictors, screening, and models (Figure 4)
# 第 7 步：省级解释变量、筛查与模型（Figure 4）
# -------------------------------
# 省级部分保留 GEB 风格的核心结构：先构建 effort 指标，再做筛查、回归、
# 相对贡献分析和稳健性检验；但图的版式改成更横向、更紧凑的 2×4 结构。

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
  variable = c("gdp_per_capita", "area_km2", "habitat_heterogeneity", "human_density", "reference_mammal_richness"),
  source_file = c("Province_variables.csv", "Province_variables.csv", "Province_variables.csv", "Province_variables.csv", "Province_variables.csv"),
  used_in_primary_bird_model = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  rationale = c(
    "Generic socioeconomic covariate transferable across taxa.",
    "Generic geographic covariate transferable across taxa.",
    "Generic environmental heterogeneity covariate transferable across taxa.",
    "Screened only; not retained because user effort is directly available from the effort table.",
    "Not used because this richness field originates from the mammal GEB workflow and is taxonomically mismatched for birds."
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
  summarise(report_sum = sum(report_count, na.rm = TRUE), user_sum = sum(user_count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = era, values_from = c(report_sum, user_sum), values_fill = 0) %>%
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
province_vif_tbl <- tibble(predictor = names(car::vif(province_vif_candidate)), vif = as.numeric(car::vif(province_vif_candidate)))

province_predictor_correlation <- province_model_df %>%
  dplyr::select(log_new_record_density, historical_report_density, current_report_density, current_user_density, gdp_per_capita, area_km2, habitat_heterogeneity) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable_1") %>%
  pivot_longer(-variable_1, names_to = "variable_2", values_to = "pearson_r")

province_model_primary <- lm(
  log_new_record_density ~ z_hist_report_density + z_curr_report_density + z_gdp_per_capita + z_area_km2 + z_habitat_heterogeneity,
  data = province_model_df
)

province_model_user_effort <- lm(
  log_new_record_density ~ z_hist_report_density + z_curr_user_density + z_gdp_per_capita + z_area_km2 + z_habitat_heterogeneity,
  data = province_model_df
)

province_model_sensitivity <- MASS::glm.nb(
  n_records ~ z_hist_report_density + z_curr_report_density + z_gdp_per_capita + z_habitat_heterogeneity + offset(log(area_km2)),
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
influential_provinces <- province_model_df %>% filter(cooks_d_primary > influence_threshold) %>% arrange(desc(cooks_d_primary))
province_model_influence <- province_model_df %>% filter(!(province %in% influential_provinces$province))

province_model_influence_sensitivity <- lm(
  log_new_record_density ~ z_hist_report_density + z_curr_report_density + z_gdp_per_capita + z_area_km2 + z_habitat_heterogeneity,
  data = province_model_influence
)

province_vif_primary_final <- tibble(predictor = names(car::vif(province_model_primary)), vif = as.numeric(car::vif(province_model_primary)), model = "Primary report-effort")
province_vif_user_final <- tibble(predictor = names(car::vif(province_model_user_effort)), vif = as.numeric(car::vif(province_model_user_effort)), model = "Alternative user-effort")
province_vif_influence_final <- tibble(predictor = names(car::vif(province_model_influence_sensitivity)), vif = as.numeric(car::vif(province_model_influence_sensitivity)), model = "Influence-filtered")
province_vif_final_tbl <- bind_rows(province_vif_primary_final, province_vif_user_final, province_vif_influence_final)

province_model_comparison <- bind_rows(
  extract_model_fit(province_model_primary, "Primary report-effort model", model_type = "lm"),
  extract_model_fit(province_model_user_effort, "Alternative user-effort model", model_type = "lm"),
  extract_model_fit(province_model_influence_sensitivity, "Influence-filtered report-effort model", model_type = "lm"),
  extract_model_fit(province_model_sensitivity, "Count-based sensitivity model", model_type = "nb")
) %>%
  mutate(max_vif = c(max(province_vif_primary_final$vif, na.rm = TRUE), max(province_vif_user_final$vif, na.rm = TRUE), max(province_vif_influence_final$vif, na.rm = TRUE), NA_real_))

province_diag_summary <- tibble(
  metric = c("n_provinces_modelled", "adjusted_r2_primary", "shapiro_p_residuals", "ncvtest_p_heteroskedasticity", "max_cooks_distance", "overdispersion_ratio_sensitivity", "n_influential_provinces", "influence_threshold_4_over_n"),
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
write.csv(province_predictor_correlation, file.path(data_dir, "province_predictor_correlation.csv"), row.names = FALSE)

province_coef_tbl <- broom::tidy(province_model_primary) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  filter(term != "(Intercept)") %>%
  mutate(predictor = recode_province_predictor(term), significant = conf.low * conf.high > 0)

province_user_effort_coef_tbl <- broom::tidy(province_model_user_effort) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  filter(term != "(Intercept)") %>%
  mutate(predictor = recode_province_predictor(term))

province_sensitivity_coef_tbl <- broom::tidy(province_model_sensitivity) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  filter(term != "(Intercept)") %>%
  mutate(predictor = recode_province_predictor(term))

province_influence_coef_tbl <- broom::tidy(province_model_influence_sensitivity) %>%
  bind_cols(wald_ci(.$estimate, .$std.error)) %>%
  filter(term != "(Intercept)") %>%
  mutate(predictor = recode_province_predictor(term))

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
  "z_hist_report_density", "Historical survey effort", "#A6CEE3",
  "z_curr_report_density", "Current survey effort", "#1F78B4",
  "z_gdp_per_capita", "Per capita GDP", "#33A02C",
  "z_area_km2", "Area", "#FB9A99",
  "z_habitat_heterogeneity", "Habitat heterogeneity", "#6A3D9A"
)

model_terms_primary <- partial_specs$term
partial_plot_list <- list()
partial_data_all <- list()

for (i in seq_len(nrow(partial_specs))) {
  focal <- partial_specs$term[i]
  focal_label <- partial_specs$predictor[i]
  focal_color <- partial_specs$color[i]
  other_terms <- setdiff(model_terms_primary, focal)
  part_df <- compute_partial_df(province_model_df, "log_new_record_density", focal, other_terms) %>%
    bind_cols(province_model_df %>% dplyr::select(province)) %>%
    mutate(term = focal, predictor = focal_label)
  partial_data_all[[focal]] <- part_df
  coef_row <- province_coef_tbl %>% filter(term == focal)
  beta_text <- paste0("beta = ", round(coef_row$estimate, 3), "; p = ", formatC(summary(province_model_primary)$coefficients[focal, "Pr(>|t|)"], format = "e", digits = 2))
  partial_plot_list[[focal]] <- ggplot(part_df, aes(x = focal_resid, y = response_resid)) +
    geom_point(size = 2.2, alpha = 0.9, colour = "#303030") +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1, colour = focal_color, fill = alpha(focal_color, 0.32)) +
    annotate("text", x = max(part_df$focal_resid, na.rm = TRUE), y = max(part_df$response_resid, na.rm = TRUE), label = beta_text, hjust = 1, vjust = 1, size = 3.8) +
    labs(x = paste0("Residualized ", focal_label), y = "Residualized log density") +
    theme_geb(base_size = 10.2)
}

partial_data_tbl <- bind_rows(partial_data_all)
write.csv(partial_data_tbl, file.path(data_dir, "province_partial_regression_data.csv"), row.names = FALSE)

calc_r2 <- function(data, response, predictors) {
  if (length(predictors) == 0) return(0)
  summary(lm(as.formula(paste(response, "~", paste(predictors, collapse = " + "))), data = data))$r.squared
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
  tibble(term = names(independent_r2), independent_r2 = as.numeric(independent_r2), percent = independent_r2 / sum(independent_r2) * 100)
}

importance_tbl <- hierarchical_partition(province_model_df, "log_new_record_density", model_terms_primary) %>% mutate(predictor = recode_province_predictor(term))

bootstrap_hp <- function(data, response, predictors, n_boot = 300) {
  res <- vector("list", n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample(seq_len(nrow(data)), size = nrow(data), replace = TRUE)
    boot_df <- data[idx, , drop = FALSE]
    res[[b]] <- hierarchical_partition(boot_df, response, predictors) %>% mutate(iteration = b)
  }
  bind_rows(res)
}

importance_boot_tbl <- bootstrap_hp(province_model_df, "log_new_record_density", model_terms_primary, n_boot = 300) %>% mutate(predictor = recode_province_predictor(term))
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
  "Historical survey effort" = "#A6CEE3",
  "Current survey effort" = "#1F78B4",
  "Per capita GDP" = "#33A02C",
  "Area" = "#FB9A99",
  "Habitat heterogeneity" = "#6A3D9A"
)

p_fig4a <- partial_plot_list[["z_hist_report_density"]]
p_fig4b <- partial_plot_list[["z_curr_report_density"]]
p_fig4c <- partial_plot_list[["z_gdp_per_capita"]]
p_fig4d <- partial_plot_list[["z_area_km2"]]
p_fig4e <- partial_plot_list[["z_habitat_heterogeneity"]]

p_fig4f <- ggplot(province_coef_tbl, aes(x = predictor, y = estimate, colour = predictor)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.18, linewidth = 0.9) +
  geom_point(size = 4.0) +
  scale_colour_manual(values = coef_colors) +
  coord_flip() +
  labs(x = NULL, y = "Slope estimate") +
  theme_geb(base_size = 10.2) +
  theme(legend.position = "none")

p_fig4g <- ggplot(importance_tbl, aes(x = reorder(predictor, percent), y = percent, fill = predictor)) +
  geom_col(width = 0.72, colour = "white") +
  coord_flip() +
  scale_fill_manual(values = coef_colors) +
  labs(x = NULL, y = "Relative importance (%)") +
  theme_geb(base_size = 10.2) +
  theme(legend.position = "none")

p_fig4h <- ggplot(province_robustness_coef_tbl, aes(x = predictor, y = estimate, colour = model)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_point(position = position_dodge(width = 0.45), size = 2.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.45), width = 0.14, linewidth = 0.72) +
  coord_flip() +
  scale_colour_manual(values = c("Primary report-effort" = "#2C7FB8", "Alternative user-effort" = "#FC8D59", "Influence-filtered" = "#31A354")) +
  labs(x = NULL, y = "Coefficient estimate", colour = NULL) +
  theme_geb(base_size = 10.2) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8.5),
    legend.spacing.x = unit(0.3, "cm")
  )

fig4 <- (p_fig4a | p_fig4b | p_fig4c | p_fig4d) /
  (p_fig4e | p_fig4f | p_fig4g | p_fig4h) +
  plot_annotation(tag_levels = "a")

save_plot_bundle(fig4, "fig_geb4_province_level_effort_drivers", width = 18.8, height = 9.4)

p_s2_diag1 <- ggplot(province_model_df, aes(x = fitted_primary, y = std_resid_primary)) +
  geom_point(size = 2.2, alpha = 0.9, colour = "#333333") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Fitted values", y = "Standardized residuals", title = "Province model residual pattern") +
  theme_geb(base_size = 10.2)

p_s2_diag2 <- ggplot(province_model_df, aes(sample = std_resid_primary)) +
  stat_qq(size = 1.8, alpha = 0.8, colour = "#333333") +
  stat_qq_line(colour = "#FC8D59") +
  labs(x = "Theoretical quantiles", y = "Standardized residual quantiles", title = "Q-Q plot") +
  theme_geb(base_size = 10.2)

p_s2_diag3 <- ggplot(province_model_df, aes(x = seq_along(cooks_d_primary), y = cooks_d_primary)) +
  geom_col(fill = "#7FCDBB") +
  geom_hline(yintercept = 4 / nrow(province_model_df), linetype = 2, colour = "#B30000") +
  labs(x = "Province index", y = "Cook's distance", title = "Influence screening") +
  theme_geb(base_size = 10.2)

p_s2_diag4 <- ggplot(province_model_comparison, aes(x = reorder(model, aic), y = aic, fill = family)) +
  geom_col(width = 0.72, colour = "white") +
  coord_flip() +
  scale_fill_manual(values = c("Gaussian LM" = "#80B1D3", "Negative binomial" = "#FDB462")) +
  labs(x = NULL, y = "AIC", fill = NULL, title = "Candidate-model comparison") +
  theme_geb(base_size = 10.2) +
  theme(legend.position = "top")

fig_s2 <- (p_s2_diag1 | p_s2_diag2) / (p_s2_diag3 | p_s2_diag4) + plot_annotation(tag_levels = "a")
save_plot_bundle(fig_s2, "fig_s2_province_model_diagnostics_and_sensitivity", width = 14.2, height = 8.8)

p_s3_robust1 <- ggplot(importance_boot_summary, aes(x = predictor, y = percent_mean, colour = predictor)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_errorbar(aes(ymin = percent_low, ymax = percent_high), width = 0.16, linewidth = 0.85) +
  geom_point(size = 3.2) +
  scale_colour_manual(values = coef_colors) +
  coord_flip() +
  labs(x = NULL, y = "Bootstrapped relative importance (%)") +
  theme_geb(base_size = 10.2) +
  theme(legend.position = "none")

p_s3_robust2 <- ggplot(province_robustness_delta_tbl %>% filter(!is.na(delta_user_minus_primary)), aes(x = predictor, y = delta_user_minus_primary)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_col(fill = "#FC8D59", width = 0.72) +
  coord_flip() +
  labs(x = NULL, y = "Delta (user effort - primary)") +
  theme_geb(base_size = 10.2)

p_s3_robust3 <- ggplot(province_robustness_delta_tbl %>% filter(!is.na(delta_influence_minus_primary)), aes(x = predictor, y = delta_influence_minus_primary)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8, colour = "#6E6E6E") +
  geom_col(fill = "#31A354", width = 0.72) +
  coord_flip() +
  labs(x = NULL, y = "Delta (influence-filtered - primary)") +
  theme_geb(base_size = 10.2)

p_s3_robust4 <- ggplot(influential_provinces, aes(x = reorder(province, cooks_d_primary), y = cooks_d_primary)) +
  geom_col(fill = "#B2182B", width = 0.72) +
  coord_flip() +
  labs(x = NULL, y = "Cook's distance") +
  theme_geb(base_size = 10.2)

fig_s3 <- (p_s3_robust1 | p_s3_robust2) / (p_s3_robust3 | p_s3_robust4) + plot_annotation(tag_levels = "a")
save_plot_bundle(fig_s3, "fig_s3_province_model_robustness", width = 14.4, height = 8.8)

# -------------------------------
# Step 8. Build integrated screening figure
# 第 8 步：构建综合数据筛查图
# -------------------------------
p_s0a <- ggplot(continuous_trait_distribution, aes(x = value, fill = trait)) +
  geom_histogram(bins = 22, colour = "white", alpha = 0.9) +
  facet_wrap(~ trait, scales = "free", ncol = 3) +
  scale_fill_manual(values = c(
    "Body mass (log10 g)" = "#0E7490",
    "HWI (log1p)" = "#14B8A6",
    "Range size (log1p provinces)" = "#2563EB",
    "Clutch size (log1p)" = "#F59E0B",
    "Congeners (log1p)" = "#D97706"
  )) +
  labs(x = "Observed value", y = "Frequency", fill = NULL, title = "Continuous-trait distributions before phylogenetic modelling") +
  theme_geb(base_size = 9.8) +
  theme(legend.position = "none")

p_s0b <- ggplot(trait_missing_summary, aes(x = reorder(variable, -pct_missing), y = pct_missing)) +
  geom_col(fill = "#6BAED6", width = 0.72) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Missing proportion", title = "Trait missingness audit") +
  theme_geb(base_size = 10.2) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_s0c <- ggplot(categorical_trait_distribution, aes(x = level, y = n, fill = variable)) +
  geom_col(colour = "white", width = 0.72) +
  facet_wrap(~ variable, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c("Endemicity" = "#C2410C", "Migration" = "#4F46E5", "Diet" = "#15803D", "Forest association" = "#166534")) +
  labs(x = NULL, y = "Species count", fill = NULL, title = "Categorical-trait frequency screening") +
  theme_geb(base_size = 9.8) +
  theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "none")

p_s0d <- coverage_by_year %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = report_sum, colour = "Report count"), linewidth = 1.05) +
  geom_line(aes(y = user_sum, colour = "User count"), linewidth = 1.05, linetype = 2) +
  scale_colour_manual(values = c("Report count" = "#1F78B4", "User count" = "#33A02C")) +
  labs(x = "Year", y = "Annual total", colour = NULL, title = "Effort coverage through time") +
  theme_geb(base_size = 10.2) +
  theme(legend.position = "top")

fig_s0 <- (p_s0a | p_s0b) / (p_s0c | p_s0d) + plot_annotation(tag_levels = "a")
save_plot_bundle(fig_s0, "fig_s0_data_screening_overview", width = 15.2, height = 9.0)

# -------------------------------
# Step 9. Write bilingual interpretation files and Excel bundle
# 第 9 步：写出双语结果解释与汇总 Excel
# -------------------------------
cont_sig_terms <- continuous_effects %>% filter(conf.low * conf.high > 0)
cat_sig_terms <- categorical_effects %>% filter(conf.low * conf.high > 0)

summary_en <- c(
  "Bird Figure 3 was rebuilt with explicit phylogenetic control using the Hackett bird phylogeny.",
  paste0("The continuous-trait phylogenetic model included ", nrow(continuous_model_tree), " species and estimated a phylogenetic signal of ", round(species_phylo_model_fit$phylogenetic_lambda[1], 3), "."),
  paste0("The categorical-trait phylogenetic model included ", nrow(categorical_model_tree), " species and estimated a phylogenetic signal of ", round(species_phylo_model_fit$phylogenetic_lambda[2], 3), "."),
  paste0("Among continuous traits, effects with 95% credible intervals excluding zero were: ", paste(cont_sig_terms$term_label, collapse = "; "), "."),
  paste0("Among categorical traits, effects with 95% credible intervals excluding zero were: ", paste(cat_sig_terms$term_label, collapse = "; "), "."),
  paste0("At the province level, the primary effort model explained ", percent(summary(province_model_primary)$r.squared, accuracy = 0.1), " of the variance in log-transformed bird new-record discovery intensity."),
  "Current survey effort remained the dominant positive province-level predictor, whereas province area retained a negative relationship after accounting for other drivers.",
  "The qualitative province-level conclusions were robust to the alternative user-effort proxy and to the exclusion of influential provinces."
)

summary_cn <- c(
  "鸟类 Figure 3 已升级为显式控制系统发育非独立性的版本，系统发育信息来自 Hackett 鸟类系统发育树。",
  paste0("连续性状系统发育模型共纳入 ", nrow(continuous_model_tree), " 个物种，估计的系统发育信号为 ", round(species_phylo_model_fit$phylogenetic_lambda[1], 3), "。"),
  paste0("分类性状系统发育模型共纳入 ", nrow(categorical_model_tree), " 个物种，估计的系统发育信号为 ", round(species_phylo_model_fit$phylogenetic_lambda[2], 3), "。"),
  paste0("在连续性状中，95% 可信区间不跨 0 的变量包括：", paste(cont_sig_terms$term_label, collapse = "；"), "。"),
  paste0("在分类性状中，95% 可信区间不跨 0 的变量包括：", paste(cat_sig_terms$term_label, collapse = "；"), "。"),
  paste0("在省级尺度上，主效应模型解释了鸟类新纪录发现强度对数值方差的 ", percent(summary(province_model_primary)$r.squared, accuracy = 0.1), "。"),
  "当前调查努力仍然是省级层面最重要的正向驱动因子，而省域面积在控制其他变量后仍表现为负向关系。",
  "无论是改用用户 effort 代理变量，还是剔除高影响省份，省级主结论都保持稳定。"
)

figure_captions <- c(
  "Figure 3. Species-level correlates of bird new distribution records in China after accounting for phylogenetic non-independence. Panel (a) shows posterior ridge distributions for continuous traits, including body mass, hand-wing index (HWI), range size, clutch size, and number of congeners. Panel (b) shows posterior ridge distributions for categorical ecological attributes, including endemicity, migration strategy, diet type, and forest-associated primary habitat. Points indicate posterior medians and thick horizontal segments indicate 95% credible intervals.",
  "图 3. 控制系统发育非独立性之后，中国鸟类新分布纪录的物种层相关因素。面板（a）展示连续性状的后验山脊分布，包括体重、手翼指数（HWI）、分布范围、窝卵数和同属物种数；面板（b）展示分类生态属性的后验山脊分布，包括特有性、迁徙策略、食性类型和森林关联的主栖息地。点表示后验中位数，粗横线表示 95% 可信区间。",
  "Figure 4. Province-level drivers of bird new-record discovery intensity in China. Panels (a-e) show partial-regression relationships for historical survey effort, current survey effort, per capita GDP, province area, and habitat heterogeneity. Panel (f) summarizes slope estimates from the primary multiple-regression model, panel (g) shows relative importance from hierarchical partitioning, and panel (h) evaluates coefficient robustness across alternative effort specifications and influence screening.",
  "图 4. 中国鸟类新纪录发现强度的省级驱动因素。面板（a-e）展示历史调查努力、当前调查努力、人均 GDP、省域面积和生境异质性的偏回归关系；面板（f）汇总主多元回归模型的斜率估计，面板（g）展示层次分解得到的相对贡献，面板（h）展示在不同 effort 设定和高影响省份筛查下系数估计的稳健性。"
)

writeLines(c("# Task summary / 任务摘要", "", "## English", summary_en, "", "## 中文", summary_cn), file.path(results_dir, "task_summary_bilingual.md"))
writeLines(c("# Figure captions / 图题", "", figure_captions), file.path(results_dir, "figure_captions_bilingual.md"))
writeLines(c("# Interpretation / 结果解释", "", "## English", summary_en, "", "## 中文", summary_cn), file.path(results_dir, "interpretation_bilingual.md"))

bundle_list <- list(
  bird_events = bird_clean,
  bird_event_qa = qa_summary,
  effort_clean = effort_raw,
  effort_missing = effort_missing_summary,
  trait_audit = candidate_trait_audit,
  trait_availability = trait_availability,
  species_pool = species_pool,
  species_continuous = continuous_model_tree,
  species_categorical = categorical_model_tree,
  phylogeny_audit = tree_match_audit,
  cont_effects = continuous_effects,
  cat_effects = categorical_effects,
  phylo_fit_summary = species_phylo_model_fit,
  phylo_param_diag = bind_rows(continuous_diag, categorical_diag),
  province_model = province_model_df,
  province_coef = province_coef_tbl,
  province_robust = province_robustness_coef_tbl,
  province_importance = importance_tbl,
  province_import_boot = importance_boot_summary,
  province_models = province_model_comparison
)

writexl::write_xlsx(bundle_list, file.path(results_dir, "bird_geb_fig3_fig4_analysis_bundle.xlsx"))

message("Upgraded phylogenetic GEB-style bird workflow completed successfully.")
