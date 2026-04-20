#!/usr/bin/env Rscript

# ============================================================
# Bird new-distribution records in China:
# McTavish-phylogeny circular tree workflow
# 中国鸟类新纪录：基于 McTavish 鸟类系统发育树的环形系统树工作流
# ============================================================
#
# Scientific question / 科学问题
# 1. Within the full species pool of birds recorded in the 2025 Catalogue of
#    Life China checklist, which species have generated corrected provincial
#    new-distribution records after accounting for synonymy and duplicate
#    publication issues?
# 2. How are these newly recorded bird species distributed across orders on a
#    published avian phylogenetic backbone, and what proportion of each order's
#    Chinese species pool do they represent?
# 1. 在《中国生物物种名录（2025）》定义的中国鸟类完整物种库中，经过同物
#    异名归并和同物种同省重复记录剔除后，哪些物种构成了校正后的省级鸟类新纪录？
# 2. 这些新纪录鸟类在已发表的全球鸟类系统发育骨架上如何分布，并且分别占其所属目
#    在中国物种库中的多大比例？
#
# Scientific background / 研究背景
# New provincial bird records are shaped jointly by taxonomy, biogeography,
# detectability, survey effort, and historical under-documentation. A simple
# taxonomic tree can summarize order composition, but it cannot show how the
# corrected Chinese bird species pool is arranged on a published phylogenetic
# backbone. The complete and dynamic tree of birds by McTavish et al. provides
# an updated, openly distributed, taxonomy-linked global avian phylogeny that
# is appropriate for visualizing where newly recorded Chinese bird species fall
# across the broader bird tree.
# 鸟类省级新纪录的形成同时受分类修订、生物地理过程、可探测性、调查努力与历史
# 调查不足共同影响。仅依赖分类层级树可以概括目组成，但不能在已发表的系统发育
# 骨架上展示“校正后的中国鸟类物种库”中哪些分支产生了新纪录。McTavish 等构建
# 的 complete and dynamic tree of birds 提供了一个公开、可追溯、与现代鸟类
# 分类体系相连的全球系统发育树，非常适合用来展示中国鸟类新纪录在鸟类演化树上的
# 空间位置与目内占比。
#
# Objectives / 研究目标
# 1. Rebuild the Chinese bird species pool directly from the 2025 Chinese
#    checklist, keeping only species-rank binomials.
# 2. Carry forward the previously corrected synonym and duplicate decisions so
#    that newly recorded species identities are canonical and publication-safe.
# 3. Match both the checklist pool and the corrected new-record species to the
#    official McTavish bird tree, using an explicit taxonomy bridge table for
#    name changes between the Chinese checklist and the tree taxonomy.
# 4. Produce a polished circular phylogeny figure that highlights newly recorded
#    species, summarizes their order-level proportions, and exports code, data,
#    diagnostics, figures, captions, and summary text in a standardized task
#    folder.
# 1. 直接从 2025 中国物种名录重建中国鸟类物种库，并严格保留种级双名记录。
# 2. 继承前面已经完成的同物异名与重复记录校正结果，确保新纪录物种身份规范且可
#    直接用于论文和数据文章。
# 3. 将中国鸟类物种库和校正后新纪录物种同时匹配到官方 McTavish 鸟类系统树，并
#    对中国名录与系统树之间的名称差异建立透明的 taxonomy bridge 表。
# 4. 生成一张高质量、专业美观的环形系统发育树图，突出显示新纪录物种及其目内比例，
#    并在标准任务目录中输出数据、代码、诊断、图表、图题和结果摘要。
#
# Analytical strategy / 分析思路
# 1. Read the Chinese checklist sheet and retain only bird species-level
#    binomials (remove subspecies and non-standard infraspecific entries).
# 2. Read the corrected canonical new-record event table that already accounts
#    for synonymy and first-publication precedence for duplicate species-province
#    events.
# 3. Read the official McTavish bird tree (`summary_dated_clements.nex`).
# 4. Match species names directly to tree tip labels, then apply a transparent
#    bridge table for updated generic placements and a small number of checklist–
#    tree taxonomy mismatches.
# 5. Quantify exact matches, bridged matches, unresolved names, and any many-to-
#    one mappings created by taxonomic lump/split differences.
# 6. Prune the published tree to the matched Chinese bird species pool and
#    aggregate tip-level metadata, including whether each tip corresponds to at
#    least one corrected new-record species.
# 7. Build a circular publication-style phylogeny with:
#    - grey background branches for the Chinese bird pool,
#    - coloured order-level clade branches and a single outer ring that marks
#      newly recorded species by order,
#    - internal percentage bubbles placed close to the root region of each major
#      order,
#    - external order labels and silhouettes styled after the reference figure.
# 8. Export diagnostics, matching tables, bridge tables, the figure, captions,
#    and a bundled Excel workbook.
# 1. 读取中国名录工作表，仅保留鸟纲的种级双名记录，去除亚种和非标准种下单元。
# 2. 读取已经完成同物异名归并和“同物种同省保留最早发表记录”的校正底表。
# 3. 读取官方 McTavish 鸟类系统树（`summary_dated_clements.nex`）。
# 4. 先做直接匹配，再通过透明的名称桥接表处理名录分类与系统树分类之间的属级变更
#    和少量树–名录不一致问题。
# 5. 量化直接匹配、桥接匹配、未解决名称以及由于 split/lump 导致的一对多/多对一
#    概念映射。
# 6. 将官方树裁剪为“中国鸟类物种库匹配子树”，并聚合 tip 层信息，判断每个 tip
#    是否对应至少一个校正后的新纪录物种。
# 7. 绘制投稿级环形系统发育图，包括：
#    - 中国鸟类物种库的灰色背景树；
#    - 仅对“新纪录物种对应路径”按目着色，其余分支保持灰色；
#    - 加宽并贴近 tip 的单层外环；
#    - 靠近根部、按目逐一放置的比例气泡；
#    - 仿照参考图的外侧目标签与剪影，并尽量为每个有新纪录的目配置剪影。
# 8. 导出诊断、匹配表、桥接表、图件、双语图题以及汇总 Excel 工作簿。
#
# Diagnostics and validation / 诊断与验证
# - Species-pool filtering audit: how many bird checklist rows are removed
#   because they are subspecies or non-standard entries.
# - Direct-match versus bridged-match audit for both the Chinese species pool
#   and the corrected new-record species set.
# - Explicit bridge table documenting every manual taxonomy translation applied.
# - Unresolved-name table retained for transparent review.
# - Many-to-one mapping check, because recent taxonomy updates can map more than
#   one checklist concept onto a single tree tip.
# - Order-level denominator and numerator checks: total Chinese species per
#   order versus corrected newly recorded species per order.
# - Figure-specific QA plot summarizing match status and order-level proportion
#   distributions before final figure export.
# - 物种库过滤审计：统计多少鸟类名录记录因亚种或非标准种下单元而被移除。
# - 中国鸟类物种库和校正后新纪录物种的“直接匹配–桥接匹配–未匹配”审计。
# - 显式桥接表：记录每一条手工 taxonomy translation。
# - 保留未解决名称表，方便后续人工复核。
# - 检查 many-to-one 映射，因为最新分类体系变化可能把多个名录概念映射到同一个
#   系统树 tip。
# - 检查按目的分母和分子：每个目中国物种总数 versus 每个目校正后新纪录物种数。
# - 生成匹配与比例分布 QA 图，在最终出图前完成透明诊断。
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
  library(ape)
  library(writexl)
  library(officer)
  library(rvg)
  library(export)
  library(httr2)
  library(jsonlite)
  library(gridGraphics)
})

set.seed(20260416)

# -------------------------------
# Step 0. Task paths and global parameters
# 第 0 步：任务路径与全局参数
# -------------------------------
get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    candidate <- sub("^--file=", "", file_arg[1])
    if (!file.exists(candidate) && grepl("~\\+~", candidate)) candidate <- gsub("~\\+~", " ", candidate)
    return(normalizePath(candidate, mustWork = FALSE))
  }
  r_candidates <- cmd_args[grepl("\\.[Rr]$", cmd_args)]
  r_candidates <- gsub("~\\+~", " ", r_candidates)
  r_candidates <- r_candidates[file.exists(r_candidates)]
  if (length(r_candidates) > 0) return(normalizePath(r_candidates[1], mustWork = FALSE))
  normalizePath(getwd())
}

script_path <- get_script_path()
code_dir <- if (dir.exists(script_path)) script_path else dirname(script_path)
if (basename(code_dir) != "code" && basename(getwd()) == "code") code_dir <- normalizePath(getwd())
task_root <- Sys.getenv("BIRD_TASK_DIR", unset = file.path(code_dir, ".."))
data_dir <- file.path(task_root, "data")
fig_dir <- file.path(task_root, "figures")
results_dir <- file.path(task_root, "results")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(data_dir, "external"), recursive = TRUE, showWarnings = FALSE)

master_xlsx <- Sys.getenv("BIRD_MASTER_XLSX", unset = "/Users/dingchenchen/Desktop/鸟类新纪录20260311.xlsx")
corrected_events_csv <- Sys.getenv(
  "BIRD_CORRECTED_EVENTS_CSV",
  unset = "/Users/dingchenchen/Documents/New records/bird-new-distribution-records/tasks/bird_identity_synonym_dedup_reanalysis/data/bird_new_records_clean_corrected.csv"
)
tree_path <- Sys.getenv(
  "BIRD_MCTAVISH_TREE",
  unset = file.path(data_dir, "external", "summary_dated_clements_Aves_1.4_Clements2023.nex")
)

tree_url_candidates <- c(
  "https://raw.githubusercontent.com/McTavishLab/AvesData/main/Tree_versions/Aves_1.4/Clements2023/summary_dated_clements.nex",
  "https://raw.githubusercontent.com/McTavishLab/AvesData/main/Tree_versions/Aves_1.4/Clements_2023/summary_dated_clements.nex"
)

sheet_catalog <- "2025中国生物物种名录"
dpi_out <- 420
figure_stub <- "fig_phy01_mctavish_bird_new_records_phylogeny"
figure_stub_consistency <- "fig_phy01_mctavish_bird_new_records_phylogeny_consistency_v2"
figure_stub_strict <- "fig_phy01_mctavish_bird_new_records_phylogeny_strict_correspondence_v3"

theme_clean <- function(base_size = 11.2, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.title = element_text(face = "bold", colour = "#111111"),
      axis.text = element_text(colour = "#1A1A1A"),
      plot.title = element_text(face = "bold", colour = "#111111"),
      plot.subtitle = element_text(colour = "#303030"),
      legend.position = "none",
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "#D9D9D9", colour = "#4D4D4D", linewidth = 0.6),
      strip.text = element_text(face = "bold")
    )
}

save_gg_bundle <- function(plot_obj, stub, width, height) {
  png_path <- file.path(fig_dir, paste0(stub, ".png"))
  pdf_path <- file.path(fig_dir, paste0(stub, ".pdf"))
  pptx_path <- file.path(fig_dir, paste0(stub, ".pptx"))
  tryCatch(
    ggsave(png_path, plot_obj, width = width, height = height, dpi = dpi_out, bg = "white"),
    error = function(e) message("PNG export failed for ", stub, ": ", e$message)
  )
  tryCatch({
    pdf(pdf_path, width = width, height = height, useDingbats = FALSE, bg = "white", onefile = FALSE)
    print(plot_obj)
    dev.off()
  }, error = function(e) message("PDF export failed for ", stub, ": ", e$message))
  tryCatch(
    export::graph2ppt(x = plot_obj, file = pptx_path, width = width, height = height, vector.graphic = TRUE, append = FALSE),
    error = function(e) message("PPTX export failed for ", stub, ": ", e$message)
  )
}

save_base_bundle <- function(draw_fun, stub, width = 13.6, height = 9.2) {
  png_path <- file.path(fig_dir, paste0(stub, ".png"))
  pdf_path <- file.path(fig_dir, paste0(stub, ".pdf"))
  pptx_path <- file.path(fig_dir, paste0(stub, ".pptx"))

  tryCatch({
    png(png_path, width = width, height = height, units = "in", res = dpi_out, bg = "white")
    draw_fun()
    dev.off()
  }, error = function(e) message("PNG export failed for ", stub, ": ", e$message))

  tryCatch({
    pdf(pdf_path, width = width, height = height, useDingbats = FALSE, bg = "white", onefile = FALSE)
    draw_fun()
    dev.off()
  }, error = function(e) message("PDF export failed for ", stub, ": ", e$message))

  tryCatch({
    ppt <- read_pptx()
    ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
    ppt <- ph_with(
      x = ppt,
      value = dml(code = {
        grid::grid.newpage()
        gridGraphics::grid.echo(draw_fun, newpage = FALSE)
      }),
      location = ph_location(left = 0, top = 0, width = 13.333, height = 7.5)
    )
    print(ppt, target = pptx_path)
  }, error = function(e) {
    message("PPTX export failed for ", stub, ": ", e$message)
    if (file.exists(png_path)) {
      ppt <- read_pptx()
      ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
      ppt <- ph_with(
        x = ppt,
        value = external_img(png_path, width = 13.333, height = 7.5),
        location = ph_location(left = 0, top = 0, width = 13.333, height = 7.5)
      )
      print(ppt, target = pptx_path)
    }
  })
}

# -------------------------------
# Step 1. Download or verify the McTavish bird tree
# 第 1 步：下载或验证 McTavish 鸟类系统树
# -------------------------------
download_tree_if_needed <- function(target_path, url_candidates) {
  if (file.exists(target_path) && file.info(target_path)$size > 1000000) {
    return(target_path)
  }
  for (u in url_candidates) {
    ok <- tryCatch({
      download.file(u, destfile = target_path, mode = "wb", quiet = TRUE)
      file.exists(target_path) && file.info(target_path)$size > 1000000
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(target_path)
  }
  stop("The McTavish tree file is unavailable locally and could not be downloaded.")
}

tree_path <- download_tree_if_needed(tree_path, tree_url_candidates)
tree_full <- read.nexus(tree_path)

# -------------------------------
# Step 2. Rebuild the Chinese bird species pool from the 2025 checklist
# 第 2 步：从 2025 中国名录重建中国鸟类物种库
# -------------------------------
strict_species_rank_filter <- function(x) {
  x <- str_squish(x)
  word_n <- str_count(x, " ") + 1L
  rank_marker <- str_detect(x, regex(" subsp\\.| spp\\.| sp\\.| cf\\.| aff\\.| x ", ignore_case = TRUE))
  word_n == 2L & !rank_marker
}

catalog_raw <- read_excel(master_xlsx, sheet = sheet_catalog, .name_repair = "minimal")
catalog_block <- catalog_raw[, 1:14]
names(catalog_block) <- c(
  "species", "species_cn", "kingdom_latin", "kingdom_cn", "phylum_latin", "phylum_cn",
  "class_latin", "class_cn", "order_raw", "order_cn", "family_raw", "family_cn",
  "genus_raw", "genus_cn"
)

checklist_all_birds <- catalog_block %>%
  mutate(across(everything(), ~ if (is.character(.x)) str_squish(.x) else .x)) %>%
  filter(class_latin == "Aves", !is.na(species), species != "")

checklist_species_pool <- checklist_all_birds %>%
  mutate(
    is_strict_species_rank = strict_species_rank_filter(species),
    order = str_to_title(str_to_lower(order_raw)),
    family = family_raw,
    genus = genus_raw
  ) %>%
  filter(is_strict_species_rank) %>%
  distinct(species, .keep_all = TRUE) %>%
  select(species, species_cn, order, order_cn, family, family_cn, genus, genus_cn)

# -------------------------------
# Step 3. Read the corrected new-record table with synonym and duplicate fixes
# 第 3 步：读取已完成同物异名与重复校正的新纪录底表
# -------------------------------
corrected_events <- read.csv(corrected_events_csv, stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(
    species = str_squish(species),
    species_cn = str_squish(species_cn),
    english_name = str_squish(english_name),
    order = str_squish(order),
    iucn = if_else(is.na(iucn) | iucn == "", "DD", iucn)
  )

pick_mode_or_first <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  tb <- sort(table(x), decreasing = TRUE)
  names(tb)[1]
}

new_record_species <- corrected_events %>%
  arrange(species, year, province) %>%
  group_by(species) %>%
  summarise(
    species_cn = pick_mode_or_first(species_cn),
    english_name = pick_mode_or_first(english_name),
    order = pick_mode_or_first(order),
    iucn = pick_mode_or_first(iucn),
    n_event_records = n(),
    .groups = "drop"
  ) %>%
  arrange(order, species)

# -------------------------------
# Step 4. Build an explicit taxonomy bridge table
# 第 4 步：建立透明的 taxonomy bridge 表
# -------------------------------
taxonomy_bridge <- tibble::tribble(
  ~source_species,              ~target_tree_label,                 ~bridge_type,                     ~bridge_note,
  "Haliaeetus humilis",         "Icthyophaga_humilis",              "Genus update",                   "Checklist genus differs from McTavish/Clements tree taxonomy.",
  "Haliaeetus leucogaster",     "Icthyophaga_leucogaster",          "Genus update",                   "Checklist genus differs from McTavish/Clements tree taxonomy.",
  "Charadrius alexandrinus",    "Anarhynchus_alexandrinus",         "Genus update",                   "Recent plover generic revision.",
  "Charadrius asiaticus",       "Anarhynchus_asiaticus",            "Genus update",                   "Recent plover generic revision.",
  "Charadrius atrifrons",       "Anarhynchus_atrifrons",            "Genus update",                   "Recent plover generic revision.",
  "Charadrius dealbatus",       "Anarhynchus_dealbatus",            "Genus update",                   "Recent plover generic revision.",
  "Charadrius leschenaultii",   "Anarhynchus_leschenaultii",        "Genus update",                   "Recent plover generic revision.",
  "Charadrius mongolus",        "Anarhynchus_mongolus",             "Genus update",                   "Recent plover generic revision.",
  "Charadrius veredus",         "Anarhynchus_veredus",              "Genus update",                   "Recent plover generic revision.",
  "Larus brunnicephalus",       "Chroicocephalus_brunnicephalus",   "Genus update",                   "Checklist gull genus differs from tree taxonomy.",
  "Larus genei",                "Chroicocephalus_genei",            "Genus update",                   "Checklist gull genus differs from tree taxonomy.",
  "Larus ichthyaetus",          "Ichthyaetus_ichthyaetus",          "Genus update",                   "Checklist gull genus differs from tree taxonomy.",
  "Larus relictus",             "Ichthyaetus_relictus",             "Genus update",                   "Checklist gull genus differs from tree taxonomy.",
  "Grus canadensis",            "Antigone_canadensis",              "Genus update",                   "Crane genus placement updated in the tree taxonomy.",
  "Grus vipio",                 "Antigone_vipio",                   "Genus update",                   "Crane genus placement updated in the tree taxonomy.",
  "Grus virgo",                 "Anthropoides_virgo",               "Genus update",                   "Crane genus placement updated in the tree taxonomy.",
  "Amaurornis cinerea",         "Poliolimnas_cinereus",             "Genus and epithet update",       "Checklist rail concept linked to current tree taxonomy.",
  "Gorsachius magnificus",      "Oroanassa_magnifica",              "Genus and epithet update",       "White-eared Night Heron updated in tree taxonomy.",
  "Otocichla mupinensis",       "Turdus_mupinensis",                "Genus update",                   "Chinese Thrush placed in Turdus in the tree taxonomy.",
  "Paradoxornis gularis",       "Psittiparus_gularis",              "Genus update",                   "Parrotbill generic revision.",
  "Paradoxornis heudei",        "Calamornis_heudei",                "Genus update",                   "Parrotbill generic revision.",
  "Pardaliparus venustulus",    "Periparus_venustulus",             "Genus update",                   "Tit generic revision.",
  "Bubo nipalensis",            "Ketupa_nipalensis",                "Genus update",                   "Eagle-owl generic revision in the tree taxonomy.",
  "Leiopicus auriceps",         "Dendrocoptes_auriceps",            "Genus update",                   "Woodpecker generic revision.",
  "Anas carolinensis",          "Anas_crecca",                      "Checklist-tree concept bridge",  "Tree follows a less split taxonomic concept than the checklist for this teal.",
  "Phoenicopterus roseus",      "Phoenicopterus_roseus",            "Direct-format safeguard",        "Explicit bridge retained to make the audit table complete."
)

tree_tip_labels <- tree_full$tip.label

apply_tree_matching <- function(df) {
  df %>%
    mutate(tree_label_direct = str_replace_all(species, " ", "_")) %>%
    left_join(taxonomy_bridge, by = c("species" = "source_species")) %>%
    mutate(
      tree_label_final = coalesce(target_tree_label, tree_label_direct),
      exact_match = tree_label_direct %in% tree_tip_labels,
      bridged_match = !exact_match & !is.na(target_tree_label) & tree_label_final %in% tree_tip_labels,
      unresolved = !exact_match & !bridged_match,
      match_status = case_when(
        exact_match ~ "Exact match",
        bridged_match ~ "Bridged match",
        TRUE ~ "Unresolved"
      )
    )
}

checklist_matched <- apply_tree_matching(checklist_species_pool)
new_record_matched <- apply_tree_matching(new_record_species)

# -------------------------------
# Step 5. Aggregate auditing tables and order-level summaries
# 第 5 步：整理审计表与按目汇总表
# -------------------------------
checklist_match_audit <- checklist_matched %>%
  count(match_status, name = "n_species") %>%
  mutate(dataset = "Chinese bird species pool")

new_record_match_audit <- new_record_matched %>%
  count(match_status, name = "n_species") %>%
  mutate(dataset = "Corrected new-record species")

matching_audit <- bind_rows(checklist_match_audit, new_record_match_audit) %>%
  select(dataset, match_status, n_species)

many_to_one_audit <- checklist_matched %>%
  filter(match_status != "Unresolved") %>%
  count(tree_label_final, name = "n_species_concepts") %>%
  filter(n_species_concepts > 1) %>%
  arrange(desc(n_species_concepts), tree_label_final)

order_pool_summary <- checklist_species_pool %>%
  count(order, name = "n_china_species")

order_new_summary <- new_record_species %>%
  count(order, name = "n_new_species")

order_summary <- order_pool_summary %>%
  left_join(order_new_summary, by = "order") %>%
  mutate(
    n_new_species = replace_na(n_new_species, 0L),
    prop_new_species = n_new_species / n_china_species,
    prop_pct = prop_new_species * 100
  ) %>%
  arrange(desc(n_new_species), desc(prop_new_species))

iucn_levels <- c("CR", "EN", "VU", "NT", "LC", "DD")
iucn_palette <- c(
  CR = "#9B0000",
  EN = "#E60000",
  VU = "#FF6B6B",
  NT = "#253CFF",
  LC = "#9F3BFF",
  DD = "#BDBDBD",
  NotNew = "#F2F2F2"
)

order_levels <- order_summary %>% filter(n_new_species > 0) %>% pull(order)
order_palette_defaults <- c(
  Passeriformes = "#19E51E",
  Charadriiformes = "#FFF000",
  Accipitriformes = "#FF1A1A",
  Anseriformes = "#00E8F2",
  Pelecaniformes = "#FF8ED1",
  Gruiformes = "#13D7E0",
  Strigiformes = "#FF8C25",
  Columbiformes = "#9CCAE1",
  Galliformes = "#B2E84D",
  Coraciiformes = "#FFBF52",
  Piciformes = "#39D6D0",
  Suliformes = "#8E6AE8",
  Procellariiformes = "#7CC7F2",
  Ciconiiformes = "#A8D8F8",
  Cuculiformes = "#71BFFF",
  Caprimulgiformes = "#8E7CD7",
  Gaviiformes = "#1EB2D8",
  Podicipediformes = "#7E57FF",
  Phoenicopteriformes = "#F4B5FF",
  Otidiformes = "#F4A100",
  Pterocliformes = "#FF5CB8",
  Trogoniformes = "#F04E98",
  Falconiformes = "#F06A6A"
)
missing_orders <- setdiff(order_levels, names(order_palette_defaults))
if (length(missing_orders) > 0) {
  extra_cols <- grDevices::hcl(
    h = seq(15, 375, length.out = length(missing_orders) + 1)[-1],
    c = 85,
    l = 68
  )
  names(extra_cols) <- missing_orders
  order_palette_defaults <- c(order_palette_defaults, extra_cols)
}
order_palette <- order_palette_defaults[order_levels]

read_icon_as_raster <- function(path) {
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (ext != "png") return(NULL)
  img <- png::readPNG(path)
  if (is.matrix(img)) return(as.raster(img))
  if (length(dim(img)) == 3) {
    ch <- dim(img)[3]
    if (ch == 2) {
      g <- img[, , 1]
      a <- img[, , 2]
      img <- array(0, dim = c(dim(img)[1], dim(img)[2], 4))
      img[, , 1] <- g
      img[, , 2] <- g
      img[, , 3] <- g
      img[, , 4] <- a
    } else if (ch == 1) {
      g <- img[, , 1]
      img <- array(0, dim = c(dim(img)[1], dim(img)[2], 3))
      img[, , 1] <- g
      img[, , 2] <- g
      img[, , 3] <- g
    }
  }
  as.raster(img)
}

tint_icon_path_to_colour <- function(path, colour_hex) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  img <- png::readPNG(path)
  target_rgb <- grDevices::col2rgb(colour_hex)[, 1] / 255

  if (is.matrix(img)) {
    alpha_mat <- ifelse(img < 0.999, 1, 0)
    out <- array(0, dim = c(nrow(img), ncol(img), 4))
    out[, , 1] <- target_rgb[1]
    out[, , 2] <- target_rgb[2]
    out[, , 3] <- target_rgb[3]
    out[, , 4] <- alpha_mat
    return(as.raster(out))
  }

  if (length(dim(img)) == 3) {
    ch <- dim(img)[3]
    alpha_mat <- switch(
      as.character(ch),
      "4" = img[, , 4],
      "2" = img[, , 2],
      "3" = ifelse(rowMeans(img[, , 1:3, drop = FALSE]) < 0.999, 1, 0),
      "1" = ifelse(img[, , 1] < 0.999, 1, 0),
      matrix(1, nrow = dim(img)[1], ncol = dim(img)[2])
    )
    out <- array(0, dim = c(dim(img)[1], dim(img)[2], 4))
    out[, , 1] <- target_rgb[1]
    out[, , 2] <- target_rgb[2]
    out[, , 3] <- target_rgb[3]
    out[, , 4] <- alpha_mat
    return(as.raster(out))
  }

  NULL
}

normalize_api_href <- function(href) {
  if (is.null(href) || length(href) == 0 || is.na(href) || href == "") return(NA_character_)
  if (grepl("^https?://", href)) return(href)
  paste0("https://api.phylopic.org", href)
}

api_get_json <- function(url) {
  tryCatch({
    httr2::request(url) %>%
      httr2::req_user_agent("Codex bird-phylogeny workflow") %>%
      httr2::req_perform() %>%
      httr2::resp_body_string() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
  }, error = function(e) NULL)
}

extract_first_non_null <- function(x, candidates) {
  for (nm in candidates) {
    val <- x[[nm]]
    if (!is.null(val)) return(val)
  }
  NULL
}

phylopic_build <- local({
  build_info <- api_get_json("https://api.phylopic.org")
  as.integer(build_info[["build"]] %||% 537L)
})

phylopic_icon_dir <- file.path(data_dir, "external", "phylopic_icons")
dir.create(phylopic_icon_dir, recursive = TRUE, showWarnings = FALSE)

legacy_icon_dirs <- c(
  "/Users/dingchenchen/Documents/New records/bird_new_records_R_output/assets/phylopic_icons",
  "/Users/dingchenchen/Documents/New project/bird_new_records_R_output/assets/phylopic_icons"
)

find_legacy_icon <- function(order_name) {
  for (icon_dir in legacy_icon_dirs[file.exists(legacy_icon_dirs)]) {
    candidate <- file.path(icon_dir, paste0(order_name, ".png"))
    if (file.exists(candidate)) return(candidate)
  }
  NA_character_
}

download_phylopic_order_icon <- function(order_name) {
  local_png <- file.path(phylopic_icon_dir, paste0(order_name, ".png"))
  if (file.exists(local_png)) {
    return(tibble::tibble(
      order = order_name,
      icon_path = local_png,
      icon_status = "existing_local",
      attribution = NA_character_,
      license = NA_character_,
      phylopic_node = NA_character_,
      phylopic_image = NA_character_
    ))
  }

  legacy_png <- find_legacy_icon(order_name)
  if (!is.na(legacy_png) && file.exists(legacy_png)) {
    file.copy(legacy_png, local_png, overwrite = TRUE)
    return(tibble::tibble(
      order = order_name,
      icon_path = local_png,
      icon_status = "copied_from_legacy",
      attribution = NA_character_,
      license = NA_character_,
      phylopic_node = NA_character_,
      phylopic_image = NA_character_
    ))
  }

  order_query <- tolower(order_name)
  node_search <- api_get_json(
    sprintf(
      "https://api.phylopic.org/nodes?build=%s&filter_name=%s&embed_items=true&page=0",
      phylopic_build,
      utils::URLencode(order_query, reserved = TRUE)
    )
  )
  items <- node_search[["_embedded"]][["items"]]
  if (is.null(items) || length(items) == 0) {
    return(tibble::tibble(
      order = order_name,
      icon_path = NA_character_,
      icon_status = "node_not_found",
      attribution = NA_character_,
      license = NA_character_,
      phylopic_node = NA_character_,
      phylopic_image = NA_character_
    ))
  }

  node_item <- items[[1]]
  node_uuid <- node_item[["uuid"]] %||% NA_character_
  primary_href <- normalize_api_href(node_item[["_links"]][["primaryImage"]][["href"]])

  image_meta <- NULL
  if (!is.na(primary_href)) {
    image_meta <- api_get_json(primary_href)
  }

  if (is.null(image_meta)) {
    if (!is.na(node_uuid)) {
      clade_meta <- api_get_json(
        sprintf(
          "https://api.phylopic.org/images?build=%s&embed_items=true&filter_clade=%s&page=0",
          phylopic_build, node_uuid
        )
      )
      clade_items <- clade_meta[["_embedded"]][["items"]]
      if (!is.null(clade_items) && length(clade_items) > 0) {
        first_img_href <- normalize_api_href(clade_items[[1]][["_links"]][["self"]][["href"]])
        if (!is.na(first_img_href)) image_meta <- api_get_json(first_img_href)
      }
    }
  }

  if (is.null(image_meta)) {
    return(tibble::tibble(
      order = order_name,
      icon_path = NA_character_,
      icon_status = "image_not_found",
      attribution = NA_character_,
      license = NA_character_,
      phylopic_node = node_uuid,
      phylopic_image = NA_character_
    ))
  }

  raster_files <- image_meta[["_links"]][["rasterFiles"]]
  raster_href <- if (!is.null(raster_files) && length(raster_files) > 0) raster_files[[1]][["href"]] else NA_character_
  if (is.na(raster_href) || raster_href == "") {
    return(tibble::tibble(
      order = order_name,
      icon_path = NA_character_,
      icon_status = "raster_not_found",
      attribution = image_meta[["attribution"]] %||% NA_character_,
      license = image_meta[["_links"]][["license"]][["href"]] %||% NA_character_,
      phylopic_node = node_uuid,
      phylopic_image = image_meta[["uuid"]] %||% NA_character_
    ))
  }

  download_ok <- tryCatch({
    utils::download.file(raster_href, destfile = local_png, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)

  tibble::tibble(
    order = order_name,
    icon_path = if (download_ok && file.exists(local_png)) local_png else NA_character_,
    icon_status = if (download_ok && file.exists(local_png)) "downloaded" else "download_failed",
    attribution = image_meta[["attribution"]] %||% NA_character_,
    license = image_meta[["_links"]][["license"]][["href"]] %||% NA_character_,
    phylopic_node = node_uuid,
    phylopic_image = image_meta[["uuid"]] %||% NA_character_
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

icon_manifest <- dplyr::bind_rows(lapply(order_levels, download_phylopic_order_icon)) %>%
  mutate(
    icon_path = ifelse(!is.na(icon_path) & file.exists(icon_path), icon_path, NA_character_)
  )

missing_icon_idx <- which(is.na(icon_manifest$icon_path) | !file.exists(icon_manifest$icon_path))
if (length(missing_icon_idx) > 0) {
  generic_icon <- c(
    file.path(phylopic_icon_dir, "Galliformes.png"),
    file.path(phylopic_icon_dir, "Passeriformes.png"),
    file.path(phylopic_icon_dir, "Charadriiformes.png")
  )
  generic_icon <- generic_icon[file.exists(generic_icon)][1]
  if (!is.na(generic_icon) && length(generic_icon) > 0) {
    for (i in missing_icon_idx) {
      fallback_path <- file.path(phylopic_icon_dir, paste0(icon_manifest$order[i], ".png"))
      file.copy(generic_icon, fallback_path, overwrite = TRUE)
      icon_manifest$icon_path[i] <- fallback_path
      icon_manifest$icon_status[i] <- paste0(icon_manifest$icon_status[i], "_fallback")
    }
  }
}

write.csv(icon_manifest, file.path(data_dir, "phylopic_icon_manifest.csv"), row.names = FALSE)

# -------------------------------
# Step 6. Aggregate metadata at tree-tip level
# 第 6 步：在系统树 tip 层聚合元数据
# -------------------------------
checklist_tip_map <- checklist_matched %>%
  filter(match_status != "Unresolved") %>%
  group_by(tree_label_final) %>%
  summarise(
    order = first(order),
    family = first(family),
    genus = first(genus),
    species_examples = paste(head(species, 4), collapse = "; "),
    pool_species_concepts = n(),
    bridge_in_pool = any(match_status == "Bridged match"),
    .groups = "drop"
  )

iucn_rank_lookup <- setNames(seq_along(iucn_levels), iucn_levels)
resolve_worst_iucn <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("DD")
  x <- ifelse(x %in% names(iucn_rank_lookup), x, "DD")
  x[which.min(iucn_rank_lookup[x])]
}

new_record_tip_map <- new_record_matched %>%
  filter(match_status != "Unresolved") %>%
  group_by(tree_label_final) %>%
  summarise(
    new_record_species_concepts = n(),
    new_record_species_list = paste(species, collapse = "; "),
    new_record_species_cn = paste(species_cn, collapse = "; "),
    new_record_iucn = resolve_worst_iucn(iucn),
    order_new = first(order),
    bridge_in_new_records = any(match_status == "Bridged match"),
    .groups = "drop"
  )

tip_metadata <- checklist_tip_map %>%
  left_join(new_record_tip_map, by = "tree_label_final") %>%
  mutate(
    order = coalesce(order_new, order),
    is_new_record = !is.na(new_record_species_concepts),
    new_record_species_concepts = replace_na(new_record_species_concepts, 0L),
    new_record_iucn = if_else(is_new_record, new_record_iucn, "NotNew")
  )

tree_china <- keep.tip(tree_full, tip_metadata$tree_label_final)
tree_china <- ladderize(tree_china, right = TRUE)

tip_metadata <- tip_metadata %>%
  mutate(tree_label_final = factor(tree_label_final, levels = tree_china$tip.label)) %>%
  arrange(tree_label_final) %>%
  mutate(
    tree_label_final = as.character(tree_label_final),
    tip_index = row_number()
  )

# -------------------------------
# Step 7. Derive geometry for the circular tree and summary side panel
# 第 7 步：计算环形系统树几何与右侧汇总面板
# -------------------------------
compute_sector_mid <- function(theta_values) {
  theta <- theta_values %% (2 * pi)
  if ((max(theta) - min(theta)) > pi) {
    theta[theta < pi] <- theta[theta < pi] + 2 * pi
  }
  mid <- mean(range(theta))
  if (mid > 2 * pi) mid <- mid - 2 * pi
  mid
}

angular_distance <- function(a, b) {
  d <- abs((a - b) %% (2 * pi))
  pmin(d, 2 * pi - d)
}

compute_theta_interval <- function(theta_values) {
  th <- theta_values %% (2 * pi)
  if ((max(th) - min(th)) > pi) th[th < pi] <- th[th < pi] + 2 * pi
  c(start = min(th), end = max(th))
}

relax_bubble_positions <- function(df, max_r, iterations = 260, padding = 0.015) {
  if (nrow(df) <= 1) {
    df$x <- df$bubble_radius * cos(df$bubble_theta)
    df$y <- df$bubble_radius * sin(df$bubble_theta)
    return(df)
  }

  theta0 <- df$bubble_theta
  theta <- theta0
  rad <- df$bubble_radius
  r <- df$bubble_size + padding

  for (iter in seq_len(iterations)) {
    moved <- FALSE
    for (i in seq_len(nrow(df) - 1)) {
      for (j in (i + 1):nrow(df)) {
        x <- rad * cos(theta)
        y <- rad * sin(theta)
        dx <- x[j] - x[i]
        dy <- y[j] - y[i]
        dist <- sqrt(dx * dx + dy * dy) + 1e-9
        min_dist <- r[i] + r[j]
        if (dist < min_dist) {
          overlap <- min_dist - dist
          rad_step <- overlap * 0.55
          angle_step <- pmin(0.016 + overlap * 0.02, 0.045)
          # Prefer radial separation so bubbles stay aligned with the intended
          # order sector. Use only small angular adjustments as a secondary
          # mechanism when bubbles remain crowded.
          # 优先沿半径方向分离，使比例球尽量贴近对应目扇区；角度只做小幅修正。
          if (rad[i] <= rad[j]) {
            rad[i] <- rad[i] - rad_step
            rad[j] <- rad[j] + rad_step
            theta[i] <- theta[i] - angle_step / 2
            theta[j] <- theta[j] + angle_step / 2
          } else {
            rad[i] <- rad[i] + rad_step
            rad[j] <- rad[j] - rad_step
            theta[i] <- theta[i] + angle_step / 2
            theta[j] <- theta[j] - angle_step / 2
          }
          moved <- TRUE
        }
      }
    }
    min_allowed <- max_r * 0.18
    max_allowed <- max_r * 0.64
    rad <- pmax(pmin(rad, max_allowed), min_allowed)
    theta <- pmax(pmin(theta, theta0 + 0.08), theta0 - 0.08)
    if (!moved) break
  }

  df$bubble_theta <- theta
  df$bubble_radius <- rad
  df$x <- rad * cos(theta)
  df$y <- rad * sin(theta)
  df
}

select_spaced_orders <- function(df, max_n = 7, min_sep = 0.42) {
  picked <- integer()
  for (i in seq_len(nrow(df))) {
    if (length(picked) == 0) {
      picked <- c(picked, i)
    } else {
      dd <- vapply(df$theta_mid[picked], function(x) angular_distance(df$theta_mid[i], x), numeric(1))
      if (all(dd >= min_sep)) picked <- c(picked, i)
    }
    if (length(picked) >= max_n) break
  }
  df[picked, , drop = FALSE]
}

order_label_table <- order_summary %>%
  filter(n_new_species > 0) %>%
  mutate(
    label_order = order,
    label_text = sprintf("%s\n%d/%d (%.1f%%)", order, n_new_species, n_china_species, prop_pct)
  )

draw_phylo_composite <- function(consistency_emphasis = FALSE, strict_correspondence = FALSE) {
  par(mar = c(0.2, 0.3, 0.2, 0.2), xpd = NA, bg = "white")
  edge_df <- tibble::tibble(
    edge_id = seq_len(nrow(tree_china$edge)),
    parent = tree_china$edge[, 1],
    child = tree_china$edge[, 2]
  )
  child_to_parent <- setNames(edge_df$parent, edge_df$child)
  child_to_edge <- setNames(edge_df$edge_id, edge_df$child)

  # Build the set of edges that belong to the ancestor paths of newly recorded
  # species. These edges are then coloured by order only when a path segment is
  # uniquely attributable to a single order; otherwise it remains grey.
  # 构建“新纪录物种祖先路径”的边集合。只有当某条边只对应单一目时才按目着色，
  # 若该边被多个目的新纪录路径共享，则保持灰色，以避免过度解释。
  edge_order_membership <- vector("list", nrow(edge_df))
  new_tip_rows <- tip_metadata %>% filter(is_new_record)
  for (i in seq_len(nrow(new_tip_rows))) {
    tip_id <- match(new_tip_rows$tree_label_final[i], tree_china$tip.label)
    if (is.na(tip_id)) next
    current_node <- tip_id
    while (!is.na(child_to_edge[as.character(current_node)])) {
      edge_id <- as.integer(child_to_edge[as.character(current_node)])
      edge_order_membership[[edge_id]] <- unique(c(edge_order_membership[[edge_id]], new_tip_rows$order[i]))
      current_node <- as.integer(child_to_parent[as.character(current_node)])
    }
  }

  edge_order_cols <- vapply(edge_order_membership, function(x) {
    if (length(x) == 1 && x %in% names(order_palette)) order_palette[[x]] else "#CFCFCF"
  }, character(1))
  edge_lwd <- vapply(edge_order_membership, function(x) {
    if (length(x) >= 1) 0.92 else 0.40
  }, numeric(1))

  base_layout <- plot.phylo(
    tree_china,
    type = "fan",
    use.edge.length = FALSE,
    show.tip.label = FALSE,
    no.margin = TRUE,
    edge.color = "#CFCFCF",
    edge.width = 0.40,
    cex = 0.08,
    plot = FALSE
  )

  plot.phylo(
    tree_china,
    type = "fan",
    use.edge.length = FALSE,
    show.tip.label = FALSE,
    no.margin = TRUE,
    edge.color = "#CFCFCF",
    edge.width = 0.40,
    cex = 0.08,
    x.lim = c(base_layout$x.lim[1] * 2.42, base_layout$x.lim[2] * 2.42),
    y.lim = c(base_layout$y.lim[1] * 1.48, base_layout$y.lim[2] * 1.48)
  )

  lp <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  n_tip <- lp$Ntip
  xx <- lp$xx[seq_len(n_tip)]
  yy <- lp$yy[seq_len(n_tip)]
  rr <- sqrt(xx^2 + yy^2)
  theta <- atan2(yy, xx)
  max_r <- max(rr)

  tip_plot_df <- tip_metadata %>%
    mutate(
      x = xx[match(tree_label_final, tree_china$tip.label)],
      y = yy[match(tree_label_final, tree_china$tip.label)],
      theta = theta[match(tree_label_final, tree_china$tip.label)]
    )

  # Overlay coloured paths for newly recorded species.
  # 叠加新纪录物种的彩色路径；非新纪录物种保持灰色背景树。
  coloured_edge_df <- edge_df %>%
    mutate(
      edge_colour = edge_order_cols,
      edge_width = edge_lwd
    ) %>%
    filter(edge_colour != "#CFCFCF")

  if (nrow(coloured_edge_df) > 0) {
    for (i in seq_len(nrow(coloured_edge_df))) {
      segments(
        x0 = lp$xx[coloured_edge_df$parent[i]],
        y0 = lp$yy[coloured_edge_df$parent[i]],
        x1 = lp$xx[coloured_edge_df$child[i]],
        y1 = lp$yy[coloured_edge_df$child[i]],
        col = coloured_edge_df$edge_colour[i],
        lwd = coloured_edge_df$edge_width[i],
        lend = "round"
      )
    }
  }

  tip_edge_df <- edge_df %>%
    filter(child <= n_tip) %>%
    mutate(tree_label_final = tree_china$tip.label[child]) %>%
    left_join(tip_plot_df, by = "tree_label_final")

  if (nrow(tip_edge_df) > 0) {
    for (i in seq_len(nrow(tip_edge_df))) {
      if (isTRUE(tip_edge_df$is_new_record[i])) {
        segments(
          x0 = lp$xx[tip_edge_df$parent[i]], y0 = lp$yy[tip_edge_df$parent[i]],
          x1 = lp$xx[tip_edge_df$child[i]], y1 = lp$yy[tip_edge_df$child[i]],
          col = order_palette[tip_edge_df$order[i]],
          lwd = 1.85,
          lend = "round"
        )
      }
    }
  }

  # Order sector arcs around the matched Chinese bird pool.
  # 在圆树外缘绘制按目划分的彩色粗弧。这里不再简单使用“某个目全部 tip 的最小角度
  # 到最大角度”，而是先识别圆周上的连续 run，再逐段绘制圆弧，避免一个目存在多个
  # 非连续片段时把其他目的区域错误覆盖进去。
  ordered_tip_df <- tip_plot_df %>%
    arrange(tip_index) %>%
    mutate(run_id = cumsum(order != lag(order, default = first(order))) + 1L)

  sector_run_df <- ordered_tip_df %>%
    group_by(run_id, order) %>%
    summarise(
      n_tip_run = n(),
      interval = list(compute_theta_interval(theta)),
      .groups = "drop"
    ) %>%
    mutate(
      theta_start = vapply(interval, function(x) x[["start"]], numeric(1)),
      theta_end = vapply(interval, function(x) x[["end"]], numeric(1)),
      theta_mid = (theta_start + theta_end) / 2,
      theta_mid = if_else(theta_mid > 2 * pi, theta_mid - 2 * pi, theta_mid)
    ) %>%
    select(-interval)

  sector_df <- sector_run_df %>%
    group_by(order) %>%
    slice_max(order_by = n_tip_run, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    left_join(order_summary, by = "order")

  for (i in seq_len(nrow(sector_run_df))) {
    th <- seq(sector_run_df$theta_start[i], sector_run_df$theta_end[i], length.out = 250)
    th <- ifelse(th > 2 * pi, th - 2 * pi, th)
    sector_lwd <- if (strict_correspondence) 9.6 else if (consistency_emphasis) 8.8 else 7.6
    lines(
      x = (max_r + 0.032) * cos(th),
      y = (max_r + 0.032) * sin(th),
      col = alpha(order_palette[sector_run_df$order[i]], 0.95),
      lwd = sector_lwd,
      lend = "round"
    )
    if (consistency_emphasis) {
      lines(
        x = (max_r + 0.050) * cos(th),
        y = (max_r + 0.050) * sin(th),
        col = alpha(order_palette[sector_run_df$order[i]], 0.55),
        lwd = 2.4,
        lend = "round"
      )
    }
  }

  # Outer ring: highlight newly recorded tips by order.
  # 外环：用更宽、更靠近 tip 的彩色小方块标识不同目的新纪录物种；
  # 非新纪录物种仍以浅灰表示。
  points(
    x = (max_r + 0.066) * cos(tip_plot_df$theta),
    y = (max_r + 0.066) * sin(tip_plot_df$theta),
    pch = 15,
    cex = if (strict_correspondence) 1.25 else if (consistency_emphasis) 1.22 else 1.18,
    col = ifelse(tip_plot_df$is_new_record, order_palette[tip_plot_df$order], "#E8E8E8")
  )

  # Internal percentage bubbles.
  # 在接近根部的位置放置按目比例圆球。这里对所有“有新纪录物种的目”
  # 都进行标识，并通过多层半径交替减轻圆球之间的重叠。
  bubble_orders <- sector_df %>%
    filter(n_new_species > 0) %>%
    arrange(theta_mid) %>%
    mutate(
      bubble_gap = c(Inf, diff(theta_mid)),
      bubble_layer = dplyr::case_when(
        bubble_gap < 0.12 ~ rep(c(1, 4, 2, 5, 3, 6), length.out = n()),
        bubble_gap < 0.20 ~ rep(c(1, 3, 5, 2, 4, 6), length.out = n()),
        TRUE ~ rep(c(1, 2, 3, 4, 5, 6), length.out = n())
      ),
      bubble_theta = theta_mid + rep(c(0, 0.05, -0.05, 0.09, -0.09, 0.12), length.out = n()),
      bubble_radius = max_r * c(0.22, 0.29, 0.36, 0.43, 0.50, 0.57)[bubble_layer],
      bubble_size = dplyr::case_when(
        prop_pct >= 60 ~ 0.066,
        prop_pct >= 40 ~ 0.060,
        prop_pct >= 20 ~ 0.054,
        TRUE ~ 0.048
      ),
      bubble_cex = dplyr::case_when(
        prop_pct >= 60 ~ 0.58,
        prop_pct >= 40 ~ 0.52,
        prop_pct >= 20 ~ 0.48,
        TRUE ~ 0.43
      )
    ) %>%
    relax_bubble_positions(max_r = max_r)

  for (i in seq_len(nrow(bubble_orders))) {
    xb <- bubble_orders$x[i]
    yb <- bubble_orders$y[i]
    symbols(
      xb, yb,
      circles = bubble_orders$bubble_size[i],
      inches = FALSE,
      add = TRUE,
      bg = alpha(order_palette[bubble_orders$order[i]], 0.94),
      fg = NA
    )
    text(xb, yb, labels = sprintf("%.1f%%", bubble_orders$prop_pct[i]), cex = bubble_orders$bubble_cex[i], col = "black")
  }

  # Outer labels and silhouettes for every order with new-record species.
  # 对每个含新纪录物种的目都在圆外添加“引导线 + 剪影 + 目名”，尽量复刻参考图
  # 的样式与标识方式。
  label_orders <- sector_df %>%
    filter(n_new_species > 0) %>%
    arrange(theta_mid) %>%
    left_join(icon_manifest, by = "order") %>%
    mutate(
      side = dplyr::case_when(
        order == "Passeriformes" ~ "left",
        TRUE ~ ifelse(cos(theta_mid) >= 0, "right", "left")
      ),
      guide_radius = max_r + 0.076,
      text_cex = dplyr::case_when(
        nchar(order) >= 18 ~ 0.96,
        nchar(order) >= 14 ~ 1.03,
        TRUE ~ 1.10
      )
    )

  label_right <- label_orders %>%
    filter(side == "right") %>%
    arrange(desc(sin(theta_mid))) %>%
    mutate(
      y_label = seq(max_r * 0.96, -max_r * 0.96, length.out = n()),
      x_anchor = max_r + 0.38,
      x_icon = max_r + 0.53,
      x_text = max_r + 0.64
    )

  label_left <- label_orders %>%
    filter(side == "left") %>%
    arrange(desc(sin(theta_mid))) %>%
    mutate(
      y_label = seq(max_r * 0.96, -max_r * 0.96, length.out = n()),
      x_anchor = -(max_r + 0.38),
      x_icon = -(max_r + 0.62),
      x_text = x_icon - 0.48
    )

  label_orders <- bind_rows(label_right, label_left) %>%
    arrange(theta_mid)

  fallback_icon <- icon_manifest %>%
    filter(!is.na(icon_path), file.exists(icon_path)) %>%
    slice_head(n = 1) %>%
    pull(icon_path)
  fallback_icon <- if (length(fallback_icon) == 0) NA_character_ else fallback_icon

  icon_rasters <- list()
  if (nrow(label_orders) > 0) {
    label_orders <- label_orders %>%
      mutate(
        icon_path = ifelse(is.na(icon_path) | !file.exists(icon_path), fallback_icon, icon_path)
      )
    valid_icons <- unique(na.omit(label_orders$icon_path[label_orders$icon_path != "" & file.exists(label_orders$icon_path)]))
    if (length(valid_icons) > 0) {
      icon_rasters <- lapply(valid_icons, read_icon_as_raster)
      names(icon_rasters) <- valid_icons
    }
  }

  for (i in seq_len(nrow(label_orders))) {
    ang <- label_orders$theta_mid[i]
    x0 <- label_orders$guide_radius[i] * cos(ang)
    y0 <- label_orders$guide_radius[i] * sin(ang)
    xi <- label_orders$x_icon[i]
    yi <- label_orders$y_label[i]
    xt <- label_orders$x_text[i]
    yt <- label_orders$y_label[i]
    adj_lr <- 0
    xh <- label_orders$x_anchor[i]

    line_col <- "#111111"
    text_col <- "#111111"
    segments(x0, y0, xh, yi, col = line_col, lwd = if (consistency_emphasis) 1.15 else 0.75)
    segments(
      xh, yi,
      xi - ifelse(label_orders$side[i] == "right", 0.04, -0.04), yi,
      col = line_col,
      lwd = if (consistency_emphasis) 1.15 else 0.75
    )
    text(
      xt, yt,
      labels = label_orders$order[i],
      cex = label_orders$text_cex[i],
      adj = c(adj_lr, 0.5),
      col = text_col,
      font = 1
    )

    icon_path <- label_orders$icon_path[i]
    if (!is.na(icon_path) && icon_path %in% names(icon_rasters) && !is.null(icon_rasters[[icon_path]])) {
      half_w <- 0.080
      half_h <- 0.054
      icon_img <- if (strict_correspondence || consistency_emphasis) {
        tint_icon_path_to_colour(icon_path, order_palette[label_orders$order[i]])
      } else {
        icon_rasters[[icon_path]]
      }
      rasterImage(
        icon_img,
        xleft = xi - half_w,
        ybottom = yi - half_h,
        xright = xi + half_w,
        ytop = yi + half_h
      )
    }
  }
}

# -------------------------------
# Step 8. Quality control plots before final export
# 第 8 步：正式出图前的质量控制与诊断图
# -------------------------------
audit_plot_df <- matching_audit %>%
  mutate(match_status = factor(match_status, levels = c("Exact match", "Bridged match", "Unresolved")))

qa_match_plot <- ggplot(audit_plot_df, aes(x = dataset, y = n_species, fill = match_status)) +
  geom_col(width = 0.68, colour = "white", linewidth = 0.4) +
  geom_text(aes(label = n_species), position = position_stack(vjust = 0.5), size = 3.2, colour = "white", fontface = "bold") +
  scale_fill_manual(values = c("Exact match" = "#4C78A8", "Bridged match" = "#F58518", "Unresolved" = "#BDBDBD")) +
  labs(x = NULL, y = "Number of species", fill = NULL) +
  theme_clean(11.2) +
  theme(legend.position = "top")

qa_order_plot <- order_summary %>%
  filter(n_new_species > 0) %>%
  ggplot(aes(x = reorder(order, prop_new_species), y = prop_new_species, fill = order)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.35) +
  geom_text(aes(label = sprintf("%.1f%%", prop_pct)), hjust = -0.05, size = 3.0) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = order_palette) +
  labs(x = NULL, y = "Order-level proportion of new-record species") +
  theme_clean(10.8)

qa_figure <- qa_match_plot + qa_order_plot + plot_layout(widths = c(1.02, 1.35))
save_gg_bundle(qa_figure, "fig_s1_phylogeny_matching_diagnostics", width = 12.8, height = 6.2)

# -------------------------------
# Step 9. Export the main circular phylogeny figure
# 第 9 步：导出主系统发育图
# -------------------------------
save_base_bundle(
  function() draw_phylo_composite(consistency_emphasis = FALSE),
  figure_stub,
  width = 15.2,
  height = 11.0
)
save_base_bundle(
  function() draw_phylo_composite(consistency_emphasis = TRUE),
  figure_stub_consistency,
  width = 15.2,
  height = 11.0
)
save_base_bundle(
  function() draw_phylo_composite(consistency_emphasis = TRUE, strict_correspondence = TRUE),
  figure_stub_strict,
  width = 15.2,
  height = 11.0
)

# -------------------------------
# Step 10. Export data products and narrative outputs
# 第 10 步：导出整理数据与说明文稿
# -------------------------------
before_after_summary <- tibble::tribble(
  ~metric, ~value,
  "Checklist bird rows before species-rank filtering", nrow(checklist_all_birds),
  "Checklist bird species after strict species-rank filtering", nrow(checklist_species_pool),
  "Orders in strict Chinese bird pool", n_distinct(checklist_species_pool$order),
  "Corrected new-record events", nrow(corrected_events),
  "Corrected new-record species", nrow(new_record_species),
  "Exact-matched Chinese checklist species", sum(checklist_matched$match_status == "Exact match"),
  "Bridged Chinese checklist species", sum(checklist_matched$match_status == "Bridged match"),
  "Unresolved Chinese checklist species", sum(checklist_matched$match_status == "Unresolved"),
  "Exact-matched new-record species", sum(new_record_matched$match_status == "Exact match"),
  "Bridged new-record species", sum(new_record_matched$match_status == "Bridged match"),
  "Unresolved new-record species", sum(new_record_matched$match_status == "Unresolved"),
  "Matched tree tips in pruned Chinese bird phylogeny", Ntip(tree_china),
  "Orders containing at least one corrected new-record species", sum(order_summary$n_new_species > 0)
)

order_colour_audit <- order_summary %>%
  filter(n_new_species > 0) %>%
  mutate(
    colour_hex = unname(order_palette[order]),
    correspondence_note = "Order label text, labelled proportion bubble, coloured new-record branches, coloured sector arc, and coloured new-record outer-ring blocks should all use the same order colour."
  )

write.csv(checklist_species_pool, file.path(data_dir, "china_bird_species_pool_strict.csv"), row.names = FALSE)
write.csv(checklist_matched, file.path(data_dir, "china_bird_species_pool_tree_matching.csv"), row.names = FALSE)
write.csv(new_record_species, file.path(data_dir, "corrected_new_record_species_unique.csv"), row.names = FALSE)
write.csv(new_record_matched, file.path(data_dir, "corrected_new_record_species_tree_matching.csv"), row.names = FALSE)
write.csv(taxonomy_bridge, file.path(data_dir, "taxonomy_bridge_table.csv"), row.names = FALSE)
write.csv(matching_audit, file.path(data_dir, "phylogeny_matching_audit.csv"), row.names = FALSE)
write.csv(many_to_one_audit, file.path(data_dir, "phylogeny_many_to_one_mapping_audit.csv"), row.names = FALSE)
write.csv(order_summary, file.path(data_dir, "order_level_new_record_proportions.csv"), row.names = FALSE)
write.csv(order_colour_audit, file.path(data_dir, "order_colour_correspondence_audit.csv"), row.names = FALSE)
write.csv(tip_metadata, file.path(data_dir, "phylogeny_tip_metadata.csv"), row.names = FALSE)
write.csv(before_after_summary, file.path(data_dir, "workflow_summary_metrics.csv"), row.names = FALSE)
write.csv(icon_manifest, file.path(data_dir, "phylopic_icon_manifest.csv"), row.names = FALSE)

writexl::write_xlsx(
  list(
    workflow_summary = before_after_summary,
    checklist_species_pool = checklist_species_pool,
    checklist_matching = checklist_matched,
    corrected_new_record_species = new_record_species,
    new_record_matching = new_record_matched,
    taxonomy_bridge = taxonomy_bridge,
    matching_audit = matching_audit,
    many_to_one_audit = many_to_one_audit,
    order_summary = order_summary,
    order_colour_correspondence_audit = order_colour_audit,
    tip_metadata = tip_metadata,
    phylopic_icon_manifest = icon_manifest
  ),
  path = file.path(results_dir, "bird_phylogeny_new_records_mctavish_bundle.xlsx")
)

caption_en <- paste(
  "Figure. Circular phylogeny of China's bird species pool showing newly recorded species on the McTavish et al. complete and dynamic bird tree.",
  "The background tree represents species-rank birds retained from the 2025 Catalogue of Life China checklist after filtering out subspecies and other infraspecific entries.",
  "Only the branches associated with corrected newly recorded species are overlaid in order-specific colours, whereas all other branches remain grey.",
  "A widened outer ring placed close to the tips marks newly recorded species by order.",
  "Percentage bubbles placed close to the rootward part of each labelled order report the proportion of corrected newly recorded species within that order's Chinese species pool.",
  "External labels and silhouettes identify each order that contains at least one corrected newly recorded species in a layout designed to mimic the supplied reference figure.",
  "Species identities follow the corrected canonical dataset that already accounts for synonymy and removes later duplicate species-province publications."
)

caption_en_strict <- paste(
  "Figure. Strict correspondence version of the circular phylogeny of China's bird species pool showing newly recorded species on the McTavish et al. complete and dynamic bird tree.",
  "Grey branches represent the background Chinese bird species pool, whereas coloured branches indicate the lineage paths of corrected newly recorded species.",
  "The coloured sector ring is drawn by contiguous order-specific segments, so each coloured arc corresponds only to the matching order rather than spanning intervening tips from other orders.",
  "The widened outer ring marks newly recorded species by order, percentage bubbles report the proportion of newly recorded species within each order's Chinese species pool, and coloured silhouettes match the corresponding order colours.",
  "Label text and guide lines are kept black to preserve structural readability."
)

caption_zh <- paste(
  "图. 基于 McTavish 等构建的 complete and dynamic bird tree 绘制的中国鸟类物种库环形系统发育图，突出显示校正后的新纪录物种。",
  "灰色背景树表示从《中国生物物种名录（2025）》中筛选得到的中国鸟类种级物种库，并已去除亚种及其他种下单元。",
  "仅对校正后新纪录物种所对应的分支路径按目着色，其余分支保持灰色；加宽且贴近 tip 的单层外环进一步标识不同目的新纪录物种。",
  "靠近根部的比例气泡对每个含新纪录物种的目进行标注，并显示该目新纪录物种数占中国该目物种总数的比例。",
  "圆外侧的目标签与代表性剪影按照参考图的标识方式排布。",
  "物种身份采用已经过同物异名归并与重复发表剔除的校正底表。"
)

caption_zh_strict <- paste(
  "图. 中国鸟类新纪录环形系统发育树的严格对应版本。",
  "灰色分支表示中国鸟类背景物种库，彩色分支表示校正后新纪录物种对应的系统发育路径。",
  "按目着色的粗圆环不再使用整目最小角度到最大角度的整段包络，而是按圆周上的连续片段逐段绘制，因此每一段色环只对应同一目的真实连续区段，不会错误覆盖夹在中间的其他目。",
  "加宽的外环表示不同目的新纪录物种，比例圆球表示该目新纪录物种占中国该目物种总数的比例，彩色剪影与对应目颜色保持一致。",
  "为保证结构清晰，目标签文字与引导线统一保留为黑色。"
)

summary_lines_en <- c(
  paste0("The strict Chinese bird species pool contained ", nrow(checklist_species_pool), " species across ", n_distinct(checklist_species_pool$order), " orders."),
  paste0("Direct matching placed ", sum(checklist_matched$match_status == "Exact match"), " checklist species and ", sum(new_record_matched$match_status == "Exact match"), " corrected newly recorded species onto the McTavish tree."),
  paste0("The explicit taxonomy bridge recovered an additional ", sum(checklist_matched$match_status == "Bridged match"), " checklist species and ", sum(new_record_matched$match_status == "Bridged match"), " newly recorded species."),
  paste0("After direct matching and transparent bridging, the final pruned Chinese bird phylogeny contained ", Ntip(tree_china), " tree tips."),
  paste0("Corrected newly recorded species were documented in ", sum(order_summary$n_new_species > 0), " orders; the richest order was ", order_summary$order[1], " with ", order_summary$n_new_species[1], " newly recorded species, representing ", sprintf("%.1f%%", order_summary$prop_pct[1]), " of the Chinese species pool in that order."),
  paste0("Unresolved species were retained in the audit tables for transparent review, rather than being silently dropped or force-matched.")
)

summary_lines_zh <- c(
  paste0("严格筛选后的中国鸟类种级物种库共包含 ", nrow(checklist_species_pool), " 个物种、", n_distinct(checklist_species_pool$order), " 个目。"),
  paste0("直接匹配可将 ", sum(checklist_matched$match_status == "Exact match"), " 个中国名录物种和 ", sum(new_record_matched$match_status == "Exact match"), " 个校正后新纪录物种挂接到 McTavish 系统树上。"),
  paste0("通过透明的 taxonomy bridge，又额外恢复了 ", sum(checklist_matched$match_status == "Bridged match"), " 个中国名录物种和 ", sum(new_record_matched$match_status == "Bridged match"), " 个新纪录物种的树上位置。"),
  paste0("经过直接匹配与名称桥接后，最终用于绘图的中国鸟类系统树子树包含 ", Ntip(tree_china), " 个 tip。"),
  paste0("校正后的新纪录物种分布于 ", sum(order_summary$n_new_species > 0), " 个目中，其中新纪录物种数最多的目是 ", order_summary$order[1], "，共 ", order_summary$n_new_species[1], " 个物种，占中国该目物种库的 ", sprintf("%.1f%%", order_summary$prop_pct[1]), "。"),
  "所有未解决名称均保留在审计表中供人工复核，而不是被静默删除或不透明地强制匹配。"
)

writeLines(
  c(
    "# Figure Captions",
    "",
    "## Main Version (English)",
    "",
    caption_en,
    "",
    "## Main Version (中文)",
    "",
    caption_zh,
    "",
    "## Strict Correspondence Version (English)",
    "",
    caption_en_strict,
    "",
    "## Strict Correspondence Version (中文)",
    "",
    caption_zh_strict
  ),
  file.path(results_dir, "figure_caption_bilingual.md")
)
writeLines(
  c(
    "# Task Summary / 结果摘要",
    "",
    "## English",
    "",
    summary_lines_en,
    "",
    "## 中文",
    "",
    summary_lines_zh
  ),
  file.path(results_dir, "task_summary_bilingual.md")
)

message("Bird McTavish phylogeny workflow completed successfully.")
