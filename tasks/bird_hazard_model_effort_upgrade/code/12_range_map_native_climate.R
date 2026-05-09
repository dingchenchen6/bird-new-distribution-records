#!/usr/bin/env Rscript
# 12_range_map_native_climate.R
# 用物种range map替代省份集合计算temp_native_anom
# temp_native_anom(物种, 年份) = 物种历史分布区范围(range map)内
#   裁切到中国范围后的年均温变化均值
# Computes range-map-based native climate anomaly,
# replacing province-aggregation approach

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(here)
})

sf_use_s2(FALSE)

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

# ── 1. 读取数据 ──────────────────────────────────────────────────────────
cat("=== Range map native climate anomaly ===\n")

risk_data <- fread(file.path(TASK_ROOT, "data",
                              "hazard_risk_upgraded_complete_case.csv"))
cat("风险集:", nrow(risk_data), "行,", uniqueN(risk_data$species), "种\n")

# 中国边界（用于裁切）
china_shp <- st_read("/Users/dingchenchen/Documents/SDMs/GS(2023)2767审图号/省面.shp",
                      quiet = TRUE)
china_boundary <- st_union(china_shp)

# 物种分布shapefile
sp_dist <- st_read("/Users/dingchenchen/Documents/SDMs/物种分布/物种分布.shp",
                    quiet = TRUE)
bird_dist_sf <- sp_dist[sp_dist$Class == "AVES", ]
cat("鸟类分布点:", nrow(bird_dist_sf), "条,",
    length(unique(bird_dist_sf$Scientific)), "种\n")

# ── 2. 省级气候面板 ────────────────────────────────────────────────
cat("\n读取省级气候面板\n")

prov_clim <- fread(file.path(TASK_ROOT, "data",
                              "climate_metrics_province_year.csv"))
cat("省级气候面板:", nrow(prov_clim), "行\n")
cat("列名:", paste(names(prov_clim), collapse = ", "), "\n")

# ── 3. 物种range map → 栅格化 ──────────────────────────────────────────
cat("\n=== 构建物种range map掩膜 ===\n")

# 获取风险集中的物种列表
risk_species <- unique(risk_data$species)

# 标准化物种名匹配
bird_sp_match <- tolower(gsub(" ", "_", bird_dist_sf$Scientific))
risk_sp_match <- tolower(gsub(" ", "_", risk_data$species))

# ── 4. 计算range map内年均温变化 ──────────────────────────────────────
cat("\n=== 计算range map内温度变化 ===\n")

# 使用省级面板的年度温度异常来近似
# 更精确的方法：用CRU或WorldClim年度栅格
# 这里用省级加权平均（以range map内各省份面积加权）

# 简化方法：用分布点所在的省份集合（但用点密度加权）
# 然后用省级年度温度异常加权平均

# ── 5. 改进方案：range map省份加权 ──────────────────────────────────────
cat("\n=== 改进方案：range map省份加权 ===\n")

# 改进逻辑：
# 1. 用物种分布点数据（而非新纪录省份集合）确定range覆盖的省份
# 2. 用分布点密度加权（替代等权平均）
# 3. 裁切到中国范围

# 标准化省份名映射
prov_name_map <- fread(file.path(TASK_ROOT, "data",
                                  "climate_metrics_province_year.csv"))
prov_names_clim <- unique(prov_name_map$province)

# 物种分布点省份名匹配
cat("物种分布点省份名样本:", paste(head(unique(bird_dist_sf$Province), 10),
                              collapse = ", "), "\n")
cat("气候面板省份名样本:", paste(head(prov_names_clim, 10),
                              collapse = ", "), "\n")

# 构建省份名映射
prov_mapping <- data.table(
  chprovince = unique(bird_dist_sf$Chprovince),
  province_en = NA_character_
)

# 手动映射关键省份
prov_map_manual <- c(
  "北京市" = "Beijing", "天津市" = "Tianjin", "河北省" = "Hebei",
  "山西省" = "Shanxi", "内蒙古自治区" = "Inner Mongolia",
  "辽宁省" = "Liaoning", "吉林省" = "Jilin", "黑龙江省" = "Heilongjiang",
  "上海市" = "Shanghai", "江苏省" = "Jiangsu", "浙江省" = "Zhejiang",
  "安徽省" = "Anhui", "福建省" = "Fujian", "江西省" = "Jiangxi",
  "山东省" = "Shandong", "河南省" = "Henan", "湖北省" = "Hubei",
  "湖南省" = "Hunan", "广东省" = "Guangdong", "广西壮族自治区" = "Guangxi",
  "海南省" = "Hainan", "重庆市" = "Chongqing", "四川省" = "Sichuan",
  "贵州省" = "Guizhou", "云南省" = "Yunnan", "西藏自治区" = "Tibet",
  "陕西省" = "Shaanxi", "甘肃省" = "Gansu", "青海省" = "Qinghai",
  "宁夏回族自治区" = "Ningxia", "新疆维吾尔自治区" = "Xinjiang",
  "台湾省" = "Taiwan", "香港特别行政区" = "Hong Kong",
  "澳门特别行政区" = "Macau"
)

# 检查是否有省份名无法映射
unmapped <- setdiff(unique(bird_dist_sf$Chprovince), names(prov_map_manual))
if (length(unmapped) > 0) {
  cat("未映射省份:", paste(head(unmapped, 20), collapse = ", "), "\n")
}

# 构建映射表
prov_map_dt <- data.table(
  chprovince = names(prov_map_manual),
  province_en = unname(prov_map_manual)
)

# ── 6. 对所有风险集物种计算改进的temp_native_anom ─────────────────────
cat("\n=== 计算range map加权的temp_native_anom ===\n")

# 准备省级年度温度异常面板
prov_temp_panel <- prov_clim[, .(province, year, temp_anom)]
cat("省级温度面板:", nrow(prov_temp_panel), "行\n")

# 预先提取bird_dist_sf的省份信息为data.table
bird_prov_dt <- as.data.table(st_drop_geometry(
  bird_dist_sf[, c("Scientific", "Chprovince")]))
bird_prov_dt[, sp_match := tolower(gsub(" ", "_", Scientific))]

# 对每个物种，从分布点确定range覆盖省份及权重
species_range_anom <- list()
n_matched <- 0
n_unmatched <- 0

for (sp in risk_species) {
  sp_match <- tolower(gsub(" ", "_", sp))

  # 在分布数据中查找（用data.table，避免sf索引问题）
  sp_prov <- bird_prov_dt[sp_match == sp_match, .(Chprovince)]

  if (nrow(sp_prov) >= 3) {
    # 获取中国范围内的分布点省份
    chprov_valid <- sp_prov$Chprovince
    chprov_valid <- chprov_valid[!is.na(chprov_valid) &
                                   chprov_valid %in% names(prov_map_manual)]

    if (length(chprov_valid) > 0) {
      # 点密度加权
      prov_w <- as.data.table(table(chprov_valid))
      setnames(prov_w, c("Chprovince", "N"))
      prov_w[, province_en := prov_map_manual[as.character(Chprovince)]]

      # 匹配到气候面板
      prov_w_matched <- prov_w[!is.na(province_en) &
                                 province_en %in% prov_names_clim]

      if (nrow(prov_w_matched) > 0) {
        # 加权平均温度异常
        sp_clim <- merge(
          prov_temp_panel[province %in% prov_w_matched$province_en],
          prov_w_matched[, .(province = province_en, w = N)],
          by = "province", all.x = TRUE)
        sp_clim[is.na(w), w := 1]

        sp_anom <- sp_clim[, .(
          temp_native_anom_range = weighted.mean(
            temp_anom, w = w, na.rm = TRUE)
        ), by = year]
        sp_anom[, species := sp]

        species_range_anom[[sp]] <- sp_anom
        n_matched <- n_matched + 1
        next
      }
    }
  }

  # 无分布点数据 → 回退到原始方法
  sp_orig <- risk_data[species == sp, .(
    temp_native_anom_range = mean(temp_native_anom, na.rm = TRUE)
  ), by = year]
  sp_orig[, species := sp]
  species_range_anom[[sp]] <- sp_orig
  n_unmatched <- n_unmatched + 1
}

cat(sprintf("匹配到range map: %d种, 未匹配: %d种\n", n_matched, n_unmatched))

# ── 7. 合并并计算新的temp_grad ──────────────────────────────────────
cat("\n=== 合并并计算改进的temp_grad ===\n")

dt_range_anom <- rbindlist(species_range_anom, fill = TRUE)

# 合并回风险集
risk_data_new <- merge(risk_data,
                        dt_range_anom[, .(species, year,
                                          temp_native_anom_range)],
                        by = c("species", "year"), all.x = TRUE)

# 重新计算year_c
risk_data_new[, year_c := year - 2013]

# 新的temp_grad
risk_data_new[!is.na(temp_native_anom_range),
              temp_grad_range := temp_anom - temp_native_anom_range]

# 标准化
risk_data_new[, temp_grad_range_z := scale(temp_grad_range)[, 1]]

# 对比新旧
cat("\n新旧temp_grad对比:\n")
cat(sprintf("  原始temp_grad_z: mean=%.3f, sd=%.3f, range=[%.3f, %.3f]\n",
            mean(risk_data_new$temp_grad_z, na.rm = TRUE),
            sd(risk_data_new$temp_grad_z, na.rm = TRUE),
            min(risk_data_new$temp_grad_z, na.rm = TRUE),
            max(risk_data_new$temp_grad_z, na.rm = TRUE)))
cat(sprintf("  Range temp_grad_range_z: mean=%.3f, sd=%.3f, range=[%.3f, %.3f]\n",
            mean(risk_data_new$temp_grad_range_z, na.rm = TRUE),
            sd(risk_data_new$temp_grad_range_z, na.rm = TRUE),
            min(risk_data_new$temp_grad_range_z, na.rm = TRUE),
            max(risk_data_new$temp_grad_range_z, na.rm = TRUE)))

# 相关性
cor_val <- cor(risk_data_new$temp_grad_z,
               risk_data_new$temp_grad_range_z, use = "complete.obs")
cat(sprintf("  相关系数: %.4f\n", cor_val))

# 保存
fwrite(risk_data_new,
       file.path(TASK_ROOT, "data",
                 "hazard_risk_upgraded_range_map_anom.csv"))
fwrite(dt_range_anom,
       file.path(TASK_ROOT, "data",
                 "species_range_native_anom.csv"))

# ── 8. 用改进指标重新拟合hazard模型 ──────────────────────────────────
cat("\n=== 用改进的temp_grad重新拟合模型 ===\n")

library(glmmTMB)

risk_data_new[, species  := factor(species)]
risk_data_new[, province := factor(province)]

# Spec B (visits) + range map temp_grad
fml_range <- as.formula(
  "event ~ year_c + temp_grad_range_z * log_effort_visits_z +
   (1|species) + (1|province)")

cat("  拟合range map模型 ... ")
fit_range <- tryCatch(
  glmmTMB(fml_range, data = risk_data_new[!is.na(temp_grad_range_z)],
          family = binomial(link = "cloglog")),
  error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(fit_range)) {
  cat("OK\n")
  cf <- summary(fit_range)$coefficients$cond
  cat("关键系数:\n")
  for (rn in rownames(cf)) {
    if (grepl("temp_grad|effort|Intercept", rn)) {
      cat(sprintf("  %-40s: est=%.4f, p=%.4g\n",
                  rn, cf[rn, "Estimate"], cf[rn, "Pr(>|z|)"]))
    }
  }

  # 对比原始模型
  fml_orig <- as.formula(
    "event ~ year_c + temp_grad_z * log_effort_visits_z +
     (1|species) + (1|province)")

  cat("  拟合原始模型 ... ")
  fit_orig <- tryCatch(
    glmmTMB(fml_orig, data = risk_data_new, family = binomial(link = "cloglog")),
    error = function(e) { cat("FAILED\n"); NULL }
  )

  if (!is.null(fit_orig)) {
    cat("OK\n")
    cat(sprintf("  原始模型 AIC: %.1f\n", AIC(fit_orig)))
    cat(sprintf("  Range模型 AIC: %.1f\n", AIC(fit_range)))

    # 交互项对比
    cf_orig <- summary(fit_orig)$coefficients$cond
    interact_orig <- cf_orig[grep(":", rownames(cf_orig)), ]
    interact_range <- cf[grep(":", rownames(cf)), ]

    cat("\n交互项对比:\n")
    cat(sprintf("  原始 temp_grad_z:log_effort_visits_z: HR=%.3f, p=%.4g\n",
                exp(interact_orig["Estimate"]), interact_orig["Pr(>|z|)"]))
    cat(sprintf("  Range temp_grad_range_z:log_effort_visits_z: HR=%.3f, p=%.4g\n",
                exp(interact_range["Estimate"]), interact_range["Pr(>|z|)"]))
  }
}

cat("\n=== 12_range_map_native_climate.R 完成 ===\n")
