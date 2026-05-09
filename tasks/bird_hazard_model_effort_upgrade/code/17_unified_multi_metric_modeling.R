#!/usr/bin/env Rscript
# ============================================================
# Scientific question / 科学问题:
# How do different climate metrics × effort metrics × spatial
# resolutions jointly explain new bird distribution records?
# 不同气候指标 × 调查努力指标 × 空间分辨率如何共同解释
# 鸟类新分布记录？
#
# Objective / 分析目标:
# Systematically fit hazard models across all combinations of
# climate metrics (6), effort metrics (4), and spatial scales
# (province, 100km grid), with unified model comparison (AIC),
# variance decomposition, and publication-quality visualization.
# 系统拟合所有 气候×努力×尺度 组合的 hazard model，
# 统一 AIC 比较和方差分解。
#
# Workflow / 分析流程:
# 1. Build unified complete-case risk sets for province & grid
# 2. Fit all climate×effort×scale interaction models
# 3. Model selection via AIC + coefficient comparison
# 4. Variance decomposition per model specification
# 5. Generate comparison tables and diagnostic plots
#
# Expected output / 预期输出:
# - Unified model comparison tables (AIC, HR, R²)
# - Variance decomposition per climate×effort combination
# - Forest plots, heatmaps, ridge plots for each dimension
# - Standardized result CSVs for downstream visualization
#
# Key assumptions / 关键假设:
# - All models use same complete-case sample per scale
# - cloglog link with (1|species)+(1|province) random effects
# - Grid models use (1|species)+(1|grid_id) random effects
#
# Main packages / 主要包: data.table, glmmTMB, performance,
#   ggplot2, patchwork, ggbeeswarm, ggridges
#
# Output directory / 输出路径:
#   tasks/bird_hazard_model_effort_upgrade/results/
#   tasks/bird_hazard_model_effort_upgrade/figures/
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(glmmTMB)
  library(performance)
  library(ggplot2)
  library(patchwork)
  library(ggbeeswarm)
  library(ggridges)
  library(here)
})

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

theme_nature <- theme_bw(base_size = 11, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey70", linewidth = 0.4),
    strip.text = element_text(size = 8)
  )

# ═══════════════════════════════════════════════════════════
# 1. 定义模型矩阵 / Define model matrix
# ═══════════════════════════════════════════════════════════

# 气候指标 / Climate metrics (6)
climate_specs <- list(
  list(id = "temp_grad",     label = "Temperature gradient",
       var = "temp_grad_z",        label_short = "Temp grad"),
  list(id = "clim_velocity", label = "Climate velocity",
       var = "climate_velocity_z", label_short = "Clim vel"),
  list(id = "mahalanobis",   label = "Mahalanobis distance",
       var = "mahalanobis_dist_z", label_short = "Mahal"),
  list(id = "exposure",      label = "Climate exposure",
       var = "climate_exposure_z", label_short = "Exposure"),
  list(id = "warming",       label = "Warming rate",
       var = "warming_rate_z",     label_short = "Warming"),
  list(id = "prec_velocity", label = "Precipitation velocity",
       var = "precip_velocity_z",  label_short = "Prec vel")
)

# 努力指标 / Effort metrics (4)
effort_specs <- list(
  list(id = "record",    label = "Record-based (legacy)",
       var = "log_effort_record_z",  label_short = "Record"),
  list(id = "visits",    label = "Observer visits",
       var = "log_effort_visits_z",  label_short = "Visits"),
  list(id = "pc1",       label = "PCA composite (PC1)",
       var = "effort_pc1_z",         label_short = "PC1"),
  list(id = "days",      label = "Birding days",
       var = "log_effort_days_z",    label_short = "Days")
)

# 空间尺度 / Spatial scales (2)
scale_specs <- list(
  list(id = "province", label = "Province level"),
  list(id = "grid100",  label = "100-km grid")
)

cat("=== 统一多指标×多尺度建模 ===\n")
cat(sprintf("模型矩阵: %d 气候 × %d 努力 × %d 尺度 = %d 组合\n",
  length(climate_specs), length(effort_specs), length(scale_specs),
  length(climate_specs) * length(effort_specs) * length(scale_specs)))

# ═══════════════════════════════════════════════════════════
# 2. 构建统一风险集 / Build unified risk sets
# ═══════════════════════════════════════════════════════════
cat("\n=== 构建省级风险集 ===\n")

risk_prov <- fread(file.path(TASK_ROOT, "data",
  "hazard_risk_upgraded_complete_case.csv"))
clim_prov <- fread(file.path(TASK_ROOT, "data",
  "climate_metrics_province_year.csv"))

risk_prov[, year_c := year - 2013]

# 合并高级气候指标 / Merge advanced climate metrics
risk_prov <- merge(risk_prov,
  clim_prov[, .(province, year,
    climate_velocity_z, precip_velocity_z,
    climate_exposure_z, warming_rate_z,
    mahalanobis_dist_z)],
  by = c("province", "year"), all.x = TRUE)

# 省级完整案例：所有6个气候+4个effort指标均非NA
all_clim_cols <- sapply(climate_specs, `[[`, "var")
all_effort_cols <- sapply(effort_specs, `[[`, "var")
required_cols <- c(all_clim_cols, all_effort_cols)

complete_mask <- complete.cases(risk_prov[, ..required_cols])
risk_prov_cc <- risk_prov[complete_mask]
cat(sprintf("省级完整案例: %d 行, %d 事件, %d 种, %d 省\n",
  nrow(risk_prov_cc), sum(risk_prov_cc$event),
  uniqueN(risk_prov_cc$species), uniqueN(risk_prov_cc$province)))

# ── 栅格风险集 / Grid risk set ────────────────────────────
cat("\n=== 构建100km栅格风险集 ===\n")

grid_base <- fread(file.path(TASK_ROOT, "data", "grid_100km_base.csv"))
grid_clim <- fread(file.path(TASK_ROOT, "data", "grid_100km_climate.csv"))
grid_effort <- fread(file.path(TASK_ROOT, "data", "grid_100km_effort.csv"))
grid_events <- fread(file.path(TASK_ROOT, "data", "events_100km_grid_assigned.csv"))

# 栅格级别可用的气候指标（原始未标准化） / Grid climate vars (raw)
# 栅格没有 mahalanobis_dist，需要从省级数据聚合
# 先标准化栅格气候变量 / Standardize grid climate variables
grid_clim_std <- copy(grid_clim)
for (v in c("bio1", "climate_velocity", "precip_velocity",
            "climate_exposure", "warming_rate", "spatial_temp_grad")) {
  if (v %in% names(grid_clim_std)) {
    mu <- mean(grid_clim_std[[v]], na.rm = TRUE)
    sigma <- sd(grid_clim_std[[v]], na.rm = TRUE)
    if (sigma > 0) {
      grid_clim_std[, (paste0(v, "_z")) := (get(v) - mu) / sigma]
    }
  }
}

# 计算省级mahalanobis_dist均值（作为栅格代理）
# / Compute province-level mahalanobis mean as grid proxy
prov_mahal_mean <- clim_prov[, mean(mahalanobis_dist_z, na.rm = TRUE)]

# 栅格temp_grad: 使用省级temp_grad_prov_z映射到栅格
# / Grid temp_grad: map province-level to grid
prov_clim_for_grid <- clim_prov[, .(province, year,
  temp_grad_z = temp_grad_prov_z,
  climate_velocity_z_prov = climate_velocity_z,
  mahalanobis_dist_z_prov = mahalanobis_dist_z)]

# 构建栅格风险集 / Build grid risk set
grid_events[, year := pub_year]
grid_event_summary <- grid_events[, .(event = 1L, species = species[1]),
  by = .(grid_id, year)]

grid_effort[, year_c := year - 2013]

# 合并栅格数据 / Merge grid data
grid_risk_base <- merge(grid_base, grid_clim_std,
  by = c("province", "grid_id"), all.x = TRUE)
grid_risk_dt <- merge(grid_risk_base,
  grid_effort[, .(grid_id, year, year_c,
    log_effort_record_z, log_effort_visits_z,
    effort_pc1_z, log_effort_days_z)],
  by = "grid_id", all.x = TRUE)

# 合并省级气候指标 / Merge province climate
grid_risk_dt <- merge(grid_risk_dt,
  prov_clim_for_grid,
  by = c("province", "year"), all.x = TRUE)

# 标记事件 / Mark events
grid_risk_dt <- merge(grid_risk_dt, grid_event_summary,
  by = c("grid_id", "year"), all.x = TRUE)
grid_risk_dt[is.na(event), event := 0L]
# 非事件行无法确定species，赋值dummy / Non-events get dummy species
grid_risk_dt[is.na(species), species := "dummy_species_for_grid"]
# 以保证与省级模型可比性 / Use province-level climate for comparability
grid_risk_dt[, climate_velocity_z := climate_velocity_z_prov]
grid_risk_dt[, mahalanobis_dist_z := mahalanobis_dist_z_prov]
grid_risk_dt[, climate_exposure_z := climate_exposure_z]
grid_risk_dt[, warming_rate_z := warming_rate_z]
grid_risk_dt[, precip_velocity_z := precip_velocity_z]

# 栅格完整案例 / Grid complete cases
grid_cc_mask <- complete.cases(grid_risk_dt[, ..required_cols])
risk_grid_cc <- grid_risk_dt[grid_cc_mask]
cat(sprintf("100km栅格完整案例: %d 行, %d 事件, %d 栅格\n",
  nrow(risk_grid_cc), sum(risk_grid_cc$event),
  uniqueN(risk_grid_cc$grid_id)))

# ═══════════════════════════════════════════════════════════
# 3. 系统拟合模型 / Systematic model fitting
# ═══════════════════════════════════════════════════════════
cat("\n=== 系统拟合模型 ===\n")

# 存储所有结果 / Store all results
all_model_results <- list()
all_coefs <- list()
all_var_decomp <- list()

model_counter <- 0
total_models <- length(climate_specs) * length(effort_specs) *
  length(scale_specs)

for (sc in scale_specs) {
  # 选择数据集 / Select dataset
  if (sc$id == "province") {
    dt <- copy(risk_prov_cc)
    dt[, species := factor(as.character(species))]
    dt[, province := factor(as.character(province))]
    re_formula <- "(1|species) + (1|province)"
  } else {
    dt <- copy(risk_grid_cc)
    dt[, species := factor(as.character(species))]
    dt[, grid_id := factor(as.character(grid_id))]
    re_formula <- "(1|species) + (1|grid_id)"
  }

  for (cl in climate_specs) {
    for (ef in effort_specs) {
      model_counter <- model_counter + 1
      combo_id <- sprintf("%s_%s_%s", sc$id, cl$id, ef$id)

      cat(sprintf("[%d/%d] %s: %s × %s × %s ... ",
        model_counter, total_models,
        sc$id, cl$label_short, ef$label_short, "M0-M4"))

      clim_var <- cl$var
      effort_var <- ef$var

      # 检查列是否存在 / Check column exists
      if (!(clim_var %in% names(dt)) || !(effort_var %in% names(dt))) {
        cat("SKIP (missing column)\n")
        next
      }

      # 构建模型公式 / Build formula
      f_M0 <- as.formula(paste0("event ~ year_c + ", re_formula))
      f_M1 <- as.formula(paste0("event ~ year_c + ", clim_var, " + ", re_formula))
      f_M2 <- as.formula(paste0("event ~ year_c + ", effort_var, " + ", re_formula))
      f_M3 <- as.formula(paste0("event ~ year_c + ", clim_var, " + ", effort_var, " + ", re_formula))
      f_M4 <- as.formula(paste0("event ~ year_c + ", clim_var, " * ", effort_var, " + ", re_formula))

      formulas <- list(M0 = f_M0, M1 = f_M1, M2 = f_M2, M3 = f_M3, M4 = f_M4)

      fit_results <- list()

      for (m_name in names(formulas)) {
        fit <- tryCatch(
          glmmTMB(formulas[[m_name]], data = dt,
            family = binomial(link = "cloglog")),
          error = function(e) {
            cat(sprintf("  %s ERROR: %s\n", m_name, conditionMessage(e)))
            NULL
          }
        )

        if (is.null(fit)) {
          fit_results[[m_name]] <- list(status = "error", aic = NA,
            convergence = NA, pdHess = NA, coefs = NULL, r2 = NULL)
          next
        }

        # 诊断 / Diagnostics
        s <- summary(fit)
        conv <- fit$fit$convergence
        pdH <- !is.null(s$optinfo$conv$pdHess) && s$optinfo$conv$pdHess

        # R² / R-squared
        r2 <- tryCatch(r2(fit), error = function(e) NULL)

        # 提取系数 / Extract coefficients
        coef_dt <- as.data.table(s$coefficients$cond, keep.rownames = "term")
        coef_dt[, model := m_name]
        coef_dt[, scale_id := sc$id]
        coef_dt[, climate_id := cl$id]
        coef_dt[, climate_label := cl$label]
        coef_dt[, climate_var := clim_var]
        coef_dt[, effort_id := ef$id]
        coef_dt[, effort_label := ef$label]
        coef_dt[, effort_var := effort_var]

        # 计算HR / Compute hazard ratios
        coef_dt[, hr := exp(Estimate)]
        coef_dt[, hr_lower := exp(Estimate - 1.96 * `Std. Error`)]
        coef_dt[, hr_upper := exp(Estimate + 1.96 * `Std. Error`)]

        fit_results[[m_name]] <- list(
          status = "ok",
          aic = AIC(fit),
          convergence = conv,
          pdHess = pdH,
          coefs = coef_dt,
          r2 = r2
        )
      }

      # 汇总结果 / Summarize results
      model_summary <- rbindlist(lapply(names(formulas), function(m) {
        r <- fit_results[[m]]
        data.table(
          scale_id = sc$id,
          climate_id = cl$id,
          climate_label = cl$label,
          climate_var = clim_var,
          effort_id = ef$id,
          effort_label = ef$label,
          effort_var = effort_var,
          model = m,
          status = r$status,
          aic = if (is.numeric(r$aic)) r$aic else NA_real_,
          convergence = r$convergence,
          pdHess = r$pdHess,
          marginal_r2 = if (!is.null(r$r2)) r$r2$R2_marginal else NA_real_,
          conditional_r2 = if (!is.null(r$r2)) r$r2$R2_conditional else NA_real_
        )
      }), fill = TRUE)

      # 计算delta_AIC / Compute delta AIC
      valid_aic <- model_summary[status == "ok" & !is.na(aic), aic]
      if (length(valid_aic) > 0) {
        model_summary[, delta_aic := aic - min(valid_aic, na.rm = TRUE)]
      } else {
        model_summary[, delta_aic := NA_real_]
      }

      all_model_results[[combo_id]] <- model_summary

      # 收集系数 / Collect coefficients
      coef_list <- lapply(names(formulas), function(m) {
        r <- fit_results[[m]]
        if (!is.null(r$coefs)) r$coefs else NULL
      })
      coef_dt <- rbindlist(coef_list[!sapply(coef_list, is.null)], fill = TRUE)
      all_coefs[[combo_id]] <- coef_dt

      # ── 方差分解 / Variance decomposition ─────────────
      r2_M3 <- model_summary[model == "M3" & status == "ok", marginal_r2]
      r2_M4 <- model_summary[model == "M4" & status == "ok", marginal_r2]

      if (length(r2_M3) == 1 && length(r2_M4) == 1 &&
          !is.na(r2_M3) && !is.na(r2_M4)) {
        var_decomp <- data.table(
          scale_id = sc$id,
          climate_id = cl$id,
          climate_label = cl$label,
          effort_id = ef$id,
          effort_label = ef$label,
          additive_r2 = r2_M3,
          interaction_r2 = r2_M4,
          delta_interaction_r2 = r2_M4 - r2_M3,
          ratio_interaction_additive = if (r2_M3 > 0)
            (r2_M4 - r2_M3) / r2_M3 else NA_real_
        )
        all_var_decomp[[combo_id]] <- var_decomp
      }

      cat("done\n")
    }
  }
}

# ═══════════════════════════════════════════════════════════
# 4. 汇总输出 / Consolidate outputs
# ═══════════════════════════════════════════════════════════
cat("\n=== 汇总输出 ===\n")

# 模型比较总表 / Master model comparison table
dt_model_all <- rbindlist(all_model_results, fill = TRUE)
fwrite(dt_model_all,
  file.path(TASK_ROOT, "results",
    "table_unified_model_comparison.csv"))
cat(sprintf("模型比较总表: %d 行\n", nrow(dt_model_all)))

# 系数总表 / Master coefficient table
dt_coef_all <- rbindlist(all_coefs, fill = TRUE)
fwrite(dt_coef_all,
  file.path(TASK_ROOT, "results",
    "table_unified_coefficients.csv"))
cat(sprintf("系数总表: %d 行\n", nrow(dt_coef_all)))

# 提取交互项系数 / Extract interaction coefficients
interact_coefs <- dt_coef_all[grepl(":", term) & !grepl("Intercept", term)]
fwrite(interact_coefs,
  file.path(TASK_ROOT, "results",
    "table_unified_interaction_coefs.csv"))
cat(sprintf("交互项系数: %d 行\n", nrow(interact_coefs)))

# 方差分解总表 / Master variance decomposition table
dt_var_all <- rbindlist(all_var_decomp, fill = TRUE)
fwrite(dt_var_all,
  file.path(TASK_ROOT, "results",
    "table_unified_variance_decomposition.csv"))
cat(sprintf("方差分解: %d 行\n", nrow(dt_var_all)))

# ═══════════════════════════════════════════════════════════
# 5. 可视化 / Visualization
# ═══════════════════════════════════════════════════════════
cat("\n=== 可视化 ===\n")

# ── 图1: 交互项HR森林图（按尺度分面） / Fig 1: Forest plot ──
if (nrow(interact_coefs) > 0) {
  interact_coefs[, combo := paste0(climate_label, " × ", effort_label)]

  p_forest <- ggplot(interact_coefs,
    aes(x = hr, y = reorder(combo, hr),
        color = scale_id)) +
    geom_vline(xintercept = 1, linetype = "dashed",
      color = "grey50", linewidth = 0.5) +
    geom_point(size = 2.5, position = position_dodge(0.5)) +
    geom_errorbar(aes(xmin = hr_lower, xmax = hr_upper),
      height = 0.3, linewidth = 0.6,
      position = position_dodge(0.5)) +
    scale_color_manual(values = c("province" = "#d94801",
      "grid100" = "#2171b5"), name = "Scale") +
    labs(x = "Hazard ratio (interaction term)",
         y = "",
         title = "Climate × Effort interaction across all specifications",
         subtitle = "Point estimates with 95% CI") +
    theme_nature +
    theme(legend.position = "bottom")

  ggsave(file.path(TASK_ROOT, "figures",
    "fig_unified_forest_interaction.png"),
    p_forest, width = 12, height = 14, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    "fig_unified_forest_interaction.pdf"),
    p_forest, width = 12, height = 14)
  cat("森林图已生成\n")
}

# ── 图2: AIC热力图（气候×努力，分尺度） / Fig 2: AIC heatmap ──
m4_results <- dt_model_all[model == "M4" & status == "ok"]

if (nrow(m4_results) > 0) {
  for (sc_id in unique(m4_results$scale_id)) {
    dt_sub <- m4_results[scale_id == sc_id]
    dt_sub[, climate_short := fcase(
      climate_id == "temp_grad", "Temp grad",
      climate_id == "clim_velocity", "Clim vel",
      climate_id == "mahalanobis", "Mahal",
      climate_id == "exposure", "Exposure",
      climate_id == "warming", "Warming",
      climate_id == "prec_velocity", "Prec vel",
      default = climate_id)]
    dt_sub[, effort_short := fcase(
      effort_id == "record", "Record",
      effort_id == "visits", "Visits",
      effort_id == "pc1", "PC1",
      effort_id == "days", "Days",
      default = effort_id)]

    p_aic <- ggplot(dt_sub, aes(x = effort_short, y = climate_short,
      fill = aic)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.0f", aic)),
        size = 3, color = "black") +
      scale_fill_viridis_c(option = "D", direction = -1,
        name = "AIC") +
      labs(x = "Effort metric", y = "Climate metric",
        title = sprintf("AIC heatmap: %s scale (M4 interaction model)",
          ifelse(sc_id == "province", "Province", "100-km grid"))) +
      theme_nature +
      theme(panel.border = element_rect(colour = "grey70"),
        axis.text.x = element_text(angle = 30, hjust = 1))

    ggsave(file.path(TASK_ROOT, "figures",
      sprintf("fig_unified_aic_heatmap_%s.png", sc_id)),
      p_aic, width = 8, height = 6, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures",
      sprintf("fig_unified_aic_heatmap_%s.pdf", sc_id)),
      p_aic, width = 8, height = 6)
  }
  cat("AIC热力图已生成\n")
}

# ── 图3: HR热力图 / Fig 3: HR heatmap ──
if (nrow(interact_coefs) > 0) {
  interact_coefs[, climate_short := fcase(
    climate_id == "temp_grad", "Temp grad",
    climate_id == "clim_velocity", "Clim vel",
    climate_id == "mahalanobis", "Mahal",
    climate_id == "exposure", "Exposure",
    climate_id == "warming", "Warming",
    climate_id == "prec_velocity", "Prec vel",
    default = climate_id)]
  interact_coefs[, effort_short := fcase(
    effort_id == "record", "Record",
    effort_id == "visits", "Visits",
    effort_id == "pc1", "PC1",
    effort_id == "days", "Days",
    default = effort_id)]

  for (sc_id in unique(interact_coefs$scale_id)) {
    dt_sub <- interact_coefs[scale_id == sc_id]

    p_hr <- ggplot(dt_sub, aes(x = effort_short, y = climate_short,
      fill = hr)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", hr)),
        size = 3.5, fontface = "bold") +
      scale_fill_gradient2(low = "#2171b5", mid = "white", high = "#d94801",
        midpoint = 1, name = "HR\n(interaction)") +
      labs(x = "Effort metric", y = "Climate metric",
        title = sprintf("Interaction HR: %s scale",
          ifelse(sc_id == "province", "Province", "100-km grid"))) +
      theme_nature +
      theme(panel.border = element_rect(colour = "grey70"),
        axis.text.x = element_text(angle = 30, hjust = 1))

    ggsave(file.path(TASK_ROOT, "figures",
      sprintf("fig_unified_hr_heatmap_%s.png", sc_id)),
      p_hr, width = 8, height = 6, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures",
      sprintf("fig_unified_hr_heatmap_%s.pdf", sc_id)),
      p_hr, width = 8, height = 6)
  }
  cat("HR热力图已生成\n")
}

# ── 图4: 方差分解对比 / Fig 4: Variance decomposition comparison ──
if (nrow(dt_var_all) > 0) {
  dt_var_long <- melt(dt_var_all,
    measure.vars = c("additive_r2", "delta_interaction_r2"),
    variable.name = "component",
    value.name = "r2")
  dt_var_long[, component := fcase(
    component == "additive_r2", "Additive (M3)",
    component == "delta_interaction_r2", "Interaction (M4-M3)"
  )]
  dt_var_long[, combo := paste0(climate_label, " × ", effort_label)]

  p_var <- ggplot(dt_var_long[scale_id == "province"],
    aes(x = r2, y = reorder(combo, r2),
        fill = component)) +
    geom_col(position = "stack", alpha = 0.85) +
    scale_fill_manual(values = c("Additive (M3)" = "#2171b5",
      "Interaction (M4-M3)" = "#d94801"),
      name = "R² component") +
    labs(x = "Marginal R²", y = "",
      title = "Variance decomposition: additive vs. interaction",
      subtitle = "Province level, all climate × effort combinations") +
    theme_nature +
    theme(legend.position = "bottom")

  ggsave(file.path(TASK_ROOT, "figures",
    "fig_unified_variance_decomposition.png"),
    p_var, width = 12, height = 14, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    "fig_unified_variance_decomposition.pdf"),
    p_var, width = 12, height = 14)

  # 分尺度方差分解对比 / Scale comparison
  if (uniqueN(dt_var_all$scale_id) > 1) {
    dt_var_long2 <- melt(dt_var_all,
      measure.vars = c("additive_r2", "delta_interaction_r2"),
      variable.name = "component", value.name = "r2")
    dt_var_long2[, component := fcase(
      component == "additive_r2", "Additive",
      component == "delta_interaction_r2", "Interaction"
    )]
    dt_var_long2[, combo := paste0(climate_label, " × ", effort_label)]

    p_var_scale <- ggplot(dt_var_long2,
      aes(x = r2, y = reorder(combo, r2 + scale_id == "grid100"),
          fill = component, alpha = scale_id)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c("Additive" = "#2171b5",
        "Interaction" = "#d94801"), name = "Component") +
      scale_alpha_manual(values = c("province" = 1, "grid100" = 0.5),
        name = "Scale") +
      labs(x = "Marginal R²", y = "",
        title = "Variance decomposition across scales") +
      theme_nature +
      theme(legend.position = "bottom")

    ggsave(file.path(TASK_ROOT, "figures",
      "fig_unified_variance_decomposition_cross_scale.png"),
      p_var_scale, width = 14, height = 16, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures",
      "fig_unified_variance_decomposition_cross_scale.pdf"),
      p_var_scale, width = 14, height = 16)
  }
  cat("方差分解图已生成\n")
}

# ── 图5: 最佳模型对比 / Fig 5: Best model comparison ──
if (nrow(m4_results) > 0) {
  # 每个尺度选AIC最低的模型 / Select best model per scale
  best_per_scale <- m4_results[, .SD[which.min(aic)], by = scale_id]

  if (nrow(best_per_scale) > 0) {
    cat("\n最佳模型:\n")
    print(best_per_scale[, .(scale_id, climate_label, effort_label, aic,
      marginal_r2, conditional_r2)])
  }
}

# ── 图6: 模型阶段AIC下降 / Fig 6: AIC reduction by model stage ──
dt_stages <- dt_model_all[status == "ok" & scale_id == "province"]
if (nrow(dt_stages) > 0) {
  # 按climate×effort分组，M0到M4的AIC变化
  dt_stages[, combo := paste0(climate_id, "_", effort_id)]

  p_aic_stage <- ggplot(dt_stages[climate_id == "temp_grad"],
    aes(x = model, y = aic, group = effort_label,
        color = effort_label)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Record-based (legacy)" = "#666666",
      "Observer visits" = "#2171b5",
      "PCA composite (PC1)" = "#d94801",
      "Birding days" = "#6a51a3"),
      name = "Effort metric") +
    labs(x = "Model stage", y = "AIC",
      title = "AIC reduction: M0 → M4 (temp_grad)",
      subtitle = "Province level") +
    theme_nature

  ggsave(file.path(TASK_ROOT, "figures",
    "fig_unified_aic_by_stage.png"),
    p_aic_stage, width = 8, height = 6, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures",
    "fig_unified_aic_by_stage.pdf"),
    p_aic_stage, width = 8, height = 6)
  cat("AIC阶段图已生成\n")
}

# ── 图7: 交互项显著性矩阵 / Fig 7: Significance matrix ──
if (nrow(interact_coefs) > 0) {
  interact_coefs[, sig_level := fcase(
    `Pr(>|z|)` < 0.001, "***",
    `Pr(>|z|)` < 0.01, "**",
    `Pr(>|z|)` < 0.05, "*",
    default = "ns")]
  interact_coefs[, sig_color := fcase(
    sig_level == "***", "p < 0.001",
    sig_level == "**", "p < 0.01",
    sig_level == "*", "p < 0.05",
    default = "ns")]

  fwrite(interact_coefs,
    file.path(TASK_ROOT, "results",
      "table_unified_significance_matrix.csv"))

  for (sc_id in unique(interact_coefs$scale_id)) {
    dt_sub <- interact_coefs[scale_id == sc_id]

    p_sig <- ggplot(dt_sub, aes(x = effort_short, y = climate_short,
      fill = `Pr(>|z|)`)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sig_level), size = 5, fontface = "bold") +
      scale_fill_gradient(low = "#d94801", high = "#fdd0a2",
        trans = "log10", name = "p-value\n(log scale)") +
      labs(x = "Effort metric", y = "Climate metric",
        title = sprintf("Interaction significance: %s scale",
          ifelse(sc_id == "province", "Province", "100-km grid"))) +
      theme_nature +
      theme(panel.border = element_rect(colour = "grey70"),
        axis.text.x = element_text(angle = 30, hjust = 1))

    ggsave(file.path(TASK_ROOT, "figures",
      sprintf("fig_unified_significance_%s.png", sc_id)),
      p_sig, width = 8, height = 6, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures",
      sprintf("fig_unified_significance_%s.pdf", sc_id)),
      p_sig, width = 8, height = 6)
  }
  cat("显著性矩阵图已生成\n")
}

cat("\n=== 17_unified_multi_metric_modeling.R 完成 ===\n")
