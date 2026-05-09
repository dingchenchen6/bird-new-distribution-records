#!/usr/bin/env Rscript
# 03_hazard_model_upgrade_visualization.R
# Hazard model 升级版顶刊级可视化
# Publication-quality visualization for the effort-upgraded hazard model

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(ggrepel)
  library(openxlsx)
  library(here)
})

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

# ── 配色方案（Nature/Science 风格）───────────────────────────────────────
pal_specs <- c(
  "spec_A" = "#2171b5",   # 蓝 — 记录型（旧）
  "spec_B" = "#238b45",   # 绿 — 访问量
  "spec_C" = "#d94801",   # 橙 — PCA 复合
  "spec_D" = "#6a51a3"    # 紫 — 观鸟天数
)

spec_labels <- c(
  "spec_A" = "Record-based\n(legacy)",
  "spec_B" = "Observer\nvisits",
  "spec_C" = "PCA\ncomposite",
  "spec_D" = "Birding\ndays"
)

theme_nature <- theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle    = element_text(size = 10, color = "grey40", hjust = 0, margin = margin(b = 6)),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "grey70", linewidth = 0.4),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 10, face = "bold"),
    strip.text       = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = "grey70")
  )

# ── 1. 读取结果 ──────────────────────────────────────────────────────────
dt_model <- fread(file.path(TASK_ROOT, "results",
                             "table_cross_specification_model_comparison.csv"))
dt_coefs <- fread(file.path(TASK_ROOT, "results",
                             "table_cross_specification_key_coefficients.csv"))

# ── 2. 森林图：交互项 HR 跨指标对比 ──────────────────────────────────────
dt_interact <- dt_coefs[grepl("temp_grad_z:", term) & model == "M4"]

if (nrow(dt_interact) > 0) {
  dt_interact[, spec_label := factor(spec_labels[spec], levels = spec_labels)]

  p_forest <- ggplot(dt_interact, aes(x = hr, y = spec_label, color = spec)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_errorbarh(aes(xmin = hr_lower, xmax = hr_upper),
                   height = 0.2, linewidth = 0.8) +
    geom_point(size = 4, shape = 18) +
    geom_text(aes(label = sprintf("%.3f", hr)),
              vjust = -1.0, size = 3.5, fontface = "bold", color = "grey20") +
    scale_color_manual(values = pal_specs, guide = "none") +
    labs(x = "Hazard ratio (HR)", y = "",
         title = "Climate-effort interaction across effort specifications",
         subtitle = "M4 model: temp_grad_z × effort_z interaction hazard ratio",
         caption = "HR > 1 indicates that climate-driven expansion is more likely recorded\nin better-surveyed regions (visibility-threshold hypothesis)") +
    theme_nature +
    theme(plot.caption = element_text(size = 9, color = "grey40", hjust = 0))

  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_forest_cross_spec.png"),
         p_forest, width = 9, height = 5, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_forest_cross_spec.pdf"),
         p_forest, width = 9, height = 5)
}

# ── 3. 山脊图：跨模型阶段和指标的系数分布 ────────────────────────────────
# 选取 temp_grad_z 主效应和交互项
dt_ridge <- dt_coefs[term %in% c("temp_grad_z",
                                   "temp_grad_z:log_effort_record_z",
                                   "temp_grad_z:log_effort_visits_z",
                                   "temp_grad_z:effort_pc1_z",
                                   "temp_grad_z:log_effort_days_z")]

if (nrow(dt_ridge) > 0) {
  dt_ridge[, term_label := ifelse(grepl(":", term), "temp_grad × effort", "temp_grad")]
  dt_ridge[, spec_label := factor(spec_labels[spec], levels = spec_labels)]

  p_ridge <- ggplot(dt_ridge, aes(x = estimate, y = spec_label, fill = spec)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_col(aes(width = 0.6), position = "dodge", alpha = 0.7, show.legend = FALSE) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, linewidth = 0.6, color = "grey30") +
    facet_wrap(~ term_label, scales = "free_x", ncol = 2) +
    scale_fill_manual(values = pal_specs) +
    labs(x = "Coefficient estimate", y = "",
         title = "Coefficient estimates across effort specifications",
         subtitle = "Point estimates with 95% CI; dashed line at zero") +
    theme_nature

  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_ridge_cross_spec.png"),
         p_ridge, width = 11, height = 5, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_ridge_cross_spec.pdf"),
         p_ridge, width = 11, height = 5)
}

# ── 4. AIC 热力图 ────────────────────────────────────────────────────────
dt_aic <- dt_model[status == "ok"]

if (nrow(dt_aic) > 0) {
  dt_aic[, spec_label := factor(spec_labels[spec], levels = spec_labels)]
  dt_aic[, model_label := factor(model, levels = paste0("M", 0:4))]

  p_aic <- ggplot(dt_aic, aes(x = model_label, y = spec_label, fill = delta_aic)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(delta_aic < 2, "*", sprintf("%.0f", delta_aic))),
              size = 3.5, color = "grey20") +
    scale_fill_gradient2(low = "#238b45", mid = "#ffffbf", high = "#d73027",
                         midpoint = 10, name = expression(Delta*AIC)) +
    labs(x = "Model stage", y = "",
         title = "Model selection: AIC delta across specifications",
         subtitle = "Lower (greener) = better fit; * = within 2 AIC of best") +
    theme_nature +
    theme(panel.grid = element_blank(),
          legend.position = "right")

  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_aic_heatmap.png"),
         p_aic, width = 8, height = 5, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_aic_heatmap.pdf"),
         p_aic, width = 8, height = 5)
}

# ── 5. HR 稳定性图：交互项 HR 随 effort 指标变化 ──────────────────────────
if (nrow(dt_interact) > 0) {
  dt_interact_ordered <- dt_interact[order(hr)]

  p_stability <- ggplot(dt_interact_ordered, aes(x = reorder(spec_label, hr), y = hr)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = hr_lower, ymax = hr_upper, color = spec),
                  width = 0.2, linewidth = 0.8) +
    geom_point(aes(color = spec), size = 4, shape = 18) +
    geom_line(aes(group = 1), color = "grey40", linewidth = 0.5) +
    scale_color_manual(values = pal_specs, guide = "none") +
    coord_flip() +
    labs(x = "", y = "Hazard ratio (temp_grad_z × effort_z)",
         title = "Interaction hazard ratio stability across effort metrics",
         subtitle = "Consistent HR > 1 across all metrics would support the visibility-threshold interpretation") +
    theme_nature

  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_hr_stability.png"),
         p_stability, width = 8, height = 5, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_hr_stability.pdf"),
         p_stability, width = 8, height = 5)
}

# ── 6. 综合图：4-panel 拼图 ──────────────────────────────────────────────
if (exists("p_forest") && exists("p_aic") && exists("p_stability") && exists("p_ridge")) {
  p_combined <- (p_forest | p_stability) / (p_aic | p_ridge) +
    plot_annotation(
      title = "Hazard model effort specification comparison",
      subtitle = "Testing robustness of the climate-effort interaction to effort metric choice",
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0),
        plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0)
      )
    )

  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_combined_4panel.png"),
         p_combined, width = 16, height = 10, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_hazard_combined_4panel.pdf"),
         p_combined, width = 16, height = 10)
}

# ── 7. Excel 结果包 ──────────────────────────────────────────────────────
wb <- createWorkbook()

addWorksheet(wb, "model_comparison")
writeData(wb, "model_comparison", dt_model)

dt_key <- fread(file.path(TASK_ROOT, "results",
                           "table_cross_specification_key_coefficients.csv"))
addWorksheet(wb, "key_coefficients")
writeData(wb, "key_coefficients", dt_key)

if (nrow(dt_interact) > 0) {
  addWorksheet(wb, "key_interaction_coefs")
  writeData(wb, "key_interaction_coefs", dt_interact)
}

if (file.exists(file.path(TASK_ROOT, "results", "table_extended_model_comparison.csv"))) {
  dt_ext <- fread(file.path(TASK_ROOT, "results", "table_extended_model_comparison.csv"))
  addWorksheet(wb, "extended_model_comparison")
  writeData(wb, "extended_model_comparison", dt_ext)
}

if (file.exists(file.path(TASK_ROOT, "results", "table_extended_model_coefficients.csv"))) {
  dt_ext_coefs <- fread(file.path(TASK_ROOT, "results",
                                   "table_extended_model_coefficients.csv"))
  addWorksheet(wb, "extended_model_coefficients")
  writeData(wb, "extended_model_coefficients", dt_ext_coefs)
}

if (file.exists(file.path(TASK_ROOT, "results", "risk_set_diagnostics.csv"))) {
  dt_diag <- fread(file.path(TASK_ROOT, "results", "risk_set_diagnostics.csv"))
  addWorksheet(wb, "risk_set_diagnostics")
  writeData(wb, "risk_set_diagnostics", dt_diag)
}

saveWorkbook(wb, file.path(TASK_ROOT, "results", "hazard_model_effort_upgrade_bundle.xlsx"),
             overwrite = TRUE)

cat("Hazard model 升级版可视化完成。\n")
