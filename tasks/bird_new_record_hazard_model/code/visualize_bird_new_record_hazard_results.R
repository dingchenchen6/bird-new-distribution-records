#!/usr/bin/env Rscript

# ============================================================
# Bird New-Record Hazard Result Bundle
# Hazard 模型结果汇总、森林图和山脊图输出脚本
# ============================================================
#
# Design goal / 设计目标
# This script is intentionally written as a clean reporting pipeline:
# 1. read threshold-specific results,
# 2. build compact summary tables,
# 3. create publication-ready figures,
# 4. export an Excel bundle for manuscript writing and GitHub sharing.
#
# 这份脚本不是简单“画图”，而是把 hazard 模型结果整理成一套适合
# 投稿、汇报和 GitHub 复现的结果包。
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggridges)
  library(purrr)
  library(readr)
  library(tidyr)
  library(writexl)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  normalizePath("code/visualize_bird_new_record_hazard_results.R", mustWork = TRUE)
}

task_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
output_root <- file.path(task_root, "hazard_model_visualizations")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "results"), recursive = TRUE, showWarnings = FALSE)

step_message <- function(step_id, title_cn, title_en = title_cn) {
  message("\n", strrep("-", 66))
  message("Step ", step_id, ". ", title_en)
  if (!identical(title_cn, title_en)) {
    message("第 ", step_id, " 步：", title_cn)
  }
  message(strrep("-", 66))
}

theme_hazard <- function(base_size = 12, base_family = "Helvetica") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, colour = "#111827"),
      plot.subtitle = element_text(size = base_size - 0.2, colour = "#374151"),
      axis.title = element_text(face = "bold", colour = "#111827"),
      axis.text = element_text(colour = "#111827"),
      legend.title = element_text(face = "bold"),
      legend.position = "top",
      strip.background = element_rect(fill = "#E5E7EB", colour = "#9CA3AF", linewidth = 0.5),
      strip.text = element_text(face = "bold", colour = "#111827"),
      panel.grid = element_blank()
    )
}

read_threshold_results <- function(task_root, threshold_label) {
  root_dir <- file.path(task_root, paste0("combined_threshold_", threshold_label, "_test"))

  list(
    model = read_csv(file.path(root_dir, "results", "model_comparison.csv"), show_col_types = FALSE) %>%
      mutate(threshold = threshold_label),
    coef = read_csv(file.path(root_dir, "results", "model_coefficients.csv"), show_col_types = FALSE) %>%
      mutate(threshold = threshold_label),
    coverage = read_csv(file.path(root_dir, "results", "input_coverage_summary.csv"), show_col_types = FALSE) %>%
      mutate(threshold = threshold_label)
  )
}

build_summary_tables <- function(all_results) {
  term_labels <- c(
    temp_grad_z = "Temperature gradient",
    log_effort_record_z = "Record effort",
    `temp_grad_z:log_effort_record_z` = "Temperature x record effort"
  )

  model_labels <- c(
    M0 = "Baseline year model",
    M1 = "Climate model",
    M2 = "Effort model",
    M3 = "Climate + effort model",
    M4 = "Joint-driver interaction model"
  )

  model_tbl <- bind_rows(map(all_results, "model")) %>%
    mutate(
      threshold = factor(threshold, levels = c("50", "100", "200")),
      model_label = recode(model, !!!model_labels)
    )

  coef_tbl <- bind_rows(map(all_results, "coef")) %>%
    filter(term %in% names(term_labels)) %>%
    mutate(
      threshold = factor(threshold, levels = c("50", "100", "200")),
      model_label = recode(model, !!!model_labels),
      term_label = recode(term, !!!term_labels)
    )

  coverage_tbl <- bind_rows(map(all_results, "coverage")) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    mutate(threshold = factor(threshold, levels = c("50", "100", "200")))

  list(
    model_tbl = model_tbl,
    coef_tbl = coef_tbl,
    coverage_tbl = coverage_tbl
  )
}

make_best_model_forest_plot <- function(model_tbl, coef_tbl) {
  best_model_tbl <- model_tbl %>%
    filter(status == "ok") %>%
    group_by(threshold) %>%
    slice_min(order_by = aic, n = 1, with_ties = FALSE) %>%
    ungroup()

  best_coef_tbl <- coef_tbl %>%
    inner_join(best_model_tbl %>% select(threshold, model), by = c("threshold", "model")) %>%
    mutate(
      threshold_label = paste0("Threshold ", threshold),
      term_label = factor(
        term_label,
        levels = c("Temperature x record effort", "Record effort", "Temperature gradient")
      )
    )

  p <- ggplot(best_coef_tbl, aes(x = hazard_ratio, y = term_label, color = threshold_label)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#6B7280", linewidth = 0.5) +
    geom_errorbar(
      aes(xmin = hazard_ratio_low, xmax = hazard_ratio_high),
      orientation = "y",
      width = 0.18,
      linewidth = 0.85,
      position = position_dodge(width = 0.55)
    ) +
    geom_point(size = 2.8, position = position_dodge(width = 0.55)) +
    scale_color_manual(values = c("Threshold 50" = "#0F766E", "Threshold 100" = "#B45309", "Threshold 200" = "#1D4ED8")) +
    labs(
      title = "Best-model hazard ratios across SDM threshold scenarios",
      subtitle = "Current main specification: temperature gradient, record effort, and their interaction",
      x = "Hazard ratio (95% CI)",
      y = NULL,
      color = "SDM threshold"
    ) +
    theme_hazard()

  list(plot = p, best_model_tbl = best_model_tbl)
}

make_ridge_plot <- function(coef_tbl) {
  ridge_tbl <- coef_tbl %>%
    mutate(
      threshold_label = paste0("Threshold ", threshold),
      ridge_group = paste0(model, " | ", threshold_label)
    ) %>%
    select(ridge_group, threshold_label, term_label, estimate, std_error) %>%
    filter(is.finite(estimate), is.finite(std_error), std_error > 0) %>%
    mutate(draw_grid = map2(estimate, std_error, ~ seq(.x - 4 * .y, .x + 4 * .y, length.out = 220))) %>%
    unnest(draw_grid) %>%
    mutate(density = dnorm(draw_grid, mean = estimate, sd = std_error))

  ggplot(
    ridge_tbl,
    aes(x = draw_grid, y = ridge_group, height = density, fill = threshold_label, group = ridge_group)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#6B7280", linewidth = 0.4) +
    ggridges::geom_ridgeline(scale = 2.4, alpha = 0.82, color = "white", linewidth = 0.25) +
    facet_wrap(~ term_label, scales = "free_x", ncol = 1) +
    scale_fill_manual(values = c("Threshold 50" = "#0F766E", "Threshold 100" = "#B45309", "Threshold 200" = "#1D4ED8")) +
    labs(
      title = "Coefficient uncertainty across model stages and SDM thresholds",
      subtitle = "Normal approximations using model estimates and standard errors",
      x = "Log-hazard coefficient",
      y = NULL,
      fill = "SDM threshold"
    ) +
    theme_hazard(base_size = 11)
}

write_result_bundle <- function(output_root, model_tbl, coef_tbl, coverage_tbl, best_model_tbl) {
  summary_tbl <- model_tbl %>%
    select(threshold, model, model_label, status, nobs, aic, bic, logLik, delta_aic) %>%
    arrange(threshold, aic)

  key_coef_tbl <- coef_tbl %>%
    select(
      threshold, model, model_label, term, term_label,
      estimate, std_error, p_value,
      hazard_ratio, hazard_ratio_low, hazard_ratio_high
    ) %>%
    arrange(threshold, model, term_label)

  write_csv(summary_tbl, file.path(output_root, "results", "table_model_comparison_across_thresholds.csv"))
  write_csv(key_coef_tbl, file.path(output_root, "results", "table_key_coefficients_across_thresholds.csv"))
  write_csv(coverage_tbl, file.path(output_root, "results", "table_input_coverage_across_thresholds.csv"))

  write_xlsx(
    list(
      model_comparison = summary_tbl,
      key_coefficients = key_coef_tbl,
      input_coverage = coverage_tbl
    ),
    file.path(output_root, "results", "hazard_model_result_bundle.xlsx")
  )

  summary_lines <- c(
    "# Hazard Visualization Summary",
    "",
    "## Best models",
    paste0("- ", best_model_tbl$threshold, " -> ", best_model_tbl$model, " (AIC = ", round(best_model_tbl$aic, 3), ")"),
    "",
    "## Output files",
    "- figures/fig_hazard_forest_best_model.png",
    "- figures/fig_hazard_ridge_coefficients.png",
    "- results/table_model_comparison_across_thresholds.csv",
    "- results/table_key_coefficients_across_thresholds.csv",
    "- results/table_input_coverage_across_thresholds.csv",
    "- results/hazard_model_result_bundle.xlsx"
  )

  writeLines(summary_lines, con = file.path(output_root, "results", "visualization_summary.md"))
}

# -------------------------------
# Step 1. Read threshold-specific model outputs
# 第 1 步：读取不同阈值下的模型输出
# -------------------------------
step_message(1, "读取阈值结果", "Read threshold-specific hazard outputs")
all_results <- map(c("50", "100", "200"), ~ read_threshold_results(task_root, .x))

# -------------------------------
# Step 2. Build summary tables
# 第 2 步：整理结果汇总表
# -------------------------------
step_message(2, "整理结果汇总表", "Build cross-threshold summary tables")
tbls <- build_summary_tables(all_results)

# -------------------------------
# Step 3. Draw a professional forest plot
# 第 3 步：绘制专业森林图
# -------------------------------
step_message(3, "绘制森林图", "Draw the best-model forest plot")
forest_out <- make_best_model_forest_plot(tbls$model_tbl, tbls$coef_tbl)
ggsave(
  file.path(output_root, "figures", "fig_hazard_forest_best_model.png"),
  forest_out$plot,
  width = 9.6,
  height = 5.6,
  dpi = 420,
  bg = "white"
)

# -------------------------------
# Step 4. Draw a ridge plot for coefficient uncertainty
# 第 4 步：绘制系数不确定性山脊图
# -------------------------------
step_message(4, "绘制山脊图", "Draw the coefficient ridge plot")
p_ridge <- make_ridge_plot(tbls$coef_tbl)
ggsave(
  file.path(output_root, "figures", "fig_hazard_ridge_coefficients.png"),
  p_ridge,
  width = 11,
  height = 8.8,
  dpi = 420,
  bg = "white"
)

# -------------------------------
# Step 5. Export tables and Excel bundle
# 第 5 步：导出结果表与 Excel bundle
# -------------------------------
step_message(5, "导出结果包", "Export summary tables and Excel bundle")
write_result_bundle(
  output_root = output_root,
  model_tbl = tbls$model_tbl,
  coef_tbl = tbls$coef_tbl,
  coverage_tbl = tbls$coverage_tbl,
  best_model_tbl = forest_out$best_model_tbl
)

message("\nVisualization bundle completed.")
