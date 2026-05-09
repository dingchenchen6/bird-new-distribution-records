#!/usr/bin/env Rscript
# 13_comprehensive_visualization.R
# 综合可视化：数据分布 + 模型结果 + Range map对比
# Comprehensive visualization suite

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(ggbeeswarm)
  library(ggridges)
  library(ggcorrplot)
  library(here)
})

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

theme_nature <- theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey70", linewidth = 0.4)
  )

# ── 1. 读取数据 ──────────────────────────────────────────────────────────
cat("=== 综合可视化 ===\n")

risk_data <- fread(file.path(TASK_ROOT, "data",
                              "hazard_risk_upgraded_complete_case.csv"))
clim_metrics <- fread(file.path(TASK_ROOT, "data",
                                 "climate_metrics_province_year.csv"))
risk_range <- fread(file.path(TASK_ROOT, "data",
                               "hazard_risk_upgraded_range_map_anom.csv"))

risk_data[, year_c := year - 2013]
risk_data <- merge(risk_data, clim_metrics[, .(province, year,
  climate_velocity_z, precip_velocity_z, climate_exposure_z,
  warming_rate_z, mahalanobis_dist_z)],
  by = c("province", "year"), all.x = TRUE)

# ── 2. 数据分布图 ──────────────────────────────────────────────────────
cat("生成数据分布图\n")

# fig_d01: 温度梯度蜂群+小提琴图
p_d01 <- ggplot(risk_data[event == 1],
                aes(x = factor(year), y = temp_grad_z)) +
  geom_violin(fill = "#fdd0a2", alpha = 0.5, color = NA) +
  geom_beeswarm(size = 0.5, alpha = 0.4, color = "#d94801") +
  labs(x = "Year", y = "Temperature gradient (z)",
       title = "Temperature gradient distribution by year",
       subtitle = "Events only; violin + beeswarm") +
  theme_nature +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(TASK_ROOT, "figures", "fig_d01_temp_grad_beeswarm.png"),
       p_d01, width = 12, height = 5, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_d01_temp_grad_beeswarm.pdf"),
       p_d01, width = 12, height = 5)

# fig_d02: 4种努力指标山脊线图
effort_cols <- c("log_effort_record_z", "log_effort_visits_z",
                 "effort_pc1_z", "log_effort_days_z")
effort_labels <- c("Record-based", "Observer visits",
                   "PCA composite", "Birding days")

effort_long <- melt(risk_data[event == 1],
                    measure.vars = effort_cols,
                    variable.name = "metric",
                    value.name = "effort_z")
effort_long[, metric_lab := factor(metric,
  levels = effort_cols, labels = effort_labels)]

p_d02 <- ggplot(effort_long, aes(x = effort_z, y = metric_lab,
                                  fill = metric_lab)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  scale_fill_manual(values = c("#2171b5", "#238b45", "#d94801", "#6a51a3")) +
  labs(x = "Standardized effort (z)", y = "",
       title = "Effort metric distributions (events)") +
  theme_nature +
  theme(legend.position = "none")

ggsave(file.path(TASK_ROOT, "figures", "fig_d02_effort_ridges.png"),
       p_d02, width = 9, height = 5, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_d02_effort_ridges.pdf"),
       p_d02, width = 9, height = 5)

# fig_d03: 相关矩阵热力图
corr_cols <- c("temp_grad_z", "prec_grad_z", "log_effort_visits_z",
               "effort_pc1_z", "climate_velocity_z", "mahalanobis_dist_z",
               "climate_exposure_z", "warming_rate_z")
corr_mat <- cor(risk_data[, ..corr_cols], use = "pairwise.complete.obs")

p_d03 <- ggcorrplot(corr_mat, hc.order = TRUE, type = "lower",
                     lab = TRUE, lab_size = 3,
                     colors = c("#2171b5", "white", "#d94801"),
                     title = "Correlation matrix: climate and effort metrics") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 13),
        axis.text = element_text(size = 8))

ggsave(file.path(TASK_ROOT, "figures", "fig_d03_correlation_matrix.png"),
       p_d03, width = 9, height = 8, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_d03_correlation_matrix.pdf"),
       p_d03, width = 9, height = 8)

# ── 3. Range map vs 原始temp_grad对比 ──────────────────────────────────
cat("生成Range map对比图\n")

# fig_d04: 新旧temp_grad散点图
p_d04 <- ggplot(risk_range[!is.na(temp_grad_range_z)],
                aes(x = temp_grad_z, y = temp_grad_range_z)) +
  geom_bin2d(bins = 60, alpha = 0.8) +
  scale_fill_viridis_c(option = "B", name = "Count") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red", linewidth = 0.5) +
  annotate("text", x = -3, y = 3,
           label = sprintf("r = %.3f",
    cor(risk_range$temp_grad_z, risk_range$temp_grad_range_z,
        use = "complete.obs")),
           size = 4, hjust = 0) +
  labs(x = "Original temp_grad_z (province set)",
       y = "Range map temp_grad_range_z",
       title = "Original vs range-map temperature gradient",
       subtitle = "Range map uses species distribution point-density weighting") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures", "fig_d04_temp_grad_comparison.png"),
       p_d04, width = 8, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_d04_temp_grad_comparison.pdf"),
       p_d04, width = 8, height = 7)

# ── 4. 模型结果可视化 ──────────────────────────────────────────────────
cat("生成模型结果图\n")

# fig_m01: 交互项HR跨模型雨林图（模拟ggdist风格）
# 读取各模型交互项系数
model_coefs <- fread(file.path(TASK_ROOT, "results",
  "table_advanced_climate_interaction_coefs.csv"))

if (nrow(model_coefs) > 0) {
  interact_coefs <- model_coefs[grepl(":", term) &
                                   !grepl("Intercept", term)]

  if (nrow(interact_coefs) > 0) {
    # 创建模型标签
    interact_coefs[, model_label := paste0(climate_label, " x ",
                                            effort_label)]

    p_m01 <- ggplot(interact_coefs,
                    aes(x = hr, y = reorder(model_label, hr),
                        color = climate_spec)) +
      geom_vline(xintercept = 1, linetype = "dashed",
                 color = "grey50", linewidth = 0.5) +
      geom_point(size = 2.5) +
      geom_errorbar(aes(xmin = hr_lower, xmax = hr_upper),
                    height = 0.3, linewidth = 0.6,
                    orientation = "y") +
      scale_color_manual(values = c("clim_orig" = "#d94801",
                                    "clim_vel" = "#2171b5",
                                    "clim_mahal" = "#6a51a3",
                                    "clim_exposure" = "#238b45",
                                    "clim_warming" = "#666666",
                                    "clim_prec_vel" = "#993404"),
                         name = "Climate metric") +
      labs(x = "Hazard ratio (interaction term)",
           y = "",
           title = "Climate x Effort interaction across model specifications",
           subtitle = "Point estimates with 95% CI") +
      theme_nature +
      theme(legend.position = "bottom")

    ggsave(file.path(TASK_ROOT, "figures",
                     "fig_m01_interaction_rainforest.png"),
           p_m01, width = 10, height = 8, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures",
                     "fig_m01_interaction_rainforest.pdf"),
           p_m01, width = 10, height = 8)
  }
}

# fig_m02: 气候x努力交互面
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  library(glmmTMB)

  risk_data[, species := factor(as.character(species))]
  risk_data[, province := factor(as.character(province))]

  fit_interact <- glmmTMB(
    event ~ year_c + temp_grad_z * log_effort_visits_z +
      (1|species) + (1|province),
    data = risk_data, family = binomial(link = "cloglog"))

  grid <- CJ(
    temp_grad_z = seq(-3, 3, length.out = 50),
    log_effort_visits_z = seq(-2, 3, length.out = 50),
    year_c = 0,
    species = factor("dummy", levels = levels(risk_data$species)),
    province = factor("dummy", levels = levels(risk_data$province))
  )
  grid[, pred := predict(fit_interact, newdata = grid,
                          type = "response",
                          allow.new.levels = TRUE)]

  p_m02 <- ggplot(grid, aes(x = temp_grad_z,
                             y = log_effort_visits_z,
                             z = pred)) +
    geom_contour_filled(bins = 12, alpha = 0.85) +
    scale_fill_viridis_d(option = "C", name = "Hazard\nprobability") +
    labs(x = "Temperature gradient (z)",
         y = "Observer visits (z)",
         title = "Climate x Effort interaction surface",
         subtitle = "Predicted hazard from glmmTMB model") +
    theme_nature

  ggsave(file.path(TASK_ROOT, "figures", "fig_m02_interaction_surface.png"),
         p_m02, width = 8, height = 7, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_m02_interaction_surface.pdf"),
         p_m02, width = 8, height = 7)
}

# ── 5. ML模型对比图 ──────────────────────────────────────────────────
cat("生成ML对比图\n")

# fig_m04: RF变量重要性 + SHAP并列
rf_vi <- fread(file.path(TASK_ROOT, "results",
                          "table_rf_variable_importance_detailed.csv"))

p_rf <- ggplot(rf_vi, aes(x = reorder(variable, importance),
                            y = importance, fill = category)) +
  geom_col(alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c("Climate" = "#d94801", "Effort" = "#2171b5",
                                "Year" = "#666666", "Interaction" = "#6a51a3",
                                "Other" = "#aaaaaa"), name = "Category") +
  labs(x = "", y = "Permutation importance",
       title = "Random Forest") +
  theme_nature +
  theme(legend.position = "none", plot.title =
          element_text(size = 11, face = "bold"))

p_m04 <- p_rf
ggsave(file.path(TASK_ROOT, "figures", "fig_m04_ml_comparison.png"),
       p_m04, width = 8, height = 5, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_m04_ml_comparison.pdf"),
       p_m04, width = 8, height = 5)

# ── 6. 未来预测综合图 ──────────────────────────────────────────────────
cat("生成未来预测综合图\n")

xgb_future <- fread(file.path(TASK_ROOT, "results",
  "table_xgboost_future_predictions.csv"))
rf_future <- fread(file.path(TASK_ROOT, "results",
  "table_rf_future_predictions.csv"))

# fig_f01: XGBoost vs RF 2050预测对比
xgb_2050 <- xgb_future[year == 2050,
  .(province, climate_scenario, effort_scenario,
    hazard_xgb = hazard_mean)]
rf_2050 <- rf_future[year == 2050,
  .(province, climate_scenario, effort_scenario,
    hazard_rf = hazard_mean)]

compare_2050 <- merge(xgb_2050, rf_2050,
  by = c("province", "climate_scenario", "effort_scenario"))

p_f01 <- ggplot(compare_2050[effort_scenario == "trend"],
                aes(x = hazard_xgb, y = hazard_rf,
                    color = climate_scenario)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50") +
  scale_color_manual(values = c("current" = "#666666",
                                "ssp245" = "#2171b5",
                                "ssp585" = "#d94801"),
                     name = "Climate scenario") +
  labs(x = "XGBoost predicted hazard",
       y = "Random Forest predicted hazard",
       title = "2050 provincial hazard: XGBoost vs Random Forest",
       subtitle = "Trend effort scenario") +
  theme_nature +
  theme(legend.position = "bottom")

ggsave(file.path(TASK_ROOT, "figures", "fig_f01_xgb_vs_rf_2050.png"),
       p_f01, width = 8, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_f01_xgb_vs_rf_2050.pdf"),
       p_f01, width = 8, height = 7)

# fig_f02: 气候情景敏感性（省级均值轨迹）
xgb_traj <- xgb_future[effort_scenario == "trend",
  .(hazard_mean = mean(hazard_mean, na.rm = TRUE)),
  by = .(year, climate_scenario)]

p_f02 <- ggplot(xgb_traj,
                aes(x = year, y = hazard_mean,
                    color = climate_scenario, group = climate_scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("current" = "#666666",
                                "ssp245" = "#2171b5",
                                "ssp585" = "#d94801"),
                     name = "Climate scenario") +
  labs(x = "Year", y = "Mean predicted hazard",
       title = "Temporal trajectory under climate scenarios",
       subtitle = "XGBoost prediction, trend effort, provincial mean") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures", "fig_f02_scenario_trajectory.png"),
       p_f02, width = 8, height = 5, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_f02_scenario_trajectory.pdf"),
       p_f02, width = 8, height = 5)

cat("\n=== 13_comprehensive_visualization.R 完成 ===\n")
