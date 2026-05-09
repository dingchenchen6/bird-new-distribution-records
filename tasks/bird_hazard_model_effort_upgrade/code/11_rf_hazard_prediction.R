#!/usr/bin/env Rscript
# 11_rf_hazard_prediction.R
# Random Forest hazard model training, variable importance, and future prediction
# Compare with XGBoost results

suppressPackageStartupMessages({
  library(data.table)
  library(ranger)
  library(ggplot2)
  library(patchwork)
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
cat("=== Random Forest hazard prediction ===\n")

risk_data <- fread(file.path(TASK_ROOT, "data", "hazard_risk_upgraded_complete_case.csv"))
clim_metrics <- fread(file.path(TASK_ROOT, "data", "climate_metrics_province_year.csv"))

risk_data[, year_c := year - 2013]

risk_data <- merge(risk_data,
                   clim_metrics[, .(province, year,
                                    climate_velocity_z, precip_velocity_z,
                                    climate_exposure_z, warming_rate_z,
                                    mahalanobis_dist_z)],
                   by = c("province", "year"), all.x = TRUE)

# 添加交互特征
risk_data[, temp_x_effort := temp_grad_z * log_effort_visits_z]
risk_data[, velocity_x_effort := climate_velocity_z * log_effort_visits_z]
risk_data[, mahal_x_effort := mahalanobis_dist_z * log_effort_visits_z]

feature_cols <- c("year_c", "temp_grad_z", "prec_grad_z",
                  "log_effort_visits_z", "effort_pc1_z",
                  "climate_velocity_z", "mahalanobis_dist_z",
                  "climate_exposure_z", "warming_rate_z",
                  "temp_x_effort", "velocity_x_effort", "mahal_x_effort")

rf_data <- risk_data[complete.cases(risk_data[, ..feature_cols])]
rf_data[, event := factor(event, levels = c("0", "1"))]

cat("RF 数据:", nrow(rf_data), "行, 事件:", sum(rf_data$event == "1"), "\n")

# ── 2. 训练 RF ──────────────────────────────────────────────────────────
cat("\n训练 Random Forest\n")

# 类别权重
n_pos <- sum(rf_data$event == "1")
n_neg <- sum(rf_data$event == "0")
class_w <- c("0" = 1, "1" = n_neg / n_pos)

rf_fit <- ranger(
  event ~ ., data = rf_data[, c(..feature_cols, "event")],
  num.trees = 1000,
  mtry = floor(sqrt(length(feature_cols))),
  min.node.size = 10,
  class.weights = class_w,
  importance = "permutation",
  seed = 42,
  verbose = FALSE
)

cat("RF OOB error:", sprintf("%.4f", rf_fit$prediction.error), "\n")

# ── 3. 变量重要性 ───────────────────────────────────────────────────────
cat("\n=== RF 变量重要性 ===\n")

vi <- data.table(
  variable = names(rf_fit$variable.importance),
  importance = as.numeric(rf_fit$variable.importance)
)
vi <- vi[order(-importance)]

vi[, category := fcase(
  variable %in% c("temp_grad_z", "prec_grad_z", "climate_velocity_z",
                  "mahalanobis_dist_z", "climate_exposure_z", "warming_rate_z"),
  "Climate",
  variable %like% "effort", "Effort",
  variable == "year_c", "Year",
  grepl("x_effort", variable), "Interaction",
  default = "Other"
)]

fwrite(vi, file.path(TASK_ROOT, "results", "table_rf_variable_importance_detailed.csv"))

cat("变量重要性排序:\n")
for (i in seq_len(nrow(vi))) {
  cat(sprintf("  %-25s: %.4f (%s)\n", vi$variable[i], vi$importance[i], vi$category[i]))
}

# 可视化
pal_varimp <- c("Climate" = "#d94801", "Effort" = "#2171b5",
                "Year" = "#666666", "Interaction" = "#6a51a3", "Other" = "#aaaaaa")

p_vi_rf <- ggplot(vi, aes(x = reorder(variable, importance), y = importance,
                            fill = category)) +
  geom_col(alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = pal_varimp, name = "Category") +
  labs(x = "", y = "Permutation importance",
       title = "Random Forest variable importance",
       subtitle = "Permutation-based importance from 1000-tree ranger model") +
  theme_nature +
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(fill = "white", color = "grey70"))

ggsave(file.path(TASK_ROOT, "figures", "fig_rf_variable_importance_detailed.png"),
       p_vi_rf, width = 9, height = 6, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_rf_variable_importance_detailed.pdf"),
       p_vi_rf, width = 9, height = 6)

# ── 4. 偏依赖图 (关键特征) ───────────────────────────────────────────────
cat("\n=== 偏依赖图 ===\n")

# 使用 iml 包计算偏依赖
if (requireNamespace("iml", quietly = TRUE)) {
  library(iml)

  X_df <- rf_data[, ..feature_cols]
  y_pred <- function(model, newdata) {
    predict(model, data = newdata)$predictions[, "1"]
  }

  predictor <- Predictor$new(rf_fit, data = X_df,
                              predict.fun = y_pred,
                              y = as.numeric(rf_data$event) - 1)

  # 选取 top 4 特征
  top4 <- vi$variable[1:4]

  pdp_list <- list()
  for (feat in top4) {
    pdp_feat <- tryCatch(FeatureEffect$new(predictor, feature = feat, method = "pdp"),
                         error = function(e) NULL)
    if (!is.null(pdp_feat)) {
      pdp_dt <- as.data.table(pdp_feat$results)
      setnames(pdp_dt, c(feat, ".value"), c("x_val", "y_val"))

      pdp_list[[feat]] <- ggplot(pdp_dt, aes(x = x_val, y = y_val)) +
        geom_line(color = "#2171b5", linewidth = 0.8) +
        labs(x = feat, y = "Predicted hazard",
             title = feat) +
        theme_nature +
        theme(plot.title = element_text(size = 10, face = "bold"))
    }
  }

  if (length(pdp_list) >= 2) {
    p_pdp_combined <- Reduce(`|`, pdp_list) +
      plot_annotation(title = "Partial dependence plots (Random Forest)",
                      theme = theme(plot.title = element_text(face = "bold", size = 14)))

    ggsave(file.path(TASK_ROOT, "figures", "fig_rf_partial_dependence.png"),
           p_pdp_combined, width = 14, height = 4, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures", "fig_rf_partial_dependence.pdf"),
           p_pdp_combined, width = 14, height = 4)
  }
} else {
  cat("  iml 包未安装，跳过偏依赖图\n")
}

# ── 5. 未来情景预测 ─────────────────────────────────────────────────────
cat("\n=== RF 未来情景预测 ===\n")

# 读取XGBoost构建的未来面板（如果存在）
if (file.exists(file.path(TASK_ROOT, "results", "table_xgboost_future_predictions.csv"))) {
  dt_future <- fread(file.path(TASK_ROOT, "results", "table_xgboost_future_predictions.csv"))
  cat("使用XGBoost相同的未来面板\n")
} else {
  # 自行构建（简化版）
  effort_panel <- fread(file.path(TASK_ROOT, "data", "effort_panel_upgraded.csv"))
  current_effort <- effort_panel[year == 2024, .(province, log_effort_visits_z)]

  provinces <- unique(risk_data$province)
  dt_future <- CJ(province = provinces, year = c(2030, 2035, 2040, 2045, 2050),
                  climate_scenario = c("current", "ssp245", "ssp585"),
                  effort_scenario = c("baseline", "trend", "doubled"))

  dt_future[, year_c := year - 2013]
  dt_future <- merge(dt_future, current_effort, by = "province", all.x = TRUE)

  dt_future[, temp_grad_z := risk_data[, mean(temp_grad_z, na.rm = TRUE)]]
  dt_future[, prec_grad_z := 0]
  dt_future[, log_effort_visits_z := log_effort_visits_z]
  dt_future[, effort_pc1_z := risk_data[, mean(effort_pc1_z, na.rm = TRUE)]]
  dt_future[, climate_velocity_z := risk_data[, mean(climate_velocity_z, na.rm = TRUE)]]
  dt_future[, mahalanobis_dist_z := risk_data[, mean(mahalanobis_dist_z, na.rm = TRUE)]]
  dt_future[, climate_exposure_z := risk_data[, mean(climate_exposure_z, na.rm = TRUE)]]
  dt_future[, warming_rate_z := risk_data[, mean(warming_rate_z, na.rm = TRUE)]]
  dt_future[, temp_x_effort := temp_grad_z * log_effort_visits_z]
  dt_future[, velocity_x_effort := climate_velocity_z * log_effort_visits_z]
  dt_future[, mahal_x_effort := mahalanobis_dist_z * log_effort_visits_z]
}

# 预测
X_future <- as.matrix(dt_future[, ..feature_cols])
rf_pred <- predict(rf_fit, data = X_future)
# ranger分类返回predictions为矩阵或factor，处理两种情况
if (is.matrix(rf_pred$predictions)) {
  dt_future[, predicted_hazard_rf := rf_pred$predictions[, "1"]]
} else {
  # 如果返回概率需要用predict(type="response")
  rf_pred_prob <- predict(rf_fit, data = X_future, type = "response")
  if (is.matrix(rf_pred_prob$predictions)) {
    dt_future[, predicted_hazard_rf := rf_pred_prob$predictions[, "1"]]
  } else {
    dt_future[, predicted_hazard_rf := as.numeric(rf_pred_prob$predictions == "1")]
  }
}

cat("RF 未来预测完成\n")

# 保存
dt_prov_future_rf <- dt_future[, .(hazard_mean = mean(predicted_hazard_rf, na.rm = TRUE)),
                                 by = .(province, year, climate_scenario, effort_scenario)]
fwrite(dt_prov_future_rf,
       file.path(TASK_ROOT, "results", "table_rf_future_predictions.csv"))

# ── 6. ML 模型对比 ──────────────────────────────────────────────────────
cat("\n=== ML vs GLMM 对比 ===\n")

comparison <- data.table(
  model = c("glmmTMB (M4, Spec B)", "Random Forest", "XGBoost"),
  type = c("Statistical", "ML", "ML"),
  handles_random_effects = c("Yes", "No", "No"),
  captures_interaction = c("Explicit", "Implicit", "Implicit"),
  interpretability = c("High (coefficients)", "Medium (PDP/VI)", "High (SHAP)"),
  key_strength = c("Inference, causal interpretation",
                    "Non-linear patterns, robustness",
                    "Prediction accuracy, SHAP explanation")
)

fwrite(comparison, file.path(TASK_ROOT, "results", "table_ml_vs_glmm_comparison.csv"))

print(comparison)

cat("\n=== 11_rf_hazard_prediction.R 完成 ===\n")
