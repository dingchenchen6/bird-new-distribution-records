#!/usr/bin/env Rscript
# 10_xgboost_shap_prediction.R
# XGBoost + SHAP: 训练、解释、未来情景预测
# XGBoost training, SHAP interpretation, future scenario prediction

suppressPackageStartupMessages({
  library(data.table)
  library(xgboost)
  library(shapviz)
  library(ggplot2)
  library(patchwork)
  library(here)
})

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

# ── 配色 ─────────────────────────────────────────────────────────────────
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
cat("=== XGBoost + SHAP ===\n")

risk_data <- fread(file.path(TASK_ROOT, "data", "hazard_risk_upgraded_complete_case.csv"))
clim_metrics <- fread(file.path(TASK_ROOT, "data", "climate_metrics_province_year.csv"))

risk_data[, year_c := year - 2013]

# 合并高级气候指标
risk_data <- merge(risk_data,
                   clim_metrics[, .(province, year,
                                    climate_velocity_z, precip_velocity_z,
                                    climate_exposure_z, warming_rate_z,
                                    mahalanobis_dist_z)],
                   by = c("province", "year"), all.x = TRUE)

# ── 2. 准备XGBoost数据 ──────────────────────────────────────────────────
cat("准备XGBoost数据\n")

feature_cols <- c("year_c", "temp_grad_z", "prec_grad_z",
                  "log_effort_visits_z", "effort_pc1_z",
                  "climate_velocity_z", "mahalanobis_dist_z",
                  "climate_exposure_z", "warming_rate_z")

# 添加交互项特征
risk_data[, temp_x_effort := temp_grad_z * log_effort_visits_z]
risk_data[, velocity_x_effort := climate_velocity_z * log_effort_visits_z]
risk_data[, mahal_x_effort := mahalanobis_dist_z * log_effort_visits_z]

feature_cols <- c(feature_cols,
                  "temp_x_effort", "velocity_x_effort", "mahal_x_effort")

# 完整案例
xgb_data <- risk_data[complete.cases(risk_data[, ..feature_cols])]

cat("数据行数:", nrow(xgb_data), "  事件数:", sum(xgb_data$event), "\n")

X <- as.matrix(xgb_data[, ..feature_cols])
y <- xgb_data$event

# 类别权重
pos_weight <- sum(y == 0) / sum(y == 1)
cat(sprintf("正类权重: %.1f\n", pos_weight))

# DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

# ── 3. 5-fold CV调参 ────────────────────────────────────────────────────
cat("\n5-fold CV调参\n")

# 使用logloss作为调参指标（AUC不触发早停）
cv_params_grid <- expand.grid(
  max_depth = c(4, 6, 8),
  eta = c(0.01, 0.05, 0.1),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.8),
  colsample_bytree = c(0.8)
)

cat(sprintf("参数组合: %d\n", nrow(cv_params_grid)))

best_score <- Inf  # logloss越低越好
best_params <- list()

for (i in seq_len(nrow(cv_params_grid))) {
  p <- cv_params_grid[i, ]
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = p$max_depth,
    eta = p$eta,
    min_child_weight = p$min_child_weight,
    subsample = p$subsample,
    colsample_bytree = p$colsample_bytree,
    scale_pos_weight = pos_weight
  )

  cv_result <- tryCatch(
    xgb.cv(params, dtrain, nrounds = 500, nfold = 5,
           early_stopping_rounds = 30, verbose = 0),
    error = function(e) { cat("  CV error:", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(cv_result)) next

  best_iter <- cv_result$best_iteration
  if (is.null(best_iter) || length(best_iter) == 0 || !is.finite(best_iter)) {
    # 如果早停未触发，取最后一轮
    best_iter <- 500
  }

  # 提取logloss
  best_val <- tryCatch({
    elog <- as.data.frame(cv_result$evaluation_log)
    ll_col <- grep("test_logloss_mean", names(elog), value = TRUE)
    if (length(ll_col) == 0) ll_col <- grep("test.*mean", names(elog), value = TRUE)[1]
    if (length(ll_col) == 0) NA_real_ else as.numeric(elog[best_iter, ll_col])
  }, error = function(e) NA_real_)

  if (is.numeric(best_val) && length(best_val) == 1 &&
        !is.na(best_val) && is.finite(best_val) && best_val < best_score) {
    best_score <- best_val
    best_params <- c(params, list(nrounds = best_iter))
  }

  if (i %% 5 == 0) cat(sprintf("  %d/%d done, best logloss: %.4f\n",
                                i, nrow(cv_params_grid), best_score))
}

if (length(best_params) == 0) {
  cat("CV调参失败，使用默认参数\n")
  best_params <- list(objective = "binary:logistic", eval_metric = "logloss",
                      max_depth = 6, eta = 0.05, min_child_weight = 5,
                      subsample = 0.8, colsample_bytree = 0.8,
                      scale_pos_weight = pos_weight, nrounds = 200)
}
cat(sprintf("最佳参数: max_depth=%d, eta=%.3f, min_child=%d, nrounds=%d, logloss=%.4f\n",
            best_params$max_depth, best_params$eta,
            best_params$min_child_weight, best_params$nrounds, best_score))

# ── 4. 训练最终模型 ─────────────────────────────────────────────────────
cat("\n训练最终XGBoost模型\n")

final_params <- best_params
final_params$scale_pos_weight <- pos_weight
nrounds <- final_params$nrounds
final_params$nrounds <- NULL

xgb_model <- xgb.train(final_params, dtrain, nrounds = nrounds, verbose = 0)

cat("模型训练完成\n")

# ── 5. SHAP分析 ──────────────────────────────────────────────────────────
cat("\n=== SHAP分析 ===\n")

# 采样（SHAP计算开销大）
set.seed(42)
n_sample <- min(2000, nrow(X))
sample_idx <- sample(seq_len(nrow(X)), n_sample)
X_sample <- X[sample_idx, ]

shp <- tryCatch(
  shapviz(xgb_model, X_pred = X_sample),
  error = function(e) { cat("  shapviz error:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(shp)) {
  # SHAP摘要图（蜂群图）
  p_shap_summary <- tryCatch(
    sv_importance(shp, kind = "beeswarm", max_display = 12) +
      labs(title = "SHAP feature importance",
           subtitle = "Beeswarm plot: feature value and direction of impact"),
    error = function(e) { cat("  beeswarm error:", conditionMessage(e), "\n"); NULL }
  )

  if (!is.null(p_shap_summary)) {
    ggsave(file.path(TASK_ROOT, "figures", "fig_shap_beeswarm.png"),
           p_shap_summary, width = 10, height = 7, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures", "fig_shap_beeswarm.pdf"),
           p_shap_summary, width = 10, height = 7)
  }

  # SHAP条形图
  p_shap_bar <- tryCatch(
    sv_importance(shp, kind = "bar", max_display = 12) +
      labs(title = "Mean |SHAP| feature importance"),
    error = function(e) { cat("  bar error:", conditionMessage(e), "\n"); NULL }
  )

  if (!is.null(p_shap_bar)) {
    ggsave(file.path(TASK_ROOT, "figures", "fig_shap_importance_bar.png"),
           p_shap_bar, width = 9, height = 6, dpi = 300)
    ggsave(file.path(TASK_ROOT, "figures", "fig_shap_importance_bar.pdf"),
           p_shap_bar, width = 9, height = 6)
  }

  # SHAP依赖图（top 4特征）
  shap_values <- tryCatch(as.matrix(shp$shap_values),
                           error = function(e) NULL)
  if (!is.null(shap_values)) {
    mean_abs_shap <- colMeans(abs(shap_values))
    top_features <- names(sort(mean_abs_shap, decreasing = TRUE))[1:4]

    p_dep_list <- list()
    for (feat in top_features) {
      p_dep_list[[feat]] <- tryCatch(
        sv_dependence(shp, v = feat) +
          labs(title = feat) +
          theme_nature +
          theme(plot.title = element_text(size = 10, face = "bold")),
        error = function(e) NULL
      )
    }
    p_dep_list <- p_dep_list[!sapply(p_dep_list, is.null)]

    if (length(p_dep_list) >= 2) {
      p_dep_combined <- Reduce(`|`, p_dep_list) +
        plot_annotation(title = "SHAP dependence plots (top 4 features)",
                        theme = theme(plot.title =
                                        element_text(face = "bold", size = 14)))

      ggsave(file.path(TASK_ROOT, "figures", "fig_shap_dependence.png"),
             p_dep_combined, width = 14, height = 4, dpi = 300)
      ggsave(file.path(TASK_ROOT, "figures", "fig_shap_dependence.pdf"),
             p_dep_combined, width = 14, height = 4)
    }

    # 保存SHAP值
    shap_dt <- as.data.table(shap_values)
    shap_dt[, sample_id := sample_idx]
    fwrite(shap_dt,
           file.path(TASK_ROOT, "results", "table_shap_values.csv"))
  }

  # SHAP交互值（气候×努力）
  cat("计算SHAP交互值...\n")
  shp_interact <- tryCatch(
    shapviz(xgb_model, X_pred = X_sample[1:500, ], interactions = TRUE),
    error = function(e) { cat("  interaction error:", conditionMessage(e), "\n"); NULL }
  )

  if (!is.null(shp_interact)) {
    p_shap_interact <- tryCatch(
      sv_interaction(shp_interact, v = "temp_grad_z", max_display = 8) +
        labs(title = "SHAP interaction: temp_grad_z with other features"),
      error = function(e) NULL
    )

    if (!is.null(p_shap_interact)) {
      ggsave(file.path(TASK_ROOT, "figures", "fig_shap_interaction_temp.png"),
             p_shap_interact, width = 9, height = 6, dpi = 300)
      ggsave(file.path(TASK_ROOT, "figures", "fig_shap_interaction_temp.pdf"),
             p_shap_interact, width = 9, height = 6)
    }
  }
} else {
  cat("  SHAP分析跳过（shapviz失败）\n")
}

# ── 6. 未来情景预测 ─────────────────────────────────────────────────────
cat("\n=== 未来情景预测 ===\n")

# 读取当前（2024）省级数据作为基线
effort_panel <- fread(file.path(TASK_ROOT, "data", "effort_panel_upgraded.csv"))
current_effort <- effort_panel[year == 2024,
                                .(province, log_effort_visits_z, effort_pc1_z)]

# 未来气候情景：假设各省份升温梯度增加
# 3种努力情景：不变、线性增长、翻倍
# 3种气候情景：当前、SSP245、SSP585

# 省份列表
provinces <- unique(risk_data$province)

# 构建预测面板
future_years <- c(2030, 2035, 2040, 2045, 2050)
scenarios <- expand.grid(
  province = provinces,
  year = future_years,
  climate_scenario = c("current", "ssp245", "ssp585"),
  effort_scenario = c("baseline", "trend", "doubled"),
  stringsAsFactors = FALSE
)
dt_future <- as.data.table(scenarios)
dt_future[, year_c := year - 2013]

# 努力情景
dt_future <- merge(dt_future, current_effort, by = "province", all.x = TRUE)

# effort_trend: 2024值 + 线性增长 (2002-2024趋势)
effort_trends <- effort_panel[year >= 2002 & year <= 2024,
                               .(effort_trend_z = coef(lm(log_effort_visits_z ~ year))[2]),
                               by = province]
dt_future <- merge(dt_future, effort_trends, by = "province", all.x = TRUE)

dt_future[, log_effort_visits_z_future := fcase(
  effort_scenario == "baseline", log_effort_visits_z,
  effort_scenario == "trend", log_effort_visits_z + effort_trend_z * (year - 2024),
  effort_scenario == "doubled", log_effort_visits_z * 2
)]

# 气候情景：temp_grad_z 增量
# SSP245: ~+0.3°C by 2050; SSP585: ~+0.8°C by 2050
temp_grad_z_2024_mean <- risk_data[, mean(temp_grad_z, na.rm = TRUE)]
temp_grad_z_2024_sd <- risk_data[, sd(temp_grad_z, na.rm = TRUE)]

dt_future[, temp_grad_z_future := fcase(
  climate_scenario == "current", temp_grad_z_2024_mean,
  climate_scenario == "ssp245", temp_grad_z_2024_mean + 0.3 / temp_grad_z_2024_sd * (year - 2024) / 26,
  climate_scenario == "ssp585", temp_grad_z_2024_mean + 0.8 / temp_grad_z_2024_sd * (year - 2024) / 26
)]

# 其他气候指标（保持2024水平，简化）
dt_future[, prec_grad_z := 0]
dt_future[, climate_velocity_z := risk_data[, mean(climate_velocity_z, na.rm = TRUE)]]
dt_future[, mahalanobis_dist_z := risk_data[, mean(mahalanobis_dist_z, na.rm = TRUE)]]
dt_future[, climate_exposure_z := risk_data[, mean(climate_exposure_z, na.rm = TRUE)]]
dt_future[, warming_rate_z := risk_data[, mean(warming_rate_z, na.rm = TRUE)]]
dt_future[, effort_pc1_z := risk_data[, mean(effort_pc1_z, na.rm = TRUE)]]

# 交互项
dt_future[, temp_grad_z := temp_grad_z_future]
dt_future[, log_effort_visits_z := log_effort_visits_z_future]
dt_future[, temp_x_effort := temp_grad_z * log_effort_visits_z]
dt_future[, velocity_x_effort := climate_velocity_z * log_effort_visits_z]
dt_future[, mahal_x_effort := mahalanobis_dist_z * log_effort_visits_z]

# 预测
X_future <- as.matrix(dt_future[, ..feature_cols])
dt_future[, predicted_hazard := predict(xgb_model, X_future, type = "response")]

cat("未来预测完成\n")
cat(sprintf("预测值范围: [%.4f, %.4f]\n",
            dt_future[, min(predicted_hazard, na.rm = TRUE)],
            dt_future[, max(predicted_hazard, na.rm = TRUE)]))

# 省级汇总：按province × year × scenario取均值
dt_prov_future <- dt_future[, .(hazard_mean = mean(predicted_hazard, na.rm = TRUE)),
                              by = .(province, year, climate_scenario, effort_scenario)]

fwrite(dt_prov_future,
       file.path(TASK_ROOT, "results", "table_xgboost_future_predictions.csv"))

# 保存模型
xgb.save(xgb_model, file.path(TASK_ROOT, "results", "xgboost_model.model"))

# ── 7. 未来热点可视化 ────────────────────────────────────────────────────
cat("\n=== 未来热点可视化 ===\n")

# 2050年热点省份排名
hotspot_2050 <- dt_prov_future[year == 2050,
                                .(hazard_mean = mean(hazard_mean)),
                                by = .(province, climate_scenario, effort_scenario)]

p_hotspot <- ggplot(hotspot_2050[effort_scenario == "trend"],
                    aes(x = reorder(province, hazard_mean),
                        y = hazard_mean, fill = climate_scenario)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("current" = "#666666",
                                "ssp245" = "#2171b5",
                                "ssp585" = "#d94801"),
                    name = "Climate scenario") +
  labs(x = "", y = "Predicted hazard probability",
       title = "2050 provincial new-record hotspots",
       subtitle = "XGBoost prediction under trend effort growth") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures", "fig_xgboost_2050_hotspot.png"),
       p_hotspot, width = 10, height = 10, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_xgboost_2050_hotspot.pdf"),
       p_hotspot, width = 10, height = 10)

# 时间轨迹图（top 10省份）
top10_prov <- hotspot_2050[climate_scenario == "ssp585" & effort_scenario == "trend",
                           ][order(-hazard_mean)][1:10, province]

p_trajectory <- ggplot(dt_prov_future[province %in% top10_prov &
                                       effort_scenario == "trend"],
                       aes(x = year, y = hazard_mean,
                           color = climate_scenario, group = climate_scenario)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~ province, scales = "free_y", ncol = 5) +
  scale_color_manual(values = c("current" = "#666666",
                                 "ssp245" = "#2171b5",
                                 "ssp585" = "#d94801"),
                     name = "Climate") +
  labs(x = "Year", y = "Predicted hazard",
       title = "Temporal trajectory of top-10 hotspot provinces",
       subtitle = "XGBoost prediction under trend effort growth") +
  theme_nature +
  theme(strip.text = element_text(size = 8))

ggsave(file.path(TASK_ROOT, "figures", "fig_xgboost_trajectory.png"),
       p_trajectory, width = 14, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_xgboost_trajectory.pdf"),
       p_trajectory, width = 14, height = 7)

cat("\n=== 10_xgboost_shap_prediction.R 完成 ===\n")
