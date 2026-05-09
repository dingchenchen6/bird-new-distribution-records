#!/usr/bin/env Rscript
# 09_variance_partitioning.R
# 方差分解：气候变化、调查努力、两者交互对新纪录的量化贡献
# 随机森林变量重要性
# Variance partitioning: climate vs effort vs interaction
# Random forest variable importance

suppressPackageStartupMessages({
  library(data.table)
  library(glmmTMB)
  library(ranger)
  library(ggplot2)
  library(patchwork)
  library(here)
})

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

# ── 配色 ─────────────────────────────────────────────────────────────────
pal_contrib <- c(
  "Climate" = "#d94801",
  "Effort" = "#2171b5",
  "Interaction" = "#6a51a3",
  "Year" = "#666666",
  "Random effects" = "#aaaaaa"
)

theme_nature <- theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey70", linewidth = 0.4),
    legend.position = "bottom", legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold")
  )

# ── 1. 读取数据 ──────────────────────────────────────────────────────────
cat("=== 方差分解与变量重要性 ===\n")

risk_data <- fread(file.path(TASK_ROOT, "data", "hazard_risk_upgraded_complete_case.csv"))
clim_metrics <- fread(file.path(TASK_ROOT, "data", "climate_metrics_province_year.csv"))

risk_data[, year_c := year - 2013]
risk_data[, species  := factor(species)]
risk_data[, province := factor(province)]

# 合并高级气候指标
risk_data <- merge(risk_data,
                   clim_metrics[, .(province, year,
                                    climate_velocity_z, precip_velocity_z,
                                    climate_exposure_z, warming_rate_z,
                                    mahalanobis_dist_z)],
                   by = c("province", "year"), all.x = TRUE)

cat("数据行数:", nrow(risk_data), "  事件数:", sum(risk_data$event), "\n")

# ── 2. 拟合嵌套模型序列（方差分解）──────────────────────────────────────
cat("\n=== 拟合嵌套模型序列 ===\n")

# 使用 Spec B (visits) 作为主要努力指标
# 方差分解逻辑：
# 完整模型 = year + temp_grad * effort + (1|sp) + (1|prov)
# R²_climate = R²(year + temp_grad + (RE)) - R²(year + (RE))
# R²_effort = R²(year + effort + (RE)) - R²(year + (RE))
# R²_interaction = R²(full) - R²(year + temp_grad + effort + (RE))
# R²_joint_climate_effort 需要通过模型比较分解

# 使用条件 R² (Nakagawa & Schielzeth 2013) 和边际 R²
# marginal R² = 固定效应方差 / 总方差
# conditional R² = (固定 + 随机) / 总方差

compute_r2 <- function(fit) {
  tryCatch({
    # 从 glmmTMB 提取方差分量
    vc <- VarCorr(fit)
    # 固定效应方差
    X <- model.matrix(fit)
    beta <- fixef(fit)$cond
    fix_var <- var(as.numeric(X %*% beta))

    # 随机效应方差
    re_var <- sum(sapply(vc$cond, function(x) sum(diag(x))))

    # 分布特定方差 (cloglog: pi^2/6)
    dist_var <- pi^2 / 6

    # 边际 R²
    marg_r2 <- fix_var / (fix_var + re_var + dist_var)
    # 条件 R²
    cond_r2 <- (fix_var + re_var) / (fix_var + re_var + dist_var)

    list(marginal = marg_r2, conditional = cond_r2,
         fix_var = fix_var, re_var = re_var)
  }, error = function(e) {
    list(marginal = NA, conditional = NA, fix_var = NA, re_var = NA)
  })
}

# 嵌套模型序列
nested_models <- list(
  M0_null = as.formula("event ~ year_c + (1|species) + (1|province)"),
  M1_climate = as.formula("event ~ year_c + temp_grad_z + (1|species) + (1|province)"),
  M2_effort = as.formula("event ~ year_c + log_effort_visits_z + (1|species) + (1|province)"),
  M3_additive = as.formula("event ~ year_c + temp_grad_z + log_effort_visits_z + (1|species) + (1|province)"),
  M4_full = as.formula("event ~ year_c + temp_grad_z * log_effort_visits_z + (1|species) + (1|province)")
)

r2_results <- list()

for (mname in names(nested_models)) {
  cat(sprintf("  Fitting %s ... ", mname))
  fit <- tryCatch(
    glmmTMB(nested_models[[mname]], data = risk_data,
            family = binomial(link = "cloglog")),
    error = function(e) { cat("FAILED\n"); NULL }
  )
  if (is.null(fit)) next

  r2 <- compute_r2(fit)
  aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)
  cat(sprintf("mR²=%.4f, cR²=%.4f, AIC=%.1f\n",
              r2$marginal, r2$conditional, aic_val))

  r2_results[[mname]] <- data.table(
    model = mname, marginal_r2 = r2$marginal,
    conditional_r2 = r2$conditional,
    fix_var = r2$fix_var, re_var = r2$re_var,
    aic = aic_val
  )
}

dt_r2 <- rbindlist(r2_results, fill = TRUE)
fwrite(dt_r2, file.path(TASK_ROOT, "results", "table_variance_decomposition_r2.csv"))

# ── 3. 方差分解计算 ──────────────────────────────────────────────────────
cat("\n=== 方差分解 ===\n")

if (nrow(dt_r2) >= 5) {
  r0  <- dt_r2[model == "M0_null", marginal_r2]
  r1  <- dt_r2[model == "M1_climate", marginal_r2]
  r2  <- dt_r2[model == "M2_effort", marginal_r2]
  r3  <- dt_r2[model == "M3_additive", marginal_r2]
  r4  <- dt_r2[model == "M4_full", marginal_r2]

  # 边际 R² 增量
  delta_year   <- r0  # year 贡献
  delta_clim   <- r1 - r0   # temp_grad 独立贡献
  delta_effort <- r2 - r0   # effort 独立贡献
  delta_joint  <- r3 - r1 - delta_effort  # 联合贡献（加法模型中的重叠）
  delta_interact <- r4 - r3  # 交互项贡献

  cat(sprintf("  Year baseline:      %.4f (%.1f%%)\n", delta_year, 100 * delta_year / r4))
  cat(sprintf("  Climate (unique):   %.4f (%.1f%%)\n", delta_clim, 100 * delta_clim / r4))
  cat(sprintf("  Effort (unique):    %.4f (%.1f%%)\n", delta_effort, 100 * delta_effort / r4))
  cat(sprintf("  Joint overlap:      %.4f (%.1f%%)\n", delta_joint, 100 * delta_joint / r4))
  cat(sprintf("  Interaction:        %.4f (%.1f%%)\n", delta_interact, 100 * delta_interact / r4))
  cat(sprintf("  Total marginal R²:  %.4f\n", r4))

  # 可视化
  vd_dt <- data.table(
    component = c("Year", "Climate\n(unique)", "Effort\n(unique)",
                  "Joint\noverlap", "Interaction"),
    delta_r2 = c(delta_year, delta_clim, delta_effort,
                 delta_joint, delta_interact),
    pct = 100 * c(delta_year, delta_clim, delta_effort,
                   delta_joint, delta_interact) / r4,
    fill = c("Year", "Climate", "Effort", "Interaction", "Interaction")
  )

  p_vd <- ggplot(vd_dt, aes(x = reorder(component, -delta_r2), y = delta_r2,
                              fill = fill)) +
    geom_col(alpha = 0.85, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.2f%%", pct)), vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = pal_contrib) +
    labs(x = "", y = expression(Delta~marginal~R^2),
         title = "Variance decomposition: climate, effort, and interaction",
         subtitle = "Incremental marginal R² from nested model sequence (Spec B: observer visits)") +
    theme_nature

  ggsave(file.path(TASK_ROOT, "figures", "fig_variance_decomposition.png"),
         p_vd, width = 9, height = 5, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_variance_decomposition.pdf"),
         p_vd, width = 9, height = 5)
}

# ── 4. 随机森林变量重要性 ────────────────────────────────────────────────
cat("\n=== 随机森林变量重要性 ===\n")

# 准备RF数据（不含随机效应，RF不处理混合模型结构）
rf_data <- risk_data[!is.na(temp_grad_z) &
                      !is.na(log_effort_visits_z) &
                      !is.na(climate_velocity_z) &
                      !is.na(mahalanobis_dist_z) &
                      !is.na(warming_rate_z),
  .(event, year_c, temp_grad_z, prec_grad_z,
    log_effort_visits_z, effort_pc1_z, log_effort_days_z,
    climate_velocity_z, mahalanobis_dist_z,
    climate_exposure_z, warming_rate_z)]

rf_data[, event := factor(event, levels = c("0", "1"))]

cat("RF 数据:", nrow(rf_data), "行, 事件:", sum(rf_data$event == "1"), "\n")

# 由于事件率极低（~4%），使用 class weights
cat("训练随机森林 ...\n")

rf_fit <- ranger(
  event ~ ., data = rf_data,
  num.trees = 1000,
  mtry = floor(sqrt(ncol(rf_data) - 1)),
  min.node.size = 10,
  class.weights = c(1, sum(rf_data$event == "0") / sum(rf_data$event == "1")),
  importance = "permutation",
  seed = 42,
  verbose = FALSE
)

cat("RF OOB error:", sprintf("%.4f", rf_fit$prediction.error), "\n")

# 变量重要性
vi <- data.table(
  variable = names(rf_fit$variable.importance),
  importance = as.numeric(rf_fit$variable.importance)
)
vi <- vi[order(-importance)]

# 分类变量
vi[, category := fcase(
  variable %in% c("temp_grad_z", "prec_grad_z", "climate_velocity_z",
                  "mahalanobis_dist_z", "climate_exposure_z", "warming_rate_z"),
  "Climate",
  variable %like% "effort", "Effort",
  variable == "year_c", "Year",
  default = "Other"
)]

fwrite(vi, file.path(TASK_ROOT, "results", "table_rf_variable_importance.csv"))

cat("\n变量重要性排序:\n")
for (i in seq_len(nrow(vi))) {
  cat(sprintf("  %-25s: %.4f (%s)\n", vi$variable[i], vi$importance[i], vi$category[i]))
}

# 可视化
pal_varimp <- c("Climate" = "#d94801", "Effort" = "#2171b5",
                "Year" = "#666666", "Other" = "#aaaaaa")

p_vi <- ggplot(vi, aes(x = reorder(variable, importance), y = importance,
                        fill = category)) +
  geom_col(alpha = 0.85, show.legend = TRUE) +
  coord_flip() +
  scale_fill_manual(values = pal_varimp, name = "Category") +
  labs(x = "", y = "Permutation importance",
       title = "Random Forest variable importance",
       subtitle = "Permutation-based importance from 1000-tree ranger model") +
  theme_nature +
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(fill = "white", color = "grey70"))

ggsave(file.path(TASK_ROOT, "figures", "fig_rf_variable_importance.png"),
       p_vi, width = 9, height = 6, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_rf_variable_importance.pdf"),
       p_vi, width = 9, height = 6)

# ── 5. 跨4种努力指标的方差分解 ─────────────────────────────────────────
cat("\n=== 跨努力指标方差分解 ===\n")

effort_vars <- c("log_effort_record_z", "log_effort_visits_z",
                 "effort_pc1_z", "log_effort_days_z")
effort_labels <- c("Record-based", "Observer visits",
                   "PCA composite", "Birding days")

vd_cross <- list()

for (i in seq_along(effort_vars)) {
  ev <- effort_vars[i]
  el <- effort_labels[i]
  cat(sprintf("  Spec %s ... ", el))

  fml_add <- as.formula(paste0("event ~ year_c + temp_grad_z + ", ev,
                                " + (1|species) + (1|province)"))
  fml_int <- as.formula(paste0("event ~ year_c + temp_grad_z * ", ev,
                                " + (1|species) + (1|province)"))

  fit_add <- tryCatch(glmmTMB(fml_add, data = risk_data,
                               family = binomial(link = "cloglog")),
                      error = function(e) NULL)
  fit_int <- tryCatch(glmmTMB(fml_int, data = risk_data,
                               family = binomial(link = "cloglog")),
                      error = function(e) NULL)

  if (is.null(fit_add) || is.null(fit_int)) { cat("FAILED\n"); next }

  r2_add <- compute_r2(fit_add)
  r2_int <- compute_r2(fit_int)
  delta_int <- r2_int$marginal - r2_add$marginal

  cat(sprintf("interaction ΔR²=%.4f\n", delta_int))

  vd_cross[[el]] <- data.table(
    effort_spec = el, effort_var = ev,
    additive_r2 = r2_add$marginal,
    interaction_r2 = r2_int$marginal,
    delta_interaction_r2 = delta_int
  )
}

if (length(vd_cross) > 0) {
  dt_vd_cross <- rbindlist(vd_cross)
  fwrite(dt_vd_cross,
         file.path(TASK_ROOT, "results", "table_cross_effort_variance_decomposition.csv"))

  # 可视化
  p_vd_cross <- ggplot(dt_vd_cross,
                        aes(x = effort_spec, y = delta_interaction_r2,
                            fill = effort_spec)) +
    geom_col(alpha = 0.85, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.4f", delta_interaction_r2)),
              vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("#2171b5", "#238b45", "#d94801", "#6a51a3")) +
    labs(x = "", y = expression(Delta~marginal~R^2~(interaction)),
         title = "Interaction contribution across effort specifications",
         subtitle = "Incremental R² from adding climate × effort interaction") +
    theme_nature

  ggsave(file.path(TASK_ROOT, "figures", "fig_interaction_contribution.png"),
         p_vd_cross, width = 8, height = 5, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_interaction_contribution.pdf"),
         p_vd_cross, width = 8, height = 5)
}

cat("\n=== 09_variance_partitioning.R 完成 ===\n")
