#!/usr/bin/env Rscript
# 14_vif_correlation_proxy_selection.R
# 相关性+VIF分析，筛选气候/努力代理变量
# 各代理变量单独跑模型，系统比较
# Correlation + VIF analysis for proxy variable selection

suppressPackageStartupMessages({
  library(data.table)
  library(glmmTMB)
  library(car)
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
cat("=== 相关性 + VIF + 代理变量筛选 ===\n")

risk_data <- fread(file.path(TASK_ROOT, "data",
                              "hazard_risk_upgraded_complete_case.csv"))
clim_metrics <- fread(file.path(TASK_ROOT, "data",
                                 "climate_metrics_province_year.csv"))

risk_data[, year_c := year - 2013]
risk_data <- merge(risk_data,
  clim_metrics[, .(province, year, climate_velocity_z,
    precip_velocity_z, climate_exposure_z,
    warming_rate_z, mahalanobis_dist_z)],
  by = c("province", "year"), all.x = TRUE)

# ── 2. 相关性矩阵 ──────────────────────────────────────────────────────
cat("\n=== 相关性矩阵 ===\n")

# 气候指标
clim_cols <- c("temp_grad_z", "prec_grad_z",
               "climate_velocity_z", "precip_velocity_z",
               "climate_exposure_z", "warming_rate_z",
               "mahalanobis_dist_z")
clim_labels <- c("Temp gradient\n(legacy)",
                 "Prec gradient\n(legacy)",
                 "Climate\nvelocity",
                 "Precip\nvelocity",
                 "Climate\nexposure",
                 "Warming\nrate",
                 "Mahalanobis\ndistance")

cor_clim <- cor(risk_data[, ..clim_cols], use = "pairwise.complete.obs")
cat("气候指标相关矩阵:\n")
print(round(cor_clim, 3))

# 努力指标
effort_cols <- c("log_effort_record_z", "log_effort_visits_z",
                 "log_effort_observers_z", "log_effort_days_z",
                 "effort_pc1_z")
effort_labels <- c("Record-\nbased", "Observer\nvisits",
                   "Observer\ncount", "Birding\ndays",
                   "PCA\ncomposite")

cor_effort <- cor(risk_data[, ..effort_cols], use = "pairwise.complete.obs")
cat("\n努力指标相关矩阵:\n")
print(round(cor_effort, 3))

# ── 3. VIF分析 ─────────────────────────────────────────────────────────
cat("\n=== VIF分析 ===\n")

# 气候指标VIF（在GLM框架下）
risk_cc <- risk_data[complete.cases(risk_data[, ..clim_cols])]

# 用线性模型近似计算VIF
lm_clim <- lm(temp_grad_z ~ prec_grad_z + climate_velocity_z +
  precip_velocity_z + climate_exposure_z + warming_rate_z +
  mahalanobis_dist_z, data = risk_cc)
vif_clim <- vif(lm_clim)
cat("气候指标VIF:\n")
for (i in seq_along(vif_clim)) {
  flag <- if (vif_clim[i] > 5) "***" else if (vif_clim[i] > 2) "**" else ""
  cat(sprintf("  %-25s: %.2f %s\n", names(vif_clim)[i], vif_clim[i], flag))
}

# 努力指标VIF
lm_effort <- lm(log_effort_visits_z ~ log_effort_record_z +
  log_effort_observers_z + log_effort_days_z + effort_pc1_z,
  data = risk_data[complete.cases(risk_data[, ..effort_cols])])
vif_effort <- vif(lm_effort)
cat("\n努力指标VIF:\n")
for (i in seq_along(vif_effort)) {
  flag <- if (vif_effort[i] > 5) "***" else if (vif_effort[i] > 2) "**" else ""
  cat(sprintf("  %-25s: %.2f %s\n", names(vif_effort)[i], vif_effort[i], flag))
}

# 保存VIF
vif_dt <- rbindlist(list(
  data.table(variable = names(vif_clim), vif = as.numeric(vif_clim),
             group = "Climate"),
  data.table(variable = names(vif_effort), vif = as.numeric(vif_effort),
             group = "Effort")
))
fwrite(vif_dt, file.path(TASK_ROOT, "results", "table_vif_analysis.csv"))

# VIF可视化
p_vif <- ggplot(vif_dt, aes(x = reorder(variable, vif), y = vif,
                              fill = group)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = c(2, 5), linetype = "dashed",
             color = c("orange", "red"), linewidth = 0.5) +
  annotate("text", x = nrow(vif_dt), y = 2.3,
           label = "VIF = 2", size = 3, hjust = 1, color = "orange") +
  annotate("text", x = nrow(vif_dt), y = 5.3,
           label = "VIF = 5", size = 3, hjust = 1, color = "red") +
  coord_flip() +
  scale_fill_manual(values = c("Climate" = "#d94801", "Effort" = "#2171b5"),
                    name = "Group") +
  labs(x = "", y = "Variance Inflation Factor",
       title = "VIF analysis: climate and effort proxies",
       subtitle = "Dashed lines: VIF = 2 (moderate) and VIF = 5 (high collinearity)") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures", "fig_vif_analysis.png"),
       p_vif, width = 9, height = 6, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_vif_analysis.pdf"),
       p_vif, width = 9, height = 6)

# 相关矩阵可视化
library(ggcorrplot)

p_cor_clim <- ggcorrplot(cor_clim, hc.order = TRUE, type = "lower",
  lab = TRUE, lab_size = 3,
  colors = c("#2171b5", "white", "#d94801"),
  title = "Climate proxy correlations") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 13),
        axis.text = element_text(size = 7))

p_cor_effort <- ggcorrplot(cor_effort, hc.order = TRUE, type = "lower",
  lab = TRUE, lab_size = 3,
  colors = c("#2171b5", "white", "#d94801"),
  title = "Effort proxy correlations") +
  theme(plot.title = element_text(face = "bold", hjust = 0, size = 13),
        axis.text = element_text(size = 7))

p_cor_combined <- (p_cor_clim | p_cor_effort) +
  plot_annotation(title = "Inter-proxy correlation matrices",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave(file.path(TASK_ROOT, "figures", "fig_proxy_correlations.png"),
       p_cor_combined, width = 16, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_proxy_correlations.pdf"),
       p_cor_combined, width = 16, height = 7)

# ── 4. 各代理变量单独跑模型 ─────────────────────────────────────────────
cat("\n=== 各代理变量单独建模 ===\n")

# 基础公式: event ~ year_c + [climate_proxy] * [effort_proxy] +
#   (1|species) + (1|province)
# 每个气候指标单独 × 每个努力指标单独

# 根据VIF筛选：去掉VIF>5的指标
high_vif_vars <- vif_dt[vif > 5, variable]
cat("高VIF变量(>5):", paste(high_vif_vars, collapse = ", "), "\n")

# 筛选后的气候代理
clim_proxies <- c(
  "temp_grad_z",
  "climate_velocity_z",
  "mahalanobis_dist_z"
)
# prec_grad_z VIF可能也高，但保留作为对照
if (!"prec_grad_z" %in% high_vif_vars) {
  clim_proxies <- c(clim_proxies, "prec_grad_z")
}
if (!"precip_velocity_z" %in% high_vif_vars) {
  clim_proxies <- c(clim_proxies, "precip_velocity_z")
}

effort_proxies <- c(
  "log_effort_visits_z",
  "log_effort_days_z",
  "effort_pc1_z"
)

cat("气候代理:", paste(clim_proxies, collapse = ", "), "\n")
cat("努力代理:", paste(effort_proxies, collapse = ", "), "\n")

# 系统性拟合
model_matrix <- CJ(clim = clim_proxies, effort = effort_proxies)
all_results <- list()
all_coefs <- list()

for (i in seq_len(nrow(model_matrix))) {
  clim_var <- model_matrix$clim[i]
  effort_var <- model_matrix$effort[i]

  fml <- as.formula(paste0(
    "event ~ year_c + ", clim_var, " * ", effort_var,
    " + (1|species) + (1|province)"))

  cat(sprintf("  [%d/%d] %s x %s ... ", i, nrow(model_matrix),
              clim_var, effort_var))

  fit <- tryCatch(
    glmmTMB(fml, data = risk_data,
            family = binomial(link = "cloglog")),
    error = function(e) { cat("FAILED\n"); NULL }
  )

  if (is.null(fit)) next

  aic_val <- AIC(fit)
  pdHess <- isTRUE(fit$sdr$pdHess)

  # 提取交互项系数
  cf <- summary(fit)$coefficients$cond
  interact_row <- grep(":", rownames(cf), value = TRUE)
  interact_est <- cf[interact_row, "Estimate"]
  interact_p <- cf[interact_row, "Pr(>|z|)"]
  interact_hr <- exp(interact_est)

  cat(sprintf("AIC=%.1f, HR=%.3f, p=%.2g, pdHess=%s\n",
              aic_val, interact_hr, interact_p, pdHess))

  all_results[[i]] <- data.table(
    clim_proxy = clim_var, effort_proxy = effort_var,
    aic = aic_val, pdHess = pdHess,
    interact_hr = interact_hr, interact_p = interact_p,
    interact_est = interact_est
  )

  # 完整系数表
  cf_dt <- as.data.table(cf, keep.rownames = TRUE)
  setnames(cf_dt, "rn", "term")
  cf_dt[, clim_proxy := clim_var]
  cf_dt[, effort_proxy := effort_var]
  all_coefs[[i]] <- cf_dt
}

dt_results <- rbindlist(all_results, fill = TRUE)
dt_coefs <- rbindlist(all_coefs, fill = TRUE)

fwrite(dt_results, file.path(TASK_ROOT, "results",
  "table_proxy_selection_model_comparison.csv"))
fwrite(dt_coefs, file.path(TASK_ROOT, "results",
  "table_proxy_selection_coefficients.csv"))

# ── 5. 模型选择可视化 ──────────────────────────────────────────────────
cat("\n=== 模型选择可视化 ===\n")

# AIC热力图（气候 × 努力）
p_aic_heat <- ggplot(dt_results,
  aes(x = effort_proxy, y = clim_proxy, fill = aic)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", aic)), size = 3.5) +
  scale_fill_viridis_c(option = "D", direction = -1, name = "AIC") +
  labs(x = "Effort proxy", y = "Climate proxy",
       title = "AIC comparison across proxy combinations",
       subtitle = "Lower AIC = better model fit") +
  theme_nature +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(TASK_ROOT, "figures", "fig_proxy_aic_heatmap.png"),
       p_aic_heat, width = 8, height = 6, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_proxy_aic_heatmap.pdf"),
       p_aic_heat, width = 8, height = 6)

# 交互项HR热力图
p_hr_heat <- ggplot(dt_results,
  aes(x = effort_proxy, y = clim_proxy, fill = interact_hr)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f%s", interact_hr,
    ifelse(interact_p < 0.001, "***",
    ifelse(interact_p < 0.01, "**",
    ifelse(interact_p < 0.05, "*", ""))))),
    size = 3.5) +
  scale_fill_viridis_c(option = "C", name = "HR\n(interaction)") +
  geom_hline(yintercept = seq_len(length(clim_proxies)) + 0.5,
             color = "grey90", linewidth = 0.3) +
  labs(x = "Effort proxy", y = "Climate proxy",
       title = "Interaction HR across proxy combinations",
       subtitle = "Significance: * p<0.05, ** p<0.01, *** p<0.001") +
  theme_nature +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(TASK_ROOT, "figures", "fig_proxy_hr_heatmap.png"),
       p_hr_heat, width = 8, height = 6, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_proxy_hr_heatmap.pdf"),
       p_hr_heat, width = 8, height = 6)

# ── 6. Effort作为offset的敏感性分析 ────────────────────────────────────
cat("\n=== Effort offset敏感性分析 ===\n")

# 模型1: 标准交互模型（temp_grad * effort）
# 模型2: effort作为offset（不估计effort系数，强制hr=1）
# 模型3: effort作为offset + temp_grad主效应
# 模型4: 无effort模型（纯气候模型）

risk_data[, species := factor(species)]
risk_data[, province := factor(province)]

offset_models <- list(
  M_interact = list(
    fml = event ~ year_c + temp_grad_z * log_effort_visits_z +
      (1|species) + (1|province),
    label = "Interaction model"
  ),
  M_offset = list(
    fml = event ~ year_c + temp_grad_z + offset(log_effort_visits_z) +
      (1|species) + (1|province),
    label = "Effort as offset"
  ),
  M_offset_interact = list(
    fml = event ~ year_c + temp_grad_z +
      offset(log_effort_visits_z) +
      temp_grad_z:offset(log_effort_visits_z) +
      (1|species) + (1|province),
    label = "Effort offset + climate interaction"
  ),
  M_no_effort = list(
    fml = event ~ year_c + temp_grad_z + (1|species) + (1|province),
    label = "No effort (climate only)"
  ),
  M_effort_only = list(
    fml = event ~ year_c + log_effort_visits_z +
      (1|species) + (1|province),
    label = "Effort only (no climate)"
  )
)

offset_results <- list()

for (mname in names(offset_models)) {
  cat(sprintf("  Fitting %s ... ", mname))
  fit <- tryCatch(
    glmmTMB(offset_models[[mname]]$fml, data = risk_data,
            family = binomial(link = "cloglog")),
    error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); NULL }
  )

  if (is.null(fit)) next

  aic_val <- AIC(fit)
  pdHess <- isTRUE(fit$sdr$pdHess)

  # 提取temp_grad系数
  cf <- summary(fit)$coefficients$cond
  tg_row <- grep("^temp_grad_z$", rownames(cf), value = TRUE)
  if (length(tg_row) > 0) {
    tg_est <- cf[tg_row, "Estimate"]
    tg_p <- cf[tg_row, "Pr(>|z|)"]
    tg_hr <- exp(tg_est)
  } else {
    tg_est <- tg_p <- tg_hr <- NA
  }

  # 交互项
  int_row <- grep(":", rownames(cf), value = TRUE)
  if (length(int_row) > 0) {
    int_est <- cf[int_row, "Estimate"]
    int_p <- cf[int_row, "Pr(>|z|)"]
    int_hr <- exp(int_est)
  } else {
    int_est <- int_p <- int_hr <- NA
  }

  cat(sprintf("AIC=%.1f, pdHess=%s\n", aic_val, pdHess))

  offset_results[[mname]] <- data.table(
    model = mname,
    label = offset_models[[mname]]$label,
    aic = aic_val, pdHess = pdHess,
    temp_grad_hr = tg_hr, temp_grad_p = tg_p,
    interact_hr = int_hr, interact_p = int_p
  )
}

dt_offset <- rbindlist(offset_results, fill = TRUE)
fwrite(dt_offset, file.path(TASK_ROOT, "results",
  "table_effort_offset_sensitivity.csv"))

print(dt_offset[, .(label, aic, temp_grad_hr, interact_hr)])

# offset敏感性可视化
dt_offset_long <- melt(dt_offset,
  measure.vars = c("temp_grad_hr", "interact_hr"),
  variable.name = "coefficient",
  value.name = "hr")

p_offset <- ggplot(dt_offset_long[!is.na(hr)],
  aes(x = reorder(label, aic), y = hr, fill = coefficient)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  coord_flip() +
  scale_fill_manual(values = c("temp_grad_hr" = "#d94801",
                                "interact_hr" = "#6a51a3"),
                    labels = c("Temp gradient", "Interaction"),
                    name = "Coefficient") +
  labs(x = "", y = "Hazard ratio",
       title = "Effort specification sensitivity analysis",
       subtitle = "Comparing interaction, offset, and no-effort models") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures",
  "fig_effort_offset_sensitivity.png"),
  p_offset, width = 10, height = 5, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures",
  "fig_effort_offset_sensitivity.pdf"),
  p_offset, width = 10, height = 5)

cat("\n=== 14_vif_correlation_proxy_selection.R 完成 ===\n")
