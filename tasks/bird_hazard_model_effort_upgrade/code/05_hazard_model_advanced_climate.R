#!/usr/bin/env Rscript
# 05_hazard_model_advanced_climate.R
# 用高级气候指标替换/扩展原始 temp_grad 的 hazard model
# 保留原始 temp_grad 作为对照基线
# Advanced climate metrics: climate_velocity, mahalanobis_dist,
# climate_exposure, warming_rate, precip_velocity
# Original metrics: temp_grad, prec_grad (baseline)

suppressPackageStartupMessages({
  library(data.table)
  library(glmmTMB)
  library(purrr)
  library(here)
})

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

# ── 1. 读取数据 ──────────────────────────────────────────────────────────
risk_data <- fread(file.path(TASK_ROOT, "data", "hazard_risk_upgraded_complete_case.csv"))
clim_metrics <- fread(file.path(TASK_ROOT, "data", "climate_metrics_province_year.csv"))

risk_data[, year_c := year - 2013]
risk_data[, species  := factor(species)]
risk_data[, province := factor(province)]

# 合并高级气候指标到风险集
risk_data <- merge(risk_data,
                   clim_metrics[, .(province, year,
                                    climate_velocity_z, precip_velocity_z,
                                    climate_exposure_z, warming_rate_z,
                                    mahalanobis_dist_z)],
                   by = c("province", "year"), all.x = TRUE)

cat("数据行数:", nrow(risk_data), "  事件数:", sum(risk_data$event), "\n")
cat("高级指标NA统计:\n")
for (v in c("climate_velocity_z", "precip_velocity_z",
            "climate_exposure_z", "warming_rate_z", "mahalanobis_dist_z")) {
  cat(sprintf("  %s: %d NA\n", v, sum(is.na(risk_data[[v]]))))
}

# ── 2. 定义模型规格 ──────────────────────────────────────────────────────
# 6 种气候指标（含原始 temp_grad 基线）
climate_specs <- list(
  clim_orig = list(climate_var = "temp_grad_z",
                   label = "Temp gradient\n(legacy)"),
  clim_velocity = list(climate_var = "climate_velocity_z",
                       label = "Climate\nvelocity"),
  clim_mahal = list(climate_var = "mahalanobis_dist_z",
                    label = "Mahalanobis\ndistance"),
  clim_exposure = list(climate_var = "climate_exposure_z",
                       label = "Climate\nexposure"),
  clim_warming = list(climate_var = "warming_rate_z",
                      label = "Warming\nrate"),
  clim_precip_vel = list(climate_var = "precip_velocity_z",
                         label = "Precipitation\nvelocity")
)

# 4 种努力指标
effort_specs <- list(
  spec_A = list(label = "Record-based", effort_var = "log_effort_record_z"),
  spec_B = list(label = "Observer visits", effort_var = "log_effort_visits_z"),
  spec_C = list(label = "PCA composite", effort_var = "effort_pc1_z"),
  spec_D = list(label = "Birding days", effort_var = "log_effort_days_z")
)

# ── 辅助函数 ──────────────────────────────────────────────────────────────
extract_coefs_manual <- function(fit) {
  cf <- summary(fit)$coefficients$cond
  vc <- vcov(fit)
  vc_cond <- if (is.list(vc)) vc[["cond"]] else vc
  se <- sqrt(diag(vc_cond))
  z <- cf[, "Estimate"] / se
  p <- 2 * pnorm(-abs(z))

  data.table(
    term      = rownames(cf),
    estimate  = cf[, "Estimate"],
    std.error = se,
    statistic = z,
    p.value   = p,
    conf.low  = cf[, "Estimate"] - 1.96 * se,
    conf.high = cf[, "Estimate"] + 1.96 * se
  )
}

# ── 3. 拟合交互模型 (M4 等价)：climate_z * effort_z ─────────────────────
# 仅拟合 M4（交互模型），因为之前已确认 M4 在所有指标下最优
all_fit_info <- list()
all_coefs    <- list()

fit_count <- 0
total_fits <- length(climate_specs) * length(effort_specs)

for (clim_name in names(climate_specs)) {
  clim_cfg <- climate_specs[[clim_name]]

  for (eff_name in names(effort_specs)) {
    eff_cfg <- effort_specs[[eff_name]]
    fit_count <- fit_count + 1

    fit_key <- paste(clim_name, eff_name, sep = "__")
    cat(sprintf("[%d/%d] Fitting %s × %s ... ",
                fit_count, total_fits, clim_name, eff_name))

    formula_str <- paste0("event ~ year_c + ", clim_cfg$climate_var,
                          " * ", eff_cfg$effort_var,
                          " + (1|species) + (1|province)")
    fml <- as.formula(formula_str)

    fit <- tryCatch(
      glmmTMB(fml, data = risk_data, family = binomial(link = "cloglog")),
      error = function(e) {
        cat(sprintf("FIT FAILED: %s\n", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(fit)) {
      all_fit_info[[fit_key]] <- data.table(
        climate_spec = clim_name, climate_label = clim_cfg$label,
        climate_var = clim_cfg$climate_var,
        effort_spec = eff_name, effort_label = eff_cfg$label,
        effort_var = eff_cfg$effort_var,
        status = "error", nobs = NA_integer_,
        aic = NA_real_, bic = NA_real_, logLik = NA_real_,
        convergence = NA_integer_, pdHess = NA
      )
      next
    }

    pdHess <- isTRUE(fit$sdr$pdHess)
    status <- if (pdHess && fit$fit$convergence == 0) "ok" else "convergence_warning"
    aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)
    bic_val <- tryCatch(BIC(fit), error = function(e) NA_real_)
    ll_val  <- tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_)

    cat(sprintf("AIC=%.1f, pdHess=%s\n", aic_val, pdHess))

    all_fit_info[[fit_key]] <- data.table(
      climate_spec = clim_name, climate_label = clim_cfg$label,
      climate_var = clim_cfg$climate_var,
      effort_spec = eff_name, effort_label = eff_cfg$label,
      effort_var = eff_cfg$effort_var,
      status = status, nobs = nobs(fit),
      aic = aic_val, bic = bic_val, logLik = ll_val,
      convergence = fit$fit$convergence, pdHess = pdHess,
      formula = formula_str
    )

    # 提取系数
    coefs_dt <- tryCatch(extract_coefs_manual(fit), error = function(e) {
      data.table(term = character(0), estimate = numeric(0),
                 std.error = numeric(0), statistic = numeric(0),
                 p.value = numeric(0), conf.low = numeric(0), conf.high = numeric(0))
    })

    if (nrow(coefs_dt) > 0) {
      coefs_dt[, climate_spec := clim_name]
      coefs_dt[, climate_label := clim_cfg$label]
      coefs_dt[, climate_var := clim_cfg$climate_var]
      coefs_dt[, effort_spec := eff_name]
      coefs_dt[, effort_label := eff_cfg$label]
      coefs_dt[, effort_var := eff_cfg$effort_var]
      all_coefs[[fit_key]] <- coefs_dt
    }
  }
}

# ── 4. 汇总模型比较表 ────────────────────────────────────────────────────
dt_model <- rbindlist(all_fit_info, fill = TRUE, use.names = TRUE)

# ΔAIC: 在每个 effort_spec 内部比较（同努力指标下比较气候指标）
dt_model[, delta_aic_within_effort := aic - min(aic, na.rm = TRUE), by = effort_spec]
# 全局 ΔAIC
dt_model[, delta_aic_global := aic - min(aic, na.rm = TRUE)]

fwrite(dt_model,
       file.path(TASK_ROOT, "results", "table_advanced_climate_model_comparison.csv"))

# ── 5. 汇总系数表 ────────────────────────────────────────────────────────
dt_coefs <- rbindlist(all_coefs, fill = TRUE, use.names = TRUE)

# 交互项
interact_pattern <- paste0(clim_cfg$climate_var, ":")  # 通用匹配
dt_interact <- dt_coefs[grepl(":", term) & !grepl("Intercept", term)]
dt_interact[, hr := exp(estimate)]
dt_interact[, hr_lower := exp(conf.low)]
dt_interact[, hr_upper := exp(conf.high)]

fwrite(dt_interact,
       file.path(TASK_ROOT, "results", "table_advanced_climate_interaction_coefs.csv"))

# 全部关键系数
key_terms <- unique(dt_coefs$term[!grepl("Intercept", dt_coefs$term)])
dt_key_coefs <- dt_coefs[term %in% key_terms]
dt_key_coefs[, hr := exp(estimate)]
dt_key_coefs[, hr_lower := exp(conf.low)]
dt_key_coefs[, hr_upper := exp(conf.high)]

fwrite(dt_key_coefs,
       file.path(TASK_ROOT, "results", "table_advanced_climate_key_coefficients.csv"))

# ── 6. VIF 检查（多气候指标模型）─────────────────────────────────────────
cat("\n--- VIF 诊断：多气候指标共线性 ---\n")

# 选用 Spec B (visits) 检查
multi_clim_vars <- c("temp_grad_z", "climate_velocity_z", "mahalanobis_dist_z",
                     "climate_exposure_z", "warming_rate_z")

vif_data <- risk_data[!is.na(log_effort_visits_z)]
vif_formula <- as.formula(paste0("event ~ ", paste(multi_clim_vars, collapse = " + "),
                                  " + log_effort_visits_z"))
vif_lm <- tryCatch(lm(vif_formula, data = as.data.frame(vif_data)), error = function(e) NULL)

if (!is.null(vif_lm)) {
  vif_vals <- tryCatch(car::vif(vif_lm), error = function(e) {
    # 如果 car 不可用，手动计算
    sapply(multi_clim_vars, function(v) {
      other_vars <- setdiff(c(multi_clim_vars, "log_effort_visits_z"), v)
      fml <- as.formula(paste(v, "~", paste(other_vars, collapse = " + ")))
      1 / (1 - summary(lm(fml, data = as.data.frame(vif_data)))$r.squared)
    })
  })
  cat("VIF values:\n")
  for (n in names(vif_vals)) {
    cat(sprintf("  %-25s: %.2f\n", n, vif_vals[n]))
  }
} else {
  cat("  VIF 计算失败\n")
}

# ── 7. 多气候指标联合模型 ────────────────────────────────────────────────
cat("\n--- 拟合多气候指标联合模型 (Spec B visits) ---\n")

# 选择低VIF的气候指标组合
multi_formulas <- list(
  M_multi_top2 = as.formula(
    "event ~ year_c + climate_velocity_z * log_effort_visits_z +
     mahalanobis_dist_z + (1|species) + (1|province)"),
  M_multi_top3 = as.formula(
    "event ~ year_c + climate_velocity_z * log_effort_visits_z +
     mahalanobis_dist_z + climate_exposure_z + (1|species) + (1|province)"),
  M_multi_full = as.formula(
    "event ~ year_c + temp_grad_z + climate_velocity_z + mahalanobis_dist_z +
     climate_exposure_z + warming_rate_z + log_effort_visits_z +
     climate_velocity_z:log_effort_visits_z + (1|species) + (1|province)")
)

multi_fit_info <- list()
multi_coefs <- list()

for (mname in names(multi_formulas)) {
  cat(sprintf("  Fitting %s ... ", mname))
  fit <- tryCatch(
    glmmTMB(multi_formulas[[mname]], data = risk_data,
            family = binomial(link = "cloglog")),
    error = function(e) { cat("FAILED\n"); NULL }
  )

  if (is.null(fit)) next

  pdHess <- isTRUE(fit$sdr$pdHess)
  aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)
  cat(sprintf("AIC=%.1f, pdHess=%s\n", aic_val, pdHess))

  multi_fit_info[[mname]] <- data.table(
    model = mname,
    formula = deparse(multi_formulas[[mname]], width.cutoff = 500),
    status = if (pdHess) "ok" else "convergence_warning",
    nobs = nobs(fit), aic = aic_val,
    bic = tryCatch(BIC(fit), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_),
    convergence = fit$fit$convergence, pdHess = pdHess
  )

  coefs_dt <- tryCatch(extract_coefs_manual(fit), error = function(e) {
    data.table(term = character(0), estimate = numeric(0),
               std.error = numeric(0), statistic = numeric(0),
               p.value = numeric(0), conf.low = numeric(0), conf.high = numeric(0))
  })
  if (nrow(coefs_dt) > 0) {
    coefs_dt[, model := mname]
    multi_coefs[[mname]] <- coefs_dt
  }
}

if (length(multi_fit_info) > 0) {
  dt_multi_model <- rbindlist(multi_fit_info, fill = TRUE)
  dt_multi_coefs <- rbindlist(multi_coefs, fill = TRUE)
  fwrite(dt_multi_model,
         file.path(TASK_ROOT, "results", "table_multi_climate_model_comparison.csv"))
  fwrite(dt_multi_coefs,
         file.path(TASK_ROOT, "results", "table_multi_climate_model_coefficients.csv"))
}

# ── 8. 核心科学判断 ──────────────────────────────────────────────────────
cat("\n=== 交互项跨气候指标稳健性 ===\n")

# 每个气候指标 x 每个努力指标的交互 HR
if (nrow(dt_interact) > 0) {
  cat("Climate × Effort 交互项 HR:\n")
  for (i in seq_len(nrow(dt_interact))) {
    row_i <- dt_interact[i]
    sig_mark <- if (is.na(row_i$p.value)) "NA"
               else if (row_i$p.value < 0.001) "***"
               else if (row_i$p.value < 0.01) "**"
               else if (row_i$p.value < 0.05) "*"
               else "n.s."
    cat(sprintf("  %-20s × %-18s | HR=%.3f [%.3f-%.3f] p=%.2e %s\n",
                row_i$climate_label, row_i$effort_label,
                row_i$hr, row_i$hr_lower, row_i$hr_upper,
                row_i$p.value, sig_mark))
  }
}

# AIC 比较：哪个气候指标拟合最优？
cat("\n=== AIC 比较（按 effort_spec 分组）===\n")
for (eff in unique(dt_model$effort_spec)) {
  sub <- dt_model[effort_spec == eff & status == "ok"]
  if (nrow(sub) > 0) {
    best <- sub[which.min(aic)]
    cat(sprintf("  %s: 最优气候指标 = %s (AIC=%.1f, ΔAIC=0)\n",
                eff, best$climate_spec, best$aic))
    for (j in seq_len(nrow(sub))) {
      cat(sprintf("    %-20s AIC=%.1f ΔAIC=%.1f\n",
                  sub$climate_spec[j], sub$aic[j], sub$delta_aic_within_effort[j]))
    }
  }
}

cat("\nHazard model 高级气候指标分析完成。\n")
