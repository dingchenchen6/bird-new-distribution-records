#!/usr/bin/env Rscript
# 02_hazard_model_effort_comparison.R
# 四种 effort 指标的 hazard model 系统对比 + 扩展模型
# Systematic comparison of 4 effort specifications in hazard models

suppressPackageStartupMessages({
  library(data.table)
  library(glmmTMB)
  library(purrr)
  library(here)
})

TASK_ROOT <- here::here("tasks", "bird_hazard_model_effort_upgrade")

# ── 1. 读取数据 ──────────────────────────────────────────────────────────
risk_data <- fread(file.path(TASK_ROOT, "data", "hazard_risk_upgraded_complete_case.csv"))

# 中心化 year 避免数值不稳定
risk_data[, year_c := year - 2013]

# 确保因子
risk_data[, species  := factor(species)]
risk_data[, province := factor(province)]

cat("数据行数:", nrow(risk_data), "  事件数:", sum(risk_data$event), "\n")

# ── 2. 定义四种 effort 指标 ───────────────────────────────────────────────
effort_specs <- list(
  spec_A = list(label = "Record-based (legacy)", effort_var = "log_effort_record_z"),
  spec_B = list(label = "Observer visits",      effort_var = "log_effort_visits_z"),
  spec_C = list(label = "PCA composite (PC1)",   effort_var = "effort_pc1_z"),
  spec_D = list(label = "Birding days",          effort_var = "log_effort_days_z")
)

# ── 3. 定义模型序列 ──────────────────────────────────────────────────────
build_formulas <- function(effort_var) {
  list(
    M0 = as.formula("event ~ year_c + (1|species) + (1|province)"),
    M1 = as.formula(paste0("event ~ year_c + temp_grad_z + (1|species) + (1|province)")),
    M2 = as.formula(paste0("event ~ year_c + ", effort_var, " + (1|species) + (1|province)")),
    M3 = as.formula(paste0("event ~ year_c + temp_grad_z + ", effort_var,
                           " + (1|species) + (1|province)")),
    M4 = as.formula(paste0("event ~ year_c + temp_grad_z * ", effort_var,
                           " + (1|species) + (1|province)"))
  )
}

# ── 辅助函数：手动提取系数 ────────────────────────────────────────────────
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

# ── 4. 拟合所有模型 ──────────────────────────────────────────────────────
all_fit_info <- list()
all_coefs    <- list()

for (spec_name in names(effort_specs)) {
  spec_cfg <- effort_specs[[spec_name]]
  cat(sprintf("\n--- Fitting %s: %s ---\n", spec_name, spec_cfg$label))

  formulas <- build_formulas(spec_cfg$effort_var)

  for (model_name in names(formulas)) {
    fit_key <- paste(spec_name, model_name, sep = "_")
    cat(sprintf("  Fitting %s ... ", fit_key))

    # 分两步：先拟合，再提取
    fit <- tryCatch(
      glmmTMB(formulas[[model_name]],
              data = risk_data,
              family = binomial(link = "cloglog")),
      error = function(e) {
        cat(sprintf("FIT FAILED: %s\n", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(fit)) {
      all_fit_info[[fit_key]] <- data.table(
        spec = spec_name, spec_label = spec_cfg$label,
        effort_var = spec_cfg$effort_var, model = model_name,
        formula = deparse(formulas[[model_name]], width.cutoff = 500),
        status = "error", nobs = NA_integer_,
        aic = NA_real_, bic = NA_real_, logLik = NA_real_,
        convergence = NA_integer_, pdHess = NA
      )
      next
    }

    pdHess <- isTRUE(fit$sdr$pdHess)
    conv_ok <- isTRUE(fit$fit$convergence == 0)
    status <- if (pdHess && conv_ok) "ok" else "convergence_warning"
    aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)
    bic_val <- tryCatch(BIC(fit), error = function(e) NA_real_)
    ll_val  <- tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_)

    cat(sprintf("AIC=%.1f, pdHess=%s, status=%s\n", aic_val, pdHess, status))

    all_fit_info[[fit_key]] <- data.table(
      spec = spec_name, spec_label = spec_cfg$label,
      effort_var = spec_cfg$effort_var, model = model_name,
      formula = deparse(formulas[[model_name]], width.cutoff = 500),
      status = status, nobs = nobs(fit),
      aic = aic_val, bic = bic_val, logLik = ll_val,
      convergence = fit$fit$convergence, pdHess = pdHess
    )

    # 提取系数
    coefs_dt <- tryCatch({
      extract_coefs_manual(fit)
    }, error = function(e) {
      cat(sprintf("    coef extraction FAILED: %s\n", conditionMessage(e)))
      data.table(term = character(0), estimate = numeric(0),
                 std.error = numeric(0), statistic = numeric(0),
                 p.value = numeric(0), conf.low = numeric(0),
                 conf.high = numeric(0))
    })

    if (nrow(coefs_dt) > 0) {
      coefs_dt[, spec := spec_name]
      coefs_dt[, spec_label := spec_cfg$label]
      coefs_dt[, effort_var := spec_cfg$effort_var]
      coefs_dt[, model := model_name]
      all_coefs[[fit_key]] <- coefs_dt
    }
  }
}

# ── 5. 汇总模型比较表 ────────────────────────────────────────────────────
dt_model_comparison <- rbindlist(all_fit_info, fill = TRUE, use.names = TRUE)
dt_model_comparison[, delta_aic := aic - min(aic, na.rm = TRUE), by = spec]

fwrite(dt_model_comparison,
       file.path(TASK_ROOT, "results", "table_cross_specification_model_comparison.csv"))

# ── 6. 汇总系数表 ────────────────────────────────────────────────────────
dt_coefs <- rbindlist(all_coefs, fill = TRUE, use.names = TRUE)

# 重点系数
key_terms <- c("temp_grad_z", "log_effort_record_z", "log_effort_visits_z",
               "effort_pc1_z", "log_effort_days_z",
               "temp_grad_z:log_effort_record_z",
               "temp_grad_z:log_effort_visits_z",
               "temp_grad_z:effort_pc1_z",
               "temp_grad_z:log_effort_days_z")

dt_key_coefs <- dt_coefs[term %in% key_terms]
dt_key_coefs[, hr := exp(estimate)]
dt_key_coefs[, hr_lower := exp(conf.low)]
dt_key_coefs[, hr_upper := exp(conf.high)]

fwrite(dt_key_coefs,
       file.path(TASK_ROOT, "results", "table_cross_specification_key_coefficients.csv"))

# ── 7. 扩展模型（M5-M7，基于 Spec B = visits）────────────────────────────
cat("\n--- Fitting extended models (M5-M7) using visits effort ---\n")

extended_formulas <- list(
  M5 = as.formula(paste0("event ~ year_c + temp_grad_z * log_effort_visits_z",
                          " + prec_grad_z + (1|species) + (1|province)")),
  M6 = as.formula(paste0("event ~ year_c + temp_grad_z * log_effort_visits_z",
                          " + mig + (1|species) + (1|province)")),
  M7 = as.formula(paste0("event ~ year_c + temp_grad_z * log_effort_visits_z",
                          " + prec_grad_z + mig + (1|species) + (1|province)"))
)

# M6/M7 需要 migration strategy
if (!"mig" %in% names(risk_data)) {
  cat("  注意：mig 列不在风险集中，M6/M7 将跳过\n")
  extended_formulas <- extended_formulas["M5"]
}

ext_fit_info <- list()
ext_coefs <- list()

for (model_name in names(extended_formulas)) {
  cat(sprintf("  Fitting %s ... ", model_name))
  tryCatch({
    fit <- glmmTMB(extended_formulas[[model_name]],
                   data = risk_data,
                   family = binomial(link = "cloglog"))

    pdHess <- isTRUE(fit$sdr$pdHess)
    status <- if (pdHess) "ok" else "convergence_warning"
    aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)

    cat(sprintf("AIC=%.1f, pdHess=%s\n", aic_val, pdHess))

    ext_fit_info[[model_name]] <- data.table(
      spec = "spec_B_extended", spec_label = "Observer visits (extended)",
      effort_var = "log_effort_visits_z", model = model_name,
      formula = deparse(extended_formulas[[model_name]], width.cutoff = 500),
      status = status, nobs = nobs(fit),
      aic = aic_val, bic = tryCatch(BIC(fit), error = function(e) NA_real_),
      logLik = tryCatch(as.numeric(logLik(fit)), error = function(e) NA_real_),
      convergence = fit$fit$convergence, pdHess = pdHess
    )

    coefs_dt <- extract_coefs_manual(fit)
    coefs_dt[, spec := "spec_B_extended"]
    coefs_dt[, spec_label := "Observer visits (extended)"]
    coefs_dt[, effort_var := "log_effort_visits_z"]
    coefs_dt[, model := model_name]
    ext_coefs[[model_name]] <- coefs_dt

  }, error = function(e) {
    cat(sprintf("FAILED: %s\n", conditionMessage(e)))
  })
}

if (length(ext_fit_info) > 0) {
  dt_ext_comparison <- rbindlist(ext_fit_info, fill = TRUE)
  dt_ext_coefs <- rbindlist(ext_coefs, fill = TRUE)
  fwrite(dt_ext_comparison,
         file.path(TASK_ROOT, "results", "table_extended_model_comparison.csv"))
  fwrite(dt_ext_coefs,
         file.path(TASK_ROOT, "results", "table_extended_model_coefficients.csv"))
}

# ── 8. 核心科学判断 ──────────────────────────────────────────────────────
cat("\n=== 核心科学判断：交互项跨指标稳健性 ===\n")

interaction_coefs <- dt_key_coefs[grepl("temp_grad_z:", term)]
if (nrow(interaction_coefs) > 0) {
  for (i in seq_len(nrow(interaction_coefs))) {
    row_i <- interaction_coefs[i]
    sig_mark <- if (is.na(row_i$p.value)) "NA"
               else if (row_i$p.value < 0.001) "***"
               else if (row_i$p.value < 0.01) "**"
               else if (row_i$p.value < 0.05) "*"
               else "n.s."
    cat(sprintf("  %s | %s | beta=%.4f, HR=%.3f [%.3f-%.3f], p=%.2e %s\n",
                row_i$spec_label, row_i$model,
                row_i$estimate, row_i$hr, row_i$hr_lower, row_i$hr_upper,
                row_i$p.value, sig_mark))
  }

  valid_ps <- interaction_coefs[!is.na(p.value), p.value]
  if (length(valid_ps) > 0) {
    all_significant <- all(valid_ps < 0.05)
    if (all_significant) {
      cat("\n  >>> 交互项在所有 effort 指标下均显著为正",
          " -> '可见度阈值'解释得到强支撑\n")
    } else {
      n_sig <- sum(valid_ps < 0.05)
      cat(sprintf("\n  >>> 交互项仅 %d/%d 种指标下显著",
                  " -> 需进一步讨论稳健性\n", n_sig, length(valid_ps)))
    }
  }
}

cat("\nHazard model effort 对比分析完成。\n")
