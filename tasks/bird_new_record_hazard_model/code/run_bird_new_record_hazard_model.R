#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

options(stringsAsFactors = FALSE)

read_config <- function(config_path) {
  cfg <- read_csv(config_path, show_col_types = FALSE) %>%
    transmute(
      key = str_trim(key),
      value = na_if(str_trim(value), "")
    )
  values <- cfg$value
  names(values) <- cfg$key
  values
}

cfg_get <- function(cfg, key, default = NULL) {
  if (!key %in% names(cfg)) {
    return(default)
  }
  value <- cfg[[key]]
  if (is.null(value) || is.na(value) || identical(value, "")) default else value
}

cfg_get_num <- function(cfg, key, default = NA_real_) {
  value <- suppressWarnings(as.numeric(cfg_get(cfg, key, default)))
  ifelse(is.na(value), default, value)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

create_output_tree <- function(output_root) {
  out <- list(
    data_clean = file.path(output_root, "data_clean"),
    diagnostics = file.path(output_root, "diagnostics"),
    results = file.path(output_root, "results"),
    logs = file.path(output_root, "logs")
  )
  walk(c(output_root, unlist(out, use.names = FALSE)), ensure_dir)
  out
}

assert_required_cols <- function(df, required_cols, label) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(label, " is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
}

path_file_info <- function(path, label) {
  if (is.null(path) || is.na(path)) {
    return(tibble(
      input_name = label,
      path = NA_character_,
      exists = FALSE,
      size_bytes = NA_real_
    ))
  }

  tibble(
    input_name = label,
    path = path,
    exists = file.exists(path),
    size_bytes = ifelse(file.exists(path), file.info(path)$size, NA_real_)
  )
}

safe_z <- function(x) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  if (length(stats::na.omit(unique(x))) <= 1) {
    return(rep(0, length(x)))
  }
  as.numeric(scale(x))
}

build_first_event_table <- function(ndr_raw) {
  ndr_raw %>%
    group_by(species, province) %>%
    summarise(
      first_year = min(year, na.rm = TRUE),
      .groups = "drop"
    )
}

build_candidate_table <- function(sdm_province) {
  sdm_province %>%
    mutate(
      potential = as.integer(potential),
      historical_presence = as.integer(dplyr::coalesce(historical_presence, 0L)),
      risk_start_year = as.integer(risk_start_year)
    ) %>%
    filter(potential == 1, historical_presence == 0)
}

expand_risk_set <- function(candidate, ndr_first, year_min, year_max) {
  candidate %>%
    left_join(ndr_first, by = c("species", "province")) %>%
    mutate(
      risk_start_year = dplyr::coalesce(risk_start_year, year_min),
      risk_start_year = pmax(risk_start_year, year_min),
      end_year = ifelse(is.na(first_year), year_max, pmin(first_year, year_max))
    ) %>%
    filter(risk_start_year <= end_year) %>%
    rowwise() %>%
    mutate(year = list(seq.int(risk_start_year, end_year))) %>%
    unnest(year) %>%
    ungroup() %>%
    mutate(event = ifelse(!is.na(first_year) & year == first_year, 1L, 0L))
}

prepare_model_data <- function(risk_data) {
  risk_data %>%
    mutate(
      temp_grad = temp_anom - temp_native_anom,
      prec_grad = prec_anom - prec_native_anom,
      log_effort_record = log(pmax(effort_record, 1)),
      log_effort_observer = log(pmax(effort_observer, 1)),
      temp_grad_z = safe_z(temp_grad),
      prec_grad_z = safe_z(prec_grad),
      temp_anom_z = safe_z(temp_anom),
      prec_anom_z = safe_z(prec_anom),
      log_effort_record_z = safe_z(log_effort_record),
      log_effort_observer_z = safe_z(log_effort_observer),
      year_f = factor(year),
      species = factor(species),
      province = factor(province),
      mig = if ("mig" %in% names(.)) factor(mig) else factor("unknown")
    )
}

make_formula <- function(response, terms) {
  stats::as.formula(paste(response, "~", paste(terms, collapse = " + ")))
}

formula_to_string <- function(formula) {
  paste(deparse(formula), collapse = " ")
}

fit_discrete_model <- function(formula, data, model_name, engine = "auto") {
  complete_data <- data %>%
    select(all.vars(formula)) %>%
    mutate(.row_id = seq_len(n())) %>%
    tidyr::drop_na()

  if (nrow(complete_data) == 0) {
    return(list(
      name = model_name,
      formula = formula_to_string(formula),
      status = "skipped",
      engine = NA_character_,
      nobs = 0L,
      model = NULL,
      error = "No complete cases after dropping missing values."
    ))
  }

  data_fit <- data[complete_data$.row_id, , drop = FALSE]
  if (dplyr::n_distinct(data_fit$event) < 2) {
    return(list(
      name = model_name,
      formula = formula_to_string(formula),
      status = "skipped",
      engine = NA_character_,
      nobs = nrow(data_fit),
      model = NULL,
      error = "Event outcome has fewer than two levels."
    ))
  }

  fit_try <- NULL
  fit_error <- NULL
  fit_engine <- NA_character_

  if (engine %in% c("auto", "glmmTMB") && requireNamespace("glmmTMB", quietly = TRUE)) {
    fit_try <- tryCatch(
      glmmTMB::glmmTMB(
        formula = formula,
        data = data_fit,
        family = stats::binomial(link = "cloglog")
      ),
      error = function(e) {
        fit_error <<- paste("glmmTMB:", e$message)
        NULL
      }
    )
    if (!is.null(fit_try)) {
      fit_engine <- "glmmTMB"
    }
  }

  if (is.null(fit_try) && engine %in% c("auto", "lme4", "glmer") && requireNamespace("lme4", quietly = TRUE)) {
    fit_try <- tryCatch(
      lme4::glmer(
        formula = formula,
        data = data_fit,
        family = stats::binomial(link = "cloglog"),
        control = lme4::glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
      ),
      error = function(e) {
        fit_error <<- paste(c(fit_error, paste("glmer:", e$message)), collapse = " | ")
        NULL
      }
    )
    if (!is.null(fit_try)) {
      fit_engine <- "glmer"
    }
  }

  if (is.null(fit_try)) {
    return(list(
      name = model_name,
      formula = formula_to_string(formula),
      status = "failed",
      engine = fit_engine,
      nobs = nrow(data_fit),
      model = NULL,
      error = fit_error
    ))
  }

  list(
    name = model_name,
    formula = formula_to_string(formula),
    status = "ok",
    engine = fit_engine,
    nobs = nrow(data_fit),
    model = fit_try,
    error = NA_character_
  )
}

extract_coef_table <- function(fit_obj) {
  if (fit_obj$status != "ok") {
    return(tibble())
  }

  if (inherits(fit_obj$model, "glmmTMB")) {
    coef_mat <- summary(fit_obj$model)$coefficients$cond
  } else {
    coef_mat <- coef(summary(fit_obj$model))
  }

  coef_tbl <- as.data.frame(coef_mat, stringsAsFactors = FALSE)
  coef_tbl$term <- rownames(coef_tbl)
  rownames(coef_tbl) <- NULL

  est_col <- intersect(c("Estimate", "estimate"), names(coef_tbl))[1]
  se_col <- intersect(c("Std. Error", "Std..Error", "std.error"), names(coef_tbl))[1]
  z_col <- intersect(c("z value", "z.value", "t value", "t.value"), names(coef_tbl))[1]
  p_col <- intersect(c("Pr(>|z|)", "Pr(>|t|)", "p.value"), names(coef_tbl))[1]

  out <- coef_tbl %>%
    transmute(
      model = fit_obj$name,
      engine = fit_obj$engine,
      term = term,
      estimate = .data[[est_col]],
      std_error = .data[[se_col]],
      statistic = if (!is.na(z_col)) .data[[z_col]] else NA_real_,
      p_value = if (!is.na(p_col)) .data[[p_col]] else NA_real_
    ) %>%
    mutate(
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error,
      hazard_ratio = exp(estimate),
      hazard_ratio_low = exp(conf_low),
      hazard_ratio_high = exp(conf_high)
    )

  out
}

extract_model_summary <- function(fit_obj) {
  if (fit_obj$status != "ok") {
    return(tibble(
      model = fit_obj$name,
      engine = fit_obj$engine,
      status = fit_obj$status,
      nobs = fit_obj$nobs,
      aic = NA_real_,
      bic = NA_real_,
      logLik = NA_real_,
      formula = fit_obj$formula,
      error = fit_obj$error
    ))
  }

  tibble(
    model = fit_obj$name,
    engine = fit_obj$engine,
    status = fit_obj$status,
    nobs = fit_obj$nobs,
    aic = AIC(fit_obj$model),
    bic = BIC(fit_obj$model),
    logLik = as.numeric(logLik(fit_obj$model)),
    formula = fit_obj$formula,
    error = fit_obj$error
  )
}

build_model_specs <- function(risk_data, include_precip = TRUE, include_observer_effort = TRUE) {
  has_temp <- "temp_grad_z" %in% names(risk_data)
  has_prec <- "prec_grad_z" %in% names(risk_data)
  has_effort_record <- "log_effort_record_z" %in% names(risk_data)
  has_effort_observer <- include_observer_effort &&
    "log_effort_observer_z" %in% names(risk_data) &&
    sum(!is.na(risk_data$log_effort_observer_z)) > 0
  has_mig <- "mig" %in% names(risk_data) && dplyr::n_distinct(stats::na.omit(risk_data$mig)) > 1
  climate_terms <- c("temp_grad_z", if (include_precip && has_prec) "prec_grad_z" else NULL)
  effort_terms <- c("log_effort_record_z", if (has_effort_observer) "log_effort_observer_z" else NULL)
  has_climate <- length(climate_terms) > 0 && all(climate_terms %in% names(risk_data))
  has_effort <- has_effort_record && length(effort_terms) > 0

  random_terms <- c("(1|species)", "(1|province)")
  specs <- list(
    M0 = c("year_f", random_terms),
    M1 = if (has_climate) c("year_f", climate_terms, random_terms) else NULL,
    M2 = if (has_effort) c("year_f", effort_terms, random_terms) else NULL,
    M3 = if (has_climate && has_effort) c(
      "year_f", climate_terms,
      effort_terms,
      if (has_mig) "mig" else NULL,
      random_terms
    ) else NULL,
    M4 = if (has_climate && has_effort) c(
      "year_f",
      climate_terms,
      effort_terms,
      "temp_grad_z:log_effort_record_z",
      if (has_mig) "mig" else NULL,
      random_terms
    ) else NULL
  )

  specs
}

write_run_summary <- function(path, risk_data, model_summary) {
  total_events <- sum(risk_data$event, na.rm = TRUE)
  best_model <- model_summary %>%
    filter(status == "ok") %>%
    arrange(aic) %>%
    slice_head(n = 1)

  lines <- c(
    "# Hazard Model Run Summary",
    "",
    "## Risk-set overview",
    paste0("- rows: ", nrow(risk_data)),
    paste0("- events: ", total_events),
    paste0("- species: ", dplyr::n_distinct(risk_data$species)),
    paste0("- provinces: ", dplyr::n_distinct(risk_data$province)),
    paste0("- year range: ", min(risk_data$year), " to ", max(risk_data$year)),
    "",
    "## Model fitting",
    paste0("- fitted models: ", sum(model_summary$status == "ok")),
    paste0("- skipped/failed models: ", sum(model_summary$status != "ok"))
  )

  if (nrow(best_model) == 1) {
    lines <- c(
      lines,
      paste0("- best AIC model: ", best_model$model[[1]], " (", round(best_model$aic[[1]], 2), ")")
    )
  }

  writeLines(lines, con = path, useBytes = TRUE)
}

run_hazard_analysis <- function(config_path) {
  cfg <- read_config(config_path)

  new_record_csv <- cfg_get(cfg, "new_record_csv")
  sdm_province_csv <- cfg_get(cfg, "sdm_province_csv")
  province_year_climate_csv <- cfg_get(cfg, "province_year_climate_csv")
  species_year_native_climate_csv <- cfg_get(cfg, "species_year_native_climate_csv")
  effort_csv <- cfg_get(cfg, "effort_csv")
  species_trait_csv <- cfg_get(cfg, "species_trait_csv", NA_character_)
  output_dir <- cfg_get(cfg, "output_dir", file.path(dirname(config_path), "hazard_model_output"))
  year_min <- as.integer(cfg_get_num(cfg, "year_min", 1950))
  year_max <- as.integer(cfg_get_num(cfg, "year_max", 2024))
  model_engine <- cfg_get(cfg, "model_engine", "auto")
  include_precip <- tolower(cfg_get(cfg, "include_precip", "true")) %in% c("true", "1", "yes", "y")
  include_observer_effort <- tolower(cfg_get(cfg, "include_observer_effort", "true")) %in% c("true", "1", "yes", "y")

  required_inputs <- list(
    new_record_csv = new_record_csv,
    sdm_province_csv = sdm_province_csv,
    province_year_climate_csv = province_year_climate_csv,
    species_year_native_climate_csv = species_year_native_climate_csv,
    effort_csv = effort_csv
  )

  input_manifest <- bind_rows(
    path_file_info(new_record_csv, "new_record_csv"),
    path_file_info(sdm_province_csv, "sdm_province_csv"),
    path_file_info(province_year_climate_csv, "province_year_climate_csv"),
    path_file_info(species_year_native_climate_csv, "species_year_native_climate_csv"),
    path_file_info(effort_csv, "effort_csv"),
    path_file_info(species_trait_csv, "species_trait_csv")
  )

  if (any(!input_manifest$exists[input_manifest$input_name %in% names(required_inputs)])) {
    missing_tbl <- input_manifest %>% filter(!exists, input_name %in% names(required_inputs))
    stop("Missing required input files: ", paste(missing_tbl$input_name, collapse = ", "))
  }

  out_dirs <- create_output_tree(output_dir)
  write_csv(input_manifest, file.path(out_dirs$diagnostics, "input_manifest.csv"))

  ndr_raw <- read_csv(new_record_csv, show_col_types = FALSE)
  sdm_province <- read_csv(sdm_province_csv, show_col_types = FALSE)
  clim_py <- read_csv(province_year_climate_csv, show_col_types = FALSE)
  clim_native <- read_csv(species_year_native_climate_csv, show_col_types = FALSE)
  effort_py <- read_csv(effort_csv, show_col_types = FALSE)
  species_trait <- if (!is.na(species_trait_csv) && file.exists(species_trait_csv)) {
    read_csv(species_trait_csv, show_col_types = FALSE)
  } else {
    tibble(species = character())
  }

  assert_required_cols(ndr_raw, c("species", "province", "year"), "new_record_csv")
  assert_required_cols(sdm_province, c("species", "province", "potential"), "sdm_province_csv")
  if (!"historical_presence" %in% names(sdm_province)) {
    sdm_province <- mutate(sdm_province, historical_presence = 0L)
  }
  if (!"risk_start_year" %in% names(sdm_province)) {
    sdm_province <- mutate(sdm_province, risk_start_year = NA_integer_)
  }
  assert_required_cols(clim_py, c("province", "year", "temp_anom", "prec_anom"), "province_year_climate_csv")
  assert_required_cols(clim_native, c("species", "year", "temp_native_anom", "prec_native_anom"), "species_year_native_climate_csv")
  assert_required_cols(effort_py, c("province", "year", "effort_record"), "effort_csv")
  if (!"effort_observer" %in% names(effort_py)) {
    effort_py <- mutate(effort_py, effort_observer = NA_real_)
  }
  assert_required_cols(species_trait, c("species"), "species_trait_csv")

  ndr_first <- build_first_event_table(ndr_raw)
  candidate <- build_candidate_table(sdm_province)
  risk_data <- expand_risk_set(candidate, ndr_first, year_min = year_min, year_max = year_max) %>%
    left_join(clim_py, by = c("province", "year")) %>%
    left_join(clim_native, by = c("species", "year")) %>%
    left_join(effort_py, by = c("province", "year")) %>%
    left_join(species_trait, by = "species") %>%
    prepare_model_data()

  write_csv(risk_data, file.path(out_dirs$data_clean, "hazard_risk_data.csv"))

  risk_summary <- tibble(
    n_candidate_species_province = nrow(candidate),
    n_risk_rows = nrow(risk_data),
    n_events = sum(risk_data$event, na.rm = TRUE),
    n_species = dplyr::n_distinct(risk_data$species),
    n_provinces = dplyr::n_distinct(risk_data$province),
    min_year = min(risk_data$year, na.rm = TRUE),
    max_year = max(risk_data$year, na.rm = TRUE)
  )
  write_csv(risk_summary, file.path(out_dirs$diagnostics, "risk_set_summary.csv"))

  model_data_availability <- tibble(
    variable = c(
      "temp_grad_z", "prec_grad_z",
      "log_effort_record_z", "log_effort_observer_z", "mig"
    ),
    available = c(
      sum(!is.na(risk_data$temp_grad_z)),
      sum(!is.na(risk_data$prec_grad_z)),
      sum(!is.na(risk_data$log_effort_record_z)),
      sum(!is.na(risk_data$log_effort_observer_z)),
      sum(!is.na(risk_data$mig))
    )
  )
  write_csv(model_data_availability, file.path(out_dirs$diagnostics, "model_data_availability.csv"))

  model_specs <- build_model_specs(
    risk_data,
    include_precip = include_precip,
    include_observer_effort = include_observer_effort
  )
  fitted_models <- imap(
    model_specs,
    ~ if (is.null(.x)) {
      list(
        name = .y,
        formula = NA_character_,
        status = "skipped",
        engine = NA_character_,
        nobs = 0L,
        model = NULL,
        error = "Required predictors are unavailable."
      )
    } else {
      fit_discrete_model(make_formula("event", .x), risk_data, model_name = .y, engine = model_engine)
    }
  )

  model_summary <- bind_rows(map(fitted_models, extract_model_summary))
  if (any(is.finite(model_summary$aic))) {
    model_summary <- model_summary %>%
      mutate(delta_aic = aic - min(aic, na.rm = TRUE))
  } else {
    model_summary <- model_summary %>%
      mutate(delta_aic = NA_real_)
  }
  write_csv(model_summary, file.path(out_dirs$results, "model_comparison.csv"))

  coef_table <- bind_rows(map(fitted_models, extract_coef_table))
  write_csv(coef_table, file.path(out_dirs$results, "model_coefficients.csv"))

  write_run_summary(file.path(out_dirs$logs, "run_summary.md"), risk_data, model_summary)

  invisible(list(
    config = cfg,
    risk_data = risk_data,
    model_summary = model_summary,
    coefficient_table = coef_table,
    fitted_models = fitted_models
  ))
}

args <- commandArgs(trailingOnly = TRUE)

if (sys.nframe() == 0) {
  if (length(args) < 1) {
    stop("Usage: Rscript code/run_bird_new_record_hazard_model.R data/hazard_model_config.csv")
  }
  run_hazard_analysis(args[[1]])
}
