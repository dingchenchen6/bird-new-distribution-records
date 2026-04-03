#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  normalizePath("code/run_bird_new_record_hazard_model_complete_case_test.R", mustWork = TRUE)
}

task_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(task_root, "code", "run_bird_new_record_hazard_model.R"))

input_risk <- file.path(task_root, "empirical_hazard_recent_test", "data_clean", "hazard_risk_data.csv")
output_root <- file.path(task_root, "empirical_hazard_recent_complete_case_test")

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "data_clean"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "results"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "logs"), recursive = TRUE, showWarnings = FALSE)

risk_data <- read_csv(input_risk, show_col_types = FALSE) %>%
  filter(
    !is.na(temp_grad_z),
    !is.na(prec_grad_z),
    !is.na(log_effort_record_z),
    !is.na(log_effort_observer_z)
  ) %>%
  mutate(
    species = factor(species),
    province = factor(province),
    year_f = factor(year),
    mig = factor(mig)
  )

write_csv(risk_data, file.path(output_root, "data_clean", "hazard_risk_complete_case.csv"))

model_specs <- list(
  M0 = c("year_f", "(1|species)", "(1|province)"),
  M1 = c("year_f", "temp_grad_z", "prec_grad_z", "(1|species)", "(1|province)"),
  M2 = c("year_f", "log_effort_record_z", "log_effort_observer_z", "(1|species)", "(1|province)"),
  M3 = c(
    "year_f", "temp_grad_z", "prec_grad_z",
    "log_effort_record_z", "log_effort_observer_z", "(1|species)", "(1|province)"
  ),
  M4 = c(
    "year_f", "temp_grad_z", "prec_grad_z",
    "log_effort_record_z", "log_effort_observer_z",
    "temp_grad_z:log_effort_record_z",
    "(1|species)", "(1|province)"
  )
)

fitted_models <- purrr::imap(
  model_specs,
  ~ fit_discrete_model(make_formula("event", .x), risk_data, model_name = .y, engine = "lme4")
)

model_summary <- bind_rows(map(fitted_models, extract_model_summary))
if (any(is.finite(model_summary$aic))) {
  model_summary <- model_summary %>%
    mutate(delta_aic = aic - min(aic, na.rm = TRUE))
} else {
  model_summary <- model_summary %>%
    mutate(delta_aic = NA_real_)
}

coef_table <- bind_rows(map(fitted_models, extract_coef_table))

write_csv(model_summary, file.path(output_root, "results", "model_comparison.csv"))
write_csv(coef_table, file.path(output_root, "results", "model_coefficients.csv"))

writeLines(
  c(
    "# Complete-Case Hazard Test",
    "",
    paste0("- risk rows: ", nrow(risk_data)),
    paste0("- events: ", sum(risk_data$event, na.rm = TRUE)),
    paste0("- species: ", dplyr::n_distinct(risk_data$species)),
    paste0("- provinces: ", dplyr::n_distinct(risk_data$province)),
    paste0("- years: ", min(risk_data$year), " to ", max(risk_data$year))
  ),
  con = file.path(output_root, "logs", "run_summary.md")
)

message("Complete-case hazard test completed.")
