# Bird new-distribution-records: new-record hazard model task

This task integrates the `hazard-model-bundle` workflow from the `bird_range_climate_shift_metrics` branch into the main bird new-records GitHub repository as a standalone analysis module.

本任务把 `hazard-model-bundle` 分支中的新纪录 hazard 分析，整合为主仓库中的一个独立任务模块，方便后续统一管理、复现、补跑和论文写作。

## Current primary workflow

The recommended script for the current main analysis is:

- `code/run_bird_new_record_hazard_model_combined_threshold_test.R`

The reporting and figure-bundle script is:

- `code/visualize_bird_new_record_hazard_results.R`

The server rerun entry is:

- `code/run_bird_new_record_hazard_server_bundle.sh`

## What this task does

This task implements an SDM-constrained discrete-time hazard framework for Chinese bird new provincial records.

核心思路是：

- 先用 `birdwatch_2002_2025 + rescue_1980_2025_gbif` 两套 SDM 省级候选省份定义风险集；
- 再把新纪录事件扩展成 `species × province × year` 的逐年风险面板；
- 使用 `cloglog` 离散时间 hazard model 检验温度梯度、记录型 survey effort 及其交互是否解释首次新纪录风险；
- 最后输出跨阈值 summary 表、森林图、山脊图和 Excel 结果包。

## Current main specification

The current main model uses:

- `threshold = 100`
- analysis window `2002–2024`
- `temperature gradient`
- `record-based survey effort`
- `temperature gradient × record effort`

The current main model does **not** include:

- precipitation
- observer effort

## Directory structure

- `code/`: hazard-model scripts and WorldClim support scripts
- `data/`: lightweight configuration files and the synced record-effort tables
- `figures/`: current publication-style forest plot and ridge plot
- `results/combined_threshold_50_test/`: threshold 50 outputs
- `results/combined_threshold_100_test/`: threshold 100 outputs
- `results/combined_threshold_200_test/`: threshold 200 outputs
- `results/hazard_model_visualizations/`: cross-threshold summary tables and Excel bundle
- `results/HAZARD_RESULTS_INTERPRETATION.md`: current interpretation notes
- `results/SERVER_RERUN_GUIDE.md`: server rerun instructions
- `results/GITHUB_REPRO_WORKFLOW.md`: GitHub-side reproducibility notes

## Current key findings

The current fitted results support the same core conclusion locally and on the server:

- `M4` is the best-supported model across thresholds `50 / 100 / 200`
- the interaction `temp_grad_z:log_effort_record_z` is positive and significant
- this supports a visibility-threshold interpretation in which climate-associated expansion is more likely to appear as a new provincial record in better-surveyed regions

For `threshold = 100`, the current main-model estimates are:

- `temp_grad_z = -0.1429`, `p = 0.0068`
- `log_effort_record_z = -0.0876`, `p = 0.4794`
- `temp_grad_z:log_effort_record_z = 0.2077`, `p = 4.26e-6`, `HR = 1.2308`

## Key outputs

### Figures

- `figures/fig_hazard_forest_best_model.png`
- `figures/fig_hazard_ridge_coefficients.png`

### Threshold-specific results

- `results/combined_threshold_100_test/results/model_comparison.csv`
- `results/combined_threshold_100_test/results/model_coefficients.csv`
- `results/combined_threshold_100_test/diagnostics/model_diagnostics.csv`
- `results/combined_threshold_100_test/diagnostics/support_filter_trace.csv`

### Cross-threshold bundle

- `results/hazard_model_visualizations/table_model_comparison_across_thresholds.csv`
- `results/hazard_model_visualizations/table_key_coefficients_across_thresholds.csv`
- `results/hazard_model_visualizations/table_input_coverage_across_thresholds.csv`
- `results/hazard_model_visualizations/hazard_model_result_bundle.xlsx`

## Recommended reading order

1. `METHODS.md`
2. `code/run_bird_new_record_hazard_model_combined_threshold_test.R`
3. `results/HAZARD_RESULTS_INTERPRETATION.md`
4. `figures/fig_hazard_forest_best_model.png`
5. `figures/fig_hazard_ridge_coefficients.png`
