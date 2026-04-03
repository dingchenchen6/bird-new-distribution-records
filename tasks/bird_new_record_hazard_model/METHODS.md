# Methods

## Chinese overview / 中文概述

本任务将中国鸟类新分布记录分析推进为一套基于 `SDM 约束风险集 + 离散时间 hazard model` 的事件史框架，重点回答：

> 在调查努力高度不均衡的背景下，气候变化与采样努力如何共同驱动中国鸟类新分布记录的形成？

本轮主模型采用更收敛、更适合论文主分析的口径：

- 风险集：`species × province × year`
- SDM 候选集：`birdwatch_2002_2025 + rescue_1980_2025_gbif`
- 主阈值：`100`
- 敏感性阈值：`50` 与 `200`
- 年份窗口：`2002–2024`
- 气候变量：温度梯度
- effort 变量：记录型 effort
- 当前主模型不纳入降水和 observer effort

## English overview

This task implements an SDM-constrained discrete-time hazard modeling framework for bird new provincial records in China.

The main analytical question is:

> How do climate change and uneven survey effort jointly shape the emergence of new bird distribution records in China?

The current main specification is intentionally streamlined for interpretability and manuscript use:

- risk set: `species × province × year`
- SDM candidates: `birdwatch_2002_2025 + rescue_1980_2025_gbif`
- primary threshold: `100`
- sensitivity thresholds: `50` and `200`
- analysis window: `2002–2024`
- climate term: temperature gradient
- effort term: record-based survey effort
- precipitation and observer effort excluded from the current main model

## Analytical workflow

### 1. Input layers

The task integrates five main input layers:

- new-record event table
- SDM-derived potential province table
- province-year climate panel
- species-year native-range climate panel
- province-year record-effort table

### 2. Risk-set definition

The analytical unit is `species × province × year`.
A combination enters the risk set only if:

- the province is predicted as potentially suitable by the SDM
- the province is not treated as historical presence
- the species has not yet generated a first new record in that province before year `t`

The event indicator is:

- `event = 1`: first new provincial record occurs in year `t`
- `event = 0`: no first event yet in year `t`

### 3. Climate and effort terms

The current temperature-gradient term is defined as:

- `temp_grad = temp_anom - temp_native_anom`

The current survey term is:

- `log_effort_record = log(effort_record)`

The key interaction is:

- `temp_grad_z : log_effort_record_z`

### 4. Model sequence

The staged model sequence is:

- `M0`: baseline year model
- `M1`: climate-only model
- `M2`: effort-only model
- `M3`: additive climate + effort model
- `M4`: joint-driver interaction model

All models are fit as discrete-time hazard GLMMs with a `cloglog` link.
The current preferred engine is `glmmTMB`, with fallback support retained in the reusable helper script.

### 5. Diagnostics

The task exports several diagnostics so the workflow is easy to audit:

- `support_filter_trace.csv`: record/species attrition across SDM, native climate, and effort support filters
- `risk_event_distribution_by_year.csv`: event counts and event rates by year
- `predictor_distribution_complete_case.csv`: predictor distribution summary for complete-case data
- `model_fit_registry.csv`: model formulas, engines, AIC, BIC, and fitting status
- `model_diagnostics.csv`: convergence and Hessian checks

### 6. Visualization workflow

The reporting script reads the three threshold-specific output folders and builds:

- cross-threshold model comparison tables
- cross-threshold key coefficient tables
- a forest plot of best-model hazard ratios
- a ridge plot of coefficient uncertainty across model stages and thresholds
- an Excel bundle for manuscript writing and repository sharing

## Reproducibility order

Recommended run order:

1. `code/test_bird_new_record_hazard_model_smoke.R`
2. `code/run_bird_new_record_hazard_model_combined_threshold_test.R 100 2002 2024`
3. `code/run_bird_new_record_hazard_model_combined_threshold_test.R 50 2002 2024`
4. `code/run_bird_new_record_hazard_model_combined_threshold_test.R 200 2002 2024`
5. `code/visualize_bird_new_record_hazard_results.R`

For server reruns, see:

- `results/SERVER_RERUN_GUIDE.md`

For current interpretation notes, see:

- `results/HAZARD_RESULTS_INTERPRETATION.md`
