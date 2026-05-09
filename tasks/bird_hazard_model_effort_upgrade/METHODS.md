# Methods

## Chinese overview / 中文概述

本任务将 SDM 约束的离散时间 hazard model 从"记录型 effort"升级为"观测者型 effort"，这是项目发表前最关键的稳健性检验。

### 核心科学问题

> 在使用观测者型调查努力（n_visits, n_observers, n_birding_days, PCA 复合指标）替换记录型 effort 后，hazard model 的核心结论——气候-努力交互项（"可见度阈值"解释）——是否仍然成立？

### 为什么这是关键检验

现有 hazard model 使用 `log(effort_record)` 作为调查努力代理变量。`effort_record` 是某省某年的所有鸟类记录总数，这是一个"检出产出"而非"采样投入"指标。它存在内生性问题：记录数部分由物种存在和可检测性决定，而非纯采样强度。

动态占域模型项目已产出基于观测者/访问/时长的省级 effort 指标（n_visits, n_observers, n_birding_days），这些是纯"采样投入"指标，不存在内生性。

### 分析设计

#### 四种 effort 指标

| 代号 | 指标 | 类型 | 用途 |
|------|------|------|------|
| Spec A | `log_effort_record_z` | 记录型（旧） | 复现对照 |
| Spec B | `log_effort_visits_z` | 观测者访问量 | 主分析首选 |
| Spec C | `effort_pc1_z` | PCA 复合指标 | 理论最严谨 |
| Spec D | `log_effort_days_z` | 观鸟天数 | 补充验证 |

#### 模型序列

每种指标下拟合 M0-M4：
- M0: `event ~ year + (1|species) + (1|province)` — 基线
- M1: `+ temp_grad_z` — 仅气候
- M2: `+ effort_z` — 仅努力
- M3: `+ temp_grad_z + effort_z` — 加性
- M4: `+ temp_grad_z * effort_z` — 交互

扩展模型（基于 Spec B）：
- M5: M4 + `prec_grad_z`
- M6: M4 + migration strategy
- M7: M4 + `prec_grad_z` + migration strategy

#### 公平对比

所有模型在同一样本上拟合：四种 effort 指标均非 NA 的风险集行，消除样本量差异的混淆。

### 模型引擎

所有模型使用 `glmmTMB`，`cloglog` 链接，`(1|species) + (1|province)` 随机效应。复数检验：收敛码 = 0，Hessian 正定。

### 诊断

- 收敛检查：`pdHess = TRUE`，`convergence == 0`
- VIF：观测者型 effort 与 `temp_grad_z` 的 VIF < 5
- 复现检验：Spec A M4 应复现现有结果（HR ≈ 1.23）

### 关键判断标准

- 若 `temp_grad_z:effort_z` 在 Spec B/C/D 下**均显著为正** → "可见度阈值"解释得到强支撑
- 若交互项**在观测者型 effort 下减弱或消失** → 现有结论需修正

## English overview

This task upgrades the discrete-time hazard model from record-based to observer-based survey effort — the single most critical robustness check before publication.

The core scientific question is whether the `temp_grad_z:effort_z` interaction (the "visibility threshold" interpretation) remains significant and positive when observer-based effort metrics replace the endogenous record-based effort.

Four effort specifications are compared systematically across five model stages (M0-M4), with extended models (M5-M7) adding precipitation gradient and migration strategy. All models are fitted on the same complete-case sample for fair comparison.

## 完整分析流程 / Full Analysis Pipeline (16 scripts)

### Phase 1: 核心稳健性检验 / Core robustness check (scripts 01–03)

| Script | Purpose |
|---|---|
| `01_build_effort_upgraded_risk_set.R` | 构建含 4 种 effort 指标的升级版风险集 / Build upgraded risk set with 4 effort metrics |
| `02_hazard_model_effort_comparison.R` | 4×5 模型矩阵 + 扩展模型 M5-M7 / 4×5 model matrix + extended M5-M7 |
| `03_hazard_model_upgrade_visualization.R` | 森林图、山脊图、AIC 热力图、HR 稳定性图、4-panel 拼图 / Forest, ridge, AIC heatmap, HR stability, 4-panel |

### Phase 2: 高级气候指标 / Advanced climate metrics (scripts 04–05)

| Script | Purpose |
|---|---|
| `04_compute_advanced_climate_metrics.R` | 计算 climate velocity, Mahalanobis distance, exposure, warming rate, precipitation velocity / Compute multi-dimensional climate metrics |
| `05_hazard_model_advanced_climate.R` | 6 种气候指标 × 4 种 effort 指标交互模型 (20 个模型) / 6 climate metrics × 4 effort specs interaction models |

### Phase 3: 栅格尺度验证 / Grid-scale validation (scripts 06–07)

| Script | Purpose |
|---|---|
| `06_build_grid_infrastructure.R` | 50km/100km 栅格风险集基础设施（基础信息、SDM、气候、effort）/ Build 50km/100km grid risk set infrastructure |
| `07_hazard_model_grid.R` | 栅格尺度 hazard model (temp_grad, climate_velocity, bio1, Mahalanobis) / Grid-level hazard models |

### Phase 4: 位移方向与方差分解 / Displacement & variance partitioning (scripts 08–09)

| Script | Purpose |
|---|---|
| `08_compute_displacement_direction.R` | 计算省级位移方向、距离、风玫瑰图 / Compute provincial displacement direction, distance, windrose |
| `09_variance_partitioning.R` | 方差分解：加性 vs 交互效应 R² / Variance partitioning: additive vs interaction R² |

### Phase 5: 机器学习验证 / Machine learning validation (scripts 10–11)

| Script | Purpose |
|---|---|
| `10_xgboost_shap_prediction.R` | XGBoost 模型 + SHAP 解释 + 未来情景预测 / XGBoost + SHAP interpretation + future scenario predictions |
| `11_rf_hazard_prediction.R` | Random Forest + 变量重要性 + 未来情景预测 / Random Forest + variable importance + future predictions |

### Phase 6: Range map 与代理变量选择 / Range map & proxy selection (scripts 12, 14)

| Script | Purpose |
|---|---|
| `12_range_map_native_climate.R` | 基于物种分布图的本地气候异常值计算 / Species range map native climate anomaly calculation |
| `14_vif_correlation_proxy_selection.R` | VIF 分析 + 相关性诊断 + 代理变量选择模型 / VIF + correlation + proxy selection models |

### Phase 7: 综合可视化与未来热点 / Comprehensive visualization & future hotspots (scripts 13, 15)

| Script | Purpose |
|---|---|
| `13_comprehensive_visualization.R` | 数据分布 + 模型结果 + ML 对比 + 未来预测综合图 / Data dist + model results + ML comparison + future prediction figures |
| `15_future_hotspot_maps.R` | 省级空间热点地图 (SSP245/585 × 2030/2040/2050 + effort 情景) / Provincial spatial hotspot maps |

### Phase 8: 多尺度未来预测 / Multi-scale future predictions (script 16)

| Script | Purpose |
|---|---|
| `16_multi_scale_future_prediction.R` | 省/市/县级 + 多分辨率网格的未来预测与地图 / Province/prefecture/county + multi-resolution grid future predictions & maps |

## Reproducibility order

1. `code/01_build_effort_upgraded_risk_set.R`
2. `code/02_hazard_model_effort_comparison.R`
3. `code/03_hazard_model_upgrade_visualization.R`
4. `code/04_compute_advanced_climate_metrics.R`
5. `code/05_hazard_model_advanced_climate.R`
6. `code/06_build_grid_infrastructure.R`
7. `code/07_hazard_model_grid.R`
8. `code/08_compute_displacement_direction.R`
9. `code/09_variance_partitioning.R`
10. `code/10_xgboost_shap_prediction.R`
11. `code/11_rf_hazard_prediction.R`
12. `code/12_range_map_native_climate.R`
13. `code/13_comprehensive_visualization.R`
14. `code/14_vif_correlation_proxy_selection.R`
15. `code/15_future_hotspot_maps.R`
16. `code/16_multi_scale_future_prediction.R`

## Dependencies

- Task A `bird_survey_effort_integration/` data outputs (effort panels, PCA indices)
- Existing hazard model helper functions from `bird_new_record_hazard_model/code/run_bird_new_record_hazard_model.R`
- Existing hazard model derived inputs from `bird_new_record_hazard_model/combined_threshold_100_test/`
- Provincial boundary shapefiles from `/Users/dingchenchen/Documents/SDMs/GS(2023)2767审图号/`
- Prefecture and county boundary shapefiles (for script 16)
