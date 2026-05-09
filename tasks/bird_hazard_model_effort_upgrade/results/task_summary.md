# Task Summary: Bird Hazard Model Effort Upgrade

## 科学结论 / Scientific Conclusion

**核心发现：气候-努力交互项在全部四种 effort 指标下均显著为正，"可见度阈值"解释得到强支撑。**

The `temp_grad_z:effort_z` interaction is significantly positive across all four effort specifications, strongly supporting the "visibility threshold" interpretation.

## 交互项核心结果 / Key Interaction Results (M4)

| Effort specification | HR (95% CI) | p-value | AIC |
|---|---|---|---|
| Spec A: Record-based (legacy) | 1.21 (1.11–1.32) | 1.7e-05 | 4207.8 |
| Spec B: Observer visits | **1.29 (1.18–1.41)** | **1.5e-08** | **4193.8** |
| Spec C: PCA composite (PC1) | 1.18 (1.09–1.27) | 5.8e-05 | 4207.8 |
| Spec D: Birding days | 1.31 (1.19–1.44) | 2.1e-08 | 4196.5 |

**Spec B (Observer visits) M4 为最优模型**：AIC 最低 (4193.8)，交互项 HR 最大且最显著。

## 风险集诊断 / Risk Set Diagnostics

- Year range: 2002–2024
- Full risk rows: 177,724
- Complete-case rows: 12,813
- Events in complete case: 512
- Species: 333 | Provinces: 32
- effort_record NA: 11,929 | effort_visits NA: 8,019

## 分析流程 / Analysis Pipeline (15 scripts)

### Phase 1: 核心稳健性检验 (scripts 01–03)

| Script | Purpose | Status |
|---|---|---|
| `01_build_effort_upgraded_risk_set.R` | 构建含 4 种 effort 指标的升级版风险集 | Complete |
| `02_hazard_model_effort_comparison.R` | 4×5 模型矩阵 + 扩展模型 M5-M7 | Complete |
| `03_hazard_model_upgrade_visualization.R` | 森林图、山脊图、AIC 热力图、HR 稳定性图、4-panel 拼图 | Complete |

### Phase 2: 高级气候指标 (scripts 04–05)

| Script | Purpose | Status |
|---|---|---|
| `04_compute_advanced_climate_metrics.R` | 计算 climate velocity, Mahalanobis distance, exposure, warming rate | Complete |
| `05_hazard_model_advanced_climate.R` | 6 种气候指标 × 4 种 effort 指标交互模型 | Complete |

**结论：原始温度梯度 (temp_grad_z) 仍是最优气候指标**，AIC 最低；Mahalanobis distance 次优 (HR = 1.13 for spec C)。

### Phase 3: 栅格尺度验证 (scripts 06–07)

| Script | Purpose | Status |
|---|---|---|
| `06_build_grid_infrastructure.R` | 50km/100km 栅格风险集基础设施 | Complete |
| `07_hazard_model_grid.R` | 栅格尺度 hazard model | Complete |

**结论：100km 栅格尺度 (n = 400,899, events = 1,594) 下交互项仍然显著。**

### Phase 4: 位移方向与方差分解 (scripts 08–09)

| Script | Purpose | Status |
|---|---|---|
| `08_compute_displacement_direction.R` | 计算省级位移方向和距离 | Complete |
| `09_variance_partitioning.R` | 方差分解 (加性 vs 交互) | Complete |

**关键方差分解结果：**

| Effort spec | Additive R² | Interaction R² | Δ R² (interaction) |
|---|---|---|---|
| Record-based | 0.0051 | 0.0265 | 0.0214 |
| Observer visits | 0.0082 | 0.0417 | 0.0336 |
| PCA composite | 0.0051 | 0.0252 | 0.0201 |
| Birding days | 0.0062 | 0.0387 | 0.0325 |

交互项解释的方差是加性模型的 3–5 倍。

### Phase 5: 机器学习验证 (scripts 10–11)

| Script | Purpose | Status |
|---|---|---|
| `10_xgboost_shap_prediction.R` | XGBoost + SHAP 解释 + 未来预测 | Complete |
| `11_rf_hazard_prediction.R` | Random Forest + 变量重要性 + 未来预测 | Complete |

**结论：XGBoost 和 RF 均隐式捕获了气候-努力交互模式，与 GLMM 互为印证。**

### Phase 6: Range map 与代理变量选择 (scripts 12, 14)

| Script | Purpose | Status |
|---|---|---|
| `12_range_map_native_climate.R` | 基于物种分布图的本地气候异常值计算 | Complete |
| `14_vif_correlation_proxy_selection.R` | VIF + 相关性分析 + 代理变量选择模型 | Complete |

**VIF 诊断：** climate_exposure_z VIF = 5.53 (略高于 5)，其余变量 VIF < 2。

**代理变量选择最优组合：** temp_grad_z × effort_pc1_z (AIC = 4207.8) 或 mahalanobis_dist_z × effort_pc1_z (AIC = 4217.9)。

### Phase 7: 综合可视化与未来热点 (scripts 13, 15)

| Script | Purpose | Status |
|---|---|---|
| `13_comprehensive_visualization.R` | 数据分布 + 模型结果 + ML 对比 + 未来预测 | Complete |
| `15_future_hotspot_maps.R` | 省级空间热点地图 (SSP245/585 × 2030/2040/2050) | Complete |

## Effort offset 敏感性检验

| Model | AIC | temp_grad HR | interact HR |
|---|---|---|---|
| M_interact (covariate) | 4193.8 | 0.87** | 1.29*** |
| M_offset (offset) | 4280.3 | 0.95 | — |
| M_no_effort | 4224.3 | 0.94 | — |

Effort 作为协变量 (covariate) 的交互模型 AIC 最优，优于 offset 模型（ΔAIC = 86.5），说明 effort 与气候存在交互效应。

## 扩展模型 (M5–M7, Spec B)

| Model | Addition | AIC | Interaction HR |
|---|---|---|---|
| M4 | — | 4193.8 | 1.29*** |
| M5 | + prec_grad_z | 4194.2 | (maintained) |
| M6 | + migration strategy | 4198.5 | (maintained) |
| M7 | + both | 4198.7 | (maintained) |

加入降水梯度和迁徙策略后，交互项效应保持稳健。

## 图件清单 / Figures (37)

### 数据诊断 (D series)
- `fig_d01_temp_grad_beeswarm` — 温度梯度年度分布
- `fig_d02_effort_ridges` — 4 种 effort 指标山脊线图
- `fig_d03_correlation_matrix` — 气候-努力相关矩阵
- `fig_d04_temp_grad_comparison` — 原始 vs range-map 温度梯度

### 核心模型 (Hazard series)
- `fig_hazard_forest_cross_spec` — 跨指标森林图
- `fig_hazard_ridge_cross_spec` — 跨指标山脊线图
- `fig_hazard_aic_heatmap` — AIC 热力图
- `fig_hazard_hr_stability` — HR 稳定性图
- `fig_hazard_combined_4panel` — 综合四面板图

### 高级气候 (M series)
- `fig_m01_interaction_rainforest` — 交互项雨林图
- `fig_m02_interaction_surface` — 气候×努力交互面
- `fig_m04_ml_comparison` — ML 变量重要性

### 位移方向 (Direction series)
- `fig_windrose_direction`, `fig_windrose_by_order`
- `fig_displacement_distance`, `fig_direction_distance_boxplot`
- `fig_lonlat_offset`

### ML & 方差分解
- `fig_shap_beeswarm`, `fig_shap_importance_bar`, `fig_shap_interaction_temp`
- `fig_xgboost_trajectory`, `fig_xgboost_2050_hotspot`
- `fig_rf_variable_importance`, `fig_rf_variable_importance_detailed`
- `fig_variance_decomposition`, `fig_interaction_contribution`

### 代理变量选择 & VIF
- `fig_vif_analysis`, `fig_proxy_correlations`
- `fig_proxy_aic_heatmap`, `fig_proxy_hr_heatmap`
- `fig_effort_offset_sensitivity`

### 未来预测热点地图
- `fig_map_xgb_2050_ssp585` — XGBoost 2050 SSP585 省级热点
- `fig_map_multi_scenario` — 多情景 (2×3) 对比图
- `fig_map_xgb_vs_rf_2050` — XGBoost vs RF 对比地图
- `fig_map_effort_sensitivity` — 努力情景敏感性地图
- `fig_f01_xgb_vs_rf_2050`, `fig_f02_scenario_trajectory`

## 结果表清单 / Result Tables (25)

| File | Content |
|---|---|
| `risk_set_diagnostics.csv` | 风险集诊断统计 |
| `table_cross_specification_model_comparison.csv` | 4×5 跨指标模型比较 |
| `table_cross_specification_key_coefficients.csv` | 跨指标关键系数 (含 HR) |
| `table_extended_model_comparison.csv` | 扩展模型 M5-M7 比较 |
| `table_extended_model_coefficients.csv` | 扩展模型系数 |
| `table_advanced_climate_model_comparison.csv` | 高级气候模型比较 |
| `table_advanced_climate_key_coefficients.csv` | 高级气候关键系数 |
| `table_advanced_climate_interaction_coefs.csv` | 高级气候交互项系数 |
| `table_multi_climate_model_comparison.csv` | 多气候指标模型比较 |
| `table_multi_climate_model_coefficients.csv` | 多气候指标系数 |
| `table_grid_model_comparison.csv` | 栅格模型比较 |
| `table_grid_model_coefficients.csv` | 栅格模型系数 |
| `table_variance_decomposition_r2.csv` | 方差分解 R² |
| `table_cross_effort_variance_decomposition.csv` | 跨 effort 方差分解 |
| `table_ml_vs_glmm_comparison.csv` | ML vs GLMM 比较 |
| `table_rf_variable_importance.csv` | RF 变量重要性 |
| `table_rf_variable_importance_detailed.csv` | RF 详细变量重要性 |
| `table_rf_future_predictions.csv` | RF 未来预测 (1440 行) |
| `table_xgboost_future_predictions.csv` | XGBoost 未来预测 (1440 行) |
| `table_vif_analysis.csv` | VIF 分析 |
| `table_proxy_selection_model_comparison.csv` | 代理变量选择模型比较 |
| `table_proxy_selection_coefficients.csv` | 代理变量选择系数 |
| `table_effort_offset_sensitivity.csv` | Effort offset 敏感性 |
| `hazard_model_effort_upgrade_bundle.xlsx` | Excel 结果包 (需更新) |

## 生成日期

2026-05-09
