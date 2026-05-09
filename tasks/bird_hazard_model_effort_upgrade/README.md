# Bird Hazard Model Effort Upgrade Task

本任务将 SDM 约束的离散时间 hazard model 从"记录型 effort"升级为"观测者型 effort"，并系统扩展至高级气候指标、栅格尺度、ML 验证、多尺度未来预测，最终形成顶刊论文。

This task upgrades the hazard model from record-based to observer-based survey effort, extends it to advanced climate metrics, grid-scale, ML validation, and multi-scale future prediction, culminating in a top-journal manuscript.

## Purpose / 目的

现有 hazard model 的 `effort_record`（所有鸟类记录数）存在内生性：记录数部分由物种检出结果决定，而非纯采样强度。本任务用占域模型的观测者型指标（n_visits, n_observers, n_birding_days, PCA 复合指标）替换旧指标，检验核心结论——"可见度阈值"交互效应——是否稳健。

## Core scientific test / 核心科学检验

| 结果 | 含义 |
|------|------|
| 交互项在所有 effort 指标下均显著为正 | "可见度阈值"解释得到强支撑 |
| 交互项在观测者型 effort 下减弱或消失 | 现有结论需修正 |

**实际结果：四种 effort 指标下交互项均显著为正 (HR 1.18–1.31, p < 6e-05)，核心结论得到强支撑。**

## Analysis design / 分析设计

- 4 种 effort 指标 × 5 个模型阶段 (M0-M4) = 20 个模型
- 6 种气候指标 × 4 种 effort 指标 = 24 个高级气候交互模型
- 扩展模型 M5-M7（+降水, +迁徙策略）
- 50km/100km 栅格尺度验证
- XGBoost + Random Forest 机器学习交叉验证
- 省/市/县级 + 多分辨率网格未来预测
- 引擎：`glmmTMB` (cloglog), XGBoost, Random Forest

## Directory structure / 目录结构

```
bird_hazard_model_effort_upgrade/
  code/        — 16 R scripts (01-16, see below)
  data/        — upgraded risk set, effort panels, grid data, climate metrics
  figures/     — 37+ publication-quality figures (see task_summary.md)
  results/     — 25 result tables, Excel bundle, model objects
  README.md    — this file
  METHODS.md   — detailed analytical workflow
```

## Scripts / 脚本 (16)

### Phase 1: 核心稳健性 (Core robustness)
| Script | Purpose |
|--------|---------|
| `01_build_effort_upgraded_risk_set.R` | 构建含 4 种 effort 指标的升级版风险集 |
| `02_hazard_model_effort_comparison.R` | 4×5 模型矩阵 + 扩展模型 M5-M7 |
| `03_hazard_model_upgrade_visualization.R` | 森林图、山脊图、AIC 热力图、HR 稳定性图、4-panel |

### Phase 2: 高级气候 (Advanced climate)
| Script | Purpose |
|--------|---------|
| `04_compute_advanced_climate_metrics.R` | 计算 climate velocity, Mahalanobis, exposure, warming rate |
| `05_hazard_model_advanced_climate.R` | 6 气候 × 4 effort 交互模型 |

### Phase 3: 栅格验证 (Grid-scale)
| Script | Purpose |
|--------|---------|
| `06_build_grid_infrastructure.R` | 50km/100km 栅格风险集 |
| `07_hazard_model_grid.R` | 栅格尺度 hazard model |

### Phase 4: 位移与方差分解 (Displacement & variance)
| Script | Purpose |
|--------|---------|
| `08_compute_displacement_direction.R` | 省级位移方向、距离、风玫瑰图 |
| `09_variance_partitioning.R` | 方差分解：加性 vs 交互 R² |

### Phase 5: ML 验证 (ML validation)
| Script | Purpose |
|--------|---------|
| `10_xgboost_shap_prediction.R` | XGBoost + SHAP + 未来预测 |
| `11_rf_hazard_prediction.R` | Random Forest + 变量重要性 + 未来预测 |

### Phase 6: Range map & 代理变量 (Range map & proxy)
| Script | Purpose |
|--------|---------|
| `12_range_map_native_climate.R` | 物种分布图本地气候异常值 |
| `14_vif_correlation_proxy_selection.R` | VIF + 相关性 + 代理变量选择 |

### Phase 7: 可视化 & 热点图 (Visualization & hotspots)
| Script | Purpose |
|--------|---------|
| `13_comprehensive_visualization.R` | 数据分布 + 模型结果 + ML 对比 + 未来预测图 |
| `15_future_hotspot_maps.R` | 省级空间热点地图 (SSP245/585 × 2030/2040/2050) |

### Phase 8: 多尺度未来预测 (Multi-scale future prediction)
| Script | Purpose |
|--------|---------|
| `16_multi_scale_future_prediction.R` | 省/市/县级 + 多分辨率网格未来预测与地图 |

## Key outputs / 关键产出

- `data/hazard_risk_upgraded_complete_case.csv` — 升级版风险集
- `results/table_cross_specification_key_coefficients.csv` — 核心科学发现
- `results/table_advanced_climate_key_coefficients.csv` — 高级气候关键系数
- `results/table_grid_model_comparison.csv` — 栅格模型比较
- `results/table_variance_decomposition_r2.csv` — 方差分解
- `results/table_xgboost_future_predictions.csv` — XGBoost 未来预测
- `results/table_rf_future_predictions.csv` — RF 未来预测
- `results/hazard_model_effort_upgrade_bundle.xlsx` — Excel 结果包
- `figures/fig_hazard_combined_4panel.png` — 综合四面板图
- `results/task_summary.md` — 完整任务总结

## Dependencies / 依赖

- Task A `bird_survey_effort_integration/data/` — 桥接后的 effort 面板
- `bird_new_record_hazard_model/code/run_bird_new_record_hazard_model.R` — helper 函数
- `bird_new_record_hazard_model/combined_threshold_100_test/` — 现有派生输入
- Provincial boundary shapefiles: `/Users/dingchenchen/Documents/SDMs/GS(2023)2767审图号/`
- Prefecture/county boundary shapefiles (for script 16)

## Recommended reading order / 建议阅读顺序

1. `METHODS.md`
2. `results/task_summary.md` — 完整任务总结
3. `results/table_cross_specification_key_coefficients.csv` — 核心科学发现
4. `figures/fig_hazard_combined_4panel.png` — 综合图
5. `results/hazard_model_effort_upgrade_bundle.xlsx` — 完整结果包
