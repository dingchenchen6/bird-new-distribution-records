# Server Rerun Guide

这份文档用于把当前 hazard 模型完整搬到服务器上重跑，并确保输出结果适合论文、汇报与 GitHub 复现。当前服务器版口径已经统一为“温度梯度 + record effort + 交互项”，不再使用 observer effort。

## 1. 服务器重跑目标

服务器端建议完成 4 件事：

1. 主模型重跑：`threshold = 100`
2. 敏感性分析：`threshold = 50` 与 `200`
3. 结果可视化：summary 表、森林图、山脊图、Excel bundle
4. 保留日志与路径清单，便于复核和 GitHub 同步

## 2. 需要准备的输入文件

主脚本现在支持通过环境变量传入路径。

必须准备：

- `HAZARD_NEW_RECORD_CSV`
- `HAZARD_EVENT_TABLE_CSV`
- `HAZARD_SPECIES_YEAR_NATIVE_CSV`
- `HAZARD_EFFORT_CSV`
- `HAZARD_EFFORT_LOOKUP_CSV`
- `HAZARD_SDM_BIRDWATCH_CSV`
- `HAZARD_SDM_RESCUE_CSV`

建议对应内容：

- `HAZARD_NEW_RECORD_CSV`
  - `bird_new_records_for_range_climate.csv`
- `HAZARD_EVENT_TABLE_CSV`
  - `table_new_record_province_year_climate_direction_displacement.csv`
- `HAZARD_SPECIES_YEAR_NATIVE_CSV`
  - `species_year_historical_range_climate.csv`
- `HAZARD_EFFORT_CSV`
  - `effort_py_record_only_server_sync.csv`
- `HAZARD_EFFORT_LOOKUP_CSV`
  - 带 `province_cn -> province` 映射的 effort 清洗表
- `HAZARD_SDM_BIRDWATCH_CSV`
  - `birdwatch_2002_2025` 的 `table_potential_province_listing_all_species.csv`
- `HAZARD_SDM_RESCUE_CSV`
  - `rescue_1980_2025_gbif` 的 `table_potential_province_listing_all_species.csv`

## 3. 推荐运行方式

### 3.1 建议先同步到服务器的最小文件集

如果服务器已经有 WorldClim 结果和 SDM 结果，那么本次真正需要同步的通常只有：

- 当前仓库代码
- `data/effort_py_record_only_server_sync.csv`
- `data/province_lookup_server_sync.csv`

这样可以避免重复传输大体积 climate 结果文件。

### 3.2 设置环境变量

```bash
cd /path/to/bird_range_climate_shift_metrics

export HAZARD_NEW_RECORD_CSV="/path/to/bird_new_records_for_range_climate.csv"
export HAZARD_EVENT_TABLE_CSV="/path/to/table_new_record_province_year_climate_direction_displacement.csv"
export HAZARD_SPECIES_YEAR_NATIVE_CSV="/path/to/species_year_historical_range_climate.csv"
export HAZARD_EFFORT_CSV="/path/to/effort_py_record_only_server_sync.csv"
export HAZARD_EFFORT_LOOKUP_CSV="/path/to/province_lookup_server_sync.csv"
export HAZARD_SDM_BIRDWATCH_CSV="/path/to/birdwatch_table_potential_province_listing_all_species.csv"
export HAZARD_SDM_RESCUE_CSV="/path/to/rescue_table_potential_province_listing_all_species.csv"
```

### 3.3 一键重跑 bundle

```bash
bash code/run_bird_new_record_hazard_server_bundle.sh
```

## 4. 运行后重点检查

先检查：

- `combined_threshold_100_test/results/input_coverage_summary.csv`
- `combined_threshold_100_test/results/model_comparison.csv`
- `combined_threshold_50_test/results/model_comparison.csv`
- `combined_threshold_200_test/results/model_comparison.csv`
- `hazard_model_visualizations/results/table_model_comparison_across_thresholds.csv`

再检查：

- `hazard_model_visualizations/figures/fig_hazard_forest_best_model.png`
- `hazard_model_visualizations/figures/fig_hazard_ridge_coefficients.png`

最后检查：

- 每个目录中的 `logs/run_summary.md`
- 每个阈值目录中的 `results/input_path_manifest.csv`

## 5. 服务器端推荐核对顺序

建议按下面顺序核对，避免“脚本跑完了但不知道哪一步出了问题”。

1. 先看 `combined_threshold_100_test/results/input_path_manifest.csv`
   目的：确认服务器实际读入的是哪一套文件。
2. 再看 `combined_threshold_100_test/results/input_coverage_summary.csv`
   目的：确认支持的新纪录数量、风险集规模和事件数是否合理。
3. 再看 `combined_threshold_100_test/results/model_comparison.csv`
   目的：确认 `M4` 是否仍为最佳模型。
4. 再看 `combined_threshold_100_test/results/model_coefficients.csv`
   目的：确认 `temp_grad_z:log_effort_record_z` 是否保持正向且显著。
5. 最后看 `hazard_model_visualizations/` 下的图和汇总表。

## 6. 当前推荐解释口径

如果服务器结果与本地一致，则当前主结论可表述为：

1. 最优模型是 `M4`，即“气候 × effort”共同驱动模型。
2. `temp_grad_z:log_effort_record_z` 为稳健正效应。
3. 结果对 `50 / 100 / 200` 阈值敏感性较低。

## 7. 仍需保留的审慎说明

当前 `province-year climate` 面板是基于已有事件层气候结果补成的测试版面板。
如果后续要进入正式发表版最终模型，仍建议在服务器上补一版完整的 `province-year annual climate panel`，并用其替换当前的补齐版输入。
