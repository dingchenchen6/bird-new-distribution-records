# GitHub Repro Workflow

这份说明对应“别人拿到 GitHub 仓库后可以复现”的最小要求。

## 1. 仓库应包含的内容

建议同步到 GitHub 的核心内容：

- `code/run_bird_new_record_hazard_model.R`
- `code/run_bird_new_record_hazard_model_combined_threshold_test.R`
- `code/visualize_bird_new_record_hazard_results.R`
- `code/run_bird_new_record_hazard_server_bundle.sh`
- `HAZARD_MODEL_WORKFLOW.md`
- `GITHUB_REPRO_WORKFLOW.md`
- `README.md`

建议同步的关键结果与轻量数据：

- `combined_threshold_100_test/results/`
- `combined_threshold_100_test/logs/run_summary.md`
- `combined_threshold_50_test/results/`
- `combined_threshold_200_test/results/`
- `hazard_model_visualizations/results/`
- `hazard_model_visualizations/figures/`

## 2. 不建议直接同步的大文件

这些文件通常过大，不适合直接进 Git：

- 完整 range map
- 全部 climate raster
- 大体量 occurrence 原始文件
- 全量中间栅格或像元级 SDM 输出

更合适的做法：

- 在 README 中明确外部原始数据路径与来源
- 只同步用于复核的关键派生表
- 在服务器脚本中保留重跑入口

## 3. 推荐复现顺序

### 本地复现

```bash
cd bird_range_climate_shift_metrics
Rscript code/run_bird_new_record_hazard_model_combined_threshold_test.R 100 2002 2024
Rscript code/run_bird_new_record_hazard_model_combined_threshold_test.R 50 2002 2024
Rscript code/run_bird_new_record_hazard_model_combined_threshold_test.R 200 2002 2024
Rscript code/visualize_bird_new_record_hazard_results.R
```

### 服务器复现

```bash
cd bird_range_climate_shift_metrics
bash code/run_bird_new_record_hazard_server_bundle.sh
```

## 4. 当前主模型口径

- SDM 候选省份：`birdwatch_2002_2025 + rescue_1980_2025_gbif`
- 主模型阈值：`100`
- 敏感性阈值：`50`、`200`
- 主模型气候项：先仅使用温度梯度，不纳入降水
- 调查努力：`report_count` 与 `user_count`

## 5. 当前结果解释边界

当前仓库已能完成：

1. 风险集构建
2. 主模型与阈值敏感性分析
3. 结果可视化与结果包导出

如果后续要进一步提高正式发表版的可重复性，建议再补：

- 更直接的 `province-year climate` 全面板来源脚本
- `mig` 等性状字段的正式引入与审计
- 服务器环境依赖说明
