# Bird SDM Distribution Modeling From China Birdwatch Records (2002-2025)

## Overview | 任务概述

**English**

This independent task rebuilds species distribution models for China's
new-record bird species using the China Birdwatch dataset from 2002 to 2025.
It does not overwrite the earlier SDM task. The workflow reads birdwatch
checklist observations, cleans and harmonizes taxonomy, restricts all
occurrences, climate layers, background sampling, prediction, and province
overlay to the China boundary, and produces maps, rasters, diagnostics, and
province-level potential distribution summaries.

**中文**

本独立任务基于 2002-2025 年中国观鸟数据集，重新开展中国鸟类新纪录物种
的物种分布建模。它不会覆盖此前的 SDM 任务。流程会读取观鸟清单记录，
完成数据清洗和分类统一，并将出现点、气候变量、背景点、预测范围以及
省级叠加全部限制在中国范围内，最终输出图件、栅格、模型诊断和省级潜在
分布汇总结果。

## Scientific Question | 科学问题

**English**

When China's new-record bird species are modeled only with birdwatch
observations collected in China from 2002 to 2025, what are their potential
climatically suitable distributions across Chinese provinces, and how much
suitable area does each potential province contain?

**中文**

当仅使用 2002-2025 年中国观鸟记录、并限定在中国范围内进行建模时，
中国鸟类新纪录物种在各省的潜在气候适生分布格局如何？每个潜在分布省份
分别包含多少适生面积及其面积比例？

## Workflow | 分析步骤

1. [01_data_input_and_taxonomy_review.R](code/01_data_input_and_taxonomy_review.R)  
   English: import and standardize the China Birdwatch occurrence data, build
   the new-record species pool, and generate the taxonomy review table with
   Avibase/IOC and BirdLife review fields.  
   中文：导入并标准化中国观鸟出现点数据，构建新纪录物种池，并生成包含
   Avibase/IOC 和 BirdLife 审查字段的分类校审表。

2. [02_data_check_and_environment_preparation.R](code/02_data_check_and_environment_preparation.R)  
   English: verify China-range occurrence inputs, prepare current climate and
   elevation predictors, and perform environmental-variable screening.  
   中文：检查中国范围出现点输入，准备当前气候和海拔变量，并执行环境变量筛选。

3. [03_sdm_modeling_and_diagnostics.R](code/03_sdm_modeling_and_diagnostics.R)  
   English: run species-level SDMs, evaluate model performance, export
   probability and binary rasters, plot suitability maps with cleaned
   observation points, and summarize province-level potential distributions.  
   中文：开展物种级 SDM 建模，评估模型表现，输出概率与二值栅格，
   绘制叠加清洗后观测点的适宜度图，并汇总省级潜在分布结果。

The integrated pipeline is implemented in
[run_bird_sdm_distribution_modeling_birdwatch_2002_2025.R](code/run_bird_sdm_distribution_modeling_birdwatch_2002_2025.R).

## Input Data | 输入数据

### Core species tables | 核心物种表

- `source_data/bird_new_records_clean.csv`
- `source_data/bird_species_pool_with_traits.csv`

### Primary occurrence source | 主要出现点来源

- Extracted China Birdwatch workbooks, expected under:
  `data/raw_china_birdwatch_2002_2025/中国观鸟数据集/`
- Required subfolders:
  - `详情数据/`
  - `鸟种数据/`
  - `观鸟统计报告/`

The workflow merges these tables by checklist `serial_id`, extracts
longitude/latitude from the `location` field, filters years to 2002-2025,
keeps only points within China, and writes a standardized occurrence CSV for
reproducible reruns.

### Optional occurrence replay | 可复现实验的出现点重放

- Standardized CSV path:
  `data/occurrence/occurrence_records_clean_standardized.csv`
- You can also point `BIRD_SDM_OCCURRENCE_CSV_PATH` to another cleaned CSV
  that keeps `shp_species`, `longitude`, and `latitude`.

### Boundary and climate data | 边界与气候数据

- Province boundary shapefile: shared China province base map in this repo
- Dash-line shapefile: shared dash-line base map in this repo
- Climate and elevation: loaded through `geodata` / local cache under
  `data/climate/` and not committed to GitHub

## Key Outputs | 关键输出

### Tables | 表格

- `data/tables/table_species_status_summary.csv`
- `data/tables/table_model_metrics_summary.csv`
- `data/tables/table_model_occurrence_points_used_all_species.csv`
- `data/tables/table_province_prediction_summary.csv`
- `data/tables/table_potential_province_listing_all_species.csv`
- `data/tables/table_raster_output_manifest.csv`

### Species-level products | 物种级产物

- Probability raster and binary raster for each modeled species
- Suitability map for each modeled species with cleaned modeling points overlaid
- Per-species point table recording the observation points actually used in the model

### Final province listing | 最终省级总表

The final province listing table keeps only successfully modeled new-record
species and reports:

- species
- potential province
- suitable-cell threshold
- suitable cell count
- suitable area (`km^2`)
- province total area (`km^2`)
- suitable area as a proportion of the species' total suitable area
- suitable area as a proportion of the province area

## Reproducible Commands | 复现命令

```bash
Rscript tasks/bird_sdm_distribution_modeling_birdwatch_2002_2025/code/01_data_input_and_taxonomy_review.R
Rscript tasks/bird_sdm_distribution_modeling_birdwatch_2002_2025/code/02_data_check_and_environment_preparation.R
Rscript tasks/bird_sdm_distribution_modeling_birdwatch_2002_2025/code/03_sdm_modeling_and_diagnostics.R --current-only --workers=1
```

## Notes | 说明

- This task is independent from the earlier SDM task and should be run as a
  separate analysis stream.
- Modeling and prediction are China-only.
- Maps use a China-focused equal-area projection and include the dash-line overlay.
- MaxEnt is included and prefers `maxent.jar`, with `maxnet` as fallback.
- Large raw birdwatch files and climate cache are better handled on the server,
  while code and key results are suitable for GitHub synchronization.
- For repository size control, the GitHub task bundle keeps code, cleaned input,
  summary figures, and core result tables; the full set of per-species rasters,
  per-species maps, and oversized presentation exports remain on the server and
  are tracked through `table_raster_output_manifest.csv`.
