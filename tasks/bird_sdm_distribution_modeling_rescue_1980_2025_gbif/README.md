# Bird SDM Rescue Modeling From China Birdwatch (1980-2025) And GBIF

## Overview | 任务概述

**English**

This independent rescue task focuses on the new-record bird species that were
not successfully modeled in the earlier China Birdwatch SDM workflow. It
expands the occurrence search window to 1980-2025, re-checks taxonomy and
synonymy, combines China Birdwatch records with GBIF occurrences, restricts
all occurrences, climate layers, background sampling, prediction, and province
overlay to the China boundary, and produces maps, rasters, diagnostics, and
province-level potential distribution summaries without overwriting the earlier
task.

**中文**

本独立补救任务聚焦于此前中国观鸟 SDM 工作流中未成功建模出图的新纪录
鸟类物种。它将出现点时间窗扩展到 1980-2025 年，重新检查分类与同物异名，
并合并中国观鸟数据与 GBIF 记录。在不覆盖旧任务的前提下，流程仍将出现点、
气候变量、背景点、预测范围以及省级叠加全部限制在中国范围内，最终输出
图件、栅格、模型诊断和省级潜在分布汇总结果。

## Scientific Question | 科学问题

**English**

For the new-record bird species that were not successfully modeled in the
earlier workflow, can expanded China Birdwatch observations from 1980 to 2025
plus GBIF occurrence records recover robust China-only SDMs, and what
province-level potential distributions and suitable-area proportions emerge?

**中文**

对于此前未成功建模的中国鸟类新纪录物种，若将中国观鸟记录扩展到
1980-2025 年并结合 GBIF 记录、且仍限定在中国范围内进行建模，是否能够
恢复稳健的 SDM 结果？相应的省级潜在分布、省级适生面积及面积比例如何？

## Workflow | 分析步骤

1. [01_data_input_and_taxonomy_review.R](code/01_data_input_and_taxonomy_review.R)  
   English: build the rescue target species pool from the earlier SDM status
   table, import and standardize China Birdwatch 1980-2025 records, and
   generate the taxonomy review table with Avibase/IOC and BirdLife review
   fields.  
   中文：基于上一轮 SDM 状态表构建补救目标物种池，导入并标准化
   1980-2025 年中国观鸟记录，并生成包含 Avibase/IOC 和 BirdLife 审查
   字段的分类校审表。

2. [02_data_check_and_environment_preparation.R](code/02_data_check_and_environment_preparation.R)  
   English: verify the rescue-task occurrence inputs from China Birdwatch and
   GBIF, prepare current climate and elevation predictors, and perform
   environmental-variable screening.  
   中文：检查来自中国观鸟与 GBIF 的补救任务出现点输入，准备当前气候和
   海拔变量，并执行环境变量筛选。

3. [03_sdm_modeling_and_diagnostics.R](code/03_sdm_modeling_and_diagnostics.R)  
   English: run the rescue SDM workflow, evaluate model performance, export
   probability and binary rasters, plot suitability maps with cleaned modeling
   points, and summarize province-level potential distributions.  
   中文：运行补救版 SDM 流程，评估模型表现，输出概率与二值栅格，
   绘制叠加清洗后建模点的适宜度图，并汇总省级潜在分布结果。

The integrated pipeline is implemented in
[run_bird_sdm_distribution_modeling_rescue_1980_2025_gbif.R](code/run_bird_sdm_distribution_modeling_rescue_1980_2025_gbif.R).

## Input Data | 输入数据

### Core species tables | 核心物种表

- `source_data/bird_new_records_clean.csv`
- `source_data/bird_species_pool_with_traits.csv`

### Primary occurrence source | 主要出现点来源

- Extracted China Birdwatch workbooks, expected under:
  `data/raw_china_birdwatch_1980_2025/中国观鸟数据集/`
- Required subfolders:
  - `详情数据/`
  - `鸟种数据/`
  - `观鸟统计报告/`

The workflow merges these tables by checklist `serial_id`, extracts
longitude/latitude from the `location` field, filters years to 1980-2025,
keeps only points within China, and writes a standardized occurrence CSV for
reproducible reruns.

### Supplemental GBIF source | GBIF 补充出现点

- Optional GBIF retrieval through `rgbif`
- Default retrieval keeps the China country filter (`CN`) so that the rescue
  task remains China-focused
- GBIF rows are harmonized to the same standardized occurrence schema and then
  merged with the cleaned China Birdwatch records

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
Rscript tasks/bird_sdm_distribution_modeling_rescue_1980_2025_gbif/code/01_data_input_and_taxonomy_review.R
Rscript tasks/bird_sdm_distribution_modeling_rescue_1980_2025_gbif/code/02_data_check_and_environment_preparation.R
Rscript tasks/bird_sdm_distribution_modeling_rescue_1980_2025_gbif/code/03_sdm_modeling_and_diagnostics.R --current-only --workers=1
```

## Notes | 说明

- This task is independent from the earlier SDM task and should be run as a
  separate rescue-analysis stream.
- Modeling and prediction are China-only.
- Maps use a China-focused equal-area projection and include the dash-line overlay.
- MaxEnt is included and prefers `maxent.jar`, with `maxnet` as fallback.
- Large raw birdwatch files, GBIF pulls, and climate cache are better handled
  on the server, while code and key results are suitable for GitHub synchronization.
- For repository size control, the GitHub task bundle keeps code, cleaned input,
  summary figures, and core result tables; the full set of per-species rasters,
  per-species maps, and oversized presentation exports remain on the server and
  are tracked through `table_raster_output_manifest.csv`.
