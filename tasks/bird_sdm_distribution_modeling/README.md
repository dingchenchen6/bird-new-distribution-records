# Bird SDM Distribution Modeling

## Overview | 任务概述

**English**

This task models the potential distributions of China's bird new-record species
within the China boundary only. The workflow uses a cleaned occurrence CSV or
optionally re-downloads occurrences from GBIF, prepares WorldClim climate and
elevation predictors, fits SDMs including MaxEnt, and summarizes province-level
potential distributions under 3, 10, 20, and 50 suitable-cell thresholds.

**中文**

本任务仅在中国范围内，对“中国鸟类新纪录物种”开展潜在分布建模。
流程支持直接读取整理好的出现点 CSV，也支持从 GBIF 重新下载并清洗出现点，
随后准备 WorldClim 气候与海拔变量，运行包含 MaxEnt 在内的 SDM，
并在 3、10、20、50 个适生栅格阈值下汇总潜在分布省份。

## Research Question | 科学问题

**English**

How do the current and future climatically suitable distributions of China's
bird new-record species vary within China when occurrence data are filtered and
modeled under a unified SDM workflow?

**中文**

在统一的 SDM 工作流下，当出现点数据经过标准化与中国范围筛选后，
中国鸟类新纪录物种在当前与未来气候条件下的适生分布会呈现怎样的空间格局？

## Workflow | 分析步骤

1. [01_data_input_and_taxonomy_review.R](code/01_data_input_and_taxonomy_review.R)  
   English: Build the modeling species pool, read occurrence inputs, and create
   the taxonomy review table with Avibase/IOC and BirdLife review fields.  
   中文：构建建模物种池、读取出现点输入，并生成包含 Avibase/IOC 与
   BirdLife 审查字段的分类校审表。

2. [02_data_check_and_environment_preparation.R](code/02_data_check_and_environment_preparation.R)  
   English: Check occurrence data within China, prepare WorldClim climate and
   elevation variables, and perform predictor screening.  
   中文：检查中国范围内的出现点数据，准备 WorldClim 气候与海拔变量，
   并执行环境变量筛选。

3. [03_sdm_modeling_and_diagnostics.R](code/03_sdm_modeling_and_diagnostics.R)  
   English: Fit species-level SDMs, evaluate model quality, generate rasters
   and maps, and summarize province-level potential distributions.  
   中文：开展物种级 SDM 建模、模型评估、栅格与地图输出，以及省级潜在
   分布汇总。

The comprehensive workflow is also available in
[run_bird_sdm_distribution_modeling.R](code/run_bird_sdm_distribution_modeling.R).

## Input Data | 输入数据

### Core species tables | 核心物种表

- `source_data/bird_new_records_clean.csv`
- `source_data/bird_species_pool_with_traits.csv`

### Occurrence input options | 出现点输入方式

**Option A. Recommended cleaned CSV | 推荐：整理后的 CSV**

- Default path: `data/occurrence/occurrence_records_clean_standardized.csv`
- Required columns: `species, longitude, latitude`
- Optional columns: `order_occurrence, family_occurrence`

**Option B. Rebuild from GBIF | 从 GBIF 重新提取**

- Set `BIRD_SDM_OCCURRENCE_SOURCE=rgbif`
- Install package `rgbif`
- The workflow will query the matched new-record species in China only

**Option C. Optional shapefile | 可选：本地 shapefile**

- Set `BIRD_SDM_OCCURRENCE_SHP_PATH` to a local shapefile path
- This is optional and is no longer required for default reproducible runs

### Boundary data | 边界数据

The workflow expects a province boundary shapefile and a dash-line shapefile.
By default, it reuses the shared base map files already stored in this
repository:

- `tasks/bird_spatiotemporal_patterns/data/shapefile_base/省.shp`
- `tasks/bird_spatiotemporal_patterns/data/shapefile_base/十段线.shp`

You can override them with:

- `BIRD_SDM_PROVINCE_SHP_PATH`
- `BIRD_SDM_DASHLINE_SHP_PATH`

### Climate data | 气候数据

No large climate cache is required in GitHub.

- Current climate and elevation are downloaded or reused through `geodata`
- Future climate uses the scenario table in `data/climate_scenario_config.csv`
- Downloaded rasters are stored under `data/climate/`, which is ignored by Git

## Reproducible Commands | 复现命令

### 1. Taxonomy review | 分类校审

```bash
Rscript tasks/bird_sdm_distribution_modeling/code/01_data_input_and_taxonomy_review.R
```

### 2. Data checks and climate preparation | 数据检查与环境准备

```bash
Rscript tasks/bird_sdm_distribution_modeling/code/02_data_check_and_environment_preparation.R
```

### 3. Current-climate SDM run | 当前气候 SDM 运行

```bash
Rscript tasks/bird_sdm_distribution_modeling/code/03_sdm_modeling_and_diagnostics.R --current-only --workers=1
```

### 4. Optional GBIF rebuild | 可选：从 GBIF 重新提取

```bash
BIRD_SDM_OCCURRENCE_SOURCE=rgbif \
Rscript tasks/bird_sdm_distribution_modeling/code/01_data_input_and_taxonomy_review.R
```

## Outputs | 输出结果

### Tables | 表格

- `data/tables/table_species_status_summary.csv`
- `data/tables/table_model_metrics_summary.csv`
- `data/tables/table_potential_province_listing_all_species.csv`
- `data/tables/table_province_prediction_summary.csv`
- `data/tables/table_raster_output_manifest.csv`

### Maps and rasters | 地图与栅格

- Species probability rasters: `data/rasters/<species_slug>/`
- Species maps: `figures/species_maps/`
- Summary figures: `figures/summaries/`

### Reports | 报告

- `results/task_summary.md`
- `results/bird_sdm_summary.pptx`

## Notes | 说明

- Modeling, prediction, background sampling, and province overlay are all
  restricted to the China boundary.
- Maps use a China-focused equal-area projection and include the dash-line
  overlay.
- MaxEnt is included. The workflow prefers `maxent.jar` when available and
  falls back to `maxnet`.
- The final province listing table keeps only successfully modeled species.
