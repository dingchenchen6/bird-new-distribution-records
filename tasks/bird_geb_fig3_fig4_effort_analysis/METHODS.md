# Methods

## 中文概述

本任务围绕两个层级展开：

1. 物种层：
   使用鸟类新纪录主表与多来源性状数据构建物种池，拟合
   - 二元 Logistic 模型：解释某物种是否产生新省级纪录
   - 负二项模型：解释某物种积累了多少条新省级纪录

2. 省级层：
   使用新提供的调查努力表，将努力划分为
   - 历史调查努力：1980-1999
   - 当前调查努力：2000-2024

   并计算面积标准化后的调查强度，再结合省级面积、人均 GDP、栖息地异质性等变量，解释鸟类新纪录发现强度的空间差异。

## English overview

This task implements a two-level analytical design:

1. Species level:
   a bird species pool was reconstructed by integrating the bird new-record table with trait information from AVONET, the Chinese ecological trait sheet, and BIRDBASE. Two complementary models were fitted:
   - a binomial model for whether a species produced any new provincial record;
   - a negative-binomial model for the number of new provincial records accumulated by each species.

2. Province level:
   the newly provided survey-effort table was divided into two eras:
   - historical effort: 1980-1999
   - current effort: 2000-2024

   Effort variables were annualized and area-standardized, and then linked to province-level structural covariates to explain spatial variation in discovery intensity.

## Key processing choices

- Duplicate bird-record events were removed at the `species-province-year` level.
- Effort outliers were screened using the IQR rule.
- Province-level primary response:
  `log(1 + records per 100,000 km2)`
- Candidate province predictors were screened for collinearity before final model selection.
- The original mammal GEB repository includes a province-level richness field, but that field was treated as a reference-only variable here because it is taxonomically mismatched for a bird-specific analysis.
- Current report effort and current user effort were first evaluated together in a screening model to diagnose redundancy; because their joint VIF values were extremely high, they were analysed in separate robustness models rather than forced into one final model.
- Relative importance was estimated using manual hierarchical partitioning with bootstrap confidence intervals.
- A count-based sensitivity model with an area offset was fitted to evaluate robustness.
- Influence diagnostics were evaluated using Cook's distance, leverage, and DFFITS. Provinces exceeding the conventional `4/n` Cook's-distance threshold were removed in an additional sensitivity model to confirm that the main effect directions were stable.

## Additional diagnostics and outputs

- `data/province_predictor_source_audit.csv`
  documents which province-level predictors were retained, screened only, or excluded because they were inherited from mammal-specific source data.
- `data/province_candidate_model_comparison.csv`
  compares the primary report-effort model, the alternative user-effort model, the influence-filtered model, and the count-based sensitivity model.
- `data/province_influential_units.csv`
  lists provinces flagged by Cook's distance in the primary province-level model.
- `figures/fig_s0_data_screening_overview.*`
  summarizes data distributions, effort outlier counts, and trait missingness.
- `figures/fig_s3_province_model_robustness.*`
  summarizes coefficient stability, model-comparison metrics, and influential provinces.

## Main script

- `code/run_bird_geb_fig3_fig4_effort_analysis.R`
