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
- Relative importance was estimated using manual hierarchical partitioning with bootstrap confidence intervals.
- A count-based sensitivity model with an area offset was fitted to evaluate robustness.

## Main script

- `code/run_bird_geb_fig3_fig4_effort_analysis.R`
