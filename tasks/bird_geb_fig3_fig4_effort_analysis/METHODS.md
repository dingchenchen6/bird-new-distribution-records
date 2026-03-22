# Methods

## Chinese overview / 中文概述

本任务分为两个分析层次，并在每个层次都加入了更严格的来源审计与诊断。

### 1. 物种层（Figure 3）

物种层分析的目标是回答：在控制系统发育非独立性之后，哪些鸟类更容易产生新的省级分布纪录。

本轮升级中，物种层的核心改动包括：

- 以 `AVONET` 和 `中国鸟类生态学特征` 为主要性状来源；
- 明确加入 `HWI`；
- 连续性状与分类性状分开建模；
- 使用 Hackett 鸟类系统发育树构建协方差矩阵，在 `brms` 中通过系统发育随机项控制物种间非独立性；
- 使用后验山脊图替代传统单纯点线图，以更直观地展示估计分布、可信区间和不确定性。

连续性状主模型包括：

- body mass
- hand-wing index (HWI)
- range size (province count)
- clutch size
- number of congeners

分类性状主模型包括：

- endemicity
- migration strategy
- diet guild
- forest-associated primary habitat

### 2. 省级层（Figure 4）

省级层分析延续 GEB 风格框架，用于回答：哪些省级结构因素与调查努力组合更容易产生鸟类新纪录。

保留和升级的要点包括：

- 历史调查努力：1980-1999
- 当前调查努力：2000-2024
- effort 以年均值和单位面积强度表示
- 响应变量使用单位面积新纪录强度的对数转换值
- 保留用户 effort 替代模型、Cook's distance 高影响点敏感性分析和 bootstrap 层次分解
- Figure 4 改为更横向的 2×4 多面板布局

## English overview

This task implements a two-tier analytical design with explicit source auditing and diagnostics.

### 1. Species-level analysis (Figure 3)

The species-level analysis asks which bird species are more likely to generate new provincial distribution records after accounting for phylogenetic non-independence.

Major upgrades in this round include:

- primary trait sourcing from `AVONET` and the Wang Yanping bird ecological dataset;
- explicit inclusion of `HWI`;
- separate phylogenetic Bernoulli models for continuous traits and categorical ecological traits;
- use of the Hackett bird phylogeny to model phylogenetic covariance in `brms`;
- posterior ridge-plot visualization to communicate uncertainty, effect direction, and interval width more clearly.

Continuous-trait model predictors:

- body mass
- hand-wing index (HWI)
- range size (province count)
- clutch size
- number of congeners

Categorical-trait model predictors:

- endemicity
- migration strategy
- diet guild
- forest-associated primary habitat

### 2. Province-level analysis (Figure 4)

The province-level analysis retains the GEB-style effort-driver framework and evaluates how effort and structural province characteristics shape discovery intensity.

Retained and upgraded components:

- historical effort: 1980-1999
- current effort: 2000-2024
- annualized and area-standardized effort metrics
- log-transformed new-record density as the primary response
- alternative user-effort model
- Cook's-distance-based influence sensitivity
- bootstrap hierarchical partitioning
- a wider 2×4 figure layout for publication use

## Source-audit rules

Primary trait sources in this round were restricted to:

- `AVONET traits`
- `中国鸟类生态学特征`
- checklist-derived taxonomic quantities from `2025中国生物物种名录`

Requested variables that were **not stably supportable** from these user-specified primary sources in the current workbook were not forced into the main model. In particular:

- `habitat breadth`
- `generation length`
- `naming/description year`

were kept in the candidate-variable audit but not used as primary-model predictors.

`Forest dependence` was operationalized as a proxy using AVONET primary habitat classes (`Forest` or `Woodland` versus non-forest habitats). It should therefore be interpreted as `forest-associated primary habitat`, not as a direct, independently validated dependence index.

## Diagnostics and validation

### Species level

- duplicate-event screening
- key-field QA checks
- candidate-variable availability audit
- missingness summaries
- continuous-trait distribution checks
- categorical-frequency screening
- phylogeny-matching audit
- posterior diagnostics (`Rhat`, `ESS`)
- posterior predictive checks
- Bayes R2
- phylogenetic signal extraction
- non-phylogenetic negative-binomial count sensitivity model

### Province level

- missingness checks for effort data
- outlier screening for report and user counts
- province-year coverage summaries
- predictor-source auditing
- correlation screening
- VIF-based collinearity diagnostics
- residual diagnostics
- heteroskedasticity test
- Cook's distance screening
- alternative effort-proxy model
- influence-filtered sensitivity model
- bootstrap hierarchical partitioning

## Primary scripts

- current recommended script:
  `code/run_bird_geb_fig3_fig4_effort_analysis_v2_phylo.R`
- retained legacy script:
  `code/run_bird_geb_fig3_fig4_effort_analysis.R`
