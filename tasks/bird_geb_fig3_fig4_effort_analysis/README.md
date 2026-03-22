# 鸟类新纪录 GEB Figure 3 / Figure 4 调查努力分析任务

本任务基于鸟类新纪录主表、用户新提供的省份-年份调查努力表，以及哺乳动物 GEB 研究中使用过的省级通用协变量，重建鸟类版的 `Figure 3` 和 `Figure 4` 分析框架。

主要内容：

- `Figure 3`：物种层相关因子分析
  - 评估体重、分布范围、迁徙性和风险组对新纪录出现概率与新纪录频次的影响
- `Figure 4`：省级调查努力驱动分析
  - 评估历史/当前调查努力与省级结构因子对新纪录发现强度的影响
- 模型诊断与敏感性分析
  - 共线性筛查
  - 残差诊断
  - bootstrap 层次分解
  - 面积 offset 敏感性模型

目录结构：

- `code/`
- `data/`
- `figures/`
- `results/`

关键输出：

- `figures/fig_geb3_species_level_correlates.*`
- `figures/fig_geb4_province_level_effort_drivers.*`
- `figures/fig_s1_species_model_diagnostics.*`
- `figures/fig_s2_province_model_diagnostics_and_sensitivity.*`
- `results/task_summary_bilingual.md`
- `results/figure_captions_bilingual.md`
- `results/bird_geb_fig3_fig4_analysis_bundle.xlsx`

说明：

- 省级主模型使用单位面积新纪录强度的对数转换值，尽量减少大面积省份对结果的机械性影响。
- 由于当前提供的数据中没有直接的省级鸟类总物种丰富度字段，因此本轮 `Figure 4` 重点聚焦于调查努力与省级结构因子的解释，而不是直接复刻哺乳动物研究中的“省级总丰富度”变量。
