# Methods | 方法说明

## Objective | 目标
Summarize bird new-distribution records into order, province, year, and IUCN status flows, with a focus on the English Sankey version for manuscript and presentation use.
将鸟类新纪录汇总为目、省份、年份和IUCN等级流向数据，并重点输出适合论文和汇报使用的英文桑基图。

## Workflow | 分析流程
1. Clean taxonomic names, province names, years, and status fields.
2. Aggregate records for `Order -> Province -> Year` and separately for `Order -> Province -> Year -> IUCN` tables.
3. Use full province labels rather than collapsing low-frequency provinces.
4. Export publication-style Sankey figures in `png`, `pdf`, and `pptx`.

1. 清洗目名、省份名、年份和濒危等级字段。
2. 分别汇总 `Order -> Province -> Year` 与 `Order -> Province -> Year -> IUCN` 数据表。
3. 保留全部省份标签，不折叠低频省份。
4. 导出 `png`、`pdf`、`pptx` 格式的投稿风格桑基图。
