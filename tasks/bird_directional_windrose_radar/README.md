# Bird Directional Windrose and Radar

## Overview | 任务概述
This task organizes the directional analysis of bird new-distribution records and outputs publication-style windrose and radar charts at both the multi-order overlay level and the per-order panel level.
本任务整理鸟类新纪录的方向性分析，并输出投稿风格的总体叠加图和按目分面图，包括风玫瑰图与雷达图。

## Folder Structure | 文件夹结构
- `figures/overall`: overlay windrose and radar plots for the major orders
- `figures/combined`: 4x4 faceted windrose and radar plots for selected major orders
- `figures/windrose_by_order`: individual windrose figures for each retained order
- `figures/radar_by_order`: individual radar figures for each retained order
- `code`: fully annotated R workflow with bilingual comments
- `data`: cleaned direction tables, color palette, selected orders, and plot-ready summaries
- `results`: concise text summary of the main findings

## Main Script | 主脚本
- `code/make_bird_directional_task_bundle.R`

## Key Outputs | 主要输出
- `figures/overall/overall_direction_windrose.png`
- `figures/overall/overall_direction_radar.png`
- `figures/combined/order_direction_windrose_facets.png`
- `figures/combined/order_direction_radar_facets.png`
- `data/bird_direction_species_level.csv`
- `data/bird_direction_order_counts.csv`
- `results/directional_results_summary.md`

## Reproducibility | 复现方式
Run the main script in R. The script exports `png`, `pdf`, and `pptx` versions automatically.
在R中运行主脚本即可复现，脚本会自动导出 `png`、`pdf` 和 `pptx` 三种格式。
