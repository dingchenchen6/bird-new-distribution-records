# Bird Spatiotemporal Patterns

## Overview | 任务概述
This task rebuilds the spatiotemporal figure set for bird new-distribution records using the user-provided shapefile base map and manuscript-style stacked bar-line charts.
本任务基于用户提供的 shp 底图，重建鸟类新纪录的时空格局图，包括地图和论文风格堆叠柱线图。

## Folder Structure | 文件夹结构
- `figures`: choropleth maps, point map, and stacked bar-line figures
- `code`: bilingual R workflow script with step-by-step comments
- `data`: plot-ready tables and the copied shapefile base map
- `results`: concise result summary

## Main Script | 主脚本
- `code/make_bird_spatiotemporal_patterns.R`

## Key Outputs | 主要输出
- `figures/fig_sp01_province_new_record_count_map.png`
- `figures/fig_sp02_province_new_record_density_map.png`
- `figures/fig_sp03_across_order_point_map.png`
- `figures/fig_sp04_province_stacked_barline.png`
- `figures/fig_sp05_year_stacked_barline.png`
