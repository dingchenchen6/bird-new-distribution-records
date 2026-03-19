# Methods | 方法说明

## Projection | 投影方式
All spatial layers are transformed into a CGCS2000-based Albers Equal Area projection suitable for China-wide thematic mapping.
所有空间图层统一转换到基于 CGCS2000 的 Albers Equal Area 投影，以适配中国尺度专题制图。

## Workflow | 分析流程
1. Read the cleaned bird record table and the province-level shapefile base map.
2. Standardize province names and derive province/year/order summary tables.
3. Build projected choropleth maps, a projected point-distribution map, and two levels of stacked bar-line charts.
4. Export PNG, PDF, and editable vector PPTX outputs.

1. 读取清洗后的鸟类记录表和省级 shp 底图。
2. 标准化省份名称，并生成年份、省份和目的统计表。
3. 构建投影地图、投影点位图，以及完整和 top10 两套堆叠柱线图。
4. 导出 PNG、PDF 和可编辑矢量 PPTX。
