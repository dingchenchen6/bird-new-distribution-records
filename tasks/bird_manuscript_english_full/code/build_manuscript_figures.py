#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
====================================================================
Scientific question / 科学问题
--------------------------------------------------------------------
EN:
How can we consolidate the major completed bird new-record analyses into a
small set of manuscript-ready composite figures that preserve the strongest
scientific messages without repeating equivalent analytical variants?

CN:
如何将已经完成的鸟类新纪录主要分析整合成少量适合论文正文的组合图，既保留最
核心的科学信息，又避免重复展示本质上相同的分析变体？

Objective / 目标
EN:
1. Select the major visual outputs that correspond to the manuscript's core
   results.
2. Combine related figures into publication-style multi-panel layouts.
3. Copy or render manuscript-specific figure files into a dedicated task
   folder so the manuscript is self-contained.
4. Export a figure plan that records the source files and intended narrative
   role of each manuscript figure.

CN:
1. 选择对应论文核心结果的主要图件。
2. 将逻辑相关的图件组合成适合投稿的多面板版式。
3. 将正文所需图件复制或渲染到独立任务文件夹，使论文包自洽。
4. 导出图件计划表，记录每张正文图的来源文件与叙事功能。

Analytical idea / 思路
EN:
We prioritize one representative figure for each major analysis domain:
spatiotemporal pattern, taxonomic flow, directionality, and ecological
correlates. Equivalent variants (for example, graticule/non-graticule maps
or radar/windrose depictions of the same overall signal) are not repeated in
main text. Instead, one preferred version is selected and related outputs are
combined only when they strengthen interpretability.

CN:
我们为每个主要分析模块保留一个代表性图件：时空格局、分类流动、方向格局和
生态相关性。对于表达相同核心结果的变体（例如带经纬网/不带经纬网地图，或同一
总体方向结构的不同图型），不在正文重复展示；仅在能显著提升解释性的情况下进
行组合。

Diagnostics and validation / 诊断与验证
EN:
The script checks that every source image exists before composition, records
all used sources in a CSV figure plan, and exports high-resolution PNGs for
Word insertion. Output dimensions and panel labels are standardized for a
clean journal-style appearance.

CN:
脚本在组合前会检查每个源图是否存在，将所有使用的来源记录到 CSV 图件计划表，
并导出高分辨率 PNG 以便插入 Word。输出尺寸和 panel label 统一标准化，确保版
式整洁、接近期刊风格。
====================================================================
"""

from pathlib import Path
from PIL import Image, ImageOps, ImageDraw, ImageFont
import pandas as pd
import shutil

ROOT = Path("/Users/dingchenchen/Documents/New records/bird_new_records_R_output/tasks")
OUT = ROOT / "bird_manuscript_english_full"
FIG_DIR = OUT / "figures"
DATA_DIR = OUT / "data"

FIG_DIR.mkdir(parents=True, exist_ok=True)
DATA_DIR.mkdir(parents=True, exist_ok=True)

FONT_REG = "/System/Library/Fonts/Supplemental/Georgia.ttf"
FONT_BOLD = "/System/Library/Fonts/Supplemental/Georgia Bold.ttf"
LABEL_FONT = ImageFont.truetype(FONT_BOLD, 90)
SUBTITLE_FONT = ImageFont.truetype(FONT_REG, 48)

WHITE = "white"
BLACK = "#111111"
GRAY = "#666666"

# Step 1. Register major figures / 注册正文主图来源
figure_plan = [
    {
        "manuscript_figure": "Figure 1",
        "file_name": "figure1_spatiotemporal_overview.png",
        "type": "Composite",
        "role": "Main spatiotemporal synthesis",
        "sources": [
            ROOT / "bird_spatiotemporal_patterns/figures/fig_sp01_province_new_record_count_map.png",
            ROOT / "bird_spatiotemporal_patterns/figures/fig_sp03_across_order_point_map.png",
            ROOT / "bird_spatiotemporal_patterns/figures/fig_sp06_province_stacked_barline_top10.png",
            ROOT / "bird_spatiotemporal_patterns/figures/fig_sp07_year_stacked_barline_top10.png",
        ],
    },
    {
        "manuscript_figure": "Figure 2",
        "file_name": "figure2_taxonomic_flow_sankey.png",
        "type": "Single",
        "role": "Taxonomic-spatial-temporal flow",
        "sources": [
            ROOT / "bird_sankey_order_province_year/figures/fig_ref01_sankey_order_province_year_en_compact_v3.png",
        ],
    },
    {
        "manuscript_figure": "Figure 3",
        "file_name": "figure3_directional_patterns.png",
        "type": "Composite",
        "role": "Directional pattern synthesis",
        "sources": [
            ROOT / "bird_directional_windrose_radar/figures/overall/overall_direction_windrose.png",
            ROOT / "bird_binomial_order_pattern/figures/fig_ref04_order_binomial_pattern_a.png",
            ROOT / "bird_directional_windrose_radar/figures/combined/order_direction_windrose_facets.png",
        ],
    },
    {
        "manuscript_figure": "Figure 4",
        "file_name": "figure4_correlates_and_mechanisms.png",
        "type": "Composite",
        "role": "Trait correlates, representativeness, and discovery mechanisms",
        "sources": [
            ROOT / "bird_full_pipeline_main_text/figures/fig14_species_trait_coefficient_models.png",
            ROOT / "bird_full_pipeline_main_text/figures/fig12_order_representativeness_scatter.png",
            ROOT / "bird_full_pipeline_main_text/figures/fig13_discovery_reason_and_method.png",
        ],
    },
    {
        "manuscript_figure": "Supplementary Figure S1",
        "file_name": "figureS1_order_direction_facets.png",
        "type": "Single",
        "role": "Order-specific directional structure",
        "sources": [
            ROOT / "bird_directional_windrose_radar/figures/combined/order_direction_windrose_facets.png",
        ],
    },
]

# Step 2. Helper functions / 辅助函数
for item in figure_plan:
    for src in item["sources"]:
        if not src.exists():
            raise FileNotFoundError(f"Missing source image: {src}")


def load_image(path: Path) -> Image.Image:
    return Image.open(path).convert("RGB")


def fit_to_box(img: Image.Image, box_w: int, box_h: int) -> Image.Image:
    """Resize while preserving aspect ratio and center on white canvas."""
    contained = ImageOps.contain(img, (box_w, box_h), Image.Resampling.LANCZOS)
    canvas = Image.new("RGB", (box_w, box_h), WHITE)
    x = (box_w - contained.width) // 2
    y = (box_h - contained.height) // 2
    canvas.paste(contained, (x, y))
    return canvas


def draw_panel_label(draw: ImageDraw.ImageDraw, x: int, y: int, label: str, subtitle: str = "") -> None:
    draw.text((x, y), label, font=LABEL_FONT, fill=BLACK)
    if subtitle:
        draw.text((x + 120, y + 22), subtitle, font=SUBTITLE_FONT, fill=GRAY)


# Step 3. Build Figure 1 / 构建图1：时空格局综述
map_count = fit_to_box(load_image(figure_plan[0]["sources"][0]), 2600, 1800)
point_map = fit_to_box(load_image(figure_plan[0]["sources"][1]), 2600, 1800)
prov_bar = fit_to_box(load_image(figure_plan[0]["sources"][2]), 2600, 1600)
year_bar = fit_to_box(load_image(figure_plan[0]["sources"][3]), 2600, 1600)

fig1 = Image.new("RGB", (5500, 3800), WHITE)
d1 = ImageDraw.Draw(fig1)
fig1.paste(map_count, (150, 130))
fig1.paste(point_map, (2750, 130))
fig1.paste(prov_bar, (150, 2050))
fig1.paste(year_bar, (2750, 2050))
draw_panel_label(d1, 90, 70, "A")
draw_panel_label(d1, 2690, 70, "B")
draw_panel_label(d1, 90, 1990, "C")
draw_panel_label(d1, 2690, 1990, "D")
fig1.save(FIG_DIR / "figure1_spatiotemporal_overview.png", dpi=(300, 300))

# Step 4. Copy Figure 2 / 复制图2：桑基图主图
shutil.copy2(figure_plan[1]["sources"][0], FIG_DIR / "figure2_taxonomic_flow_sankey.png")

# Step 5. Build Figure 3 / 构建图3：方向格局
windrose = fit_to_box(load_image(figure_plan[2]["sources"][0]), 2500, 1700)
binomial = fit_to_box(load_image(figure_plan[2]["sources"][1]), 2500, 1700)
facets = fit_to_box(load_image(figure_plan[2]["sources"][2]), 5200, 4200)

fig3 = Image.new("RGB", (5500, 6350), WHITE)
d3 = ImageDraw.Draw(fig3)
fig3.paste(windrose, (150, 130))
fig3.paste(binomial, (2850, 130))
fig3.paste(facets, (150, 1950))
draw_panel_label(d3, 90, 70, "A")
draw_panel_label(d3, 2790, 70, "B")
draw_panel_label(d3, 90, 1890, "C")
fig3.save(FIG_DIR / "figure3_directional_patterns.png", dpi=(300, 300))

# Step 6. Build Figure 4 / 构建图4：相关性与机制
coeff = fit_to_box(load_image(figure_plan[3]["sources"][0]), 5200, 2100)
repr_scatter = fit_to_box(load_image(figure_plan[3]["sources"][1]), 2500, 1650)
reason_method = fit_to_box(load_image(figure_plan[3]["sources"][2]), 2500, 1650)

fig4 = Image.new("RGB", (5500, 4250), WHITE)
d4 = ImageDraw.Draw(fig4)
fig4.paste(coeff, (150, 130))
fig4.paste(repr_scatter, (150, 2450))
fig4.paste(reason_method, (2850, 2450))
draw_panel_label(d4, 90, 70, "A")
draw_panel_label(d4, 90, 2390, "B")
draw_panel_label(d4, 2790, 2390, "C")
fig4.save(FIG_DIR / "figure4_correlates_and_mechanisms.png", dpi=(300, 300))

# Step 7. Copy supplementary figure / 复制补充图
shutil.copy2(figure_plan[4]["sources"][0], FIG_DIR / "figureS1_order_direction_facets.png")

# Step 8. Export figure plan / 导出图件计划
rows = []
for item in figure_plan:
    for src in item["sources"]:
        rows.append({
            "manuscript_figure": item["manuscript_figure"],
            "output_file": str(FIG_DIR / item["file_name"]),
            "type": item["type"],
            "role": item["role"],
            "source_file": str(src),
        })

pd.DataFrame(rows).to_csv(DATA_DIR / "manuscript_figure_plan.csv", index=False)
print("Manuscript figures built successfully.")
