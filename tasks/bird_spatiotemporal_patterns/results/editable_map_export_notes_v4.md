# Editable Map Export Notes v4

## Summary
Two new PowerPoint exports were created for the province-level new-record count map and the record-point distribution map:

- `fig_sp01_province_new_record_count_map_editable_v4.pptx`
- `fig_sp03_across_order_point_map_editable_v4.pptx`

## Technical route
The direct `officer + rvg::dml()` route was tested first, but both the original `geom_sf` maps and the simplified polygon/path rebuild triggered a low-level graphics-engine segmentation fault on this machine.

To keep the export reproducible and more editable than raster-based PowerPoint files, the maps were rebuilt as simplified polygon/path/point/text layers, exported to standalone SVG files, and then assembled into PowerPoint using `PptxGenJS`.

## Practical interpretation
These new PPTX files are SVG-backed vector slides. In modern PowerPoint, SVG objects usually preserve vector quality and can often be converted to shapes or ungrouped for further editing, making them substantially more editable than the previous image-like exports.

## Two-slide deck
A combined deck was also assembled:

- `fig_sp01_sp03_editable_v4_2slides.pptx`

Slide 1 contains the province-level new-record count map, and Slide 2 contains the record-point distribution map.

## Server note
A server rerun was requested, but no active remote execution bridge or server helper script was available in this session for this specific map-export task. The current deliverable is therefore a fully reproducible local workflow with mirrored outputs and GitHub synchronization.
