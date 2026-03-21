# Methods note for manuscript task
# 论文任务方法说明

## Goal / 目标
Produce a complete English manuscript draft by synthesizing the existing bird new-record analytical outputs without duplicating equivalent figure variants.
在不重复展示等价图件变体的前提下，整合已有鸟类新纪录分析输出，生成一篇完整英文论文草稿。

## Workflow / 工作流程
1. Select one preferred manuscript figure for each major analytical domain.
2. Combine related figures into publication-style multi-panel layouts.
3. Draft the manuscript in English from title through conclusion.
4. Create main manuscript tables directly from exported CSV files.
5. Build a DOCX file with embedded figures and tables, plus a markdown source file.
6. Run a text-level and media-level diagnostic check on the DOCX.

## Validation / 核查
- All required source figures were checked before manuscript assembly.
- The generated DOCX was inspected at the text structure level using `python-docx`.
- Embedded figure media were counted directly from the DOCX archive.
- A build diagnostics CSV was exported for transparency.
