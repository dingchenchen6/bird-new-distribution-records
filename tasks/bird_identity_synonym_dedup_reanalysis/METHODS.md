# Methods Notes for Identity/Synonym and Duplicate Reanalysis

## Why this task exists

The original bird analyses were based on the previously cleaned master table. The two new audit workbooks add explicit evidence on:

- row-level species-identity anomalies
- same-species same-province repeated provincial records

These issues affect the denominator of almost every downstream descriptive and inferential analysis. Therefore the corrected workflow first rebuilds a canonical event table and only then reruns the main analysis families.

## Canonical correction workflow

### 1. Raw event reconstruction

The master workbook `鸟类新纪录20260311.xlsx` is read from the sheet `2000-2025鸟类新记录`. A stable `record_id` is assigned using the row order in the workbook.

### 2. Scientific-name normalization

Scientific names are normalized by:

- replacing non-breaking spaces and underscores
- trimming and squishing whitespace
- extracting a species-level binomial
- standardizing genus capitalization and species lowercase

This step makes row-level name anomalies comparable across tables and improves joining to trait tables and the national checklist.

### 3. Audit-source priority

Row-level candidate canonical names are assembled from:

1. `疑似真正错配`
2. `大小写等格式不统一`
3. `重复记录`

Priority is assigned in the same order, so that explicit mismatch corrections override pure formatting corrections, which in turn override the duplicate audit workbook.

### 4. Conflict logging

If a single `record_id` receives more than one candidate canonical name, the record is written to `identity_correction_conflicts.csv`. The highest-priority source is retained, but the conflict remains auditable.

### 5. Province-level duplicate resolution

After row-level identity correction, repeated records are grouped by:

- canonical species
- standardized province

Within each group, the earliest publication year is retained. If multiple rows share the same earliest year, the smallest `record_id` is retained. All retained and dropped rows are written to `duplicate_resolution_log.csv`.

## Diagnostics

The reanalysis explicitly exports:

- audit coverage summary
- identity correction map
- conflict log
- duplicate group table
- duplicate resolution log
- key-field missingness summary
- coordinate screening table
- trait-pool matching audit
- before-versus-after summary

## Downstream reruns

The corrected base table is then used to rerun:

- order summary
- Sankey diagram
- directional windrose and radar plots
- spatiotemporal maps and bar-line figures
- GEB-style Fig. 3 / Fig. 4 analyses

The original task outputs are preserved; corrected results are written into a new task package.
