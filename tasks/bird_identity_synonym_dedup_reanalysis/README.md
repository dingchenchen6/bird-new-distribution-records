# Bird Identity, Synonym, and Duplicate Reanalysis

This task package rebuilds the bird new-record analyses after incorporating two user-supplied rule tables:

1. species-identity anomalies and suspected synonym / naming inconsistencies
2. same-species same-province repeated records that should retain only the earliest publication

The original task folders are preserved untouched. This task creates a new corrected base table and reruns the major downstream analyses from that corrected base.

## Structure

- `code/`
  - `00_run_all_corrected_reanalysis.R`: master runner
  - `01_build_corrected_canonical_dataset.R`: canonical species identity correction + duplicate resolution
  - `02_make_corrected_order_summary.R`: corrected order summary table
  - `03_make_corrected_sankey.R`: corrected Sankey figure
- `data/`
  - corrected clean event table
  - corrected trait-pool table
  - audit logs, conflict logs, duplicate-resolution logs, and QA summaries
- `figures/`
  - QA figure showing before-versus-after changes and audit contributions
- `results/`
  - bilingual summary
- `order_summary_corrected/`
  - corrected order summary outputs
- `sankey_corrected/`
  - corrected Sankey outputs
- `directional_corrected/`
  - corrected directional windrose/radar outputs
- `spatiotemporal_corrected/`
  - corrected spatiotemporal maps and bar-line figures
- `geb_fig3_fig4_corrected/`
  - corrected GEB-style Fig. 3 / Fig. 4 analysis package

## Key rules implemented

- Species identity is determined by the scientific name and row-level anomaly audit tables.
- Candidate identity sources are prioritized as:
  - `true_mismatch`
  - `format_mismatch`
  - `duplicate_audit`
  - raw master table
- Same-species same-province repeated records retain only the earliest publication year.
- If the publication year ties, the smallest row id is retained as a deterministic tiebreaker.

## Main corrected outputs

- Corrected clean event table:
  - `data/bird_new_records_clean_corrected.csv`
- Corrected trait-ready species pool:
  - `data/bird_species_pool_with_traits_corrected.csv`
- Corrected order summary:
  - `order_summary_corrected/`
- Corrected Sankey:
  - `sankey_corrected/`
- Corrected directional analyses:
  - `directional_corrected/`
- Corrected spatiotemporal analyses:
  - `spatiotemporal_corrected/`
- Corrected GEB Fig. 3 / Fig. 4:
  - `geb_fig3_fig4_corrected/`
