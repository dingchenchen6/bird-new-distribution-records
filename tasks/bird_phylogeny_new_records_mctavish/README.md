# Bird Phylogeny of New Records Using the McTavish Bird Tree

This task rebuilds the phylogeny figure for newly recorded birds in China using:

- the **2025 Chinese Catalogue of Life checklist** as the Chinese bird species pool;
- the **corrected canonical new-record dataset** that already incorporates synonym resolution and duplicate-event removal;
- the **McTavish et al. complete and dynamic tree of birds** as the published phylogenetic backbone.

The workflow is designed to be transparent and publication-ready. It exports:

- a circular phylogeny figure (`png`, `pdf`, `pptx`);
- QA diagnostics for tree matching and order-level proportions;
- the strict Chinese bird species pool table;
- tree-matching audit tables for the full Chinese species pool and corrected new-record species;
- an explicit taxonomy bridge table;
- order-level proportion summaries;
- tip-level metadata for the pruned Chinese bird tree;
- bilingual figure captions and task summary text;
- a bundled Excel workbook for downstream writing and review.

## Main script

- `code/run_bird_phylogeny_new_records_mctavish.R`

## Main outputs

- `figures/fig_phy01_mctavish_bird_new_records_phylogeny.png`
- `figures/fig_phy01_mctavish_bird_new_records_phylogeny.pdf`
- `figures/fig_phy01_mctavish_bird_new_records_phylogeny.pptx`
- `figures/fig_s1_phylogeny_matching_diagnostics.png`
- `results/bird_phylogeny_new_records_mctavish_bundle.xlsx`
- `results/figure_caption_bilingual.md`
- `results/task_summary_bilingual.md`

## Notes

- The Chinese bird species pool is defined strictly at the **species-rank binomial** level from the 2025 Chinese checklist.
- Newly recorded species inherit the previously corrected identity decisions, including synonym handling and duplicate publication removal.
- When the Chinese checklist and the McTavish tree differ in genus placement or taxonomic concept, the workflow applies a **fully explicit bridge table** and exports that bridge table for inspection.
- Unresolved species are **not** silently removed from the audit; they are retained in the matching tables for transparent review.
