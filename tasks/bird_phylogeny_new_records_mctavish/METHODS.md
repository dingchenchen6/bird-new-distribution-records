# Methods

## Data sources

This task combines three inputs:

1. The `2025中国生物物种名录` sheet from the master bird workbook, used to define the Chinese bird species pool.
2. The corrected canonical bird new-record dataset produced in the synonym-and-deduplication task.
3. The McTavish et al. complete and dynamic bird tree (`summary_dated_clements.nex`), stored locally in `data/external/`.

## Species-pool definition

The Chinese checklist includes species and infraspecific records. To obtain a clean species-level denominator for order-level proportions, the workflow:

- retains only rows where `纲拉丁名 = Aves`;
- removes blank names;
- keeps only strict binomials (two-word Latin names);
- removes entries marked with strings such as `subsp.`, `spp.`, `sp.`, `cf.`, `aff.`, or hybrid markers.

This yields the strict Chinese bird species pool used in all denominators.

## Corrected new-record species

Newly recorded species are not rebuilt from the raw event table here. Instead, this task reuses the already corrected canonical dataset in which:

- synonymy has been resolved to a canonical species identity;
- duplicate species–province records have been reduced to the earliest publication;
- the corrected species identities are therefore stable for downstream plotting and summary statistics.

## Tree matching and taxonomy bridge

Both the Chinese checklist and the corrected new-record species are matched to the McTavish tree by converting binomial names from `Genus species` to `Genus_species`.

Two levels of matching are then distinguished:

- `Exact match`: the converted checklist or corrected species name is already present in the tree;
- `Bridged match`: a manual taxonomy bridge is used because the Chinese checklist and the McTavish tree use different generic placements or slightly different taxonomic concepts;
- `Unresolved`: neither direct matching nor the explicit bridge can place the species on the tree.

Every manual bridge is exported in `taxonomy_bridge_table.csv`.

## Figure design

The final phylogeny figure has two components:

1. A circular phylogeny of the Chinese bird species pool pruned from the McTavish tree.
2. A side lollipop panel summarizing the proportion of corrected newly recorded species in each order.

Within the circular phylogeny:

- the Chinese bird pool is shown as a grey background tree;
- newly recorded species are highlighted with coloured terminal branches;
- the first outer ring marks newly recorded species by order;
- the second outer ring marks the IUCN category of newly recorded species;
- internal percentage bubbles highlight the major orders and display the order-level proportion of newly recorded species.

## Diagnostics

The workflow exports:

- filtered species-pool tables;
- full matching tables for the Chinese species pool and corrected new-record species;
- unresolved-name lists;
- a many-to-one mapping audit;
- order-level numerator–denominator summaries;
- a QA figure summarizing matching status and order-level proportions.
