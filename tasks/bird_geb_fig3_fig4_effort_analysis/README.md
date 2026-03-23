# Bird new-distribution-records: GEB-style Figure 3 and Figure 4 task

This task rebuilds the bird analogue of the GEB species-level and province-level analyses with the newly supplied survey-effort table and an upgraded trait framework.

## Current primary workflow

The recommended script for the current round is:

- `code/run_bird_geb_fig3_fig4_effort_analysis_v2_phylo.R`

A legacy non-phylogenetic script is retained for reference:

- `code/run_bird_geb_fig3_fig4_effort_analysis.R`

## What the upgraded workflow does

### Figure 3: species-level trait correlates

The upgraded Figure 3 now:

- uses bird trait sources centred on `AVONET` and Wang Yanping et al.'s bird ecological dataset;
- explicitly includes `HWI` (hand-wing index);
- adds `clutch size`, `number of congeners`, `endemicity`, `diet type`, and `forest-associated primary habitat`;
- separates `continuous traits` and `categorical ecological traits` into two phylogenetic Bernoulli models;
- controls phylogenetic non-independence using the Hackett bird phylogeny;
- visualizes posterior effects with a journal-style ridge-plot layout.

### Figure 4: province-level effort drivers

The upgraded Figure 4:

- retains the GEB-style province-level effort framework;
- quantifies historical effort, current effort, GDP, province area, and habitat heterogeneity;
- keeps sensitivity checks for alternative effort proxies and influential provinces;
- uses a wider horizontal panel layout instead of a tall vertical composition.

## Data-source rules used in this round

Primary species-trait sources:

- `AVONET traits`
- `中国鸟类生态学特征`
- `2025中国生物物种名录` for taxonomic context and congener counts

Important interpretation rule:

- variables such as `habitat breadth`, `generation length`, and `naming/description year` were requested conceptually, but they are **not claimed as primary-model variables** unless they can be stably supported from the user-specified primary sources in the current workbook.
- `Forest dependence` is operationalized here as a proxy based on AVONET primary habitat (`Forest` or `Woodland` versus non-forest habitats), and should therefore be interpreted as `forest-associated primary habitat`, not as a direct mechanistic dependence index.

## Directory structure

- `code/`
- `data/`
- `figures/`
- `results/`

## Key outputs

### Main figures

- `figures/fig_geb3_species_level_correlates.*`
- `figures/fig_geb4_province_level_effort_drivers.*`

### Supplementary figures

- `figures/fig_s0_data_screening_overview.*`
- `figures/fig_s1_species_model_diagnostics.*`
- `figures/fig_s2_province_model_diagnostics_and_sensitivity.*`
- `figures/fig_s3_province_model_robustness.*`

### Key data tables

- `data/species_candidate_variable_audit.csv`
- `data/species_candidate_variable_availability.csv`
- `data/bird_species_continuous_model_dataset_tree_matched.csv`
- `data/bird_species_categorical_model_dataset_tree_matched.csv`
- `data/species_phylo_model_fit_summary.csv`
- `data/species_phylo_continuous_effects.csv`
- `data/species_phylo_categorical_effects.csv`
- `data/province_candidate_model_comparison.csv`
- `data/province_level_primary_coefficients.csv`
- `data/province_level_robustness_coefficients.csv`

### Result narratives and bundles

- `results/task_summary_bilingual.md`
- `results/figure_captions_bilingual.md`
- `results/interpretation_bilingual.md`
- `results/bird_geb_fig3_fig4_analysis_bundle.xlsx`

## Main modelling notes

- Species-level primary response: whether a species generated at least one new provincial record (`0/1`).
- Species-level supplementary sensitivity response: number of new records per species.
- Province-level primary response: `log(1 + records per 100,000 km2)`.
- Current report effort and current user effort are not forced into the same primary model because they are strongly collinear; they are compared in alternative specifications.
- The `Richness` field inherited from the mammal GEB repository is audited but not used as a bird richness predictor.
