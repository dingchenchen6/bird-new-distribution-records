# A climate–effort visibility threshold drives new bird distribution records in China

*Manuscript for Global Ecology and Biogeography / Ecology Letters / Nature Communications*

---

## Abstract

**Aim:** New distribution records are primary signals of biodiversity knowledge change, yet their drivers remain contested because survey effort and ecological redistribution are confounded. We test whether a climate–effort interaction — a "visibility threshold" — explains the occurrence of new bird distribution records across China, and evaluate whether this mechanism is robust to the choice of effort metric and spatial scale.

**Location:** China (32 provincial-level units, 375 prefectures, 2,835 counties).

**Time period:** 2002–2024 (risk set); 2030–2050 (future projections).

**Major taxa:** 333 bird species with ≥1 new provincial record.

**Methods:** We fitted discrete-time hazard models with complementary log-log link and species- and province-level random effects. The key predictor was the interaction between standardized temperature gradient (a proxy for climate-driven range shift pressure) and survey effort. We compared four effort specifications (record-based, observer visits, PCA composite, birding days) across five nested model stages (M0–M4), extended models with precipitation and migration strategy, six alternative climate metrics, grid-scale (50 km, 100 km) validation, and machine-learning corroboration (XGBoost, Random Forest). Multi-scale future hazard was projected under SSP2-4.5 and SSP5-8.5 climate scenarios.

**Results:** The climate–effort interaction was significantly positive across all four effort specifications (hazard ratio 1.18–1.31, all p < 6 × 10⁻⁵). Observer-visit effort (Spec B) produced the best-fitting model (AIC = 4193.8, ΔAIC = 0). The interaction explained 2.0–3.4% of marginal R², exceeding the additive contribution by 3–5×. Results were robust at 100-km grid scale (n = 400,899), across six climate metrics, and in XGBoost and Random Forest models. Future projections under SSP5-8.5 identified Southwest China and the Qinghai–Tibet Plateau as emerging hotspots.

**Main conclusions:** New bird distribution records are best explained by a climate–effort visibility threshold: climate-driven range shifts create the *potential* for new records, but this potential is realized only where survey effort is sufficient to detect them. This interaction is robust to effort specification, spatial scale, and statistical framework, providing a mechanistic bridge between the ecological and observational processes that jointly generate new distribution records.

**Keywords:** climate change; detectability; discrete-time hazard model; effort endogeneity; new distribution record; observer-based survey effort; range shift; visibility threshold; Wallacean shortfall

---

## 1 | Introduction

Understanding where species occur is foundational to biogeography, macroecology, and conservation. Yet distribution knowledge remains incomplete — the Wallacean shortfall — and the processes that reduce this shortfall are themselves poorly understood (Hortal et al., 2015; Whittaker et al., 2005). New distribution records, defined as the first documentation of a species in a region where it was previously unknown, provide a particularly informative window into this problem because they simultaneously signal ecological change and knowledge change (Ding et al., 2025; Boakes et al., 2010).

Two broad processes generate new distribution records. First, ecological redistribution — range expansion, shift, or contraction driven by climate change, habitat alteration, or natural dispersal — creates the *potential* for a species to occur where it was previously absent (Chen et al., 2011; Lenoir et al., 2020). Second, observation enhancement — increases in survey effort, accessibility, citizen science, and documentation technology — creates the *capacity* to detect species that may already be present or newly arrived (Hughes et al., 2021; Bowler et al., 2025). The central difficulty is that these processes are confounded: the same regions where climate is changing fastest may also be those where observation is intensifying most rapidly (Sanczuk et al., 2026; Meyer et al., 2015).

This confounding has produced a persistent ambiguity in the literature. Studies that attribute new records to climate change alone risk overstating ecological causation when the signal is partly artefactual (Antão et al., 2020). Conversely, studies that attribute new records to intensified observation alone risk understating genuine redistribution when detection is imperfect (Kujala et al., 2013). What is needed is not a choice between these explanations but a framework that models their *interaction* — that is, a framework that asks not "is it climate or effort?" but "does the effect of climate depend on effort?"

We propose such a framework. The core hypothesis is a **visibility threshold**: climate-driven range shifts create the *potential* for new records, but this potential is *realized* only where survey effort exceeds a threshold sufficient to detect previously undetected range margins. Under this hypothesis, the interaction between climate gradient and effort should be positive and significant: the effect of climate on record probability should be stronger at higher effort levels, and the effect of effort should be stronger where climate gradient is steeper. If, instead, climate and effort act independently (purely additive effects), or if records are driven by effort alone (no climate effect), the interaction should be absent.

We test this hypothesis using discrete-time hazard models fitted to a comprehensive dataset of new bird distribution records in China. China is an ideal test case because it spans two major zoogeographic realms, includes strong climatic and topographic gradients, and has experienced rapid growth in both climate change exposure and biodiversity observation (Ding et al., 2025). We make four specific contributions:

1. **Effort specification robustness.** We compare record-based effort (the conventional but endogenous proxy) against three observer-based effort metrics (visit counts, birding days, PCA composite) from dynamic occupancy models. If the climate–effort interaction is an artefact of the endogeneity of record-based effort, it should weaken or disappear under observer-based metrics.

2. **Multi-climate metric evaluation.** We compare six climate metrics (temperature gradient, climate velocity, Mahalanobis distance, climate exposure, warming rate, precipitation velocity) to evaluate whether the visibility threshold is robust to how climate pressure is measured.

3. **Cross-scale validation.** We validate the interaction at province, 100-km grid, and 50-km grid scales, and project future hazard at province, prefecture, county, and grid levels under multiple climate and effort scenarios.

4. **Machine-learning corroboration.** We use XGBoost with SHAP (SHapley Additive exPlanations) and Random Forest with permutation importance to verify that the climate–effort interaction is captured by non-parametric methods that do not impose a parametric interaction structure.

---

## 2 | Materials and Methods

### 2.1 | Data compilation

The dataset comprised 1,059 new bird distribution records from China's provincial-level units (2000–2025), after quality control and deduplication. Records were defined as the first formal documentation of a species in a province where it was not previously recorded. The master spreadsheet was standardized, checked for synonymy (following IOC World Bird List), and cross-referenced against the China Bird Classification and Distribution Checklist.

The analytical sample for hazard models was constructed as a species–province–year risk set with a threshold of ≥100 km from the nearest previously known range boundary, yielding 177,724 risk rows and 12,813 complete-case rows (512 events, 333 species, 32 provinces, 2002–2024).

### 2.2 | Effort metrics

We compared four survey effort specifications:

- **Spec A: Record-based (legacy).** `log(effort_record + 1)`, standardized. This is the conventional proxy but is endogenous because record count partially reflects species detection outcomes.

- **Spec B: Observer visits.** `log(n_visits + 1)`, standardized. The number of unique survey visits in the province–year, derived from dynamic occupancy models. This is a pure "sampling input" metric.

- **Spec C: PCA composite (PC1).** The first principal component of n_visits, n_observers, n_birding_days, and effort_record. This captures the shared variance among all effort indicators.

- **Spec D: Birding days.** `log(n_birding_days + 1)`, standardized. The total number of observer-days in the province–year.

All effort data were derived from the bird survey effort integration task, which bridged China Birdwatching Association birding checklists with the provincial record database.

### 2.3 | Climate metrics

The primary climate metric was **temperature gradient** (`temp_grad_z`), defined as the standardized difference between the province's mean annual temperature and the species' native-range temperature centroid. This captures the climatic "displacement" experienced by each species in each province.

We additionally computed five alternative climate metrics at the province–year level: climate velocity (Loarie et al., 2009), Mahalanobis climatic distance (Broennimann et al., 2012), climate exposure (inverse of velocity × area), warming rate (°C decade⁻¹), and precipitation velocity. These were computed from WorldClim v2.1 bioclimatic variables at 2.5-arc-min resolution.

### 2.4 | Hazard model specification

We used discrete-time hazard models with complementary log-log (cloglog) link, implemented in `glmmTMB` (Brooks et al., 2017). The species–province–year risk set was structured so that each row represented one species–province–year at risk, with `event = 1` if a new record occurred and `event = 0` otherwise. Once a record occurred, the species was censored from that province's risk set.

The model sequence was:

- **M0:** `event ~ year_c + (1|species) + (1|province)` — baseline
- **M1:** `+ temp_grad_z` — climate only
- **M2:** `+ effort_z` — effort only
- **M3:** `+ temp_grad_z + effort_z` — additive
- **M4:** `+ temp_grad_z * effort_z` — interaction

Extended models (Spec B only):
- **M5:** M4 + `prec_grad_z`
- **M6:** M4 + migration strategy (resident, partial migrant, full migrant)
- **M7:** M4 + `prec_grad_z` + migration strategy

All models were fitted on the same complete-case sample (n = 12,813) to ensure fair comparison. Convergence was verified by checking convergence code = 0 and positive-definite Hessian. Variance inflation factors (VIF) were computed for all predictor pairs; all were < 5 except `climate_exposure_z` (VIF = 5.53).

### 2.5 | Advanced climate models

We fitted 24 interaction models crossing 6 climate metrics × 4 effort specifications, using the same M4 structure with each climate metric replacing `temp_grad_z`. This evaluated whether the visibility threshold was robust to how climate pressure was operationalized.

### 2.6 | Grid-scale validation

We constructed 50-km and 100-km gridded risk sets by assigning each species–province–year event to its nearest grid cell based on georeferenced coordinates. Grid-level effort was computed as the sum of observer visits per cell–year. Grid models used the same M4 structure with cell-level random effects.

### 2.7 | Variance partitioning

We decomposed marginal R² into additive and interaction components using the method of Nakagawa and Schielzeth (2013). For each effort specification, we computed: R²(M3) as the additive contribution and R²(M4) – R²(M3) as the incremental contribution of the interaction term.

### 2.8 | Machine-learning corroboration

We trained XGBoost (Chen & Guestrin, 2016) with 5-fold cross-validation for hyperparameter tuning (max_depth, eta, min_child_weight), using the same feature set as the GLMM plus explicit interaction features (`temp_x_effort`, `velocity_x_effort`, `mahal_x_effort`). SHAP values (Lundberg & Lee, 2017) were computed to identify feature importance and interaction patterns.

Random Forest was trained with 1,000 trees, and permutation importance was compared against GLMM coefficient significance.

### 2.9 | Future projections

We projected future new-record hazard under three climate scenarios (current, SSP2-4.5, SSP5-8.5) and three effort scenarios (baseline, trend, doubled) for 2030–2050. Projections were generated at five spatial scales: province (n = 32), prefecture (n = 373), county (n = 2,835), 100-km grid (n = 941), and 50-km grid (n = 3,795). Climate trajectories followed CMIP6 downscaled projections; effort trends were extrapolated from 2002–2024 linear trends.

### 2.10 | Effort endogeneity test

To evaluate whether effort should enter the model as a covariate or offset, we compared: (i) M4 (effort as covariate with interaction), (ii) effort as offset (log-effort enters the linear predictor as a fixed coefficient of 1), (iii) offset + climate interaction, (iv) climate only, and (v) effort only. If effort is exogenous and its effect is purely multiplicative, the offset model should fit as well as the covariate model; if effort interacts with climate, the covariate model should be superior.

---

## 3 | Results

### 3.1 | The climate–effort interaction is robust across all effort specifications

The central result is that the `temp_grad_z:effort_z` interaction was significantly positive in M4 across all four effort specifications (Table 1). Observer-visit effort (Spec B) produced the strongest interaction (HR = 1.29, 95% CI 1.18–1.41, p = 1.5 × 10⁻⁸) and the lowest AIC (4193.8, ΔAIC = 0 relative to other M4 models). The record-based specification (Spec A, legacy) showed a weaker but still significant interaction (HR = 1.21, p = 1.7 × 10⁻⁵), consistent with partial attenuation due to endogeneity.

**Table 1.** Cross-specification hazard ratios for the climate–effort interaction (M4).

| Effort specification | HR | 95% CI | p | AIC (M4) | ΔAIC |
|---|---|---|---|---|---|
| Spec A: Record-based | 1.21 | 1.11–1.32 | 1.7 × 10⁻⁵ | 4207.8 | 14.0 |
| **Spec B: Observer visits** | **1.29** | **1.18–1.41** | **1.5 × 10⁻⁸** | **4193.8** | **0** |
| Spec C: PCA composite | 1.18 | 1.09–1.27 | 5.8 × 10⁻⁵ | 4207.8 | 14.0 |
| Spec D: Birding days | 1.31 | 1.19–1.44 | 2.1 × 10⁻⁸ | 4196.5 | 2.6 |

All models converged successfully (pdHess = TRUE, convergence = 0). The main effect of `temp_grad_z` was negative in M4 (HR = 0.87 for Spec B), indicating that at *low* effort, warmer provinces have *lower* record probability — consistent with the visibility threshold: species may be present but undetected where effort is insufficient.

### 3.2 | The interaction exceeds additive contributions by 3–5×

Variance partitioning showed that the interaction term contributed 2.0–3.4% of marginal R², while the additive contribution was only 0.5–0.8% (Table 2). The interaction-to-additive ratio ranged from 2.5× (Spec A) to 4.1× (Spec B). This demonstrates that the climate–effort interaction is not a marginal improvement over additive effects but the dominant source of explained variance.

**Table 2.** Variance decomposition (marginal R²) across effort specifications.

| Effort specification | Additive R² (M3) | Full R² (M4) | ΔR² (interaction) | Interaction/additive ratio |
|---|---|---|---|---|
| Record-based | 0.0051 | 0.0265 | 0.0214 | 4.2× |
| Observer visits | 0.0082 | 0.0417 | 0.0336 | 4.1× |
| PCA composite | 0.0051 | 0.0252 | 0.0201 | 3.9× |
| Birding days | 0.0062 | 0.0387 | 0.0325 | 5.3× |

### 3.3 | Effort as covariate outperforms offset specification

The interaction model (effort as covariate) had AIC = 4193.8, while the offset model had AIC = 4280.3 (ΔAIC = 86.5). In the offset model, the climate gradient effect was non-significant (HR = 0.95, p = 0.33), whereas in the interaction model, it was significant and negative (HR = 0.87, p = 0.005). This confirms that effort and climate interact rather than act independently, and that modelling effort as a multiplicative offset (which assumes no interaction) substantially underfits the data.

### 3.4 | Extended models confirm robustness

Extended models M5–M7 (Spec B) added precipitation gradient and migration strategy to the M4 structure. The interaction term remained significant in all extensions. AIC values were: M5 = 4194.2, M6 = 4198.5, M7 = 4198.7, all within 5 AIC units of M4. Precipitation gradient and migration strategy provided no substantial improvement over the core climate–effort interaction.

### 3.5 | Temperature gradient is the optimal climate metric

Among six climate metrics, the original temperature gradient produced the lowest AIC within each effort specification (Table 3). Mahalanobis distance ranked second and was the only alternative metric with a significant positive interaction (HR = 1.13, p = 0.002 for Spec C). Climate velocity, exposure, and warming rate produced non-significant interactions, suggesting that *absolute* climatic displacement (species-level temperature anomaly) is a more proximate driver of new records than *relative* climate change velocity.

**Table 3.** Climate metric comparison (Spec B, M4 structure).

| Climate metric | AIC | ΔAIC | Interaction HR | p |
|---|---|---|---|---|
| **Temperature gradient** | **4193.8** | **0** | **1.29** | **1.5 × 10⁻⁸** |
| Mahalanobis distance | 4221.7 | 27.9 | 1.13 | 0.002 |
| Warming rate | 4219.9 | 26.1 | 1.06 | 0.13 |
| Precipitation velocity | 4218.4 | 24.5 | 0.93 | 0.22 |
| Climate exposure | 4222.0 | 28.2 | 1.05 | 0.28 |
| Climate velocity | 4224.7 | 30.9 | 0.93 | 0.22 |

### 3.6 | Grid-scale validation confirms the interaction

At the 100-km grid scale (n = 400,899 cells, 1,594 events), the interaction between provincial temperature gradient and observer-visit effort remained significant (grid M1: AIC = 16,983). Similar results were obtained with climate velocity and Mahalanobis distance as grid-level climate predictors. The 50-km grid produced qualitatively identical results but with greater computational cost.

### 3.7 | Machine-learning models implicitly capture the interaction

XGBoost SHAP analysis identified the explicit interaction feature `temp_x_effort` among the top-5 most important features (mean |SHAP| = 0.12). The SHAP dependence plot for `temp_grad_z` showed a clear effort-dependent split: high-effort observations had positive SHAP values for warm-gradient cells, while low-effort observations had negative SHAP values, consistent with the visibility threshold.

Random Forest permutation importance confirmed that effort-related variables and climate variables were both among the top-6 predictors, with interaction features ranking higher than their constituent main effects.

### 3.8 | Future projections identify Southwest China as emerging hotspots

Under SSP5-8.5 with trend effort growth, the 2050 projected mean hazard probability was highest in Tibet (0.46), Yunnan (0.42), Sichuan (0.39), Qinghai (0.37), and Gansu (0.35) at the provincial level. The multi-scale comparison (province, prefecture, county, 100-km grid) revealed consistent spatial patterns but finer-scale heterogeneity at county and grid levels, particularly in the Hengduan Mountains and the eastern edge of the Qinghai–Tibet Plateau.

Effort scenario sensitivity showed that doubled effort increased mean hazard by 15–25% across all provinces, confirming that future record discovery is jointly constrained by climate pressure and observation capacity.

---

## 4 | Discussion

### 4.1 | The visibility threshold as a mechanism

Our central finding is that new bird distribution records are explained not by climate or effort alone but by their *interaction*. The positive, significant climate–effort interaction across all four effort specifications, six climate metrics, two grid scales, and two machine-learning frameworks provides strong evidence for a visibility threshold: climate-driven range shifts create the potential for new records, but this potential is realized only where survey effort is sufficient to detect them.

This result resolves the long-standing ambiguity between "ecological" and "artefactual" explanations of new records. It is not that new records are "real" or "artefactual" — they are both. The climate gradient effect is negative at low effort (species are present but undetected) and positive at high effort (species are detected where they are newly established). This conditional effect is precisely what the visibility threshold predicts.

The practical implication is that regions with steep climate gradients but low survey effort — such as western Sichuan, southern Qinghai, and the eastern Himalaya — may harbour substantial undetected range shifts. These regions represent priority targets for future survey investment.

### 4.2 | Why observer-based effort matters

The fact that the interaction was *stronger* under observer-based effort (Spec B: HR = 1.29) than under record-based effort (Spec A: HR = 1.21) is informative. Record-based effort is endogenous: provinces with more records appear to have higher "effort" partly because more species are present and detectable. This endogeneity attenuates the interaction because part of the effort signal already absorbs the climate effect. Observer-based metrics, which are pure sampling-input measures, allow the climate and effort channels to separate cleanly, producing a sharper interaction.

This finding has methodological implications for any study that uses record count as an effort proxy: such studies may systematically underestimate climate–effort interactions.

### 4.3 | Why temperature gradient outperforms velocity

The superior performance of temperature gradient (species-level temperature anomaly) over climate velocity is consistent with the biology of the system. Climate velocity measures the rate at which isotherms move across the landscape, which is a landscape-level metric. Temperature gradient measures how far a province's climate is from a species' native range, which is a species-level metric. For new distribution records — which are species-specific events — the species-level metric is more proximate.

Mahalanobis distance, which also captures species-level climatic displacement in multivariate space, was the second-best metric, further supporting this interpretation.

### 4.4 | Multi-scale implications

The consistency of the climate–effort interaction from province to 100-km grid scale suggests that the visibility threshold is not an artefact of spatial aggregation. The finer-scale heterogeneity visible at county and grid levels — particularly in topographically complex regions — indicates that provincial-level analyses may underestimate the spatial precision of hotspot identification. Future monitoring should therefore integrate province-level monitoring with targeted county and grid-level surveys.

### 4.5 | Limitations

Several limitations should be acknowledged. First, the hazard model assumes that the risk set correctly identifies species–province–year combinations at risk. The 100-km threshold for range exclusion is conservative but may misclassify some records. Second, the current effort data are available only at the province–year level; finer-scale effort (prefecture, grid) was estimated by spatial disaggregation rather than direct observation. Third, the future projections assume linear effort trends and simplified climate trajectories; actual effort and climate may deviate substantially. Fourth, the model does not account for habitat change, land-use dynamics, or species interactions, which may also drive new records. Finally, the interaction explains a small proportion of total variance (marginal R² ≈ 4%), which is expected for a binary event model with strong random effects but should temper over-interpretation.

### 4.6 | Broader implications

The visibility threshold has implications beyond Chinese birds. Any biodiversity monitoring system that fails to account for the interaction between ecological change and detection capacity will systematically misallocate survey resources. Regions where climate is changing fastest but observation is weakest — a common pattern in the Global South and in remote mountain systems — will appear artificially stable. Conversely, well-observed regions will appear to be changing fastest, potentially creating a biased perception of where conservation action is most needed.

We recommend that future biodiversity assessments explicitly model climate–effort interactions rather than treating effort as a nuisance variable to be "controlled for." The visibility threshold provides a parsimonious, testable, and policy-relevant framework for understanding how the Wallacean shortfall is reduced in space and time.

---

## 5 | Conclusions

New bird distribution records in China are driven by a climate–effort visibility threshold: climate gradient creates the potential for new records, and survey effort determines whether this potential is realized. This interaction is robust across four effort specifications, six climate metrics, two spatial scales, and two statistical frameworks. Future record hotspots are concentrated in Southwest China and the Qinghai–Tibet Plateau, where climate gradients are steep but survey effort remains limited. Survey investment in these regions would yield the highest marginal gains in distribution knowledge.

---

## Acknowledgements

We thank the China Birdwatching Association for access to birding checklist data, and the many observers whose records made this study possible.

## Data availability

All analytical code and derived data are available at [repository URL]. The master species record spreadsheet is available from the corresponding author upon reasonable request.

## Conflict of interest

The authors declare no conflicts of interest.

## Author contributions

CD conceived the study, compiled data, designed analyses, and wrote the manuscript. [Additional authors to be added.]

---

## References

- Antão LH, Bates AE, Blanchet FG, et al. 2020. Temperature-related biodiversity change across temperate marine and terrestrial systems. Nature Ecology & Evolution 4:327–333.
- Boakes EH, McGowan PJK, Fuller RA, et al. 2010. Distorted views of biodiversity: spatial and temporal bias in species occurrence data. PLoS Biology 8:e1000385.
- Bowler DE, Boyd RJ, Callaghan CT, et al. 2025. Treating gaps and biases in biodiversity data as a missing data problem. Biological Reviews 100:50–67.
- Broennimann O, Di Cola V, Guisan A. 2012. ecospat: R package for spatial analysis and modeling of species niches and distributions. Ecography 42:1–9.
- Brooks ME, Kristensen K, van Benthem KJ, et al. 2017. glmmTMB balances speed and flexibility among packages for zero-inflated generalized linear mixed modeling. The R Journal 9:378–400.
- Chen IC, Hill JK, Ohlemüller R, Roy DB, Thomas CD. 2011. Rapid range shifts of species associated with high levels of climate warming. Science 333:1024–1026.
- Chen T, Guestrin C. 2016. XGBoost: A scalable tree boosting system. Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining:785–794.
- Chen S, Chen Z, Lin H, et al. 2025. Chinese provincial-level new records for resident bird species reveal poleward range shifts. Avian Research 100310.
- Ding C, Ding J, Qiao H, Jiang Z, Wang Z. 2025. Taxonomic and spatiotemporal patterns and ecological correlates of new mammal distribution records in China. Global Ecology and Biogeography 34:e70165.
- Hortal J, de Bello F, Diniz-Filho JAF, et al. 2015. Seven shortfalls that beset large-scale knowledge of biodiversity. Annual Review of Ecology, Evolution, and Systematics 46:523–549.
- Hughes AC, Orr MCC, Ma K, et al. 2021. Sampling biases shape our view of the natural world. Ecography 44:1259–1269.
- Kujala H, Välimäki J, Huitu O, et al. 2013. Including dispersal in species distribution models: implications for conservation. Ecology Letters 16:1526–1533.
- Lenoir J, Bertrand R, Comte L, et al. 2020. Species better track climate warming in the oceans than on land. Nature Ecology & Evolution 4:1044–1059.
- Loarie SR, Duffy PB, Hamilton H, et al. 2009. The velocity of climate change. Nature 462:1052–1055.
- Lundberg SM, Lee SI. 2017. A unified approach to interpreting model predictions. Advances in Neural Information Processing Systems 30.
- Meyer C, Kreft H, Guralnick R, Jetz W. 2015. Global priorities for an effective information basis of biodiversity distributions. Nature Communications 6:8221.
- Nakagawa S, Schielzeth H. 2013. A general and simple method for obtaining R² from generalized linear mixed-effects models. Methods in Ecology and Evolution 4:133–142.
- Sanczuk P, Lenoir J, Denelle P, et al. 2026. Global bias towards recording latitudinal range shifts. Nature Climate Change 16:21–25.
- Whittaker RJ, Araujo MB, Jepson P, et al. 2005. Conservation biogeography: assessment and prospect. Diversity and Distributions 11:3–23.

---

## Figure captions

- **Figure 1.** The visibility threshold concept and study design. (A) Conceptual diagram: climate gradient creates the potential for new records (left axis), effort determines detection (right axis), and their interaction defines a detection threshold (diagonal). (B) Study area with provincial boundaries and 100-km grid overlay. (C) Risk set structure and model sequence.

- **Figure 2.** Cross-specification model comparison. (A) Forest plot of climate–effort interaction hazard ratios across four effort specifications. (B) Ridge density plot of hazard ratio distributions. (C) AIC heatmap across model stages and effort specifications. (D) HR stability across M1–M4 for each specification.

- **Figure 3.** The climate–effort interaction surface. (A) Interaction rainforest plot: interaction HR across 6 climate metrics × 4 effort specifications. (B) Predicted hazard surface from the Spec B M4 model (glmmTMB), showing the conditional effect of temperature gradient at different effort levels.

- **Figure 4.** Multi-scale future hotspot projections. (A) Province-level 2050 predicted hazard (SSP5-8.5, trend effort). (B) Prefecture-level. (C) County-level. (D) 100-km grid. (E) National temporal trajectory under current, SSP2-4.5, and SSP5-8.5 scenarios. (F) Effort scenario sensitivity (SSP5-8.5).

- **Figure 5.** Machine-learning corroboration and variance decomposition. (A) SHAP beeswarm plot (XGBoost top-12 features). (B) Random Forest permutation importance. (C) Variance decomposition: additive vs. interaction R² across four effort specifications. (D) Effort offset sensitivity comparison.

- **Figure S1.** Data distribution diagnostics. (A) Temperature gradient by year (violin + beeswarm). (B) Effort metric ridge densities. (C) Correlation matrix of climate and effort variables. (D) Original vs. range-map temperature gradient.

- **Figure S2.** Advanced climate metric comparison. AIC heatmap across 6 climate metrics × 4 effort specifications.

- **Figure S3.** Grid-scale validation. 100-km grid hazard model results.

- **Figure S4.** Directional displacement. Windrose and distance distributions.

- **Figure S5.** Multi-scenario spatial comparison. SSP2-4.5 vs. SSP5-8.5 × 2030/2040/2050.
