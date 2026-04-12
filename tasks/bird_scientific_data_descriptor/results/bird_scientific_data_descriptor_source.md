# A standardized database of provincial bird distribution records in China (2000-2025)

## Abstract

Reliable information on species distributions is essential for biogeography, macroecology, and biodiversity conservation, yet much of the evidence for distribution dynamics remains scattered across local literature, reported in inconsistent formats, and difficult to integrate into interoperable datasets. Birds are a particularly important group for documenting distribution change because they respond rapidly to climate variation, land-use change, and survey expansion, but provincial-level new distribution records in China are still largely buried in peer-reviewed Chinese-language publications. Here we describe the China Bird New Record database (CBNR), a standardized dataset of provincial-level bird new distribution records compiled from peer-reviewed literature published between 2000 and 2025. After literature screening, AI-assisted information extraction, manual validation, taxonomic harmonization, synonym resolution, and duplicate removal based on earliest publication within each species-province combination, the analytical release contains 1,021 validated provincial-level new-record events, plus one ancillary audit-trace record retained in the internal corrected table. The dataset spans 520 bird species across 23 orders and 33 provincial-level administrative units, and includes 25 metadata fields covering taxonomy, publication year, province, coordinates, IUCN status, discovery reason, and detection information. We provide detailed data-processing rules, row-level audit logs, technical validation summaries, and reproducible code to support reuse in biodiversity inventory research, distribution-shift assessments, sampling-bias analyses, and conservation planning. By converting fragmented literature records into a transparent, taxonomically standardized, and georeferenced database, CBNR helps address the Wallacean shortfall for Chinese birds and provides a reusable data foundation for future regional and macroecological analyses.

## Background & Summary

Species distributions are a foundational information layer for ecology, biogeography, and conservation biology because they link biodiversity patterns to environmental gradients, population persistence, extinction risk, and management decisions. However, our understanding of species’ true ranges remains incomplete, especially for under-sampled taxa and geographically complex regions. This distributional knowledge gap, widely referred to as the Wallacean shortfall, is particularly consequential when species records are sparse, geographically biased, or reported in formats that are difficult to synthesize across studies.

China provides an exceptional context for addressing this challenge. The country spans two major zoogeographic realms, supports globally important avian diversity, and lies along several major migratory flyways. At the same time, its large area, environmental heterogeneity, uneven historical survey intensity, and rapid recent growth in biodiversity monitoring have produced a large and dynamic body of bird distribution information. New provincial bird records are increasingly reported in peer-reviewed journals, but many of these publications remain difficult to discover systematically and are not represented in standardized, analysis-ready databases. As a result, much of the evidence needed to evaluate avian distribution change, identify survey gaps, and compare discovery patterns among taxa remains fragmented.

The need for a standardized bird new-record dataset is heightened by the fact that provincial-level new records can arise from multiple, partially overlapping processes. Some reflect genuine range dynamics associated with climate change, habitat modification, or dispersal. Others arise because under-sampled areas, poorly documented taxa, improved field methods, or improved taxonomic resolution make previously overlooked occurrences detectable. Distinguishing among these possibilities requires well-structured data with transparent metadata on time, place, taxonomy, and evidence.

Recent work on Chinese mammals has shown that new distribution records can reveal strong taxonomic, spatial, and effort-related biases in biodiversity knowledge, and that integrating species-level and province-level perspectives can help identify where survey effort is most likely to yield new information. The bird case is both analogous and distinct. Birds are more mobile than most terrestrial mammal groups, are strongly influenced by migratory behaviour and habitat change, and are recorded across a wider range of environments and observation contexts. A dedicated bird new-record database therefore offers major value not only for documenting provincial first records, but also for linking taxonomic knowledge, survey effort, detectability, and distribution dynamics in a rapidly changing system.

To address this need, we assembled the China Bird New Record database (CBNR), a curated, standardized, and georeferenced database of provincial-level bird new distribution records in China from 2000 to 2025. The present Data Descriptor focuses on the construction, structure, validation, and reuse value of this dataset. Compared with a simple literature list, CBNR contributes four major advances. First, it consolidates peer-reviewed evidence that was previously scattered across many local or taxon-specific publications. Second, it standardizes taxonomy against current national checklist resources and explicitly resolves row-level naming inconsistencies and same-species same-province duplicate records. Third, it harmonizes key metadata fields such as publication year, province, coordinates, and detection information into a machine-readable structure. Fourth, it provides a reproducible data-processing workflow with audit logs and validation outputs, improving transparency for downstream ecological and biogeographical analyses.

In its current corrected analytical release, CBNR contains 1,021 validated species-province-year events with complete core metadata, representing 520 species across 23 orders and 33 provincial-level administrative units. The corrected event table contains 25 standardized fields and is accompanied by row-level logs documenting identity correction and duplicate resolution. These features make CBNR suitable for research on taxonomic discovery patterns, spatial and temporal sampling biases, range-edge dynamics, conservation prioritization, and comparative biodiversity-inventory design.

[[FIGURE1]]

## Methods

### Literature search and screening

We systematically searched peer-reviewed publications reporting bird new distribution records from China between 2000 and 2025. Literature retrieval was conducted primarily through China National Knowledge Infrastructure (CNKI; https://www.cnki.net/) and Google Scholar (https://scholar.google.com), using combinations of Chinese and English search terms related to birds, avian records, discovery, new range, new distribution record, and provincial occurrence. Because many provincial new-record studies are published in specialist Chinese journals rather than in globally indexed outlets, we supplemented keyword searches by manually screening journals that regularly publish faunal notes, local avifaunal updates, and new-record reports.

We retained publications only when they satisfied two criteria: (1) they represented peer-reviewed sources with evidence sufficient for taxonomic verification, such as photographs, specimens, or explicitly documented identification procedures; and (2) the reported occurrence constituted a first provincial-level record according to the source publication and subsequent verification against national and regional references. Citizen-science platform records were not used as primary data sources for this database because expert validation, taxonomic certainty, and stable long-term accessibility could not be guaranteed consistently across records and platforms.

After full-text screening, 764 unique peer-reviewed source articles contributed at least one validated event to the corrected analytical release. The start year of 2000 was chosen because the digital accessibility of publications, documentation standards for new records, and the consistency of modern taxonomic frameworks all improved substantially after the late twentieth century, making records from 2000 onward much more suitable for standardized synthesis.

### AI-assisted data extraction

For each article, we extracted core information on species identity, discovery location, province, date, coordinates, elevation, habitat, migratory status, detection method, identification method, inferred discovery reason, and publication metadata. To improve scalability while maintaining reproducibility, we used an AI-assisted extraction workflow based on the Qwen3-Max-2026-01-23 model accessed via the Alibaba Cloud Model Studio API. The model was instructed to return structured JSON outputs for a predefined metadata schema rather than free text.

A set of 100 articles was used as a training and calibration subset. For these papers, AI-extracted records were compared against manually extracted records prepared by researchers. Prompt settings were iteratively refined until the extraction accuracy for structurally critical fields reached 100% within the training workflow. DOI fields showed lower initial accuracy because some source PDFs lacked complete DOI information or included formatting errors; these entries were subsequently corrected manually during the cleaning process. After model-assisted extraction, all records were reviewed during data harmonization, and a further manual spot check was conducted on approximately 10% of the full dataset to confirm consistency among source text, extracted fields, and cleaned outputs.

[[TABLE1]]

### Data standardization, taxonomic harmonization, and duplicate resolution

The raw extraction outputs were standardized into a unified tabular schema. We harmonized species names against the Catalogue of Life China 2025 Annual Checklist to ensure taxonomic consistency across publications that may have used outdated combinations, inconsistent spellings, or alternative naming conventions. All scientific names were normalized to species-level binomials for integration with downstream checklist and trait resources.

A major feature of the present release is the incorporation of two row-level audit workbooks provided during the project update process. The first workbook flagged records with suspected mismatches among Chinese names, English names, and scientific names, as well as formatting inconsistencies. The second workbook documented repeated records for the same species in the same province. We converted both audit workbooks into row-indexed correction tables keyed to the original record sequence in the master spreadsheet. Candidate canonical names were prioritized in the following order: true mismatch corrections, format-only corrections, duplicate-audit workbook, and finally the raw scientific-name field from the master table.

Where the audit tables indicated the same accepted species identity under different naming variants, the accepted binomial was retained. Where multiple candidate canonical names were theoretically possible for a single row, the highest-priority source was retained and the record was written to a conflict log. In the current corrected release, no unresolved row-level conflicts remained after priority filtering.

To ensure that repeated reporting did not inflate event counts, we resolved duplicate records at the species-province level. For each canonical species and province combination, we retained the earliest publication year. If multiple rows shared the same earliest year, the smallest row identifier was retained as a deterministic tiebreaker. This approach follows the principle that a provincial new record should be counted only once: the first formal publication establishes the event, while later reports represent re-documentation rather than additional first records.

Core metadata fields were then standardized into a clean event table with 25 variables. Province names were harmonized to English administrative-unit labels. IUCN status values were standardized to accepted category abbreviations where possible. Discovery reasons were grouped into interpretable categories such as range shift or distribution change, survey gap or under-sampling, taxonomic revision, mixed cases, and unclear records. Core publication metadata were combined into a stable paper identifier to support paper-level counting and traceability.

### Georeferencing and metadata harmonization

When source articles did not provide explicit decimal coordinates, coordinates were assigned based on the locality descriptions reported in the publication, using authoritative map resources and place-name matching. Coordinates were stored in decimal degrees and checked for plausibility within the spatial extent of China. Elevation values, when provided in the source publication, were retained and standardized in metres. Habitat descriptions were harmonized into comparable descriptive categories while preserving their ecological meaning as reported in the source articles.

To maximize reuse, the final release was accompanied by a metadata dictionary describing each variable, its conceptual unit, and example values. We also retained links between the corrected analytical table and the underlying correction logs, allowing users to reconstruct how each row was standardized.

[[TABLE2]]

## Data Records

The corrected analytical release contains 1,021 fully validated species-province-year events derived from 764 peer-reviewed publications, spanning 520 species in 23 orders across 33 provincial-level administrative units between 2000 and 2025. The internal corrected event table contains one additional ancillary row retained for audit traceability, but this row is excluded from analytical summaries requiring complete species-province-year keys. The dataset is organized around a core clean event table and several audit and validation tables.

The primary dataset is the corrected clean event table, which includes 25 standardized fields covering taxonomic identity, publication year, province, coordinates, conservation status, discovery reason, detection metadata, and audit provenance. Companion files include a row-level identity-correction map, duplicate-resolution log, conflict screen, coordinate screen, trait-pool matching audit, and before-versus-after summary of denominator changes during cleaning.

In the corrected analytical release, Passeriformes contributed the largest number of newly documented species, followed by Charadriiformes, Anseriformes, Accipitriformes, and Pelecaniformes. Spatially, the largest numbers of provincial new records were concentrated in Xizang, Yunnan, Hunan, Shaanxi, and Gansu. These patterns should not be interpreted as pure ecological signals alone; rather, they reflect the combined effects of avian diversity, geographic complexity, historical survey gaps, and renewed sampling effort.

[[TABLE3]]

The dataset files generated in the corrected workflow are organized into separate components for data, diagnostics, and downstream analysis products. The corrected clean event table and corrected trait-ready species table are stored alongside the row-level audit outputs. Additional derivative products, including Sankey diagrams, directional summaries, spatiotemporal maps, and GEB-style analytical outputs, are distributed in task-specific subfolders to support different types of reuse.

## Technical Validation

Validation was implemented at four levels: extraction accuracy, taxonomic consistency, coordinate plausibility, and event-level duplication control.

First, AI-assisted extraction was benchmarked against manual extraction using 100 training articles. Most critical fields, including species names, province, publication year, coordinates, and article metadata, reached 100% accuracy during the calibrated extraction workflow, whereas DOI values showed lower raw accuracy because several source PDFs lacked standardized DOI presentation. DOI inconsistencies were resolved manually during cleaning.

Second, all taxonomic names were harmonized against the Catalogue of Life China 2025 Annual Checklist. Records using outdated names, inconsistent binomials, or formatting variants were standardized to accepted species-level names. The row-level anomaly workbook was used to explicitly detect and resolve Chinese-name, English-name, and scientific-name mismatches. This process changed the canonical species binomial for 121 rows in the corrected dataset and generated a transparent row-level mapping table for review.

Third, coordinates were screened for completeness and geographic plausibility. In the corrected analytical release, all complete analytical records include longitude and latitude values. Coordinate screening tables were generated to flag values outside the expected Chinese geographic extent. The complete analytical release contains no missing coordinates in its core event rows.

Fourth, same-species same-province duplicates were resolved using the earliest-publication rule described above. Before this step, 1,077 rows had complete species-province-year keys; after applying the duplicate rule, 1,021 analytical rows remained. In total, 54 duplicated species-province groups were identified and 56 later duplicate rows were removed from the analytical release. No unresolved identity conflicts remained after applying the audit-source priority rules.

Beyond the main data descriptor, the project also includes downstream validation-oriented outputs such as direction-classification tables, spatiotemporal summaries, and model-based diagnostics for the GEB-style analytical modules. These derivative outputs are not part of the core data record itself, but they provide an additional layer of verification that the standardized event table behaves consistently in ecological analyses.

[[FIGURE2]]

[[FIGURE3]]

## Usage Notes

CBNR is intended for studies of biodiversity inventories, provincial faunal updates, macroecological synthesis, distribution-gap assessment, sampling-bias evaluation, and conservation planning. Because the database is based on peer-reviewed reports of provincial first records, it is especially suitable for analyses that treat new records as signals of knowledge gain, survey expansion, or range reassessment.

Users should note four points when reusing the dataset. First, a provincial new record is an administrative event rather than a direct estimate of absolute occupancy, abundance, or recent colonization. A new record may reflect a genuine distribution shift, improved detectability, taxonomic clarification, or simply the publication of a previously overlooked occurrence. Second, the temporal distribution of records is influenced by publication timing and research activity, not solely by the timing of biological change. Third, some variables such as habitat description preserve a degree of source-specific wording and may require additional harmonization for large comparative analyses. Fourth, users interested in strict analytical reproducibility should rely on the corrected analytical release rather than older intermediate project tables, because the present release incorporates explicit synonym resolution and province-level duplicate filtering.

The dataset is particularly well suited for integration with trait databases, checklist resources, and spatial covariates. It can support analyses of taxonomic discovery bias, spatiotemporal hotspots of knowledge gain, directional range-edge dynamics, and province-level drivers of new record detection. Because the underlying correction logs are retained, users can also subset the data to inspect taxonomic harmonization decisions or reconstruct alternative cleaning rules if needed.

## Code Availability

All code used to build the corrected analytical release, produce the quality-control diagnostics, and rerun the main bird new-record analysis tasks is maintained in the project repository: [GitHub repository URL to be confirmed at release]. The current working repository and corrected task package are organized into modular task folders, including scripts for canonical identity correction, duplicate resolution, directional analyses, spatiotemporal analyses, and GEB-style figure generation.

## Acknowledgements

This manuscript builds on a broader research programme on biodiversity knowledge shortfalls, new distribution records, and ecological drivers of species discovery in China. We thank the journal editors, reviewers, and data compilers whose work made the underlying provincial bird records traceable and reusable. We also acknowledge the developers and maintainers of the Catalogue of Life China, CNKI, Google Scholar, and supporting geospatial resources that enabled data verification and standardization.

## Author Contributions

Chenchen Ding conceived the bird new-record database, coordinated data compilation and curation, designed the validation workflow, and drafted the manuscript. [Co-authors and contribution details to be finalized according to the target author list.]

## Competing Interests

The authors declare no competing interests.

## References

Beck, J., Ballesteros-Mejia, L., Nagel, P. & Kitching, I. J. Online solutions and the Wallacean shortfall: what does GBIF contribute to our knowledge of species’ ranges? *Divers. Distrib.* **19**, 1043-1050 (2013).

Darwin, C. *On the Origin of Species by Means of Natural Selection* (John Murray, 1859).

Ding, C., Ding, J., Qiao, H., Jiang, Z. & Wang, Z. Taxonomic and spatiotemporal patterns and ecological correlates of new mammal distribution records in China. *Glob. Ecol. Biogeogr.* **34**, e70165 (2025).

Diniz-Filho, J. A. F. *et al.* Macroecological links between the Linnean, Wallacean, and Darwinian shortfalls. *Front. Biogeogr.* **15**, e59566 (2023).

Feng, X. *et al.* The global significance of biodiversity science in China. *Natl Sci. Rev.* **8**, nwab032 (2021).

Hortal, J. *et al.* Seven shortfalls that beset large-scale knowledge of biodiversity. *Annu. Rev. Ecol. Evol. Syst.* **46**, 523-549 (2015).

Huang, G. *et al.* Wildlife conservation and management in China: achievements, challenges and perspectives. *Natl Sci. Rev.* **8**, nwab042 (2021).

Hughes, A. C., Orr, M. C., Ma, K., Costello, M. J. & Qiao, H. Sampling biases shape our view of the natural world. *Ecography* **44**, 1259-1269 (2021).

Jiang, J. *et al.* [National reports on new vertebrate species and records in China; references to be finalized in journal style].

Mi, X. *et al.* The global significance of biodiversity science in China: an overview. *Natl Sci. Rev.* **8**, nwab032 (2021).

Oliver, R. Y., Meyer, C., Ranipeta, A., Winner, K. & Jetz, W. Global and national trends, gaps, and opportunities in documenting and monitoring species distributions. *PLoS Biol.* **19**, e3001336 (2021).

Whittaker, R. J., Araújo, M. B., Jepson, P., Ladle, R. J., Watson, J. E. & Willis, K. J. Conservation biogeography: assessment and prospect. *Divers. Distrib.* **11**, 3-23 (2005).

Zhang, R. [Chinese zoogeography reference to be formatted according to journal style].
