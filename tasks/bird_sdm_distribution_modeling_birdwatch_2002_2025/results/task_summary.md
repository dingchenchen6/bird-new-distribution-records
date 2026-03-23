# Bird SDM distribution modeling task summary

- Generated at: 2026-03-23 08:37:05
- New-record species pool: 578
- Exact/manual/auto matched species ready for modeling: 540
- Successfully modeled species: 477
- Unmatched species requiring Avibase/IOC + BirdLife review: 38
- Province sensitivity thresholds: 3, 10, 20, 50, 100, 200 cells

## Algorithm availability

- GLM: available (stats::glm)
- GAM: available (mgcv::gam)
- GBM: available (gbm::gbm)
- RF: available (randomForest::randomForest)
- MaxEnt: available (maxnet::maxnet)

## Notes

- Taxonomy review includes explicit Avibase/IOC and BirdLife review fields.
- Province sensitivity analysis is written for 3-cell, 10-cell, 20-cell, 50-cell, 100-cell, 200-cell thresholds.
- MaxEnt uses maxent.jar when Java and the jar are available; otherwise it falls back to maxnet if installed.
- Occurrence points, climate rasters, background sampling, and predictions are all restricted to the China boundary.
- Species maps are rendered in a China-focused equal-area projection and overlay the provincial boundary together with the nine-dash line base layer.
- Potential distribution GeoTIFF rasters are indexed in table_raster_output_manifest.csv for every successfully modeled species.
- Current-climate preparation first checks the task climate cache for readable WorldClim bioclim and elevation files, and writes the validation result to table_climate_data_manifest.csv.
- Run progress is written to data/progress/progress_summary.csv and data/progress/progress_events.log during batch execution.
- The final potential-province table only keeps successfully modeled species and lists each province, suitable-cell count, and suitable area for every active threshold.
- WorldClim elevation is appended as an environmental predictor and enters the variable-screening workflow together with the bioclim variables.
