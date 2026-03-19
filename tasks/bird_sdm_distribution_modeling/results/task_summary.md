# Bird SDM distribution modeling task summary

- Generated at: 2026-03-19 21:30:26
- New-record species pool: 578
- Exact/manual/auto matched species ready for modeling: 196
- Successfully modeled species: 1
- Unmatched species requiring Avibase/IOC + BirdLife review: 382
- Province sensitivity thresholds: 3, 10, 20, 50 cells

## Algorithm availability

- GLM: available (stats::glm)
- GAM: available (mgcv::gam)
- GBM: available (gbm::gbm)
- RF: available (randomForest::randomForest)
- MaxEnt: available (maxnet::maxnet)

## Notes

- Taxonomy review includes explicit Avibase/IOC and BirdLife review fields.
- Province sensitivity analysis is written for 3-cell, 10-cell, 20-cell, 50-cell thresholds.
- MaxEnt uses maxent.jar when Java and the jar are available; otherwise it falls back to maxnet if installed.
- Occurrence points, climate rasters, background sampling, and predictions are all restricted to the China boundary.
- The final potential-province table only keeps successfully modeled species and lists each province, suitable-cell count, and suitable area for every active threshold.
- WorldClim elevation is appended as an environmental predictor and enters the variable-screening workflow together with the bioclim variables.
