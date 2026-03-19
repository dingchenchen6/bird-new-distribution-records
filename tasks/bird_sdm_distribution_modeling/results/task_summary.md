# Bird SDM distribution modeling task summary

- Generated at: 2026-03-19 20:36:17
- New-record species pool: 578
- Exact/manual/auto matched species ready for modeling: 196
- Successfully modeled species: 1
- Unmatched species requiring Avibase/IOC + BirdLife review: 382
- Province sensitivity thresholds: 3 cells and 10 cells

## Algorithm availability

- GLM: available (stats::glm)
- GAM: available (mgcv::gam)
- GBM: available (gbm::gbm)
- RF: available (randomForest::randomForest)
- MaxEnt: available (maxnet::maxnet)

## Notes

- Taxonomy review includes explicit Avibase/IOC and BirdLife review fields.
- Province sensitivity analysis is written for both 3-cell and 10-cell thresholds.
- MaxEnt uses maxent.jar when Java and the jar are available; otherwise it falls back to maxnet if installed.
- Occurrence points, climate rasters, background sampling, and predictions are all restricted to the China boundary.
