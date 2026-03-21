# Bird SDM distribution modeling task summary

- Generated at: 2026-03-21 11:20:20
- New-record species pool: 578
- Exact/manual/auto matched species ready for modeling: 196
- Successfully modeled species: 0
- Unmatched species requiring Avibase/IOC + BirdLife review: 382
- Province sensitivity thresholds: 3, 10, 20, 50 cells

## Algorithm availability

- GLM: available (stats::glm)
- GAM: available (mgcv::gam)
- GBM: available (gbm::gbm)
- RF: available (randomForest::randomForest)
- MaxEnt: available (maxnet::maxnet)

## Notes

- Prepare-only mode completed: generated taxonomy review tables and configuration templates.
- Fill taxonomy_manual_overrides.csv after Avibase/IOC + BirdLife review to recover more unmatched species.
