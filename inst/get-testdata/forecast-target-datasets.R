# forecast and target data sets for all output types
# ----------------------------------------------------------------------------
# load the package
devtools::load_all()

# forecast data with
# 4 prediction tasks in total: 2 locations (MA, TX), 2 target end dates
# two missing forecasts
forecast_data <- hubExamples::forecast_outputs |>
  dplyr::filter(
    target_end_date %in% as.Date(c("2022-11-26", "2022-12-10")),
    !(model_id == "MOBS-GLEAM_FLUH" &
      location == "25" &
      target_end_date == as.Date("2022-11-26")),
    !(model_id == "PSI-DICE" &
      location == "48" &
      target_end_date == as.Date("2022-12-10"))
  )

# target data with matching locations and target end dates
target_data <- hubExamples::forecast_oracle_output |>
  dplyr::filter(
    target_end_date %in% unique(forecast_data$target_end_date),
    location %in% unique(forecast_data$location)
  )

# save the data
saveRDS(
  forecast_data,
  file = testthat::test_path(
    "testdata/for-model_importance/forecast_data_all_outputs.rds"
  )
)
saveRDS(
  target_data,
  file = testthat::test_path(
    "testdata/for-model_importance/target_data_all_outputs.rds"
  )
)
