# forecast and target data sets for all output types
# ----------------------------------------------------------------------------
# load the package
devtools::load_all()

# forecast data with
# 4 prediction tasks in total: 2 locations (MA, TX), 2 target end dates
# two missing forecasts
forecast_data_raw <- hubExamples::forecast_outputs |>
  dplyr::filter(
    output_type %in% c("median"),
    target_end_date %in% as.Date(c("2022-11-26", "2022-12-10"))
  )

# Specify forecasts to remove
forecast_to_remove <- tibble(
  model_id = c("MOBS-GLEAM_FLUH", "PSI-DICE"),
  location = c("25", "48"),
  target_end_date = as.Date(c("2022-11-26", "2022-12-10"))
)

# Filter out the specified forecasts from the original data
forecast_data <- forecast_data_raw |>
  dplyr::anti_join(
    forecast_to_remove,
    by = c("model_id", "location", "target_end_date")
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
