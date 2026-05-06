# This script is used to get the forecast data for the vignette example.

library(hubExamples)
library(dplyr)
# Load the forecast data from hubExamples
forecast_data_raw <- hubExamples::forecast_outputs |>
  dplyr::filter(
    output_type %in% c("median"),
    target_end_date %in% as.Date(c("2022-11-26", "2022-12-10"))
  )

# Specify forecasts to remove: MOBS-GLEAM_FLUH for location 25 on 2022-11-26,
# PSI-DICE for location 48 on 2022-12-10
forecast_to_remove <- data.frame(
  model_id = c("MOBS-GLEAM_FLUH", "PSI-DICE"),
  location = c("25", "48"),
  target_end_date = as.Date(c("2022-11-26", "2022-12-10"))
)

# Filter out the specified forecasts from the original data
forecast_data <- forecast_data_raw |>
  anti_join(
    forecast_to_remove,
    by = c("model_id", "location", "target_end_date")
  )

# saveRDS(forecast_data, file = "./inst/for-vignettes/vignette-example-forecast_data.rds")

target_data <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    target_end_date %in% unique(forecast_data$target_end_date),
    location %in% unique(forecast_data$location),
    target == "wk inc flu hosp"
  ) |>
  # Rename columns to match the oracle output format
  rename(
    oracle_value = observation
  )
# saveRDS(target_data, file = "./inst/for-vignettes/vignette-example-oracle_data.rds")
