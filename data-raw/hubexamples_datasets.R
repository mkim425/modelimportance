# Datasets sourced from hubExamples v1.0.0 (hubverse-org/hubExamples@v1.0.0)

library(hubExamples)
library(dplyr)

# Example data in Vignettes (get-started)
forecast_data_raw <- hubExamples::forecast_outputs |>
  dplyr::filter(
    .data$output_type %in% c("median"),
    .data$target_end_date %in% as.Date(c("2022-11-26", "2022-12-10"))
  )

target_data_raw <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    target_end_date %in% unique(forecast_data_raw$target_end_date),
    location %in% unique(forecast_data_raw$location),
    target == "wk inc flu hosp"
  ) |>
  # Rename columns to match the oracle output format
  rename(oracle_value = observation)


# Example data in Vignettes (article)
forecast_data_example <- hubExamples::forecast_outputs |>
  dplyr::filter(
    .data$output_type %in% c("quantile"),
    .data$location == "25"
  )

target_data_example <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    target_end_date %in% unique(forecast_data_example$target_end_date),
    location %in% unique(forecast_data_example$location),
    target == "wk inc flu hosp"
  )

# Example data used in inst/for-vignettes/get-vignette-runtime_data.R
forecast_data_ma_h1 <- hubExamples::forecast_outputs |>
  dplyr::filter(
    output_type %in% c("median"),
    location == "25",
    horizon == 1,
    target_end_date == "2022-12-24"
  )

target_data_ma <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    location == "25",
    target == "wk inc flu hosp",
    target_end_date == "2022-12-24"
  )

usethis::use_data(
  forecast_data_raw,
  target_data_raw,
  forecast_data_example,
  target_data_example,
  forecast_data_ma_h1,
  target_data_ma,
  overwrite = TRUE
)
