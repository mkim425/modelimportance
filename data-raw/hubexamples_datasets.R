# Datasets sourced from hubExamples v1.0.0 (hubverse-org/hubExamples@v1.0.0)
# Run this script to regenerate data/ if hubExamples is updated.

library(hubExamples)
library(dplyr)

# Dates used across all vignettes and examples
example_dates <- as.Date(c(
  "2022-11-19",
  "2022-11-26",
  "2022-12-03",
  "2022-12-10",
  "2022-12-17",
  "2022-12-24",
  "2022-12-31",
  "2023-01-07"
))

# median (both locations) + quantile (location "25" only)
forecast_outputs <- hubExamples::forecast_outputs |>
  filter(
    (output_type == "median" & location %in% c("25", "48")) |
      (output_type == "quantile" & location == "25")
  )

# observed values for the two locations and dates used in vignettes
forecast_target_ts <- hubExamples::forecast_target_ts |>
  filter(
    location %in% c("25", "48"),
    target_end_date %in% example_dates,
    target == "wk inc flu hosp"
  )

# oracle output for the two dates and locations used in get-testdata script
forecast_oracle_output <- hubExamples::forecast_oracle_output |>
  filter(
    target_end_date %in% as.Date(c("2022-11-26", "2022-12-10")),
    location %in% c("25", "48")
  )

usethis::use_data(
  forecast_outputs,
  forecast_target_ts,
  forecast_oracle_output,
  overwrite = TRUE
)
