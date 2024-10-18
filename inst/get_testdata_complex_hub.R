## Code to prepare `example_model_outputs` dataset for testing
# FluSight Forecasts
# 4 dates: "2022-10-22", "2022-11-19", "2022-12-17", "2023-01-14"
# 53 locations including US
# 3 targets: "wk inc flu hosp", "wk flu hosp rate category", "wk flu hosp rate"
# 4 weeks horizon: 0, 1, 2, 3
# 6 output types: "quantile", "median", "mean", "sample", "pmf", "cdf"
# 3 model_id: "Flusight-baseline", "MOBS-GLEAM_FLUH", "PSI-DICE"

library(hubUtils)
library(distfromq)

# Sample Flusight Forecast Hub
hub_path <- "inst/testhubs/example-complex-forecast-hub"
example_complex_model_output <- hubUtils::connect_hub(hub_path) |>
  dplyr::collect()
