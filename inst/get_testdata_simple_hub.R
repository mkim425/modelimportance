## Code to prepare `example_model_outputs` dataset for testing
# COVID-19 Forecasts
# 3 dates: "2022-12-05", "2022-12-12", "2022-12-19"
# 3 locations: "20", "25", "US"
# 1 target: "inc covid hosp"
# 21 horizons: -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
# 2 output types: "quantile", "median"
# output_type_id: 23 quantile levels, NA
# 3 model_id: "UMass-ar", "UMass-gbq", "simple_hub-baseline"

library(hubUtils)
library(distfromq)

# Sample US COVID-19 Forecast Hub (Hospitalization)
hub_path <- "inst/testhubs/example-simple-forecast-hub"
example_model_output <- hubUtils::connect_hub(hub_path) |>
  dplyr::collect()

# quantile levels to keep
q_levels <- c("0.050", "0.100", "0.250", "0.500", "0.750", "0.900", "0.950")

# example quantilemodel output
example_quantile_model_output <- example_model_output |>
  dplyr::filter(
    horizon %in% c(0, 7),
    location %in% c("25", "US"),
    output_type == "quantile",
    format(output_type_id, nsmall = 3) %in% q_levels
  )

saveRDS(example_quantile_model_output,
  file = "tests/testdata/simple_example_quantile_model_output.rds"
)

# example median model output
example_median_model_output <- example_model_output |>
  dplyr::filter(
    horizon %in% c(0, 7),
    location %in% c("25", "US"),
    output_type == "median"
  )

saveRDS(example_median_model_output,
  file = "tests/testdata/simple_example_median_model_output.rds"
)

# example mean model output
example_mean_model_output <- example_model_output |>
  dplyr::filter(
    horizon %in% c(0, 7),
    location %in% c("25", "US"),
    output_type == "quantile"
  ) |>
  dplyr::group_by(origin_date, horizon, location, target, model_id) |>
  dplyr::summarize(
    value = distfromq::make_r_fn(ps = output_type_id, qs = value)(1e5) |>
      mean(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    output_type = "mean",
    output_type_id = "NA"
  )

saveRDS(example_mean_model_output,
  file = "tests/testdata/simple_example_mean_model_output.rds"
)

# example target data (true values)
simple_example_target_data <-
  read.csv(paste0(hub_path, "/target-data/covid-hospitalizations.csv"))

saveRDS(simple_example_target_data,
  file = "tests/testdata/simple_example_target_data.rds"
)
