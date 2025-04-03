## Code to prepare `example_model_outputs` dataset for testing
# FluSight Forecasts
# 4 reference dates: "2022-10-22", "2022-11-19", "2022-12-17", "2023-01-14"
# 53 locations including US
# 3 targets: "wk inc flu hosp", "wk flu hosp rate category", "wk flu hosp rate"
# horizon: 0, 1, 2, 3
# 6 output types: "quantile", "median", "mean", "sample", "pmf", "cdf"
# 3 model_id: "Flusight-baseline", "MOBS-GLEAM_FLUH", "PSI-DICE"

library(hubUtils)

# Sample Flusight Forecast Hub
hub_path <- test_path("testhubs", "example-complex-forecast-hub")
example_complex_model_output <- hubData::connect_hub(hub_path) |>
  hubData::collect_hub()

# quantile levels to keep
q_levels <- c("0.1", "0.25", "0.5", "0.75", "0.9")

# example quantilemodel output
example_quantile_model_output <- example_complex_model_output |>
  dplyr::filter(
    horizon %in% c(1, 3),
    location %in% c("25", "US"),
    output_type == "quantile",
    output_type_id %in% q_levels
  )

saveRDS(example_quantile_model_output,
  file = "tests/testthat/testdata/flu_example_quantile_model_output.rds"
)

# example median model output
example_median_model_output <- example_complex_model_output |>
  dplyr::filter(
    horizon %in% c(1, 3),
    location %in% c("25", "US"),
    output_type == "median"
  )

saveRDS(example_median_model_output,
  file = "tests/testthat/testdata/flu_example_median_model_output.rds"
)

# example mean model output
example_mean_model_output <- example_complex_model_output |>
  dplyr::filter(
    horizon %in% c(1, 3),
    location %in% c("25", "US"),
    output_type == "mean"
  )

saveRDS(example_mean_model_output,
  file = "tests/testthat/testdata/flu_example_mean_model_output.rds"
)

# example sample model output
example_sample_model_output <- example_complex_model_output |>
  dplyr::filter(
    horizon %in% c(1, 3),
    location %in% c("25", "US"),
    output_type == "sample"
  )

saveRDS(example_sample_model_output,
  file = "tests/testthat/testdata/flu_example_sample_model_output.rds"
)

# example pmf model output
example_pmf_model_output <- example_complex_model_output |>
  dplyr::filter(
    horizon %in% c(1, 3),
    location %in% c("25", "US"),
    output_type == "pmf"
  )

saveRDS(example_pmf_model_output,
  file = "tests/testthat/testdata/flu_example_pmf_model_output.rds"
)

# example cdf model output
example_cdf_model_output <- example_complex_model_output |>
  dplyr::filter(
    horizon %in% c(1, 3),
    location %in% c("25", "US"),
    output_type == "cdf"
  )

saveRDS(example_cdf_model_output,
  file = "tests/testthat/testdata/flu_example_cdf_model_output.rds"
)

# example target data (true values)
flu_example_target_data <-
  read.csv(paste0(hub_path, "/target-data/oracle-output.csv"))

saveRDS(flu_example_target_data,
  file = "tests/testthat/testdata/flu_example_target_data.rds"
)
