library(dplyr)

test_that("validate_input_data() requires a single output type in a dataset", {
  # data to test
  target_data <- readRDS(
    testthat::test_path("testdata/flu_example_target_data.rds")
  )

  forecast_pmfs <- readRDS(
    testthat::test_path("testdata/flu_example_pmf_model_output.rds")
  )
  forecast_means <- readRDS(
    testthat::test_path("testdata/flu_example_mean_model_output.rds")
  )
  forecast_quantiles <- readRDS(
    testthat::test_path("testdata/flu_example_quantile_model_output.rds")
  )
  forecast_mix <- rbind(forecast_means, forecast_quantiles)
  na_row <- forecast_quantiles[1, ]
  na_row$output_type <- NA
  forecast_data_na <- rbind(na_row, forecast_quantiles[2:7, ])

  reduced_target_data <- target_data |>
    filter(target_end_date != min(forecast_means$target_end_date))

  # test
  expect_error(
    validate_input_data(forecast_mix, target_data),
    "The input data must contain a single output type."
  )

  expect_error(
    validate_input_data(forecast_data_na, target_data),
    "The output type has a missing value."
  )

  expect_equal(
    validate_input_data(forecast_quantiles, target_data) |>
      dplyr::select(output_type) |>
      unique() |>
      as.character(),
    unique(forecast_quantiles$output_type)
  )

  expect_equal(
    validate_input_data(forecast_pmfs, target_data) |>
      dplyr::select(output_type) |>
      unique() |>
      as.character(),
    unique(forecast_pmfs$output_type)
  )

  expect_error(
    validate_input_data(forecast_means, reduced_target_data),
    "All values in the 'target_end_date' column of the forecast data must
         present in the 'target_end_date' column of the target data."
  )
})
