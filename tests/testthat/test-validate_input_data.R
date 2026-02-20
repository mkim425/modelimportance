library(dplyr)

# data for testing
target_data <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_target_data.rds"
  )
)
forecast_pmfs <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_pmf_model_output.rds"
  )
)
forecast_means <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_mean_model_output.rds"
  )
)
forecast_quantiles <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_qntl_model_output.rds"
  )
)

test_that("validate_input_data() requires a single output type in a dataset", {
  forecast_mix <- rbind(forecast_means, forecast_quantiles)
  na_row <- forecast_quantiles[1, ]
  na_row$output_type <- NA
  forecast_data_na <- rbind(na_row, forecast_quantiles[2:7, ])

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
})

test_that("The output type must be one of specified types", {
  forecast_mix <- rbind(forecast_means, forecast_quantiles)
  forecast_mix$output_type <- "invalid"

  # test
  expect_error(
    validate_input_data(forecast_mix, target_data),
    "The output type is not supported.
      It must be one of 'median', 'mean', 'quantile', or 'pmf'."
  )
})

test_that("validate_input_data() requires exactly one forecast date column", {
  forecast_quantiles2 <- forecast_quantiles |>
    mutate(origin_date = reference_date)

  # test
  expect_error(
    validate_input_data(forecast_quantiles2, target_data),
    paste0(
      "The input 'forecast_data' must contain exactly one of the columns:",
      " 'forecast_date', 'origin_date', 'reference_date'."
    )
  )
})

test_that("At least one common task id column should be in both input data", {
  task_ids <- get_task_id_cols(forecast_means)
  forecast_means2 <- forecast_means |>
    select(-all_of(task_ids)) |>
    mutate(target_id = "target1")

  # test
  expect_error(
    validate_input_data(forecast_means2, target_data),
    "'forecast_data' and 'oracle_output_data' have no common task id column."
  )
})

test_that("Unique tasks on forecast_data should be all in the target data", {
  forecast_means[1, "target_end_date"] <- as.Date("2024-12-24")

  expect_error(
    validate_input_data(forecast_means, target_data),
    paste("All the different tasks on the 'forecast_data' must present",
      "in the 'oracle_output_data' column of the target data.",
      sep = " "
    )
  )
})
