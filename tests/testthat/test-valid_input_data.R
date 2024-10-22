library(dplyr)

test_that("valid_input_data() requires a single output type in a dataset", {
  # data to test
  forecast_data1 <- readRDS(
    system.file("testdata",
      "simple_example_mean_model_output.rds",
      package = "modelimportance"
    )
  )
  forecast_data2 <- readRDS(
    system.file("testdata",
      "simple_example_quantile_model_output.rds",
      package = "modelimportance"
    )
  )
  forecast_data_mix <- rbind(forecast_data1, forecast_data2)
  na_row <- forecast_data2[1, ]
  na_row$output_type <- NA
  forecast_data_na <- rbind(na_row, forecast_data2[2:7, ])

  # test
  expect_error(
    valid_input_data(forecast_data_mix),
    "The input data must contain a single output type."
  )

  expect_error(
    valid_input_data(forecast_data_na),
    "The output type has a missing value."
  )

  expect_equal(
    valid_input_data(forecast_data2) |>
      dplyr::select(output_type) |>
      unique() |>
      as.character(),
    forecast_data2$output_type[1]
  )
})
