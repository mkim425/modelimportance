library(dplyr)

# data for testing
target_data <- readRDS(
  testthat::test_path("testdata/flu_example_target_data.rds")
)
# data with output type 'median'
forecast_median <- readRDS(
  testthat::test_path("testdata/flu_example_median_model_output.rds")
)

test_that("Error when using 'linear pool' with 'median' output type", {
  expect_error(
    model_importance(
      forecast_data = forecast_median,
      oracle_output_data = target_data,
      ensemble_fun = "linear_pool"
    ),
    "Error: 'linear pool' cannot be used when output type is 'median'."
  )
})
