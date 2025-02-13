library(dplyr)
library(hubEnsembles)
library(hubEvals)
# data for testing
target_data <- readRDS(
  testthat::test_path("testdata/flu_example_target_data.rds")
) |> mutate(target_end_date = as.Date(target_end_date))

forecast_mean <- readRDS(
  testthat::test_path("testdata/flu_example_mean_model_output.rds")
)
valid_tbl_mean <- validate_input_data(forecast_mean, target_data)
mean_data_list <- split_data_by_task(valid_tbl_mean,
  weighted = FALSE,
  training_window_length = 0
)

forecast_median <- readRDS(
  testthat::test_path("testdata/flu_example_median_model_output.rds")
)
valid_tbl_median <- validate_input_data(forecast_median, target_data)
median_data_list <- split_data_by_task(valid_tbl_median,
  weighted = FALSE,
  training_window_length = 0
)


test_that("Compute when using 'linear pool' with 'mean' output type", {
  # replace with simple values for easy calculation
  dat <- mean_data_list[[16]]
  dat$value <- c(30, 12, 18)
  idx <- with(
    target_data,
    target_end_date == unique(dat$target_end_date) &
      output_type == unique(dat$output_type) &
      location == unique(dat$location)
  )
  target_data$oracle_value[idx] <- 10
  # calculate the expected output
  se_ensemble <- c(
    (mean(dat$value) - target_data$oracle_value[idx])^2,
    (mean(dat$value[2:3]) - target_data$oracle_value[idx])^2,
    (mean(dat$value[c(1, 3)]) - target_data$oracle_value[idx])^2,
    (mean(dat$value[1:2]) - target_data$oracle_value[idx])^2
  )
  expected_importance <- (se_ensemble - se_ensemble[1])[-1]

  expect_equal(
    score_untrained(
      single_task_data = dat,
      oracle_output_data = target_data,
      model_id_list = unique(valid_tbl_mean$model_id),
      ensemble_fun = "linear_pool",
      importance_algorithm = "lomo",
      subset_wt = "equal",
      metric = "se_point"
    ) |> dplyr::pull(importance),
    expected_importance
  )
})

test_that("Assign NAs for missing data", {
  # This test is to check if the function assigns NAs for missing data
  # in the set-up of linear pool ensemble for mean output type.
  # replace with simple values for easy calculation
  dat <- mean_data_list[[1]]
  dat$value <- c(30, 18)
  idx <- with(
    target_data,
    target_end_date == unique(dat$target_end_date) &
      output_type == unique(dat$output_type) &
      location == unique(dat$location)
  )
  target_data$oracle_value[idx] <- 10
  # calculate the expected output
  se_ensemble <- c(
    (mean(dat$value) - target_data$oracle_value[idx])^2,
    (mean(dat$value[2]) - target_data$oracle_value[idx])^2,
    (mean(dat$value[1]) - target_data$oracle_value[idx])^2,
    NA
  )
  expected_importance <- (se_ensemble - se_ensemble[1])[-1]

  expect_equal(
    score_untrained(
      single_task_data = dat,
      oracle_output_data = target_data,
      model_id_list = unique(valid_tbl_mean$model_id),
      ensemble_fun = "linear_pool",
      importance_algorithm = "lomo",
      subset_wt = "equal",
      metric = "se_point"
    ) |> dplyr::pull(importance),
    expected_importance
  )
})
