library(dplyr)
library(hubEnsembles)
library(hubEvals)
# data for testing
source(testthat::test_path("testdata/testdata-score_untrained-mean.R"))

test_that("Compute when using 'linear pool' with 'mean' output type", {
  # calculate importance scores with mean output and linear pool
  calculated <- score_untrained(
    single_task_data = dat_mean,
    oracle_output_data = target_data_mean,
    model_id_list = unique(valid_tbl_mean$model_id),
    ensemble_fun = "linear_pool",
    importance_algorithm = "lomo",
    subset_wt = "equal",
    metric = "se_point"
  ) |>
    dplyr::select(model_id, importance) |>
    as.data.frame()
  # remove the metrics attribute from the calculated importance
  attr(calculated, "metrics") <- NULL
  # expected values
  exp_imp_mean_lp <- exp_imp_mean |>
    filter(
      ens_fun == "linear_pool",
      test_purp == "properly assigned"
    ) |>
    dplyr::select(model_id, importance)
  # test: compare the calculated importance with the expected importance
  expect_equal(calculated, exp_imp_mean_lp)
})

test_that("Assign NAs for missing data", {
  # This test is to check if the function assigns NAs for missing data
  # in the set-up of linear pool ensemble for mean output type.
  # calculate importance scores with mean output and linear pool
  calculated <- score_untrained(
    single_task_data = sub_dat_mean,
    oracle_output_data = target_data_mean,
    model_id_list = unique(valid_tbl_mean$model_id),
    ensemble_fun = "linear_pool",
    importance_algorithm = "lomo",
    subset_wt = "equal",
    metric = "se_point"
  ) |>
    dplyr::select(model_id, importance) |>
    as.data.frame()
  # remove the metrics attribute from the calculated importance
  attr(calculated, "metrics") <- NULL
  # expected values
  exp_imp_mean_na_lp <- exp_imp_mean |>
    filter(
      ens_fun == "linear_pool",
      test_purp == "missing data"
    ) |>
    dplyr::select(model_id, importance)
  # test: compare the calculated importance with the expected importance
  expect_equal(calculated, exp_imp_mean_na_lp)
})
