# test with mean output type
test_that("adjust_metric converts se_point to rse_point", {
  input_df_mean <- data.frame(
    model_id = c("model_1", "model_2", "model_3"),
    se_point = c(4, 9, 16)
  )

  expected_df_mean <- data.frame(
    model_id = c("model_1", "model_2", "model_3"),
    rse_point = c(2, 3, 4)
  )
  adjusted_df <- adjust_metric(input_df_mean)
  expect_equal(adjusted_df, expected_df_mean)
})

# test with pmf output type
input_df_pmf <- data.frame(
  model_id = c("model_1", "model_2", "model_3"),
  log_score = c(Inf, 9, 16)
)

expected_df_pmf_default <- data.frame(
  model_id = c("model_1", "model_2", "model_3"),
  log_score = c(10, 9, 16)
)

expected_df_pmf_specified <- data.frame(
  model_id = c("model_1", "model_2", "model_3"),
  log_score = c(2, 9, 16)
)

test_that("adjust_metric converts -Inf to defalut minimum score", {
  adjusted_df_defalut <- adjust_metric(input_df_pmf)
  expect_equal(adjusted_df_defalut, expected_df_pmf_default)
})

test_that("adjust_metric converts -Inf to specified minimum score", {
  adjusted_df_specified <- adjust_metric(input_df_pmf, log_min_val = -2)
  expect_equal(adjusted_df_specified, expected_df_pmf_specified)
})

# other output types, e.g., median with ae_point metric
test_that("adjust_metric handles data frame without se_point or log_score", {
  input_df_other <- data.frame(
    model_id = c("model_1", "model_2", "model_3"),
    ae_point = c(1, 2, 3)
  )

  adjusted_df_other <- adjust_metric(input_df_other)
  expect_equal(adjusted_df_other, input_df_other)
})
