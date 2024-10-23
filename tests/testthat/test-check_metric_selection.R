test_that("check_metric_selection() checks for valid combinations of
          output type and scoring rule", {
  # valid combinations
  expect_true(check_metric_selection("mean", "MAE"))
  expect_true(check_metric_selection("mean", "MSE"))
  expect_true(check_metric_selection("median", "MAE"))
  expect_true(check_metric_selection("median", "MSE"))
  expect_true(check_metric_selection("quantile", "WIS"))
  expect_true(check_metric_selection("quantile", "Logscore"))
  expect_true(check_metric_selection("pmf", "CRPS"))
  expect_true(check_metric_selection("cdf", "CRPS"))

  # invalid combinations
  expect_error(
    check_metric_selection("mean", "WIS"),
    "The scoring rule needs to be either MAE or MSE"
  )
  expect_error(
    check_metric_selection("mean", "Logscore"),
    "The scoring rule needs to be either MAE or MSE"
  )
  expect_error(
    check_metric_selection("mean", "CRPS"),
    "The scoring rule needs to be either MAE or MSE"
  )
  expect_error(
    check_metric_selection("median", "WIS"),
    "The scoring rule needs to be either MAE or MSE"
  )
  expect_error(
    check_metric_selection("median", "Logscore"),
    "The scoring rule needs to be either MAE or MSE"
  )
  expect_error(
    check_metric_selection("median", "CRPS"),
    "The scoring rule needs to be either MAE or MSE"
  )
  expect_error(
    check_metric_selection("quantile", "MAE"),
    "The scoring rule needs to be either WIS or Logscore"
  )
  expect_error(
    check_metric_selection("quantile", "MSE"),
    "The scoring rule needs to be either WIS or Logscore"
  )
  expect_error(
    check_metric_selection("quantile", "CRPS"),
    "The scoring rule needs to be either WIS or Logscore"
  )
  expect_error(
    check_metric_selection("pmf", "MAE"),
    "The scoring rule needs to be CRPS"
  )
  expect_error(
    check_metric_selection("pmf", "MSE"),
    "The scoring rule needs to be CRPS"
  )
  expect_error(
    check_metric_selection("pmf", "WIS"),
    "The scoring rule needs to be CRPS"
  )
  expect_error(
    check_metric_selection("pmf", "Logscore"),
    "The scoring rule needs to be CRPS"
  )
  expect_error(
    check_metric_selection("cdf", "MAE"),
    "The scoring rule needs to be CRPS"
  )
  expect_error(
    check_metric_selection("cdf", "MSE"),
    "The scoring rule needs to be CRPS"
  )
  expect_error(
    check_metric_selection("cdf", "WIS"),
    "The scoring rule needs to be CRPS"
  )
  expect_error(
    check_metric_selection("cdf", "Logscore"),
    "The scoring rule needs to be CRPS"
  )
  expect_error(
    check_metric_selection("sample", "MAE"),
    "sample model output type is under development and not yet supported.
         Please use a different output type."
  )
  expect_error(
    check_metric_selection("sample", "MSE"),
    "sample model output type is under development and not yet supported.
         Please use a different output type."
  )
  expect_error(
    check_metric_selection("sample", "WIS"),
    "sample model output type is under development and not yet supported.
         Please use a different output type."
  )
  expect_error(
    check_metric_selection("sample", "Logscore"),
    "sample model output type is under development and not yet supported.
         Please use a different output type."
  )
  expect_error(
    check_metric_selection("sample", "CRPS"),
    "sample model output type is under development and not yet supported.
         Please use a different output type."
  )
  expect_error(
    check_metric_selection("other", "MAE"),
    "invalid output type."
  )
  expect_error(
    check_metric_selection("other", "MSE"),
    "invalid output type."
  )
  expect_error(
    check_metric_selection("other", "WIS"),
    "invalid output type."
  )
  expect_error(
    check_metric_selection("other", "Logscore"),
    "invalid output type."
  )
  expect_error(
    check_metric_selection("other", "CRPS"),
    "invalid output type."
  )
})
