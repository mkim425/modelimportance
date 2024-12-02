test_that("check_metric_selection() checks for valid scoring rule", {
  # valid combinations
  expect_true(check_metric_selection("mean", "se_point"))
  expect_true(check_metric_selection("median", "ae_point"))
  expect_true(check_metric_selection("quantile", "wis"))
  expect_true(check_metric_selection("pmf", "log_score"))

  # invalid combinations
  expect_error(
    check_metric_selection("mean", "wis"),
    "The scoring rule needs to be se_point."
  )
  expect_error(
    check_metric_selection("mean", "log_score"),
    "The scoring rule needs to be se_point."
  )
  expect_error(
    check_metric_selection("median", "wis"),
    "The scoring rule needs to be ae_point."
  )
  expect_error(
    check_metric_selection("median", "log_score"),
    "The scoring rule needs to be ae_point."
  )
  expect_error(
    check_metric_selection("quantile", "ae_point"),
    "The scoring rule needs to be wis."
  )
  expect_error(
    check_metric_selection("quantile", "se_point"),
    "The scoring rule needs to be wis."
  )
  expect_error(
    check_metric_selection("quantile", "log_score"),
    "The scoring rule needs to be wis."
  )
  expect_error(
    check_metric_selection("pmf", "ae_point"),
    "The scoring rule needs to be log_score."
  )
  expect_error(
    check_metric_selection("pmf", "se_point"),
    "The scoring rule needs to be log_score."
  )
  expect_error(
    check_metric_selection("pmf", "wis"),
    "The scoring rule needs to be log_score."
  )
  expect_error(
    check_metric_selection("sample", "ae_point"),
    "cdf and sample model output types are under development and not yet
    supported. Please use a different output type."
  )
  expect_error(
    check_metric_selection("sample", "se_point"),
    "cdf and sample model output types are under development and not yet
    supported. Please use a different output type."
  )
  expect_error(
    check_metric_selection("sample", "wis"),
    "cdf and sample model output types are under development and not yet
    supported. Please use a different output type."
  )
  expect_error(
    check_metric_selection("sample", "log_score"),
    "cdf and sample model output types are under development and not yet
    supported. Please use a different output type."
  )
  expect_error(
    check_metric_selection("cdf", "ae_point"),
    "cdf and sample model output types are under development and not yet
    supported. Please use a different output type."
  )
  expect_error(
    check_metric_selection("cdf", "se_point"),
    "cdf and sample model output types are under development and not yet
    supported. Please use a different output type."
  )
  expect_error(
    check_metric_selection("cdf", "wis"),
    "cdf and sample model output types are under development and not yet
    supported. Please use a different output type."
  )
  expect_error(
    check_metric_selection("cdf", "log_score"),
    "cdf and sample model output types are under development and not yet
    supported. Please use a different output type."
  )
  expect_error(
    check_metric_selection("other", "ae_point"),
    "invalid output type."
  )
  expect_error(
    check_metric_selection("other", "se_point"),
    "invalid output type."
  )
  expect_error(
    check_metric_selection("other", "wis"),
    "invalid output type."
  )
  expect_error(
    check_metric_selection("other", "log_score"),
    "invalid output type."
  )
})
