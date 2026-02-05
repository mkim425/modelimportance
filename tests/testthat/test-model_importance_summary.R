# Unit tests for `model_importance_summary()` function with 2 summary functions:
# mean and median

library(dplyr)
library(checkmate)
library(hubEvals)
library(purrr)
future::plan("sequential") # Set up sequential plan for testing

# data frame of raw importance scores per output type
file_names <- c(
  exp_raw_imp_mean = "exp_raw_imp_scores_mean.rds",
  exp_raw_imp_quantile = "exp_raw_imp_scores_quantile.rds",
  exp_raw_imp_median = "exp_raw_imp_scores_median.rds",
  exp_raw_imp_pmf = "exp_raw_imp_scores_pmf.rds"
)
raw_imp_list <- map(
  file_names,
  ~ readRDS(testthat::test_path("testdata/for-model_importance", .x))
)

raw_imp_scores <- dplyr::bind_rows(raw_imp_list)
# data frame of raw importance scores with imputed missing values
imputed_file_names <- c(
  exp_imputed_imp_mean = "exp_imputed_imp_scores_mean.rds",
  exp_imputed_imp_quantile = "exp_imputed_imp_scores_quantile.rds",
  exp_imputed_imp_median = "exp_imputed_imp_scores_median.rds",
  exp_imputed_imp_pmf = "exp_imputed_imp_scores_pmf.rds"
)
imputed_imp_list <- map(
  imputed_file_names,
  ~ readRDS(testthat::test_path("testdata/for-model_importance", .x))
)
imputed_imp_scores <- dplyr::bind_rows(imputed_imp_list)

# Get unique calculation argument combinations
calc_args_list <- imputed_imp_scores |>
  dplyr::distinct(calc_args) |>
  dplyr::pull(calc_args)

param_grid <- expand.grid(
  args = calc_args_list,
  handle_na = c("drop", "worst", "average"),
  stringsAsFactors = FALSE
)
## Test: model_importance_summary() function works properly with different
## na_action options and summary function mean/median
pmap(
  param_grid,
  function(args, handle_na) {
    test_that(
      paste0(
        "model_importance_summary() works with calc_args: ", args,
        " /na_action = ", handle_na
      ),
      {
        # filter the raw and imputed importance scores for the given args
        raw_scores_subset <- raw_imp_scores |>
          dplyr::filter(calc_args == args)
        imputed_scores_subset <- imputed_imp_scores |>
          dplyr::filter(calc_args == args, na_action == handle_na)
        # compute expected summary importance scores
        exp_summary_scores_mean <- imputed_scores_subset |>
          dplyr::group_by(.data$model_id) |>
          dplyr::summarise(
            importance_score_mean = mean(.data$importance),
            .groups = "drop"
          ) |>
          dplyr::arrange(desc(importance_score_mean))
        exp_summary_scores_median <- imputed_scores_subset |>
          dplyr::group_by(.data$model_id) |>
          dplyr::summarise(
            importance_score_median = median(.data$importance),
            .groups = "drop"
          ) |>
          dplyr::arrange(desc(importance_score_median))
        # compute actual summary importance scores
        actual_summary_scores_mean <- raw_scores_subset |>
          model_importance_summary(
            by = "model_id", na_action = handle_na, fun = mean
          )
        actual_summary_scores_median <- raw_scores_subset |>
          model_importance_summary(
            by = "model_id", na_action = handle_na, fun = median
          )
        # compare expected and actual results
        expect_equal(
          actual_summary_scores_mean, exp_summary_scores_mean,
          tolerance = 1e-8, ignore_attr = TRUE
        )
        expect_equal(
          actual_summary_scores_median, exp_summary_scores_median,
          tolerance = 1e-8, ignore_attr = TRUE
        )
      }
    )
  }
)

## Test: importance_summary class and its methods
test_that("importance_summary class and methods", {
  data <- raw_imp_scores |>
    dplyr::filter(calc_args == "mean_output-simple_ensemble-lomo-equal-mean")
  summary_scores <- model_importance_summary(
    importance_scores = data,
    by = "model_id",
    na_action = "drop",
    fun = mean
  )
  # test for class and its methods
  expect_s3_class(summary_scores, "importance_summary")
  expect_error(print(summary_scores), NA)
  expect_error(summary(summary_scores), NA)
  expect_error(plot(summary_scores), NA)
})
