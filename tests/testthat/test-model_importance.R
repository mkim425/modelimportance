# Unit tests for `model_importance()` function

library(dplyr)
library(hubEvals)
library(purrr)
future::plan("sequential") # Set up sequential plan for testing

# forecast data list
f_all_data <- readRDS(
  testthat::test_path(
    "testdata/for-model_importance/forecast_data_all_outputs.rds"
  )
)
data_list <- list(
  dat_mean = f_all_data |> dplyr::filter(output_type == "mean"),
  dat_quantile = f_all_data |> dplyr::filter(output_type == "quantile"),
  dat_median = f_all_data |> dplyr::filter(output_type == "median"),
  dat_pmf = f_all_data |> dplyr::filter(output_type == "pmf")
)
# target data
target_data <- readRDS(
  testthat::test_path(
    "testdata/for-model_importance/target_data_all_outputs.rds"
  )
)

# list of expected values for testing
exp_file_names <- c(
  exp_raw_imp_mean = "exp_raw_imp_scores_mean.rds",
  exp_raw_imp_quantile = "exp_raw_imp_scores_quantile.rds",
  exp_raw_imp_median = "exp_raw_imp_scores_median.rds",
  exp_raw_imp_pmf = "exp_raw_imp_scores_pmf.rds"
)
exp_imp_list <- map(
  exp_file_names,
  ~ readRDS(testthat::test_path("testdata/for-model_importance", .x))
)

# combination of arguments
params <- expand.grid(
  output_type = c("mean", "quantile", "median", "pmf"),
  ens_fun = c("simple_ensemble", "linear_pool"),
  agg_fun = c("mean", "median"),
  algorithm = c("lomo", "lasomo"),
  subset_weight = c("equal", "perm_based"),
  stringsAsFactors = FALSE
) |>
  # adjustments: no agg_fun for linear_pool, subset_weight only for lasomo
  mutate(
    agg_fun = ifelse(ens_fun == "linear_pool", "NA", agg_fun),
    subset_weight = ifelse(algorithm == "lomo", "equal", subset_weight)
  ) |>
  filter(!(output_type == "median" & ens_fun == "linear_pool")) |>
  distinct() |>
  arrange(algorithm, output_type, ens_fun, agg_fun, subset_weight)

## Test: model_importance() function works properly with different combinations
## of arguments
pmap(
  params,
  function(output_type, ens_fun, agg_fun, algorithm, subset_weight) {
    test_that(
      paste(
        "Testing if the function works properly with output type:",
        output_type,
        "/ensemble function:",
        ens_fun,
        "/aggregation function:",
        agg_fun,
        "/algorithm:",
        algorithm,
        "/subset_weight:",
        subset_weight
      ),
      {
        # get the data corresponding to the arguments
        selected_data <- data_list[[paste0("dat_", output_type)]]
        selected_expected_importance <- exp_imp_list[[paste0(
          "exp_raw_imp_",
          output_type
        )]]
        # calculate importance scores with the given arguments
        if (ens_fun != "linear_pool") {
          calculated <- suppressMessages(model_importance(
            forecast_data = selected_data,
            oracle_output_data = target_data,
            ensemble_fun = ens_fun,
            importance_algorithm = algorithm,
            subset_wt = subset_weight,
            agg_fun = agg_fun,
            min_log_score = -10
          ))
        } else {
          calculated <- suppressMessages(model_importance(
            forecast_data = selected_data,
            oracle_output_data = target_data,
            ensemble_fun = ens_fun,
            importance_algorithm = algorithm,
            subset_wt = subset_weight,
            min_log_score = -10
          ))
        }
        # expected data frame of importance scores
        expected_df <- selected_expected_importance |>
          filter(
            calc_args ==
              paste0(
                output_type,
                "_output-",
                ens_fun,
                "-",
                algorithm,
                "-",
                subset_weight,
                "-",
                agg_fun
              )
          ) |>
          select(-calc_args)
        # test: compare the calculated importance with the expected importance,
        # ignoring their attributes
        expect_equal(
          calculated,
          expected_df,
          tolerance = 1e-3,
          ignore_attr = TRUE
        )
      }
    )
  }
)

## Test: `simple_ensemble` and `linear_pool` perform the same calculation for
## `mean` and `pmf`
test_that("simple_ensemble and linear_pool give same results for mean and pmf", {
  for (output_type in c("mean", "pmf")) {
    selected_data <- data_list[[paste0("dat_", output_type)]]
    # calculate importance scores with simple_ensemble
    imp_simple_ens <- suppressMessages(model_importance(
      forecast_data = selected_data,
      oracle_output_data = target_data,
      ensemble_fun = "simple_ensemble",
      importance_algorithm = "lomo",
      subset_wt = "equal",
      min_log_score = -10
    ))
    # calculate importance scores with linear_pool
    imp_linear_pool <- suppressMessages(model_importance(
      forecast_data = selected_data,
      oracle_output_data = target_data,
      ensemble_fun = "linear_pool",
      importance_algorithm = "lomo",
      subset_wt = "equal",
      min_log_score = -10
    ))
    # test: compare the two importance scores
    expect_equal(
      imp_simple_ens,
      imp_linear_pool,
      tolerance = 1e-3,
      ignore_attr = TRUE
    )
  }
})

## Test: model_imp_tbl class and its methods
test_that("model_imp_tbl class and methods are valid", {
  selected_data <- data_list[["dat_quantile"]]
  imp_scores <- suppressMessages(model_importance(
    forecast_data = selected_data,
    oracle_output_data = target_data,
    ensemble_fun = "simple_ensemble",
    importance_algorithm = "lomo",
    subset_wt = "equal",
    agg_fun = "mean",
    min_log_score = -10
  ))
  # test for class and print/plot methods
  expect_s3_class(imp_scores, "model_imp_tbl")
  expect_error(print(imp_scores), NA)
  expect_error(plot(imp_scores), NA)
  # test for summary method
  s <- summary(imp_scores)
  expect_s3_class(s, "summary.model_imp_tbl")
  expect_true(is.data.frame(s$task_winners))
  expect_true(is.data.frame(s$model_summary))
  expect_true(is.data.frame(s$all_tasks))
  expect_equal(length(s$all_models), length(unique(imp_scores$model_id)))
})

# Test: LOMO algorithm runs within a reasonable time with a larger number of
# models and tasks
test_that("lomo runs with a large number of models and tasks", {
  testthat::skip_on_cran()

  # Create a larger dataset with more models and tasks
  n_models <- 50
  n_tasks <- 1000
  fake_models <- paste0("model_", seq_len(n_models))
  fake_fdat <- data_list[[1]] |>
    slice(1) |>
    select(-model_id, -value) |>
    slice(rep(seq_len(n()), each = n_tasks)) |>
    mutate(target_end_date = as.Date("2022-11-26") + (row_number() - 1)) |>
    slice(rep(seq_len(n()), each = n_models)) |>
    mutate(
      model_id = rep(fake_models, times = n() / n_models),
      value = rnorm(n(), mean = 500, sd = 100)
    )
  fake_target_data <- target_data |>
    filter(location == "25", output_type == "mean") |>
    slice(1) |>
    slice(rep(seq_len(n()), each = n_tasks)) |>
    mutate(
      target_end_date = as.Date("2022-11-26") + (row_number() - 1),
      oracle_value = oracle_value + rnorm(n(), mean = 100, sd = 50)
    )
  # test that the function runs without error
  expect_error(
    suppressMessages(model_importance(
      forecast_data = fake_fdat,
      oracle_output_data = fake_target_data,
      ensemble_fun = "simple_ensemble",
      importance_algorithm = "lomo",
      subset_wt = "equal",
      agg_fun = "mean",
      min_log_score = -10
    )),
    regexp = NA
  )
})

# Test: LASOMO algorithm runs within a reasonable time with a larger number of
# models
test_that("lasomo runs with a large number of models and tasks", {
  testthat::skip_on_cran()

  # Create a larger dataset with more models and tasks
  n_models <- 10
  n_tasks <- 10
  fake_models <- paste0("model_", seq_len(n_models))
  fake_fdat <- data_list[[1]] |>
    slice(1) |>
    select(-model_id, -value) |>
    slice(rep(seq_len(n()), each = n_tasks)) |>
    mutate(target_end_date = as.Date("2022-11-26") + (row_number() - 1)) |>
    slice(rep(seq_len(n()), each = n_models)) |>
    mutate(
      model_id = rep(fake_models, times = n() / n_models),
      value = rnorm(n(), mean = 500, sd = 100)
    )
  fake_target_data <- target_data |>
    filter(location == "25", output_type == "mean") |>
    slice(1) |>
    slice(rep(seq_len(n()), each = n_tasks)) |>
    mutate(
      target_end_date = as.Date("2022-11-26") + (row_number() - 1),
      oracle_value = oracle_value + rnorm(n(), mean = 100, sd = 50)
    )
  # test: the function runs without error
  expect_error(
    suppressMessages(model_importance(
      forecast_data = fake_fdat,
      oracle_output_data = fake_target_data,
      ensemble_fun = "simple_ensemble",
      importance_algorithm = "lasomo",
      subset_wt = "equal",
      agg_fun = "mean",
      min_log_score = -10
    )),
    regexp = NA
  )
})
