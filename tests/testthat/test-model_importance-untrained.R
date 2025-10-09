# Unit tests for `model_importance()` with untrained ensembles

library(dplyr)
library(hubEnsembles)
library(hubEvals)
library(purrr)

# forecast data list
f_all_data <- readRDS(
  testthat::test_path("testdata/forecast_data_all_outputs.rds")
)
data_list <- list(
  dat_mean = f_all_data |> dplyr::filter(output_type == "mean")
)
# target data
target_data <- readRDS(
  testthat::test_path("testdata/target_data_all_outputs.rds")
)

# list of expected values for testing
exp_file_names <- c(
  exp_overall_imp_mean_untrained = "exp_overall_imp_mean_untrained.rds"
)
exp_imp_list <- map(
  exp_file_names,
  ~ readRDS(testthat::test_path("testdata", .x))
)

# combination of arguments
params <- expand.grid(
  output_type = c("mean"),
  ens_fun = c("simple_ensemble", "linear_pool"),
  agg_fun = c("mean", "median"),
  algorithm = c("lomo", "lasomo"),
  subset_weight = c("equal", "perm_based"),
  na_method = c("worst", "average", "drop"),
  stringsAsFactors = FALSE
) |>
  # adjustments: no agg_fun for linear_pool, subset_weight only for lasomo
  mutate(
    agg_fun = ifelse(ens_fun == "linear_pool", "NA", agg_fun),
    subset_weight = ifelse(algorithm == "lomo", "equal", subset_weight)
  ) |>
  # filter(!(output_type == "median" & ens_fun == "linear_pool")) |>
  distinct() |>
  arrange(algorithm, output_type, ens_fun, agg_fun, subset_weight)

## Test: model_importance function works properly, given no missing data
pmap(
  params,
  function(output_type, ens_fun, agg_fun, algorithm, subset_weight, na_method) {
    test_that(paste(
      "Testing if the function works properly with output type:", output_type,
      "/ensemble function:", ens_fun,
      "/aggregation function:", agg_fun,
      "/algorithm:", algorithm,
      "/subset_weight:", subset_weight,
      "/na method:", na_method
    ), {
      # get the data corresponding to the arguments
      selected_data <- data_list[[paste0("dat_", output_type)]]
      selected_expected_importance <- exp_imp_list[[paste0(
        "exp_overall_imp_", output_type, "_untrained"
      )]]
      # calculate importance scores with the given arguments
      if (ens_fun != "linear_pool") {
        calculated <- suppressMessages(model_importance(
          forecast_data = selected_data,
          oracle_output_data = target_data,
          ensemble_fun = ens_fun,
          importance_algorithm = algorithm,
          subset_wt = subset_weight,
          na_action = na_method,
          agg_fun = agg_fun
        ))
      } else {
        calculated <- suppressMessages(model_importance(
          forecast_data = selected_data,
          oracle_output_data = target_data,
          ensemble_fun = ens_fun,
          importance_algorithm = algorithm,
          subset_wt = subset_weight,
          na_action = na_method
        ))
      }
      # expected values
      expected_value <- selected_expected_importance |>
        filter(
          ens_mthd == paste0(
            "untrained-", ens_fun, "-", agg_fun, "-", algorithm, "-",
            subset_weight
          ),
          na_action == na_method
        ) |>
        dplyr::select(model_id, mean_importance) |>
        as.data.frame() |>
        arrange(desc(mean_importance))
      # test: compare the calculated importance with the expected importance,
      # ignoring their attributes
      expect_equal(calculated, expected_value,
        tolerance = 1e-1, ignore_attr = TRUE
      )
    })
  }
)
