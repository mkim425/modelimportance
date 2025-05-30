# Unit tests for `score_untrained()` with LASOMO algorithm

library(dplyr)
library(hubEnsembles)
library(hubEvals)
library(purrr)
library(furrr)
library(future)

# forecast data list
file_names <- c(
  dat_mean = "dat_mean.rds",
  dat_median = "dat_median.rds",
  dat_quantile = "dat_qntl.rds"
)
data_list <- map(file_names, ~ readRDS(testthat::test_path("testdata", .x)))
# target data list
target_file_names <- c(
  target_mean = "target_mean.rds",
  target_median = "target_median.rds",
  target_quantile = "target_qntl.rds"
)
target_data_list <- map(
  target_file_names,
  ~ readRDS(testthat::test_path("testdata", .x))
)
# list of expected values for testing
exp_file_names <- c(
  exp_imp_mean_lasomo = "exp_imp_mean_untrained_lasomo.rds",
  exp_imp_median_lasomo = "exp_imp_median_untrained_lasomo.rds",
  exp_imp_quantile_lasomo = "exp_imp_qntl_untrained_lasomo.rds"
)
exp_imp_list <- map(
  exp_file_names,
  ~ readRDS(testthat::test_path("testdata", .x))
)

# combination of arguments
output_type <- c("mean", "median", "quantile")
agg_fun <- c("mean", "median")
subset_weight <- c("equal", "perm_based")

params <- expand.grid(
  output_type = output_type,
  ens_fun = "simple_ensemble",
  agg_fun = agg_fun,
  subset_weight = subset_weight,
  stringsAsFactors = FALSE
) |>
  rbind(expand.grid(
    output_type = output_type,
    ens_fun = "linear_pool",
    agg_fun = NA,
    subset_weight = subset_weight
  )) |>
  mutate(metric = case_when(
    output_type == "mean" ~ "se_point",
    output_type == "median" ~ "ae_point",
    output_type == "quantile" ~ "wis",
    output_type == "pmf" ~ "log_score"
  )) |>
  filter(!(output_type == "median" & ens_fun == "linear_pool")) |>
  arrange(output_type, ens_fun, agg_fun, subset_weight)

## Test: score_untrained function works properly when ensemble function is
## simple_ensemble and aggregation function is mean or median.
pmap(
  params,
  function(output_type, ens_fun, agg_fun, subset_weight, metric) {
    test_that(paste(
      "Testing if the function works properly with output type:", output_type,
      "/ensemble function:", ens_fun,
      "/aggregation function:", agg_fun,
      "/subset_weight:", subset_weight,
      "/metric:", metric
    ), {
      # get the data corresponding to the arguments
      selected_data <- data_list[[paste0("dat_", output_type)]]
      selected_target_data <- target_data_list[[paste0(
        "target_", output_type
      )]]
      selected_expected_importance <- exp_imp_list[[paste0(
        "exp_imp_", output_type, "_lasomo"
      )]]
      if (ens_fun != "linear_pool") {
        # calculate importance scores with the given arguments
        calculated <- score_untrained(
          single_task_data = selected_data,
          oracle_output_data = selected_target_data,
          model_id_list = unique(selected_data$model_id),
          ensemble_fun = ens_fun,
          importance_algorithm = "lasomo",
          subset_wt = subset_weight,
          metric = metric,
          agg_fun = agg_fun
        ) |>
          dplyr::select(model_id, importance) |>
          as.data.frame()
      } else {
        calculated <- score_untrained(
          single_task_data = selected_data,
          oracle_output_data = selected_target_data,
          model_id_list = unique(selected_data$model_id),
          ensemble_fun = ens_fun,
          importance_algorithm = "lasomo",
          subset_wt = subset_weight,
          metric = metric
        ) |>
          dplyr::select(model_id, importance) |>
          as.data.frame()
      }
      # expected values
      expected_value <- selected_expected_importance |>
        filter(
          ens_mthd == paste0(ens_fun, "-", agg_fun),
          subset_wt == subset_weight,
          test_purp == "properly assigned"
        ) |>
        dplyr::select(model_id, importance) |>
        as.data.frame()
      # test: compare the calculated importance with the expected importance,
      # ignoring their attributes
      expect_equal(calculated, expected_value,
        tolerance = 1e-1, ignore_attr = TRUE
      )
    })
  }
)


## Test: check if the function assigns NAs for missing data
## in the set-up of simple mean ensemble.
reduced_params <- filter(
  params,
  ens_fun == "simple_ensemble", agg_fun == "mean"
) |>
  dplyr::select(output_type, subset_weight, metric)

pmap(reduced_params, function(output_type, subset_weight, metric) {
  test_that(paste(
    "Assign NAs for missing data with output type:", output_type,
    "/subset_weight:", subset_weight,
    "/metric:", metric
  ), {
    # get the data corresponding to the arguments
    selected_data <- data_list[[paste0("dat_", output_type)]]
    selected_target_data <- target_data_list[[paste0(
      "target_", output_type
    )]]
    selected_expected_importance <- exp_imp_list[[paste0(
      "exp_imp_", output_type, "_lasomo"
    )]]
    model_id_list <- unique(selected_data$model_id)
    if (output_type != "quantile") {
      sub_dat <- selected_data |> filter(model_id %in% model_id_list[1:3])
    } else {
      sub_dat <- selected_data |> filter(model_id %in% model_id_list[c(1, 3)])
    }
    # calculate importance scores with mean output and simple mean ensemble
    calculated <- score_untrained(
      single_task_data = sub_dat,
      oracle_output_data = selected_target_data,
      model_id_list = unique(selected_data$model_id),
      ensemble_fun = "simple_ensemble",
      importance_algorithm = "lasomo",
      subset_wt = subset_weight,
      metric = metric
    ) |>
      dplyr::select(model_id, importance) |>
      arrange(model_id) |>
      as.data.frame()
    # expected values
    expected_value <- selected_expected_importance |>
      filter(
        ens_mthd == "simple_ensemble-mean",
        subset_wt == subset_weight,
        test_purp == "missing data"
      ) |>
      dplyr::select(model_id, importance) |>
      arrange(model_id) |>
      as.data.frame()
    # Remove the metrics attribute
    attr(expected_value, "metrics") <- NULL
    # test: compare the calculated importance with the expected importance,
    # ignoring their attributes
    expect_equal(calculated, expected_value,
      tolerance = 1e-1, ignore_attr = TRUE
    )
  })
})
