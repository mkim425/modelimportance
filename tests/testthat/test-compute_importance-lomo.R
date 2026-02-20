# Unit tests for `compute_importance()` with LOMO algorithm

library(dplyr)
library(hubEnsembles)
library(hubEvals)
library(purrr)

# forecast data list
file_names <- c(
  dat_mean = "dat_mean.rds",
  dat_median = "dat_median.rds",
  dat_quantile = "dat_qntl.rds",
  dat_pmf = "dat_pmf.rds"
)
data_list <- map(
  file_names,
  ~ readRDS(testthat::test_path("testdata/for-compute_importance", .x))
)
# target data list
target_file_names <- c(
  target_mean = "target_mean.rds",
  target_median = "target_median.rds",
  target_quantile = "target_qntl.rds",
  target_pmf = "target_pmf.rds"
)
target_data_list <- map(
  target_file_names,
  ~ readRDS(testthat::test_path("testdata/for-compute_importance", .x))
)
# list of expected values for testing
exp_file_names <- c(
  exp_imp_mean_lomo = "exp_imp_mean_lomo.rds",
  exp_imp_median_lomo = "exp_imp_median_lomo.rds",
  exp_imp_quantile_lomo = "exp_imp_qntl_lomo.rds",
  exp_imp_pmf_lomo = "exp_imp_pmf_lomo.rds"
)
exp_imp_list <- map(
  exp_file_names,
  ~ readRDS(testthat::test_path("testdata/for-compute_importance", .x))
)

# combination of arguments
output_type <- c("mean", "median", "quantile", "pmf")
agg_fun <- c("mean", "median")

params <- expand.grid(
  output_type = output_type,
  ens_fun = "simple_ensemble",
  agg_fun = agg_fun,
  stringsAsFactors = FALSE
) |>
  rbind(data.frame(
    output_type = output_type,
    ens_fun = "linear_pool",
    agg_fun = NA
  )) |>
  mutate(
    metric = case_when(
      output_type == "mean" ~ "se_point",
      output_type == "median" ~ "ae_point",
      output_type == "quantile" ~ "wis",
      output_type == "pmf" ~ "log_score"
    )
  ) |>
  filter(!(output_type == "median" & ens_fun == "linear_pool")) |>
  arrange(output_type, ens_fun)

## Test: compute_importance function works properly when ensemble function is
## simple_ensemble and aggregation function is mean or median.
pmap(
  params,
  function(output_type, ens_fun, agg_fun, algorithm, metric) {
    test_that(
      paste(
        "Testing if the function works properly with output type:",
        output_type,
        "ensemble function:",
        ens_fun,
        "aggregation function:",
        agg_fun,
        "metric:",
        metric
      ),
      {
        # get the data corresponding to the arguments
        selected_data <- data_list[[paste0("dat_", output_type)]]
        selected_target_data <- target_data_list[[paste0(
          "target_",
          output_type
        )]]
        selected_expected_importance <- exp_imp_list[[paste0(
          "exp_imp_",
          output_type,
          "_lomo"
        )]]
        if (ens_fun != "linear_pool") {
          # calculate importance scores with the given arguments
          calculated <- compute_importance(
            single_task_data = selected_data,
            oracle_output_data = selected_target_data,
            model_id_list = unique(selected_data$model_id),
            ensemble_fun = ens_fun,
            importance_algorithm = "lomo",
            subset_wt = "equal",
            metric = metric,
            agg_fun = agg_fun,
            min_log_score = -10
          ) |>
            dplyr::select(model_id, importance) |>
            as.data.frame()
        } else {
          calculated <- compute_importance(
            single_task_data = selected_data,
            oracle_output_data = selected_target_data,
            model_id_list = unique(selected_data$model_id),
            ensemble_fun = ens_fun,
            importance_algorithm = "lomo",
            subset_wt = "equal",
            metric = metric,
            min_log_score = -10
          ) |>
            dplyr::select(model_id, importance) |>
            as.data.frame()
        }
        # expected values
        expected_value <- selected_expected_importance |>
          filter(
            ens_mthd == paste0(ens_fun, "-", agg_fun),
            test_purp == "properly assigned"
          ) |>
          dplyr::select(model_id, importance) |>
          as.data.frame()
        # test: compare the calculated importance with the expected importance,
        # ignoring their attributes
        expect_equal(
          calculated,
          expected_value,
          tolerance = 1e-3,
          ignore_attr = TRUE
        )
      }
    )
  }
)


## Test: check if the function assigns NAs for missing data
## in the set-up of simple mean ensemble.
reduced_params <- filter(
  params,
  ens_fun == "simple_ensemble",
  agg_fun == "mean"
) |>
  dplyr::select(output_type, metric)

pmap(reduced_params, function(output_type, metric) {
  test_that(
    paste(
      "Assign NAs for missing data with output type:",
      output_type,
      "metric:",
      metric
    ),
    {
      # get the data corresponding to the arguments
      selected_data <- data_list[[paste0("dat_", output_type)]]
      selected_target_data <- target_data_list[[paste0(
        "target_",
        output_type
      )]]
      selected_expected_importance <- exp_imp_list[[paste0(
        "exp_imp_",
        output_type,
        "_lomo"
      )]]
      model_id_list <- unique(selected_data$model_id)
      sub_dat <- selected_data |> filter(model_id %in% model_id_list[c(1, 3)])
      # calculate importance scores with mean output and simple mean ensemble
      calculated <- compute_importance(
        single_task_data = sub_dat,
        oracle_output_data = selected_target_data,
        model_id_list = unique(selected_data$model_id),
        ensemble_fun = "simple_ensemble",
        importance_algorithm = "lomo",
        subset_wt = "equal",
        metric = metric,
        min_log_score = -10
      ) |>
        dplyr::select(model_id, importance) |>
        as.data.frame()
      # expected values
      expected_value <- selected_expected_importance |>
        filter(
          ens_mthd == "simple_mean",
          test_purp == "missing data"
        ) |>
        dplyr::select(model_id, importance) |>
        as.data.frame()
      # Remove the metrics attribute
      attr(expected_value, "metrics") <- NULL
      # test: compare the calculated importance with the expected importance,
      # ignoring their attributes
      expect_equal(
        calculated,
        expected_value,
        tolerance = 1e-3,
        ignore_attr = TRUE
      )
    }
  )
})
