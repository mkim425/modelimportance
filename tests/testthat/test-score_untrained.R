library(dplyr)
library(hubEnsembles)
library(hubEvals)
library(purrr)

# forecast data list
data_list <- list(
  dat_mean = readRDS(testthat::test_path("testdata/dat_mean.rds")),
  dat_median = readRDS(testthat::test_path("testdata/dat_median.rds"))
)
# target data list
target_data_list <- list(
  target_mean = readRDS(testthat::test_path("testdata/target_dat_mean.rds")),
  target_median = readRDS(testthat::test_path("testdata/target_dat_median.rds"))
)
# list of expected values for testing
exp_imp_list <- list(
  exp_imp_mean = readRDS(testthat::test_path("testdata/exp_imp_mean.rds")),
  exp_imp_median = readRDS(testthat::test_path("testdata/exp_imp_median.rds"))
)

# combination of arguments
output_type <- c("mean", "median")
agg_fun <- c("mean", "median")
algorithm <- c("lomo")

params <- expand.grid(
  output_type = output_type,
  ens_fun = "simple_ensemble",
  agg_fun = agg_fun,
  algorithm = algorithm,
  stringsAsFactors = FALSE
) |>
  rbind(data.frame(
    output_type = output_type,
    ens_fun = "linear_pool",
    agg_fun = NA,
    algorithm = algorithm
  )) |>
  mutate(metric = case_when(
    output_type == "mean" ~ "se_point",
    output_type == "median" ~ "ae_point",
    output_type == "quantile" ~ "wis",
    output_type == "pmf" ~ "log_score"
  )) |>
  filter(!(output_type == "median" & ens_fun == "linear_pool")) |>
  arrange(output_type, ens_fun)

## Test: score_untrained function works properly
pmap(params, function(output_type, ens_fun, agg_fun, algorithm, metric) {
  test_that(paste(
    "Testing if the function works properly with output type:", output_type,
    "ensemble function:", ens_fun,
    "aggregation function:", agg_fun,
    "importance algorithm:", algorithm,
    "metric:", metric
  ), {
    # get the data corresponding to the arguments
    selected_data <- data_list[[paste0("dat_", output_type)]]
    selected_target_data <- target_data_list[[paste0(
      "target_", output_type
    )]]
    selected_expected_importance <- exp_imp_list[[paste0(
      "exp_imp_", output_type
    )]]
    # calculate importance scores with the given arguments
    calculated <- score_untrained(
      single_task_data = selected_data,
      oracle_output_data = selected_target_data,
      model_id_list = unique(selected_data$model_id),
      ensemble_fun = ens_fun,
      importance_algorithm = algorithm,
      subset_wt = "equal",
      metric = metric,
      agg_fun = agg_fun
    ) |>
      dplyr::select(model_id, importance) |>
      as.data.frame()
    # remove the metrics attribute from the calculated importance
    attr(calculated, "metrics") <- NULL
    # expected values
    expected_value <- selected_expected_importance |>
      filter(
        ens_mthd == paste0(ens_fun, "-", agg_fun),
        algorithm == algorithm,
        test_purp == "properly assigned"
      ) |>
      dplyr::select(model_id, importance)
    # test: compare the calculated importance with the expected importance
    expect_equal(calculated, expected_value)
  })
})

## Test: check if the function assigns NAs for missing data
## in the set-up of simple mean ensemble.
reduced_params <- filter(
  params,
  ens_fun == "simple_ensemble", agg_fun == "mean"
) |>
  dplyr::select(output_type, algorithm, metric)

pmap(reduced_params, function(output_type, algorithm, metric) {
  test_that(paste(
    "Assign NAs for missing data with output type:", output_type,
    "metric:", metric
  ), {
    # get the data corresponding to the arguments
    selected_data <- data_list[[paste0("dat_", output_type)]]
    selected_target_data <- target_data_list[[paste0(
      "target_", output_type
    )]]
    selected_expected_importance <- exp_imp_list[[paste0(
      "exp_imp_", output_type
    )]]
    sub_dat <- selected_data[c(1, 3, 4), ]
    # calculate importance scores with mean output and linear pool
    calculated <- score_untrained(
      single_task_data = sub_dat,
      oracle_output_data = selected_target_data,
      model_id_list = unique(selected_data$model_id),
      ensemble_fun = "simple_ensemble",
      importance_algorithm = algorithm,
      subset_wt = "equal",
      metric = metric
    ) |>
      dplyr::select(model_id, importance) |>
      as.data.frame()
    # remove the metrics attribute from the calculated importance
    attr(calculated, "metrics") <- NULL
    # expected values
    expected_value <- selected_expected_importance |>
      filter(
        ens_mthd == "simple_mean",
        algorithm == algorithm,
        test_purp == "missing data"
      ) |>
      dplyr::select(model_id, importance)
    # test: compare the calculated importance with the expected importance
    expect_equal(calculated, expected_value)
  })
})
