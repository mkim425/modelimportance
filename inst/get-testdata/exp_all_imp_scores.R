## Generate expected importance scores and summaries, which will be used in
## tests for model_importance() and model_importance_summary().
## output type: mean, quantile, median
#  =============================================================================
# load the package to make its internal functions available
devtools::load_all()
source(system.file(
  "get-testdata/helper-exp_all_imp_scores.R",
  package = "modelimportance"
))

target_data <- readRDS(
  testthat::test_path(
    "testdata/for-model_importance/target_data_all_outputs.rds"
  )
)

output_types <- c("mean", "quantile", "median", "pmf")
measures <- c("se_point", "wis", "ae_point", "log_score")

# loop over the output types and corresponding measures
for (k in seq_along(output_types)) {
  f_data <- readRDS(
    testthat::test_path(
      "testdata/for-model_importance/forecast_data_all_outputs.rds"
    )
  ) |> dplyr::filter(output_type == output_types[k])

  model_ids <- unique(f_data$model_id)
  # split the data by task (total: 4 tasks)
  df_list_by_task <- split_data_by_task(valid_tbl = f_data)

  # Simple ensemble  =================================================
  # loop over the parameter combinations and tasks
  score_result <- vector("list", nrow(params_simple))
  na_replaced_simple <- vector("list", nrow(params_simple))
  for (i in seq_len(nrow(params_simple))) {
    imp_alg <- params_simple$imp_alg[i]
    subset_weight <- params_simple$subset_weight[i]
    agg_fun <- params_simple$agg_fun[i]
    # calculate importance scores with the given arguments
    score_result[[i]] <- furrr::future_map_dfr(
      df_list_by_task,
      function(single_task_df) {
        res <- compute_importance(
          single_task_data = single_task_df,
          oracle_output_data = target_data,
          model_id_list = model_ids,
          ensemble_fun = "simple_ensemble",
          importance_algorithm = imp_alg,
          subset_wt = subset_weight,
          metric = measures[k],
          agg_fun = agg_fun,
          min_log_score = -10
        )
      }
    ) |>
      # add a column to identify the parameter combination
      mutate(calc_args = paste(
        paste0(output_types[k], "_output"),
        "simple_ensemble", imp_alg, subset_weight, agg_fun,
        sep = "-"
      ))
    # replace NA values (if any) in 3 ways: "worst", "average", "drop"
    na_replaced_simple[[i]] <- score_result[[i]] |>
      group_by(across(all_of(c("horizon", "location", "target_end_date")))) |>
      group_split() |>
      furrr::future_map_dfr(replace_na)
  }
  df_score_results_simple <- do.call(rbind, score_result)
  df_result_imputed_simple <- do.call(rbind, na_replaced_simple)
  # Linear pool ensemble  ============================================
  if (output_types[k] != "median") {
    # loop over the parameter combinations and tasks
    score_result_lp <- vector("list", nrow(params_lp))
    na_replaced_lp <- vector("list", nrow(params_lp))
    for (i in seq_len(nrow(params_lp))) {
      imp_alg <- params_lp$imp_alg[i]
      subset_weight <- params_lp$subset_weight[i]
      # calculate importance scores with the given arguments
      score_result_lp[[i]] <- furrr::future_map_dfr(
        df_list_by_task,
        function(single_task_df) {
          res <- compute_importance(
            single_task_data = single_task_df,
            oracle_output_data = target_data,
            model_id_list = model_ids,
            ensemble_fun = "linear_pool",
            importance_algorithm = imp_alg,
            subset_wt = subset_weight,
            metric = measures[k],
            min_log_score = -10
          )
        }
      ) |>
        # add a column to identify the parameter combination
        mutate(calc_args = paste(
          paste0(output_types[k], "_output"),
          "linear_pool", imp_alg, subset_weight, "NA",
          sep = "-"
        ))
      # replace NA values (if any) in 3 ways: "worst", "average", "drop"
      na_replaced_lp[[i]] <- score_result_lp[[i]] |>
        group_by(across(all_of(c("horizon", "location", "target_end_date")))) |>
        group_split() |>
        furrr::future_map_dfr(replace_na)
    }
    df_score_results_lp <- do.call(rbind, score_result_lp)
    df_result_imputed_lp <- do.call(rbind, na_replaced_lp)
    # combine the results from simple ensemble and linear pool ensemble
    all_df_score_results <- rbind(
      df_score_results_simple, df_score_results_lp
    )
    all_df_result_imputed <- rbind(
      df_result_imputed_simple, df_result_imputed_lp
    )
  } else {
    all_df_score_results <- df_score_results_simple
    all_df_result_imputed <- df_result_imputed_simple
  }
  # save the result
  file_name1 <- paste0(
    "testdata/for-model_importance/exp_raw_imp_scores_", output_types[k], ".rds"
  )
  file_name2 <- paste0(
    "testdata/for-model_importance/exp_imputed_imp_scores_", output_types[k],
    ".rds"
  )
  saveRDS(all_df_score_results, file = testthat::test_path(file_name1))
  saveRDS(all_df_result_imputed, file = testthat::test_path(file_name2))
}
