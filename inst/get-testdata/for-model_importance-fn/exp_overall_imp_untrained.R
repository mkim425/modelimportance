## Generate expected overall importance scores for the untrained ensemble models
## output type: mean, quantile, median
#  =============================================================================
# load the package to make its internal functions available
devtools::load_all()
source(system.file(
  "get-testdata/for-model_importance-fn/helper-exp_overall_imp.R",
  package = "modelimportance"
))

target_data <- readRDS(
  testthat::test_path("testdata/target_data_all_outputs.rds")
)

output_types <- c("mean", "quantile", "median") # , "pmf")
measures <- c("se_point", "wis", "ae_point") # , "log_score")

# loop over the output types and corresponding measures
for (k in seq_along(output_types)) {
  f_data <- readRDS(
    testthat::test_path("testdata/forecast_data_all_outputs.rds")
  ) |> dplyr::filter(output_type == output_types[k])

  model_ids <- unique(f_data$model_id)
  # split the data by task (total: 4 tasks)
  df_list_by_task <- split_data_by_task(
    valid_tbl = f_data, weighted = FALSE, training_window_length = 0
  )

  # Untrained simple ensemble  =================================================
  # loop over the parameter combinations and tasks
  result_simple <- vector("list", nrow(params_simple))
  for (i in seq_len(nrow(params_simple))) {
    imp_alg <- params_simple$imp_alg[i]
    subset_weight <- params_simple$subset_weight[i]
    agg_fun <- params_simple$agg_fun[i]
    # calculate importance scores with the given arguments
    score_result <- furrr::future_map_dfr(
      df_list_by_task,
      function(single_task_df) {
        res <- score_untrained(
          single_task_data = single_task_df,
          oracle_output_data = target_data,
          model_id_list = model_ids,
          ensemble_fun = "simple_ensemble",
          importance_algorithm = imp_alg,
          subset_wt = subset_weight,
          metric = measures[k],
          agg_fun = agg_fun
        )
        # replace NA values (if any) in 3 ways: "worst", "average", "drop"
        replace_na(res)
      }
    )
    # aggregate the scores over tasks
    result_simple[[i]] <- aggregate_scores(score_result) |>
      # add a column to identify the parameter combination
      mutate(
        ens_mthd = paste0(
          "untrained-simple_ensemble-", agg_fun, "-", imp_alg, "-",
          subset_weight
        )
      )
  }
  df_untrained_simple <- do.call(rbind, result_simple)
  # Untrained linear pool ensemble  ============================================
  if (output_types[k] != "median") {
    # loop over the parameter combinations and tasks
    result_lp <- vector("list", nrow(params_lp))
    for (i in seq_len(nrow(params_lp))) {
      imp_alg <- params_lp$imp_alg[i]
      subset_weight <- params_lp$subset_weight[i]
      # calculate importance scores with the given arguments
      score_result <- furrr::future_map_dfr(
        df_list_by_task,
        function(single_task_df) {
          res <- score_untrained(
            single_task_data = single_task_df,
            oracle_output_data = target_data,
            model_id_list = model_ids,
            ensemble_fun = "linear_pool",
            importance_algorithm = imp_alg,
            subset_wt = subset_weight,
            metric = measures[k]
          )
          # replace NA values (if any) in 3 ways: "worst", "average", "drop"
          replace_na(res)
        }
      )
      # aggregate the scores over tasks
      result_lp[[i]] <- aggregate_scores(score_result) |>
        # add a column to identify the parameter combination
        mutate(
          ens_mthd = paste0(
            "untrained-linear_pool-NA-", imp_alg, "-", subset_weight
          )
        )
    }
    df_untrained_lp <- do.call(rbind, result_lp)
    # combine the results from simple ensemble and linear pool
    final_df <- rbind(df_untrained_simple, df_untrained_lp)
  } else {
    final_df <- df_untrained_simple
  }
  # save the result
  file_name <- paste0(
    "testdata/exp_overall_imp_", output_types[k], "_untrained.rds"
  )
  saveRDS(final_df, file = testthat::test_path(file_name))
}
