#' @title Calculate model importance for a single task when using an untrained
#' ensemble
#' @description
#' Evaluate the importance of ensemble component models by quantifying their
#' contribution to the prediction accuracy of an untrained ensemble for each
#' combination of model task.
#'
#' @inheritParams model_importance
#' @param single_task_data A data.frame with the predictions for a single
#' forecast task in a model_out_tbl format. The data must contain only one
#' `output_type`, which must be one of the following:
#' `mean`, `median`, `quantile`, or `pmf`.
#' @param model_id_list A list of all component model IDs to be used in the
#' ensemble. If a model is not present in the `single_task_data`, it means that
#' the model did not submit predictions for the given task.
#' This list is used to identify missing models in the ensemble.
#' @param metric A character string specifying the metric to be used for scoring
#' the model output. The metric is determined by the `output_type` and must be
#' one of the following: `se_point`, `ae_point`, `wis`, or `log_score`.
#'
#' @returns A data.frame with columns
#' `task_id`, `output_type`, `model_id`, `task_level_importance`.
#'
#' @import hubEnsembles
#' @import hubEvals
#' @inherit model_importance details

score_untrained <- function(single_task_data, oracle_output_data, model_id_list,
                            ensemble_fun, importance_algorithm, subset_wt,
                            metric, ...) {
  # models in the single_task_data
  models <- unique(single_task_data$model_id)
  missing_model <- setdiff(model_id_list, single_task_data$model_id)
  # Compute importance score when importance_algorithm is 'lomo'
  if (importance_algorithm == "lomo") {
    # `ens_fun` is a function from hubEnsembles specified by `ensemble_fun`
    ens_fun <- getFromNamespace(ensemble_fun, ns = asNamespace("hubEnsembles"))
    ens_all <- ens_fun(single_task_data,
      weights = NULL,
      model_id = "ensemble-all"
    )

    # build ensemble forecasts by leaving one model out
    ens_lomo <- lapply(models, function(x) {
      single_task_data |>
        dplyr::filter(.data$model_id != x) |>
        ens_fun(
          weights = NULL,
          model_id = paste0("ens.wo.", x)
        )
    })
    ensemble_data <- rbind(ens_all, dplyr::bind_rows(ens_lomo))
    # score the ensemble forecasts
    score_ens_all <- score_model_out(ensemble_data,
      oracle_output_data,
      metrics = metric
    ) |>
      rename(calculated_metric = any_of(
        c("ae_point", "se_point", "wis", "log_score")
      )) |>
      left_join(ensemble_data, by = "model_id")
    # calculate importance scores
    ensemble_all_value <- score_ens_all |>
      dplyr::filter(.data$model_id == "ensemble-all") |>
      dplyr::pull(.data$calculated_metric)
    df_importance <- score_ens_all |>
      dplyr::mutate(
        importance = .data$calculated_metric - ensemble_all_value
      ) |>
      dplyr::filter(.data$model_id != "ensemble-all") |>
      dplyr::mutate(model_id = gsub("ens.wo.", "", .data$model_id)) |>
      dplyr::select(-.data$calculated_metric)
  } else {
    df_importance <- NULL
  }
  # Insert NAs for missing models
  if (length(missing_model) > 0) {
    fixed_cols <- df_importance |>
      select(-c(.data$model_id, .data$value, .data$importance)) |>
      distinct()
    missing_model_rows <- data.frame(
      model_id = missing_model, fixed_cols,
      value = NA, importance = NA
    )
    # bind the new rows to df_importance
    importance_scores <- bind_rows(df_importance, missing_model_rows)
  } else {
    importance_scores <- df_importance
  }
  importance_scores
}
