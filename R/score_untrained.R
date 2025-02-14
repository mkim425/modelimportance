#' @title Calculate model importance for a single task when using an untrained
#' ensemble
#' @description
#' Evaluate the importance of ensemble component models by quantifying their
#' contribution to the prediction accuracy of an untrained ensemble for each
#' combination of model task.
#'
#' @param single_task_data A data.frame with the predictions for a single
#' forecast task in a model_out_tbl format. The data must contain only one
#' `output_type`, which must be one of the following:
#' `mean`, `median`, `quantile`, or `pmf`.
#' @param oracle_output_data Ground truth data for the variables that are used
#' to define modeling targets. This data must follow the oracle output format.
#' @param model_id_list A list of all component model IDs to be used in the
#' ensemble. If a model is not present in the `single_task_data`, it means that
#' the model did not submit predictions for the given task.
#' This list is used to identify missing models in the ensemble.
#' @param ensemble_fun A character string specifying a ensemble method, either
#' "simple_ensemble" or "linear_pool"; `c("simple_ensemble", "linear_pool")`.
#' * When `"simple_ensemble"` is specified, the ensemble is generated using the
#' optional `agg_fun` function in `...` (see 'Details'). It takes into account
#' the weight option specified in `weighted`.
#' * When `"linear_pool"` is specified, ensemble model outputs are created as
#' a linear pool of component model outputs. This method supports only
#' an `output_type` of `mean`, `quantile`, or `pmf`.
#' @param importance_algorithm A character string specifying algorithm for model
#' importance calculation; `c("lomo", "lasomo")`.
#' `"lomo"` stands for leave-one-model-out and
#' `"lasomo"` stands for leave all subsets of models out.
#' @param subset_wt A character string specifying method for assigning weight
#' to subsets when using `lasomo` algorithm; `c("equal", "perm_based")`.
#' @param metric A character string specifying the metric to be used for scoring
#' the model output. The metric is determined by the `output_type` and must be
#' one of the following: `se_point`, `ae_point`, `wis`, or `log_score`.
#' @param ... Optional arguments passed to `ensemble_fun` when it is specified
#' as `"simple_ensemble"`. See 'Details'.
#'
#' @returns A data.frame with columns
#' `task_id`, `output_type`, `model_id`, `task_level_importance`.
#'
#' @import hubEnsembles
#' @import hubEvals
#' @details
#' Additional argument in `...` is `agg_fun`, which is a character string name
#' for a function specifying aggregation method of component model outputs.
#' Default is `mean`, meaning that equally (or weighted) mean is calculated
#' across all component model outputs for each unique `output_type_id`.
#' This can be `median` or a custom function (e.g., geometric_mean. Details
#' can be found in
#' https://hubverse-org.github.io/hubEnsembles/articles/hubEnsembles.html)
#'
#' @examples \dontrun{
#' library(dplyr)
#' library(hubEnsembles)
#' library(hubEvals)
#' forecast_data <- hubExamples::forecast_outputs |>
#'   dplyr::filter(
#'     output_type %in% c("mean"),
#'     location == "25",
#'     horizon == 1
#'   )
#' target_data <- hubExamples::forecast_target_ts |>
#'   dplyr::filter(
#'     date %in% unique(forecast_data$target_end_date),
#'     location == "25"
#'   ) |>
#'   # Rename columns to match the oracle output format
#'   rename(
#'     target_end_date = date,
#'     oracle_value = observation
#'   )
#'
#' valid_tbl <- validate_input_data(forecast_means, target_data)
#' all_models <- unique(valid_tbl$model_id)
#' list_datasets <- split_data_by_task(
#'   valid_tbl,
#'   weighted = FALSE,
#'   training_window_length = 0
#' )
#'
#' # Example with the default arguments.
#' score_untrained(
#'   single_task_data = list_datasets[[1]], oracle_output_data = target_data,
#'   model_id_list = all_models, ensemble_fun = "simple_ensemble",
#'   importance_algorithm = "lomo", subset_wt = "equal", metric = "se_point"
#' )
#' # Example with the additional argument in `...`.
#' score_untrained(
#'   single_task_data = list_datasets[[1]], oracle_output_data = target_data,
#'   model_id_list = all_models, ensemble_fun = "simple_ensemble",
#'   importance_algorithm = "lomo", subset_wt = "equal", metric = "se_point",
#'   agg_fun = median
#' )
#' }
score_untrained <- function(single_task_data, oracle_output_data, model_id_list,
                            ensemble_fun, importance_algorithm, subset_wt,
                            metric, ...) {
  # models in the single_task_data
  models <- unique(single_task_data$model_id)
  missing_model <- setdiff(model_id_list, single_task_data$model_id)
  # Compute importance score when importance_algorithm is 'lomo'
  if (importance_algorithm == "lomo") {
    ens_all <- switch(ensemble_fun,
      "simple_ensemble" = hubEnsembles::simple_ensemble(single_task_data,
        weights = NULL,
        model_id = "enseble-all"
      ),
      "linear_pool" = hubEnsembles::linear_pool(single_task_data,
        weights = NULL,
        model_id = "enseble-all"
      )
    )

    # build ensemble forecasts by leaving one model out
    ens_lomo <- lapply(models, function(x) {
      single_task_data_lomo <- single_task_data |>
        dplyr::filter(.data$model_id != x)
      switch(ensemble_fun,
        "simple_ensemble" = hubEnsembles::simple_ensemble(single_task_data_lomo,
          weights = NULL,
          model_id = paste0("ens.wo.", x)
        ),
        "linear_pool" = hubEnsembles::linear_pool(single_task_data_lomo,
          weights = NULL,
          model_id = paste0("ens.wo.", x)
        )
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
    df_importance <- score_ens_all |>
      dplyr::mutate(
        importance = .data$calculated_metric - first(.data$calculated_metric)
      ) |>
      dplyr::filter(.data$model_id != "enseble-all") |>
      dplyr::mutate(model_id = gsub("ens.wo.", "", .data$model_id)) |>
      dplyr::select(-.data$calculated_metric)
  } else {
    df_importance <- NULL
  }
  # Insert NAs for missing models
  if (length(missing_model) > 0) {
    # copy the first row of df_importance
    new_rows <- df_importance[1, ]
    # replicate the 1st row for the number of missing models
    new_rows <- new_rows[rep(1, length(missing_model)), ]
    # assign missing models to model_id
    new_rows$model_id <- missing_model
    # NA for all numeric columns, including 'importance'
    new_rows <- new_rows |>
      dplyr::mutate_if(is.numeric, ~NA)
    # bind the new rows to df_importance
    importance_scores <- bind_rows(df_importance, new_rows)
  } else {
    importance_scores <- df_importance
  }
  importance_scores
}
