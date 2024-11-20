#' @title Quantify ensemble component model contributions to ensemble
#' prediction accuracy
#' @description
#' Evaluate ensemble component model's importance based on a measure of their
#' contribution to ensemble prediction accuracy for each combination of
#' model task.
#'
#' @param forecast_data A data.frame with the predictions that is or can be
#' coerced to a model_out_tbl format.
#' @param target_data Ground truth data for the variables that are used to
#' define modeling targets. This data must follow the oracle output format.
#' See 'Details'.
#' @param ensemble_fun A character string specifying a ensemble method, either
#' "simple_ensemble" or "linear_pool"; `c("simple_ensemble", "linear_pool")`.
#' * When `"simple_ensemble"` is specified, the ensemble is generated using the
#' optional `agg_fun` function in `...` (see 'Details'). It takes into account
#' the weight option specified in `weighted`.
#' * When `"linear_pool"` is specified, ensemble model outputs are created as
#' a linear pool of component model outputs. This method supports only
#' an `output_type` of `mean`, `quantile`, or `pmf`.
#' @param weighted Boolean indicating whether model weighting should be done.
#' If `FALSE`, all models are given equal weight.
#' If `TRUE`, model weights are estimated.
#' @param training_window_length An integer value representing the time interval
#' of historical data used during the training process
#' to estimate model weights.
#' Default is `0`, meaning that no prior data is available for training.
#' @param importance_algorithm A character string specifying algorithm for model
#' importance calculation; `c("lomo", "lasomo")`.
#' `"lomo"` stands for leave-one-model-out and
#' `"lasomo"` stands for leave all subsets of models out.
#' @param subset_wt A character string specifying method for assigning weight
#' to subsets when using `lasomo` algorithm; `c("equal", "perm_based")`.
#' @param scoring_rule A character string specifying metric to use to calculate
#' importance; `c("ae_point", "se_point", "wis", "logscore")`.
#' Specify one of them depending on which is available for the output type in
#' the input data.
#' @param na_action A character string specifying treatment for missing data;
#' `c("worst," "average," "drop").` `"worst"` replaces missing values with
#' the smallest value from the other models. `"average"` replaces
#' missing values with the average value from the other models.
#' `"drop"` removes missing values.
#' @param ... Optional arguments passed to `ensemble_fun` when it is specified
#' as `"simple_ensemble"`. See 'Details'.
#' @return A data.frame with columns
#' `task_id`, `output_type`, `model`, `importance_score`.
#' @import hubExamples
#' @export
#' @details
#' The `target_data` in the oracle output format should contain independent
#' task ID columns (e.g. `location`, `target_date`, and `age_group`),
#' `output_type` and `output_type_id` columns if the output is either `pmf` or
#' `cdf`, and `oracle_value` column for the observed values.
#' TBD for more details.
#'
#' Additional argument in ... is `agg_fun`, which is a character string name
#' for a function specifying aggregation method of component model outputs.
#' Default is `mean`, meaning that equally (or weighted) mean is calculated
#' across all component model outputs for each unique `output_type_id`.
#' This can be `median` or a custom function (e.g., geometric_mean. Details
#' can be found in
#' https://hubverse-org.github.io/hubEnsembles/articles/hubEnsembles.html)
#' @examples \dontrun{
#' library(dplyr)
#' library(hubExamples)
#' forecast_data <- hubExamples::forecast_outputs |>
#'   dplyr::filter(
#'     output_type %in% c("quantile"),
#'     location == "25",
#'     horizon == 1
#'   )
#' target_data <- hubExamples::forecast_target_ts |>
#'   dplyr::filter(
#'     date %in% unique(forecast_data$reference_date),
#'     location == "25"
#'   )
#'
#' model_importance(
#'   forecast_data = forecast_data, target_data = target_data,
#'   ensemble_fun = "simple_ensemble", weighted = FALSE,
#'   training_window_length = 0, importance_algorithm = "lomo",
#'   subset_wt = "equal", scoring_rule = "wis", na_action = "drop"
#' )
#' # Example with the additional argument in `...`.
#' model_importance(
#'   forecast_data = forecast_data, target_data = target_data,
#'   ensemble_fun = "simple_ensemble", weighted = FALSE,
#'   training_window_length = 0, importance_algorithm = "lomo",
#'   subset_wt = "equal", scoring_rule = "wis", na_action = "drop",
#'   agg_fun = median
#' )
#' }
model_importance <- function(forecast_data,
                             target_data,
                             ensemble_fun = c("simple_ensemble", "linear_pool"),
                             weighted = FALSE,
                             training_window_length = 0,
                             importance_algorithm = c("lomo", "lasomo"),
                             subset_wt = c("equal", "perm_based"),
                             scoring_rule = c(
                               "ae_point", "se_point", "wis", "logscore"
                             ),
                             na_action = c("worst", "average", "drop"),
                             ...) {
  # validate inputs
  validate_inputs(
    forecast_data, target_data, ensemble_fun, weighted, training_window_length,
    importance_algorithm, subset_wt, scoring_rule, na_action
  )

  # check if forecast_data contains exactly one of the columns:
  # "forecast_date", "origin_date", "reference_date"
  validate_one_forecast_date_col(forecast_data)

  # validate input data: get a model_out_tbl format with a single output type
  # and combine two datasets
  valid_tbl <- validate_input_data(forecast_data, target_data)

  # validate that the selected metric is suitable for each output_type
  unique_output_type <- unique(valid_tbl$output_type)
  check_metric_selection(unique_output_type, scoring_rule)

  # forecast_dates
  forecast_date_list <- valid_tbl |>
    dplyr::select(
      dplyr::any_of(
        c("forecast_date", "origin_date", "reference_date")
      )
    ) |>
    pull() |>
    unique()

  # Give a message for the user to check the forecast dates
  message(sprintf(
    "The input data has forecast from %s to %s.
    There are a total of %d forecast dates.",
    min(forecast_date_list), max(forecast_date_list), length(forecast_date_list)
  ))

  score_result <- forecast_data
  return(score_result)
}
