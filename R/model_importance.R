#' Evaluate ensemble component model's importance based on a measure of their
#' contribution to ensemble prediction accuracy for each combination of
#' model task.
#'
#' @param forecast_data A data.frame with the predictions that is or can be
#' coerced to a model_out_tbl format.
#' @param target_data Ground truth data for the variables that are used to
#' define modeling targets.
#' @param ensemble_fun A character string specifying a ensemble method, either
#' "simple_ensemble" or "linear_pool"; `c("simple_ensemble", "linear_pool")`.
#' * When `"simple_ensemble"` is specified, the ensemble is generated using
#' the function selected in `agg_fun`, and
#' it takes into account the weight option specified in `weighted`.
#' * When `"linear_pool"` is specified, ensemble model outputs are created as
#' a linear pool of component model outputs. This method supports only
#' an `output_type` of `mean`, `quantile`, or `pmf`.
#' @param agg_fun A character string name for a function specifying aggregation
#' method of component model outputs. Default is `mean`, meaning that equally
#' (or weighted) mean is calculated across all component model outputs for each
#' unique `output_type_id`. This can be `median` or a custom function
#' (e.g., geometric_mean. Details can be found in
#' https://hubverse-org.github.io/hubEnsembles/articles/hubEnsembles.html)
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
#'
#' @return A data.frame with columns
#' `task_id`, `output_type`, `model`, `importance_score`.
#' @export
#'
#' @examples
model_importance <- function(forecast_data,
                             target_data,
                             ensemble_fun = c("simple_ensemble", "linear_pool"),
                             agg_fun = mean,
                             weighted = FALSE,
                             training_window_length = 0,
                             importance_algorithm = c("lomo", "lasomo"),
                             subset_wt = c("equal", "perm_based"),
                             scoring_rule = c(
                               "ae_point", "se_point", "wis", "logscore"
                             ),
                             na_action = c("worst", "average", "drop")) {
  # validate inputs
  validate_inputs(
    forecast_data, true_value, ensemble_fun, agg_fun, weighted,
    training_window_length, importance_algorithm, subset_wt,
    scoring_rule, na_action
  )

  # validate input data and get a model_out_tbl format with a single output type
  valid_tbl <- validate_input_data(forecast_data)

  # validate that the selected metric is suitable for each output_type
  output_type <- valid_tbl$output_type |> unique()
  check_metric_selection(output_type, scoring_rule)

  # forecast_dates
  forecast_dates <- valid_tbl |>
    dplyr::select(
      dplyr::any_of(
        c("forecast_date", "origin_date", "reference_date")
      )
    ) |>
    pull() |>
    unique()

  # Give a message for the user to check the forecast dates
  message(
    "The input data has forecast from ", min(forecast_dates),
    " to ", max(forecast_dates), "."
  )

  score_result <- forecast_data
  return(score_result)
}
