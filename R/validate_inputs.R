#' validate inputs
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
#' an `output_type` of `mean`, `quantile`, `cdf`, or `pmf`.
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
#' importance; `c("MAE", "MSE", "WIS", "CRPS", "Logscore")`. Specify one of them
#' depending on which is available for the output type in the input data.
#' @param na_action A character string specifying treatment for missing data;
#' `c("worst," "average," "drop").` `"worst"` replaces missing values with
#' the smallest value from the other models. `"average"` replaces
#' missing values with the average value from the other models.
#' `"drop"` removes missing values.
#'
#' @noRd
validate_inputs <- function(forecast_data, target_data, ensemble_fun, agg_fun,
                            weighted, training_window_length,
                            importance_algorithm, subset_wt, scoring_rule,
                            na_action) {
  # validate inputs
  if (!is.data.frame(forecast_data)) {
    stop("Invalid input: 'forecast_data' must be a data frame.")
  }
  if (!is.data.frame(target_data)) {
    stop("Invalid input: 'target_data' must be a data frame.")
  }
  if (!is.logical(weighted)) {
    stop("Invalid value for 'weighted'. It must be either TRUE or FALSE.
         Default is FALSE.")
  }
  if (!is.integer(training_window_length)) {
    stop("Invalid value for 'training_window_length'. It must be an integer.
         Default is 0.")
  }
  if (!is.function(agg_fun)) {
    stop("Invalid input for 'agg_fun' must be one of 'mean', 'median' or
         a custom function.")
  }
  ensemble_fun <- match.arg(ensemble_fun)
  importance_algorithm <- match.arg(importance_algorithm)
  subset_wt <- match.arg(subset_wt)
  scoring_rule <- match.arg(scoring_rule)
  na_action <- match.arg(na_action)
}
