#' @title Quantifies the contribution of ensemble component models to ensemble
#' prediction accuracy for each prediction task.
#' @description
#' We measure each ensemble component model's contribution to the ensemble
#' prediction accuracy for each model task.
#'
#' This function requires that one column represent the forecast date (or
#' a date from which each forecast originates or is made in reference to) and
#' that column be named one of `forecast_date`, `origin_date`, and
#' `reference_date`.
#'
#' For each `output_type`, the corresponding scoring rule applied to calculate
#' the importance is as follows.
#'
#' \tabular{lll}{
#'   \strong{Output Type} \tab \strong{Scoring Rule} \tab \strong{Description}
#'   \cr
#'   mean \tab rse_point \tab evaluate using the root squared error \cr
#'   median \tab ae_point\tab evaluate using the absolute error \cr
#'   quantile \tab wis \tab evaluate using the weighted interval score\cr
#'   pmf \tab log_score \tab
#'   {evaluate using the logarithm of the probability assigned to the true
#'   outcome} \cr
#' }
#'
#' @param forecast_data A data.frame with the predictions that is or can be
#' coerced to a `model_out_tbl` format, which is the standard S3 class model
#' output format defined by the 'hubverse' convention
#' (https://docs.hubverse.io/en/latest/#). If it fails to be coerced to a
#' `model_out_tbl` format, an error message will be returned.
#' Only one `output_type` is allowed in the data.frame, and it must be one of
#' the following: `mean`, `median`, `quantile`, or `pmf`.
#' @param oracle_output_data Ground truth data for the variables that are used
#' to define modeling targets. This data must follow the oracle output format.
#' See 'Details'.
#' @param importance_algorithm A character string specifying algorithm for model
#' importance calculation; `c("lomo", "lasomo")`.
#' `"lomo"` stands for leave-one-model-out and
#' `"lasomo"` stands for leave all subsets of models out.
#' For `"lasomo"`, `'furrr'` and `'future'` packages need to be installed for
#' parallel execution.
#' @param subset_wt A character string specifying method for assigning weight
#' to subsets when using `lasomo` algorithm; `c("equal", "perm_based")`.
#' * `"equal"` assigns equal weight to all subsets.
#' * `"perm_based"` assigns weight averaged over all possible permutations as in
#' the Shapley value.
#' Ignored if `lomo` method is used. Default is `"equal"`, if not specified.
#' @param ensemble_fun A character string specifying a ensemble method, either
#' "simple_ensemble" or "linear_pool"; `c("simple_ensemble", "linear_pool")`.
#' * When `"simple_ensemble"` is specified, the ensemble is generated using the
#' optional `agg_fun` function in `...` (see 'Details').
#' * When `"linear_pool"` is specified, ensemble model outputs are created as
#' a linear pool of component model outputs. This method supports only
#' an `output_type` of `mean`, `quantile`, or `pmf`.
#' @param min_log_score A numeric value specifying a minimum threshold for log
#' scores for the `pmf` output. This threshold prevents issues with extremely
#' low probabilities assigned to the true outcome, which would otherwise lead to
#' undefined or negative infinite log scores.
#' Any probability lower than this threshold will be adjusted to this minimum
#' value. The default value is set to -10, following the CDC FluSight
#' thresholding convention. Users may choose a different value based on their
#' practical needs.
#' @param ... Optional arguments passed to `ensemble_fun` when it is specified
#' as `"simple_ensemble"`. See 'Details'.
#'
#' @return A data.frame with columns `model_id`, `reference_date`,
#' `output_type`, and `importance`, along with any task ID columns (e.g.,
#' `location`, `horizon`, and `target_end_date`) present in the input
#' `forecast_data`.
#' Note that `reference_date` is used as the name for the forecast date column,
#' regardless of its original name in the input `forecast_data`.
#'
#' @importFrom methods is
#' @export
#'
#' @details
#' The `oracle_output_data` is a data frame that contains the ground truth
#' values for the variables used to define modeling targets. It is referred to
#' as “oracle” because it is formatted as if an oracle made a perfect point
#' prediction equal to the truth. This data must follow the oracle output format
#' defined in the hubverse standard, which includes independent task ID columns
#' (e.g., `location`, `target_date`), the `output_type` column specifying the
#' output type of the predictions and an `oracle_value` column for the observed
#' values. As in the forecast data, if the `output_type` is either `"quantile"`
#' or `"pmf"`, the `output_type_id` column is often required to provide further
#' identifying information.
#'
#' The `model_out_tbl` and `oracle_output_data` must have the same task ID
#' columns and `output_type`, including `output_type_id` if necessary, which are
#' used to match the predictions with the ground truth data.
#'
#' Additional argument in `...` is `agg_fun`, which is a character string name
#' for a function specifying aggregation method of component model outputs.
#' Default is `mean`, indicating that equally weighted mean is calculated
#' across all component model outputs for each unique `output_type_id`.
#' This can be `median` or a custom function (e.g., geometric_mean. Details
#' can be found in
#' https://hubverse-org.github.io/hubEnsembles/articles/hubEnsembles.html).
#'
#' This function uses the `furrr` and `future` for parallelization.
#' To enable parallel execution, please set a parallel backend, e.g., via
#' `future::plan()`.
#'
#' @examples \dontrun{
#' library(dplyr)
#' forecast_data <- hubExamples::forecast_outputs |>
#'   dplyr::filter(
#'     output_type %in% c("quantile"),
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
#' # Example with the default arguments.
#' model_importance(
#'   forecast_data = forecast_data, oracle_output_data = target_data,
#'   ensemble_fun = "simple_ensemble", importance_algorithm = "lomo",
#'   subset_wt = "equal"
#' )
#' # Example with the additional argument in `...`.
#' model_importance(
#'   forecast_data = forecast_data, oracle_output_data = target_data,
#'   ensemble_fun = "simple_ensemble", importance_algorithm = "lomo",
#'   subset_wt = "equal", agg_fun = median
#' )
#' }
model_importance <- function(forecast_data,
                             oracle_output_data,
                             ensemble_fun = c("simple_ensemble", "linear_pool"),
                             importance_algorithm = c("lomo", "lasomo"),
                             subset_wt = c("equal", "perm_based"),
                             min_log_score = -10,
                             ...) {
  # validate inputs
  validate_inputs(
    forecast_data, oracle_output_data, ensemble_fun, importance_algorithm,
    subset_wt, min_log_score
  )
  # set defaults
  ensemble_fun <- match.arg(ensemble_fun)
  importance_algorithm <- match.arg(importance_algorithm)
  subset_wt <- match.arg(subset_wt)

  # validate input data: get a model_out_tbl format with a single output type
  valid_tbl <- validate_input_data(forecast_data, oracle_output_data)

  # validate ensemble_fun when output_type is median
  data_output_type <- unique(valid_tbl$output_type)
  if (data_output_type == "median" && ensemble_fun == "linear_pool") {
    stop("Error: 'linear pool' cannot be used when output type is 'median'.")
  }

  # forecast_dates of the predictions to be evaluated
  forecast_date_list <- unique(valid_tbl$reference_date)

  # Message for the user to check the forecast dates
  send_message(
    "date_range", min(forecast_date_list), max(forecast_date_list),
    length(forecast_date_list)
  )

  # model ids
  model_id_list <- unique(valid_tbl$model_id)
  # Message for the user to check the model IDs
  send_message("model_list", model_id_list)

  # Corresponding metric to the output type
  metric <- case_when(
    unique(valid_tbl$output_type) == "median" ~ "ae_point",
    unique(valid_tbl$output_type) == "mean" ~ "se_point",
    unique(valid_tbl$output_type) == "quantile" ~ "wis",
    unique(valid_tbl$output_type) == "pmf" ~ "log_score"
  )
  # Message for the user about log_score handling
  if (metric == "log_score") {
    send_message("metric_logscore")
  }

  ## Implement importance score calculation
  # check if the necessary packages are installed
  if (is(future::plan(), "sequential")) {
    send_message("future_plan")
  }

  # Group by single task
  df_list_by_task <- split_data_by_task(valid_tbl)

  # Call the function to calculate importance scores
  score_result <- furrr::future_map_dfr(
    df_list_by_task,
    function(single_task_data) {
      compute_importance(
        single_task_data, oracle_output_data, model_id_list,
        ensemble_fun, importance_algorithm, subset_wt,
        metric, min_log_score, ...
      )
    }
  )
  # Reorder columns to place `model_id` and `reference_date` first,
  # task-related columns in the middle, and `output_type` and `importance` last.
  score_result |>
    dplyr::select(
      "model_id", "reference_date",
      dplyr::everything()
    ) |>
    dplyr::relocate(c("output_type", "importance"),
      .after = dplyr::last_col()
    )
}
