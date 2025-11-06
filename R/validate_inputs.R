#' validate inputs
#'
#' @inheritParams model_importance
#'
#' @noRd
validate_inputs <- function(forecast_data, oracle_output_data,
                            ensemble_fun = c("simple_ensemble", "linear_pool"),
                            importance_algorithm = c("lomo", "lasomo"),
                            subset_wt = c("equal", "perm_based"),
                            na_action = c("worst", "average", "drop"),
                            min_log_score = -10) {
  # validate inputs
  if (!is.data.frame(forecast_data)) {
    stop("Invalid input: 'forecast_data' must be a data frame.")
  }
  if (!is.data.frame(oracle_output_data)) {
    stop("Invalid input: 'oracle_output_data' must be a data frame.")
  }
  if (min_log_score >= 0) {
    stop("Invalid value for 'min_log_score'. It must be a negative number.
         Default is -10.")
  }
  ensemble_fun <- match.arg(ensemble_fun)
  importance_algorithm <- match.arg(importance_algorithm)
  subset_wt <- match.arg(subset_wt)
  na_action <- match.arg(na_action)
}
