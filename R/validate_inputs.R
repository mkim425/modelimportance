#' validate inputs
#'
#' @inheritParams model_importance
#'
#' @noRd
validate_inputs <- function(forecast_data, oracle_output_data,
                            ensemble_fun = c("simple_ensemble", "linear_pool"),
                            weighted = FALSE,
                            training_window_length = 0,
                            importance_algorithm = c("lomo", "lasomo"),
                            subset_wt = c("equal", "perm_based"),
                            na_action = c("worst", "average", "drop"),
                            min_log_score = -10) {
  window_len <- training_window_length
  # validate inputs
  if (!is.data.frame(forecast_data)) {
    stop("Invalid input: 'forecast_data' must be a data frame.")
  }
  if (!is.data.frame(oracle_output_data)) {
    stop("Invalid input: 'oracle_output_data' must be a data frame.")
  }
  if (!is.logical(weighted)) {
    stop("Invalid value for 'weighted'. It must be either TRUE or FALSE.
         Default is FALSE.")
  }
  if (!(is.numeric(window_len) && window_len == as.integer(window_len))) {
    stop("Invalid value for 'training_window_length'. It must be an integer.
         Default is 0.")
  }
  if (weighted && window_len == 0) {
    stop(paste0(
      "Invalid combination: 'training_window_length' should be ",
      "a positive integer when 'weighted' is TRUE."
    ))
  }
  if (!weighted && window_len != 0) {
    message(
      "Note: 'training_window_length' is ignored when 'weighted' is FALSE."
    )
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
