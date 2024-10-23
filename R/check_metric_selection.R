#' Validate metric selection for output types
#'
#' @param output_type A character string specifying the type of prediction
#' output. Should be one of
#' "mean", "median", "quantile", "pmf", "cdf", or "sample".
#' @param scoring_rule A character string specifying the scoring rule to use
#' as input
#'
#' @return TRUE if inputs are valid, otherwise an error is raised.
#' @export
#'
#' @examples \dontrun{
#' # Invalid combinations
#' check_metric_selection("mean", "WIS") # Raises an error
#' check_metric_selection("quantile", "MAE") # Raises an error
#' check_metric_selection("mean", "MSE") # Returns TRUE
#' check_metric_selection("quantile", "WIS") # Returns TRUE
#' }
check_metric_selection <- function(output_type, scoring_rule) {
  if (output_type %in% c("mean", "median")) {
    if (!(scoring_rule %in% c("MAE", "MSE"))) {
      stop("The scoring rule needs to be either MAE or MSE")
    }
  } else if (output_type == "quantile") {
    if (!(scoring_rule %in% c("WIS", "Logscore"))) {
      stop("The scoring rule needs to be either WIS or Logscore")
    }
  } else if (output_type %in% c("pmf", "cdf")) {
    if (scoring_rule != "CRPS") {
      stop("The scoring rule needs to be CRPS")
    }
  } else if (output_type == "sample") {
    stop("sample model output type is under development and not yet supported.
         Please use a different output type.")
  } else {
    stop("invalid output type.")
  }
  # return TRUE if all validations pass
  return(TRUE)
}
