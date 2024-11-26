#' Validate metric selection for output types
#'
#' @param output_type A character string specifying the type of prediction
#' output. Should be one of "mean", "median", "quantile", "pmf", or "sample".
#' @param scoring_rule A character string specifying the scoring rule to use
#' as input. Should be one of "ae_point", "se_point", "wis", or "log_score".
#'
#' @return An error is raised if any of the inputs is not valid.
#'
#' @examples \dontrun{
#' # Invalid combinations
#' check_metric_selection("mean", "wis") # Raises an error
#' check_metric_selection("quantile", "ae_point") # Raises an error
#' check_metric_selection("mean", "se_point") # Returns TRUE
#' check_metric_selection("quantile", "wis") # Returns TRUE
#' }
check_metric_selection <- function(output_type, scoring_rule) {
  if (output_type == "median") {
    if (scoring_rule != "ae_point") {
      stop("The scoring rule needs to be ae_point.")
    }
  } else if (output_type == "mean") {
    if (scoring_rule != "se_point") {
      stop("The scoring rule needs to be se_point.")
    }
  } else if (output_type == "quantile") {
    if (scoring_rule != "wis") {
      stop("The scoring rule needs to be wis.")
    }
  } else if (output_type == "pmf") {
    if (scoring_rule != "log_score") {
      stop("The scoring rule needs to be log_score.")
    }
  } else if (output_type %in% c("cdf", "sample")) {
    stop("cdf and sample model output types are under development and not yet
    supported. Please use a different output type.")
  } else {
    stop("invalid output type.")
  }
  # return TRUE if all validations pass
  return(TRUE)
}
