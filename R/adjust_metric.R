#' Adjust metric values if necessary
#'
#' @description for `se_point` metric, convert it to `rse_point`;
#' for `log_score` metric, replace -Inf values with min_log_score.
#'
#' @param df A data.frame containing metric values.
#' @param min_log_score A numeric value to replace -Inf log_score values.
#' Default is -10.
#'
#' @returns A data.frame with adjusted metric values.
#' @noRd
adjust_metric <- function(df, min_log_score) {
  # if metric is se_point, convert it to rse_point
  if ("se_point" %in% names(df)) {
    df <- df |>
      mutate(rse_point = sqrt(.data$se_point)) |>
      select(-"se_point")
  }
  # if metric is log_score and the value is Inf, convert it to min_log_score
  if ("log_score" %in% names(df) && any(is.infinite(df$log_score))) {
    if (min_log_score == -10) {
      message(paste(
        "A negative infinite log_score value ('-Inf') occurred",
        "due to the zero probability assigned to the true outcome. \n",
        "It is convered to -10 by default.",
        "To use a different value for the minimum score,",
        "set the 'min_log_score' argument."
      ))
    } else {
      message(paste0(
        "A negative infinite log_score value ('-Inf') is replaced with ",
        min_log_score, "."
      ))
    }

    df$log_score[df$log_score == Inf] <- min_log_score
  }
  df
}
