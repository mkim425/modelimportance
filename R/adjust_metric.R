#' Adjust metric values if necessary
#'
#' @description for `se_point` metric, convert it to `rse_point`;
#' for `log_score` metric, replace -Inf values with min_log_score.
#'
#' @param df A data.frame containing metric values.
#' @param log_min_val A numeric value to replace -Inf log_score values.
#' Default is -10.
#'
#' @returns A data.frame with adjusted metric values.
#' @noRd
adjust_metric <- function(df, log_min_val = -10) {
  # if metric is se_point, convert it to rse_point
  if ("se_point" %in% names(df)) {
    df <- df |>
      mutate(rse_point = sqrt(.data$se_point)) |>
      select(-"se_point")
  }
  # if metric is log_score and the value is Inf, convert it to log_min_val
  if ("log_score" %in% names(df) && any(is.infinite(df$log_score))) {
    df$log_score[df$log_score == -Inf] <- log_min_val
  }
  df
}
