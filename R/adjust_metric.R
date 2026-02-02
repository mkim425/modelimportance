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
  # if metric is log_score, convert values less than log_min_val (the minimum
  # threshold) to the assigned log_min_val (default -10)
  # note: these are negative log values from score_model_out(),
  # so "less than" means "greater than" numerically
  if ("log_score" %in% names(df)) {
    df$log_score[df$log_score > -log_min_val] <- -log_min_val
  }
  df
}
