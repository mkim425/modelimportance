# =============================================================================
# Helper functions to
# 1) deal with NAs from missing data by 3 different approaches:
#    a) replace NAs with the average importance score of other models
#    b) replace NAs with the worst (minimum) importance score of other models
#    c) drop models with NAs
# 2) aggregate importance scores across different tasks
# =============================================================================

# Function to replace NAs in importance scores calculated by score_untrained()
# Args:
#   score_df: data frame with columns model_id and importance
# Returns:
#   data frame with NAs replaced in 3 different ways and an additional column
#   na_action indicating how NAs were handled
replace_na <- function(score_df) {
  avg <- score_df |>
    mutate(
      across(.data$importance, ~ coalesce(., mean(., na.rm = TRUE))),
      na_action = "average"
    )
  worst <- score_df |>
    mutate(
      across(.data$importance, ~ coalesce(., min(., na.rm = TRUE))),
      na_action = "worst"
    )
  drop <- score_df |>
    filter(!is.na(.data$importance)) |>
    mutate(na_action = "drop")
  bind_rows(avg, worst, drop)
}

# Function to aggregate importance scores across different tasks
# Args:
#   score_result: data frame with calculated importance scores per task
# Returns:
#   data frame with aggregated importance scores per model and na_action

aggregate_scores <- function(score_result) {
  score_result |>
    group_by(.data$model_id, .data$na_action) |>
    summarise(mean_importance = mean(.data$importance), .groups = "drop") |>
    arrange(.data$na_action, desc(.data$mean_importance))
}
