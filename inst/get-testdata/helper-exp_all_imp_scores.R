# =============================================================================
# Helpers:
# 1) function to deal with NAs from missing data by 3 different approaches:
#    a) replace NAs with the average importance score of other models
#    b) replace NAs with the worst (minimum) importance score of other models
#    c) drop models with NAs
# 2) function to aggregate importance scores across different tasks
# 3) parameter settings for scoring with simple_ensemble and linear_pool
# =============================================================================

library(dplyr)

# 1)
# Function to replace NAs in importance scores calculated by score_untrained()
# Args:
#   score_df: data frame with columns model_id and importance
# Returns:
#   data frame with NAs replaced in 3 different ways and an additional column
#   na_action indicating how NAs were handled
replace_na <- function(score_df) {
  avg <- score_df |>
    dplyr::mutate(
      dplyr::across("importance", ~ coalesce(., mean(., na.rm = TRUE))),
      na_action = "average"
    )
  worst <- score_df |>
    dplyr::mutate(
      dplyr::across("importance", ~ coalesce(., min(., na.rm = TRUE))),
      na_action = "worst"
    )
  drop <- score_df |>
    dplyr::filter(!is.na(.data$importance)) |>
    dplyr::mutate(na_action = "drop")
  dplyr::bind_rows(avg, worst, drop)
}

# 2) Function to summarize importance scores across different tasks
# Args:
#   score_result: data frame with calculated importance scores per task
#   fun: aggregation function to summarize importance scores (default: mean)
#   ...: additional arguments for the aggregation function
# Returns:
#   data frame with summarized importance scores per model and na_action

summarize_scores <- function(score_result, fun = mean, ...) {
  score_result |>
    dplyr::group_by(.data$model_id, .data$na_action) |>
    dplyr::summarise(
      mean_importance = mean(.data$importance),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$na_action, desc(.data$mean_importance))
}

# 3) Set parameters for scoring
# Ensemble method: simple_ensemble
params_simple <- expand.grid(
  imp_alg = c("lomo", "lasomo"),
  subset_weight = c("equal", "perm_based"),
  agg_fun = c("mean", "median"),
  stringsAsFactors = FALSE
) |>
  dplyr::filter(!(imp_alg == "lomo" & subset_weight == "perm_based"))
# Ensemble method: linear_pool
params_lp <- expand.grid(
  imp_alg = c("lomo", "lasomo"),
  subset_weight = c("equal", "perm_based"),
  stringsAsFactors = FALSE
) |>
  dplyr::filter(!(imp_alg == "lomo" & subset_weight == "perm_based"))
