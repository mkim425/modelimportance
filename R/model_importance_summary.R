#' Summarize model importance scores produced by `model_importance()`
#' across tasks
#'
#' @description `model_importance_summary` summarizes model importance scores
#' calculated for individual prediction tasks into a summary statistic for
#' each model. This function handles `NA` values in importance scores generated
#' when a model did not contribute to the ensemble prediction for a given task
#' by missing its forecast submission.
#'
#' @param importance_scores A data frame containing model importance scores for
#' individual prediction tasks, as produced by the `model_importance()`
#' function. The data frame should include columns `model_id`, `reference_date`,
#' any task ID columns (e.g., `location`, `horizon`, and `target_end_date`),
#' `output_type`, and `importance`.
#' @param by A character vector with column names specifying the grouping
#' variable(s) for summarization. Default is `"model_id"`, which summarizes
#' importance scores for each model across all tasks.
#' @param na_action A character string specifying how to handle `NA` values
#' generated during importance score calculation for each task, occurring when a
#' model did not contribute to the ensemble prediction for a given task by
#' missing its forecast submission.
#' Three options are available: `c("drop", "worst", "average")`.
#' For each specific prediction task, each option works as follows:
#' * `"drop"` removes `NA`s.
#' * `"worst"` replaces `NA`s with the smallest value among importance metrics
#' of the other models.
#' * `"average"` replaces `NA`s with the average value from the other
#' models' importance metrics.
#' @param fun A function used to summarize importance scores.
#' Default is `mean()`
#' @param ... Additional arguments passed to the summary function `fun`.
#' See the documentation of the corresponding function for details.
#'
#' @returns A data.frame with columns `model_id` and `importance_score_<fun>`,
#' where `<fun>` is the name of the summary function used
#' (e.g., `importance_score_mean` when `fun = mean`).
#' The output is sorted in descending order of the summary importance scores.
#' @export
#' @importFrom checkmate assert_data_frame assert_subset assert_function
#' @examples \dontrun{
#' library(dplyr)
#' library(hubExamples)
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
#' importance_scores <- model_importance(
#'   forecast_data = forecast_data, oracle_output_data = target_data,
#'   ensemble_fun = "simple_ensemble", importance_algorithm = "lomo",
#'   subset_wt = "equal"
#' )
#' model_importance_summary(importance_scores, by = "model_id")
#' }
model_importance_summary <- function(importance_scores, by = "model_id",
                                     na_action = c("drop", "worst", "average"),
                                     fun = mean, ...) {
  # check inputs
  assert_data_frame(importance_scores)
  required_cols <- c("model_id", "reference_date", "output_type", "importance")
  assert_subset(required_cols, names(importance_scores), empty.ok = FALSE)
  assert_subset(by, names(importance_scores), empty.ok = FALSE)
  assert_function(fun)
  na_action <- match.arg(na_action)

  # task specific columns
  task_id_cols <- setdiff(colnames(importance_scores), required_cols)
  # column name for the summary importance score
  fun_args <- list(...)
  colname <- paste0("importance_score_", deparse(substitute(fun)), sep = "")

  # NA handling
  if (na_action == "worst") {
    imputed_scores <- importance_scores |>
      dplyr::group_by(across(all_of(task_id_cols))) |>
      dplyr::mutate(across(
        importance,
        ~ coalesce(., min(., na.rm = TRUE))
      )) |>
      ungroup()
  } else if (na_action == "average") {
    imputed_scores <- importance_scores |>
      dplyr::group_by(across(all_of(task_id_cols))) |>
      dplyr::mutate(across(
        importance,
        ~ coalesce(., mean(., na.rm = TRUE))
      )) |>
      ungroup()
  } else {
    imputed_scores <- importance_scores |>
      dplyr::filter(!is.na(importance)) |>
      ungroup()
  }
  # summarize importance scores by the specified grouping variable(s)
  imputed_scores |>
    dplyr::group_by(!!sym(by)) |>
    dplyr::summarise(!!colname := {
      do.call(fun, list(x = .data$importance, !!!fun_args))
    }, .groups = "drop") |>
    dplyr::arrange(desc(!!sym(colname)))
}
