#' Print method for model importance score table
#'
#' @param x An object of class `model_imp_tbl`.
#' @param ... Additional arguments passed to the print method.
#' @export
print.model_imp_tbl <- function(x, ...) {
  cat("Model importance result by task\n")
  cat("---------------------------------\n")
  print(as.data.frame(x))
}

#' Summary method for model importance score table
#'
#' @param object An object of class `model_imp_tbl`.
#' @param ... Additional arguments passed to the print method.
#' @importFrom dplyr n
#' @importFrom rlang .data
#' @export
summary.model_imp_tbl <- function(object, ...) {
  # columns in the importance score table
  task_id_cols <- get_task_id_cols(object)

  # compute model statistics
  model_stats <- object |>
    dplyr::group_by(model_id) |>
    dplyr::summarise(
      n_tasks = n(),
      min_importance = min(importance, na.rm = TRUE) |> round(2),
      max_importance = max(importance, na.rm = TRUE) |> round(2),
      n_NA = sum(is.na(importance)),
      .groups = "drop"
    ) |>
    as.data.frame()
  # best model by task
  task_winners <- object |>
    dplyr::group_by(across(all_of(task_id_cols))) |>
    dplyr::slice_max(importance, n = 1, with_ties = FALSE) |>
    dplyr::select(
      all_of(task_id_cols),
      top_model = model_id,
      max_score = importance
    ) |>
    dplyr::ungroup() |>
    as.data.frame() |>
    dplyr::mutate(max_score = round(max_score, 2))

  # create summary list
  summary_list <- list(
    all_models = unique(object$model_id),
    all_tasks = object |>
      dplyr::select(dplyr::all_of(task_id_cols)) |>
      dplyr::distinct() |>
      as.data.frame(),
    model_summary = model_stats,
    task_winners = task_winners
  )
  # set class
  class(summary_list) <- "summary.model_imp_tbl"
  summary_list
}

#' Print method for summary of model importance score table
#'
#' @param x An object of class `summary.model_imp_tbl`.
#' @param ... Additional arguments passed to the print method.
#' @importFrom rlang .data
#' @importFrom utils head
#' @export
print.summary.model_imp_tbl <- function(x, ...) {
  # summary statements
  cat("=== Summary of importance scores by task ===\n")
  cat("Number of models:", length(x$all_models), "\n")
  cat("Number of tasks:", nrow(x$all_tasks), "\n")

  cat(
    "\n=== Top scoring model by task for a subset of tasks",
    strrep("=", 40),
    "\n"
  )
  x$task_winners |>
    dplyr::mutate(importance = round(.data$max_score, 2)) |>
    dplyr::select(-.data$max_score) |>
    head(3) |>
    print(row.names = FALSE)
  cat("--------------------------------------------\n")
  cat(paste(
    "* More details are available in the summary object",
    "(e.g., $all_tasks, $model_summary, $task_winners).\n",
    sep = " "
  ))
  invisible(x)
}


#' Aggregate model importance scores across tasks to compute overall importance
#' for each model
#' @param x An object of class `model_imp_tbl`.
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
#' (e.g., `fun = quantile, probs = 0.25` for a quartile summary)
#' @returns A data frame with columns `model_id` and `importance_score_<fun>`,
#' where `<fun>` is the name of the summary function
#' used (e.g., `importance_score_mean` when `fun = mean`).
#' The output is sorted in descending order of the summary importance scores.
#' @details
#' This method extends `stats::aggregate` for objects of class `model_imp_tbl`.
#' @importFrom checkmate assert_data_frame assert_subset assert_function
#' @importFrom stats aggregate
#' @importFrom dplyr desc ungroup
#' @importFrom rlang .data sym syms
#' @rdname aggregate.model_imp_tbl
#' @export

aggregate.model_imp_tbl <- function(
  x,
  by = "model_id",
  na_action = c("drop", "worst", "average"),
  fun = mean,
  ...
) {
  # check inputs
  assert_data_frame(x)
  required_cols <- c("model_id", "reference_date", "output_type", "importance")
  assert_subset(required_cols, names(x), empty.ok = FALSE)
  assert_subset(by, names(x), empty.ok = FALSE)
  assert_function(fun)
  na_action <- match.arg(na_action)

  # task specific columns
  task_id_cols <- get_task_id_cols(x)
  # column name for the summary importance score
  fun_args <- list(...)
  colname <- paste0("importance_score_", deparse(substitute(fun)), sep = "")
  # NA handling
  if (na_action == "worst") {
    imputed_scores <- x |>
      dplyr::group_by(across(all_of(task_id_cols))) |>
      dplyr::mutate(across(
        importance,
        ~ coalesce(., min(., na.rm = TRUE))
      )) |>
      ungroup()
  } else if (na_action == "average") {
    imputed_scores <- x |>
      dplyr::group_by(across(all_of(task_id_cols))) |>
      dplyr::mutate(across(
        importance,
        ~ coalesce(., mean(., na.rm = TRUE))
      )) |>
      ungroup()
  } else {
    imputed_scores <- x |>
      dplyr::filter(!is.na(importance)) |>
      ungroup()
  }
  # summarize importance scores by the specified grouping variable(s)
  summary_df <- imputed_scores |>
    # use unquote symbols: !!!syms(by) to handles column(s) specified in `by`
    dplyr::group_by(!!!syms(by)) |>
    # dynamically created a column named by summary function and additional
    # arguments passed through `fun_args`
    dplyr::summarise(
      !!colname := {
        do.call(fun, list(x = .data$importance, !!!fun_args))
      },
      .groups = "drop"
    ) |>
    # unquote symbol !!sym(colname) handles the dynamically created column name
    dplyr::arrange(desc(!!sym(colname))) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), round, 2)
    )

  cat("Overall model importance across tasks\n")
  cat(strrep("-", 40), "\n")
  print(summary_df)
}
