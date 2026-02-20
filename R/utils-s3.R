#' Print method for model importance score table
#'
#' @param x An object of class `model_imp_tbl`.
#' @param ... Additional arguments passed to the print method.
#'
#' @export
print.model_imp_tbl <- function(x, ...) {
  cat("Model importance result by task\n")
  cat("---------------------------------\n")
  print(as.data.frame(x))
}

#' Summary method for model importance score table
#'
#' @param object An object of class `model_imp_tbl`.
#' @param ... Additional arguments passed to the summary method.
#'
#' @export
summary.model_imp_tbl <- function(object, ...) {
  # columns in the importance score table
  task_id_cols <- get_task_id_cols(object)

  # compute model statistics
  model_stats <- object |>
    dplyr::group_by(model_id) |>
    dplyr::summarise(
      n_tasks = n(),
      min_importance = min(importance, na.rm = TRUE),
      max_importance = max(importance, na.rm = TRUE),
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
    as.data.frame()

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
#'
#' @export
print.summary.model_imp_tbl <- function(x, ...) {
  # summary statements
  cat("\n=== Summary of importance scores by task ===\n")
  cat("Number of models:", length(x$all_models), "\n")
  cat("Number of tasks:", nrow(x$all_tasks), "\n\n")

  cat("\n=== Top scoring model by task", strrep("=", 40), "\n")
  x$task_winners |>
    dplyr::mutate(importance = round(.data$max_score, 2)) |>
    dplyr::select(-.data$max_score) |>
    print(row.names = FALSE)
  cat("--------------------------------------------\n")
  cat("* More details available in the summary object (e.g., $all_tasks).\n")
  invisible(x)
}

#' Plot method for model importance score table
#'
#' @param x An object of class `model_imp_tbl`.
#' @param ... Additional arguments passed to the plot method.
#'
#' @importFrom ggplot2 ggplot aes geom_col coord_flip geom_hline facet_grid
#' @importFrom ggplot2 labs theme vars
#' @importFrom rlang sym syms
#' @export
plot.model_imp_tbl <- function(x, ...) {
  # columns in the importance score table
  task_id_cols <- get_task_id_cols(x)

  # create ggplot object
  ggplot(
    x,
    aes(
      x = model_id,
      y = importance,
      fill = model_id
    )
  ) +
    # create bar plot
    geom_col() +
    # flip coordinates
    coord_flip() +
    # add a line at y = 0 to indicate baseline
    geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
    # plot by task
    facet_grid(cols = vars(!!!syms(task_id_cols)), scales = "free_x") +
    labs(
      title = "Model Importance by Task",
      x = "Model ID",
      y = "Importance"
    ) +
    theme(legend.position = "none")
}

# =============================================================================
#' Print method for overall importance summary
#'
#' @param x An object of class `importance_summary`.
#' @param ... Additional arguments passed to the print method.
#'
#' @export
print.importance_summary <- function(x, ...) {
  cat("Overall model importance across tasks\n")
  cat(strrep("-", 40), "\n")
  print(as.data.frame(x))
}


#' Summary method for overall importance summary
#'
#' @param object An object of class `importance_summary`.
#' @param ... Additional arguments passed to the summary method.
#'
#' @export
summary.importance_summary <- function(object, ...) {
  cat("Summary statistics of overall model importance scores\n")
  cat(strrep("-", 55), "\n")
  summary.data.frame(object)
}

#' Plot method for overall importance summary
#'
#' @param x An object of class `importance_summary`.
#' @param ... Additional arguments passed to the plot method.
#'
#' @importFrom ggplot2 ggplot aes geom_col coord_flip geom_hline labs theme vars
#' @importFrom rlang sym syms
#' @export
plot.importance_summary <- function(x, ...) {
  y_col <- names(x)[startsWith(names(x), "importance_score_")]
  # create ggplot object
  ggplot(
    x,
    aes(
      x = model_id,
      y = !!sym(y_col),
      fill = model_id
    )
  ) +
    # create bar plot
    geom_col() +
    # flip coordinates
    coord_flip() +
    # add a line at y = 0 to indicate baseline
    geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
    labs(
      title = "Overall model importance across tasks",
      x = "Model ID",
      y = "Importance"
    ) +
    theme(legend.position = "none")
}
