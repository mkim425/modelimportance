#' Helper function to check if a single task has at least 2 distinct models and
#' filter out invalid tasks.
#'
#' @param df_list_by_task A list of data.frames. Each data.frame corresponds
#' to a single task.
#' @param min_models An integer specifying the minimum number of distinct models
#' required for each task. Default is 2.
#'
#' @returns A list of data.frames that only includes tasks with models of at
#' least `min_models`.
#' @importFrom utils capture.output
#' @noRd
filter_valid_tasks <- function(df_list_by_task, min_models = 2) {
  # task_id_cols are the columns that identify a task,
  # excluding model_id, output_type_id, and value
  task_id_cols <- setdiff(
    colnames(df_list_by_task[[1]]),
    c("model_id", "output_type_id", "value")
  )
  # Check if each task has at least min_models distinct models
  task_model_count <- vapply(
    df_list_by_task,
    function(df) length(unique(df$model_id)),
    integer(1)
  )
  # Identify tasks that do not meet the minimum model requirement
  invalid_tasks_idx <- which(task_model_count < min_models)
  if (length(invalid_tasks_idx) > 0) {
    invalid_tasks <- purrr::map_dfr(invalid_tasks_idx, function(idx) {
      single_task_data <- df_list_by_task[[idx]]
      current_task <- single_task_data |>
        select(all_of(task_id_cols)) |>
        distinct() |>
        as.data.frame()
    })
    # Print a message listing the invalid tasks
    message(
      paste0(
        "\nNote: ",
        "The following tasks do not meet the minimum model requirement of ",
        min_models, " models:\n",
        paste(capture.output(print(invalid_tasks)), collapse = "\n")
      )
    )
    valid_df_list_by_task <- df_list_by_task[-invalid_tasks_idx]
  } else {
    valid_df_list_by_task <- df_list_by_task
  }

  # Handle different scenarios based on the number of valid tasks
  if (length(valid_df_list_by_task) == 0) {
    # If no valid tasks remain after filtering, stop execution
    stop("No valid tasks with at least 2 distinct models to evaluate.",
         call. = FALSE)
  } else if (length(valid_df_list_by_task) == length(df_list_by_task)) {
    # If all tasks are valid, print a confirmation message
    message(paste0(
      "\nAll tasks meet the minimum model requirement of ",
      min_models, " models."
    ))
  } else {
    # If some tasks were filtered out, print a summary message
    message(
      paste0(
        "\nModel evaluation will proceed with only the valid tasks.",
        "\n  Number of tasks before filtering: ", length(df_list_by_task),
        "\n  Number of valid tasks after filtering: ",
        length(valid_df_list_by_task), "\n"
      )
    )
  }

  return(valid_df_list_by_task)
}
