#' Split the input data by a single task and make a list of data sets
#'
#' @description This function splits the input data by a single task with a
#' combination of task IDs and returns a list of data sets, each corresponding
#' to a single task.
#'
#' @param valid_tbl A data.frame containing forecast data and target data,
#' processed by the function `validate_input_data()`.

#' @return A list of data sets, each corresponding to a single task.
#' @noRd
split_data_by_task <- function(valid_tbl) {
  # Get task specific columns
  split_cols <- get_task_id_cols(valid_tbl)
  # List of data split by task
  valid_tbl |>
    dplyr::group_by(across(all_of(split_cols))) |>
    dplyr::group_split()
}
