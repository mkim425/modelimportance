#' Split the input data by a single task and make a list of data sets
#'
#' @description This function splits the input data by a single task with a
#' combination of horizon, location, and target_end_date
#'
#' @param valid_tbl A data.frame containing forecast data and target data,
#' processed by the function `validate_input_data()`.

#' @return A list of data sets, each corresponding to a single task.

split_data_by_task <- function(valid_tbl) {
  # Get columns to use for splitting data by task
  ## commonly required columns
  required_cols <- c(
    "reference_date", "model_id", "output_type", "output_type_id", "value"
  )
  ## task specific columns
  split_cols <- setdiff(colnames(valid_tbl), required_cols)
  # List of data split by task
  valid_tbl |>
    dplyr::group_by(across(all_of(split_cols))) |>
    dplyr::group_split()
}
