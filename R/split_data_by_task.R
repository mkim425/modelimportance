#' Split the input data by a single task.
#'
#' @description This function splits the input data by a single task with a
#' combination of horizon, location, and target_end_date
#'
#' @inheritParams model_importance
#' @param valid_tbl A data.frame containing forecast data and target data,
#' processed by the function `validate_input_data()`.

#' @return A list of data sets, each corresponding to a single task.

split_data_by_task <- function(valid_tbl, weighted, training_window_length) {
  # Split data and make a list of data sets
  if (!weighted) {
    # Get columns to use for splitting data by task
    split_cols <- c("horizon", "location", "target_end_date")
    # List of data split by task
    list_datasets <- valid_tbl |>
      dplyr::group_by(across(all_of(split_cols))) |>
      dplyr::group_split()
  } else {
    # List of reference dates
    all_ref_dates <- sort(unique(valid_tbl$reference_date))
    # Length of reference dates
    len_ref_dates <- length(all_ref_dates)
    if (len_ref_dates <= training_window_length) {
      stop(
        "The number of reference_date must greater than the training window
        length."
      )
    } else {
      # Create a list of data split by reference date including the training
      n_dates <- len_ref_dates - training_window_length
      list_datasets <- vector(mode = "list", length = n_dates)
      for (i in seq_len(n_dates)) {
        list_datasets[[i]] <- valid_tbl |>
          dplyr::filter(
            .data$reference_date <= all_ref_dates[i + training_window_length],
            .data$reference_date >= all_ref_dates[i]
          )
      }
    }
  }

  list_datasets
}
