#' Split the input data by a single task.
#'
#' @description This function splits the input data by a single task with a
#' combination of horizon, location, and target_end_date
#'
#' @param valid_tbl A data.frame containing forecast data and target data,
#' processed by the function `validate_input_data()`.
#' @param weighted Boolean indicating whether model weighting should be done.
#' If `FALSE`, all models are given equal weight.
#' If `TRUE`, model weights are estimated.
#' @param training_window_length An integer value representing the time interval
#' of historical data used during the training process
#' to estimate model weights.
#' Default is `0`, meaning that no prior data is available for training.
#' @return A list of data sets, each corresponding to a single task.
#'
#' @examples \dontrun{
#' library(dplyr)
#' library(purrr)
#' library(hubExamples)
#' forecast_data <- hubExamples::forecast_outputs |>
#'   dplyr::filter(
#'     output_type %in% c("mean"),
#'     location %in% c("25", "48"),
#'     horizon %in% c(1, 2)
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
#' valid_tbl <- validate_input_data(forecast_data, target_data)
#' split_data_by_task(valid_tbl, weighted = FALSE, training_window_length = 0)
#' split_data_by_task(valid_tbl, weighted = TRUE, training_window_length = 1)
#' }
split_data_by_task <- function(valid_tbl, weighted, training_window_length) {
  # Use of a trained ensemble or not.
  training_status <- ifelse(weighted, "trained", "untrained")

  # Split data and make a list of data sets
  if (training_status == "untrained") {
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
      list_datasets <- list()
      for (i in 1:(len_ref_dates - training_window_length)) {
        list_datasets[[i]] <- valid_tbl |>
          dplyr::filter(
            .data$reference_date <= all_ref_dates[i + training_window_length],
            .data$reference_date >= all_ref_dates[i]
          )
      }
    }
  }

  return(list_datasets)
}
