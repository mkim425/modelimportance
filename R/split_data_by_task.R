#' Split the input data by a single task.
#'
#' @description This function splits the input data by a single task with a
#' combination of horizon, location, and target_end_date
#'
#' @param valid_tbl A data.frame containing forecast data and target data,
#' processed by the function `validate_input_data()`.
#'
#' @return A list of data sets, each corresponding to a single task.
#' @export
#'
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
#' valid_tbl <- validate_input_data(forecast_data, target_data)
#' split_data_by_task(valid_tbl)
#' }
split_data_by_task <- function(valid_tbl) {
  # Get columns to use for splitting data by task
  split_cols <- c("horizon", "location", "target_end_date")

  # List of data sets split by task
  datasets_by_task <- valid_tbl |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(split_cols)
      )
    ) |>
    dplyr::group_split()

  return(datasets_by_task)
}
