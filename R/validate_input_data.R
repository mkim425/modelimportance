#' Check the input forecast data structure and validate it.
#'
#' @param forecast_data A data.frame with the predictions that is or can be
#' coerced to a model_out_tbl format.
#' @param oracle_output_data A data.frame with the target values.
#' This data must follow the oracle output format.
#' @return a model_out_tbl format for forecast data
#'
#' @import hubUtils
#' @import dplyr
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
#' }
validate_input_data <- function(forecast_data, oracle_output_data) {
  valid_tbl <- forecast_data |>
    # Convert model output to a `model_out_tbl` class object
    hubUtils::as_model_out_tbl() |>
    # Validate a `model_out_tbl` object
    hubUtils::validate_model_out_tbl()

  # Check if NA exists in the output type
  if (sum(is.na(valid_tbl$output_type)) != 0) {
    stop("The output type has a missing value.")
  }

  # Check if the data contain a single output type
  num_output_type <- valid_tbl$output_type |>
    unique() |>
    length()
  if (num_output_type != 1) {
    stop("The input data must contain a single output type.")
  }

  # Check if there is exactly one column representing the forecast date
  possible_col_names <- c("forecast_date", "origin_date", "reference_date")
  matching_name <- intersect(colnames(valid_tbl), possible_col_names)
  if (length(matching_name) == 1) {
    # standardize the column to a single unified name:'reference_date'
    names(valid_tbl)[names(valid_tbl) == matching_name] <- "reference_date"
  } else {
    stop(
      "The input 'forecast_data' must contain exactly one of the columns: ",
      paste0("'", possible_col_names, "'", collapse = ", "), "."
    )
  }

  # Ensure that target_end_date is 'Date' class.
  oracle_output_data$target_end_date <- as.Date(
    oracle_output_data$target_end_date
  )
  # Check if all values in the target_end_date column of the forecast data are
  # present in the oracle_output_data
  if (!all(valid_tbl$target_end_date %in% oracle_output_data$target_end_date)) {
    stop("All values in the 'target_end_date' column of the forecast data must
         present in the 'target_end_date' column of the target data.")
  }

  return(valid_tbl)
}
