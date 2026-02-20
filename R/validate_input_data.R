#' Check the input forecast data structure and validate it.
#'
#' @inheritParams model_importance
#'
#' @return a model_out_tbl format for forecast data
#'
#' @import hubUtils
#' @import dplyr
#' @noRd

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

  # Check the output type
  unique_output_type <- unique(valid_tbl$output_type)
  # The data must contain a single output type
  if (length(unique_output_type) != 1) {
    stop("The input data must contain a single output type.")
  }
  # The output_type must be one of 'median', 'quantile', 'mean', or 'pmf'
  if (!unique_output_type %in% c("median", "quantile", "mean", "pmf")) {
    stop(
      "The output type is not supported.
      It must be one of 'median', 'mean', 'quantile', or 'pmf'."
    )
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

  # Check if the oracle_output_data contains the required columns
  assert_subset(c("output_type", "oracle_value", "target_end_date"),
    names(oracle_output_data),
    empty.ok = FALSE
  )
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

  valid_tbl
}
