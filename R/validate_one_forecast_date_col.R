#' Validate columns for dates on which the forecast was made:
#' should contain exactly one of
#' `forecast_date`, `origin_date`, and `reference_date`.
#'
#' @param valid_tbl A `model_out_tbl` format that has a single output type
#'
#' @noRd
validate_one_forecast_date_col <- function(valid_tbl) {
  # List of valid columns to check
  valid_cols <- c("forecast_date", "origin_date", "reference_date")

  # Columns present in the input data frame
  present_cols <- intersect(colnames(valid_tbl), valid_cols)

  # check if exactly one of the valid columns is present
  if (length(present_cols) != 1) {
    stop(
      "The input data must contain exactly one of the columns: c(",
      paste0("'", valid_cols, "'", collapse = ", "),
      "). Currently, it has ", length(present_cols),
      " matching column(s): ",
      paste0("'", present_cols, "'", collapse = ", "), "."
    )
  }
}
