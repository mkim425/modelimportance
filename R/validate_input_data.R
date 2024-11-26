#' Check the input forecast data structure
#'
#' @param forecast_data A data.frame with the predictions that is or can be
#' coerced to a model_out_tbl format.
#' @param oracle_output_data A data.frame with the target values.
#' This data must follow the oracle output format.
#' @return a model_out_tbl format that has a single output type
#' @export
#' @import hubUtils
#' @import dplyr
#' @examples \dontrun{
#' library(hubUtils)
#' library(dplyr)
#' hub_path <- system.file("testhubs/flusight", package = "hubUtils")
#' hub_con <- connect_hub(hub_path)
#' hub_con |>
#'   filter(output_type == "quantile") |>
#'   collect() |>
#'   validate_input_data()
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
  columns_to_check <- c("forecast_date", "origin_date", "reference_date")
  if (length(intersect(colnames(valid_tbl), columns_to_check)) != 1) {
    stop("The input 'forecast_data' must contain exactly one of the columns: ",
         paste0("'", columns_to_check, "'", collapse = ", "), ".")
  }

  # Ensure that target_end_date is 'Date' class.
  oracle_output_data$target_end_date <-
          as.Date(oracle_output_data$target_end_date)
  # Check if all values in the target_end_date column of the forecast data are
  # present in the target data
  if (!all(valid_tbl$target_end_date %in% oracle_output_data$target_end_date)) {
    stop("All values in the 'target_end_date' column of the forecast data must
         present in the 'target_end_date' column of the target data.")
  }

  # get task_id columns to use for joining forecast and target data
  indep_task_id_cols <- c("location", "target_end_date", "age_group", "target")
  if (unique(valid_tbl$output_type) %in% c("pmf", "cdf")) {
    task_id_cols <- intersect(
      colnames(oracle_output_data),
      c(indep_task_id_cols, "output_type", "output_type_id")
    )
  } else {
    task_id_cols <- intersect(
      colnames(oracle_output_data),
      c(indep_task_id_cols, "output_type")
    )
  }

  # Combine forecast data and target data into a single data frame
  valid_tbl <- valid_tbl |>
    dplyr::left_join(oracle_output_data, by = task_id_cols)

  return(valid_tbl)
}
