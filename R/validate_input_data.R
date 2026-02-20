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

  # Check if forecast_data and oracle_output_data have common task id columns
  task_id_cols <- get_task_id_cols(valid_tbl)
  core_task_id_cols <- intersect(task_id_cols, colnames(oracle_output_data))
  if (length(core_task_id_cols) == 0) {
    stop(
      "'forecast_data' and 'oracle_output_data' have no common task id column."
    )
  }
  # Match the class of the common task id columns in the oracle_output_data
  # to the class of those in the forecast_data.
  oracle_output_data[core_task_id_cols] <- Map(
    function(x, ref_col) {
      if (inherits(ref_col, "Date")) {
        as.Date(x)
      } else {
        as(x, class(ref_col)[1])
      }
    },
    oracle_output_data[core_task_id_cols],
    valid_tbl[core_task_id_cols]
  )

  # Check if all the different tasks on the forecast data are present in the
  # oracle_output_data.
  unique_tasks_forecast <- forecast_data |>
    select(all_of(core_task_id_cols)) |>
    distinct()
  unique_tasks_observation <- oracle_output_data |>
    select(all_of(core_task_id_cols)) |>
    distinct()
  if (nrow(setdiff(unique_tasks_forecast, unique_tasks_observation)) != 0) {
    stop(
    paste("All the different tasks on the 'forecast_data' must present",
           "in the 'oracle_output_data' column of the target data.",
          sep = " ")
    )
  }

  valid_tbl
}
