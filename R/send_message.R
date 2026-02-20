#' Send a message about the model run status
#'
#' @param status Character string indicating the status of the model run.
#' @noRd

send_message <- function(status, ...) {
  messages <- list(
    date_range = function(first_date, last_date, n_dates) {
      sprintf(
        paste(
          "Evaluating forecasts from %s to %s ",
          "(a total of %d forecast date(s)).\n"
        ),
        first_date, last_date, n_dates
      )
    },
    metric_logscore = paste(
      "If a log_score is below the specified `min_log_score` value,",
      "it is replaced with `min_log_score`. \n",
      "The default value is -10, but you can modify it",
      "using the 'min_log_score' argument.",
      sep = " "
    ),
    model_list = function(model_id_list) {
      paste(
        "The available model IDs are:\n",
        paste("\t", model_id_list, collapse = "\n"),
        sprintf("\n(a total of %d models)\n", length(model_id_list))
      )
    },
    future_plan = paste(
      "Note: This function uses 'furrr' and 'future' for parallelization.",
      "To enable parallel execution, please set future::plan(multisession).",
      sep = "\n"
    ),
    lasomo_computational_time_warning = paste(
      "\033[31mWarning: The LASOMO algorithm can be computationally intensive,",
      "especially with a large number of models (> 12).",
      "\nConsider using the LOMO algorithm or reducing the number of models.",
      "\033[39m\n",
      sep = " "
    )
  )

  if (status %in% names(messages)) {
    msg <- messages[[status]]
    if (is.function(msg)) {
      message(msg(...))
    } else {
      message(msg)
    }
  } else {
    stop("Invalid status for send_message().")
  }
}
