% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_data_by_task.R
\name{split_data_by_task}
\alias{split_data_by_task}
\title{Split the input data by a single task.}
\usage{
split_data_by_task(valid_tbl)
}
\arguments{
\item{valid_tbl}{A data.frame containing forecast data and target data,
processed by the function `validate_input_data()`.}
}
\value{
A list of data sets, each corresponding to a single task.
}
\description{
This function splits the input data by a single task with a
combination of horizon, location, and target_end_date
}
\examples{
\dontrun{
library(dplyr)
library(hubExamples)
forecast_data <- hubExamples::forecast_outputs |>
  dplyr::filter(
    output_type \%in\% c("quantile"),
    location == "25",
    horizon == 1
  )
target_data <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    date \%in\% unique(forecast_data$target_end_date),
    location == "25"
  ) |>
  # Rename columns to match the oracle output format
  rename(
    target_end_date = date,
    oracle_value = observation
  )
valid_tbl <- validate_input_data(forecast_data, target_data)
split_data_by_task(valid_tbl)
}
}
