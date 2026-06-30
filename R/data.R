#' Raw forecast outputs for get-started vignette
#'
#' A subset of `hubExamples::forecast_outputs` filtered to median output type
#' and two target end dates (2022-11-26, 2022-12-10).
#'
#' @format A data frame (model_out_tbl) with 12 rows and 9 columns:
#' \describe{
#'   \item{model_id}{Character. Unique model identifier.}
#'   \item{reference_date}{Date. Date the forecast was generated.}
#'   \item{target}{Character. Forecast target.}
#'   \item{horizon}{Integer. Number of weeks ahead.}
#'   \item{location}{Character. FIPS code for the US location.}
#'   \item{target_end_date}{Date. Target date for the forecast.}
#'   \item{output_type}{Character. Prediction representation type.}
#'   \item{output_type_id}{Character. Identifier for the output type level.}
#'   \item{value}{Numeric. Predicted value.}
#' }
#' @source Sourced from `hubExamples::forecast_outputs`
#' (hubverse-org/hubExamples v1.0.0).
"forecast_data_raw"

#' Raw target data for get-started vignette
#'
#' A subset of `hubExamples::forecast_target_ts` matching the locations and
#' target end dates in `forecast_data_raw`, with the observed values column
#' renamed to `oracle_value`.
#'
#' @format A data frame with 4 rows and 4 columns:
#' \describe{
#'   \item{target_end_date}{Date. Date of the observation.}
#'   \item{target}{Character. Target name.}
#'   \item{location}{Character. FIPS code for the US location.}
#'   \item{oracle_value}{Numeric. Observed value.}
#' }
#' @source Sourced from `hubExamples::forecast_target_ts`
#' (hubverse-org/hubExamples v1.0.0).
"target_data_raw"

#' Example forecast outputs for modelimportance article vignette
#'
#' A subset of `hubExamples::forecast_outputs` filtered to quantile output type
#' and location "25" (Massachusetts), covering the 2022/23 influenza season
#' (from "2022-11-19" to "2023-01-07").
#'
#' @format A data frame (model_out_tbl) with 168 rows and 9 columns:
#' \describe{
#'   \item{model_id}{Character. Unique model identifier.}
#'   \item{reference_date}{Date. Date the forecast was generated.}
#'   \item{target}{Character. Forecast target.}
#'   \item{horizon}{Integer. Number of weeks ahead.}
#'   \item{location}{Character. FIPS code for the US location.}
#'   \item{target_end_date}{Date. Target date for the forecast.}
#'   \item{output_type}{Character. Prediction representation type.}
#'   \item{output_type_id}{Character. Identifier for the output type level.}
#'   \item{value}{Numeric. Predicted value.}
#' }
#' @source Sourced from `hubExamples::forecast_outputs`
#' (hubverse-org/hubExamples v1.0.0).
"forecast_data_example"

#' Example target data for modelimportance article vignette
#'
#' A subset of `hubExamples::forecast_target_ts` matching the locations and
#' target end dates in `forecast_data_example`.
#'
#' @format A data frame with 8 rows and 4 columns:
#' \describe{
#'   \item{target_end_date}{Date. Date of the observation.}
#'   \item{target}{Character. Target name.}
#'   \item{location}{Character. FIPS code for the US location.}
#'   \item{observation}{Numeric. Observed value.}
#' }
#' @source Sourced from `hubExamples::forecast_target_ts`
#' (hubverse-org/hubExamples v1.0.0).
"target_data_example"

#' Forecast outputs for Massachusetts, horizon 1, used in vignette runtime data
#'
#' A subset of `hubExamples::forecast_outputs` filtered to median output type,
#' location "25" (Massachusetts), horizon 1, and target end date 2022-12-24.
#'
#' @format A data frame (model_out_tbl) with 3 rows and 9 columns:
#' \describe{
#'   \item{model_id}{Character. Unique model identifier.}
#'   \item{reference_date}{Date. Date the forecast was generated.}
#'   \item{target}{Character. Forecast target.}
#'   \item{horizon}{Integer. Number of weeks ahead.}
#'   \item{location}{Character. FIPS code for the US location.}
#'   \item{target_end_date}{Date. Target date for the forecast.}
#'   \item{output_type}{Character. Prediction representation type.}
#'   \item{output_type_id}{Character. Identifier for the output type level.}
#'   \item{value}{Numeric. Predicted value.}
#' }
#' @source Sourced from `hubExamples::forecast_outputs`
#' (hubverse-org/hubExamples v1.0.0).
"forecast_data_ma_h1"

#' Target data for Massachusetts used in vignette runtime data
#'
#' A subset of `hubExamples::forecast_target_ts` for location "25"
#' (Massachusetts) and target end date 2022-12-24.
#'
#' @format A data frame with 1 row and 4 columns:
#' \describe{
#'   \item{target_end_date}{Date. Date of the observation.}
#'   \item{target}{Character. Target name.}
#'   \item{location}{Character. FIPS code for the US location.}
#'   \item{observation}{Numeric. Observed value.}
#' }
#' @source Sourced from `hubExamples::forecast_target_ts`
#' (hubverse-org/hubExamples v1.0.0).
"target_data_ma"
