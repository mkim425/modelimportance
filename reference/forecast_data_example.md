# Example forecast outputs for modelimportance article vignette

A subset of `hubExamples::forecast_outputs` filtered to quantile output
type and location "25" (Massachusetts), covering the 2022/23 influenza
season (from "2022-11-19" to "2023-01-07").

## Usage

``` r
forecast_data_example
```

## Format

A data frame (model_out_tbl) with 168 rows and 9 columns:

- model_id:

  Character. Unique model identifier.

- reference_date:

  Date. Date the forecast was generated.

- target:

  Character. Forecast target.

- horizon:

  Integer. Number of weeks ahead.

- location:

  Character. FIPS code for the US location.

- target_end_date:

  Date. Target date for the forecast.

- output_type:

  Character. Prediction representation type.

- output_type_id:

  Character. Identifier for the output type level.

- value:

  Numeric. Predicted value.

## Source

Sourced from `hubExamples::forecast_outputs` (hubverse-org/hubExamples
v1.0.0).
