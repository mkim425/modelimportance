# Forecast outputs for Massachusetts, horizon 1, used in vignette runtime data

A subset of `hubExamples::forecast_outputs` filtered to median output
type, location "25" (Massachusetts), horizon 1, and target end date
2022-12-24.

## Usage

``` r
forecast_data_ma_h1
```

## Format

A data frame (model_out_tbl) with 3 rows and 9 columns:

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
