# Raw target data for get-started vignette

A subset of `hubExamples::forecast_target_ts` matching the locations and
target end dates in `forecast_data_raw`, with the observed values column
renamed to `oracle_value`.

## Usage

``` r
target_data_raw
```

## Format

A data frame with 4 rows and 4 columns:

- target_end_date:

  Date. Date of the observation.

- target:

  Character. Target name.

- location:

  Character. FIPS code for the US location.

- oracle_value:

  Numeric. Observed value.

## Source

Sourced from `hubExamples::forecast_target_ts` (hubverse-org/hubExamples
v1.0.0).
