# Example target data for modelimportance article vignette

A subset of `hubExamples::forecast_target_ts` matching the locations and
target end dates in `forecast_data_example`.

## Usage

``` r
target_data_example
```

## Format

A data frame with 8 rows and 4 columns:

- target_end_date:

  Date. Date of the observation.

- target:

  Character. Target name.

- location:

  Character. FIPS code for the US location.

- observation:

  Numeric. Observed value.

## Source

Sourced from `hubExamples::forecast_target_ts` (hubverse-org/hubExamples
v1.0.0).
