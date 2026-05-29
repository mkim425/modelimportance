# Target data for Massachusetts used in vignette runtime data

A subset of `hubExamples::forecast_target_ts` for location "25"
(Massachusetts) and target end date 2022-12-24.

## Usage

``` r
target_data_ma
```

## Format

A data frame with 1 row and 4 columns:

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
