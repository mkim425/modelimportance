# Quantifies the contribution of ensemble component models to ensemble prediction accuracy for each prediction task.

We measure each ensemble component model's contribution to the ensemble
prediction accuracy for each model task. (See also
[`model_importance_summary`](https://mkim425.github.io/modelimportance/reference/model_importance_summary.md)
for a summary of importance scores across multiple tasks.)

This function requires that one column represent the forecast date (or a
date from which each forecast originates or is made in reference to) and
that column be named one of `forecast_date`, `origin_date`, and
`reference_date`.

For each `output_type`, the corresponding scoring rule applied to
calculate the importance is as follows.

|                 |                  |                                                                              |
|-----------------|------------------|------------------------------------------------------------------------------|
| **Output Type** | **Scoring Rule** | **Description**                                                              |
| mean            | rse_point        | evaluate using the root squared error                                        |
| median          | ae_point         | evaluate using the absolute error                                            |
| quantile        | wis              | evaluate using the weighted interval score                                   |
| pmf             | log_score        | evaluate using the logarithm of the probability assigned to the true outcome |

## Usage

``` r
model_importance(
  forecast_data,
  oracle_output_data,
  ensemble_fun = c("simple_ensemble", "linear_pool"),
  importance_algorithm = c("lomo", "lasomo"),
  subset_wt = c("equal", "perm_based"),
  min_log_score = -10,
  ...
)
```

## Arguments

- forecast_data:

  A data.frame with the predictions that is or can be coerced to a
  `model_out_tbl` format, which is the standard S3 class model output
  format defined by the 'hubverse' convention
  (https://docs.hubverse.io/en/latest/#). If it fails to be coerced to a
  `model_out_tbl` format, an error message will be returned. Only one
  `output_type` is allowed in the data.frame, and it must be one of the
  following: `mean`, `median`, `quantile`, or `pmf`.

- oracle_output_data:

  Ground truth data for the variables that are used to define modeling
  targets. This data must follow the oracle output format. See
  'Details'.

- ensemble_fun:

  A character string specifying a ensemble method, either
  "simple_ensemble" or "linear_pool";
  `c("simple_ensemble", "linear_pool")`.

  - When `"simple_ensemble"` is specified, the ensemble is generated
    using the optional `agg_fun` function in `...` (see 'Details').

  - When `"linear_pool"` is specified, ensemble model outputs are
    created as a linear pool of component model outputs. This method
    supports only an `output_type` of `mean`, `quantile`, or `pmf`.

- importance_algorithm:

  A character string specifying algorithm for model importance
  calculation; `c("lomo", "lasomo")`. `"lomo"` stands for
  leave-one-model-out and `"lasomo"` stands for leave all subsets of
  models out. For `"lasomo"`, `'furrr'` and `'future'` packages need to
  be installed for parallel execution.

- subset_wt:

  A character string specifying method for assigning weight to subsets
  when using `lasomo` algorithm; `c("equal", "perm_based")`.

  - `"equal"` assigns equal weight to all subsets.

  - `"perm_based"` assigns weight averaged over all possible
    permutations as in the Shapley value. Ignored if `lomo` method is
    used. Default is `"equal"`, if not specified.

- min_log_score:

  A numeric value specifying a minimum threshold for log scores for the
  `pmf` output. This threshold prevents issues with extremely low
  probabilities assigned to the true outcome, which would otherwise lead
  to undefined or negative infinite log scores. Any probability lower
  than this threshold will be adjusted to this minimum value. The
  default value is set to -10, following the CDC FluSight thresholding
  convention. Users may choose a different value based on their
  practical needs.

- ...:

  Optional arguments passed to `ensemble_fun` when it is specified as
  `"simple_ensemble"`. See 'Details'.

## Value

A `model_imp_tbl` S3 class object with columns `model_id`,
`reference_date`, `output_type`, and `importance`, along with any task
ID columns (e.g., `location`, `horizon`, and `target_end_date`) present
in the input `forecast_data`. Note that `reference_date` is used as the
name for the forecast date column, regardless of its original name in
the input `forecast_data`.

## Details

The `oracle_output_data` is a data frame that contains the ground truth
values for the variables used to define modeling targets. It is referred
to as “oracle” because it is formatted as if an oracle made a perfect
point prediction equal to the truth. This data must follow the oracle
output format defined in the hubverse standard, which includes
independent task ID columns (e.g., `location`, `target_date`), the
`output_type` column specifying the output type of the predictions and
an `oracle_value` column for the observed values. As in the forecast
data, if the `output_type` is either `"quantile"` or `"pmf"`, the
`output_type_id` column is often required to provide further identifying
information.

The `model_out_tbl` and `oracle_output_data` must have the same task ID
columns and `output_type`, including `output_type_id` if necessary,
which are used to match the predictions with the ground truth data.

Additional argument in `...` is `agg_fun`, which is a character string
name for a function specifying aggregation method of component model
outputs. Default is `mean`, indicating that equally weighted mean is
calculated across all component model outputs for each unique
`output_type_id`. This can be `median` or a custom function (e.g.,
geometric_mean. Details can be found in
https://hubverse-org.github.io/hubEnsembles/articles/hubEnsembles.html).

This function uses the `furrr` and `future` for parallelization. To
enable parallel execution, please set a parallel backend, e.g., via
[`future::plan()`](https://future.futureverse.org/reference/plan.html).

## See also

[`model_importance_summary`](https://mkim425.github.io/modelimportance/reference/model_importance_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
forecast_data <- hubExamples::forecast_outputs |>
  dplyr::filter(
    output_type %in% c("quantile"),
    location == "25",
    horizon == 1
  )
target_data <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    target_end_date %in% unique(forecast_data$target_end_date),
    location == "25"
  ) |>
  # Rename columns to match the oracle output format
  rename(
    oracle_value = observation
  )
# Example with the default arguments.
model_importance(
  forecast_data = forecast_data, oracle_output_data = target_data,
  ensemble_fun = "simple_ensemble", importance_algorithm = "lomo",
  subset_wt = "equal"
)
# Example with the additional argument in `...`.
model_importance(
  forecast_data = forecast_data, oracle_output_data = target_data,
  ensemble_fun = "simple_ensemble", importance_algorithm = "lomo",
  subset_wt = "equal", agg_fun = median
)
} # }
```
