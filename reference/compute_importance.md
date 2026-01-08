# Calculate model importance for a single task

Evaluate the importance of ensemble component models by quantifying
their contribution to the prediction accuracy of an ensemble for each
combination of model task.

## Usage

``` r
compute_importance(
  single_task_data,
  oracle_output_data,
  model_id_list,
  ensemble_fun,
  importance_algorithm,
  subset_wt,
  metric,
  min_log_score,
  ...
)
```

## Arguments

- single_task_data:

  A data.frame with the predictions for a single forecast task in a
  model_out_tbl format. The data must contain only one `output_type`,
  which must be one of the following: `mean`, `median`, `quantile`, or
  `pmf`.

- oracle_output_data:

  Ground truth data for the variables that are used to define modeling
  targets. This data must follow the oracle output format. See
  'Details'.

- model_id_list:

  A list of all component model IDs to be used in the ensemble. If a
  model is not present in the `single_task_data`, it means that the
  model did not submit predictions for the given task. This list is used
  to identify missing models in the ensemble.

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

- metric:

  A character string specifying the metric to be used for scoring the
  model output. The metric is determined by the `output_type` and must
  be one of the following: `se_point`, `ae_point`, `wis`, or
  `log_score`. Note that for the `mean` output type, `se_point` is used
  by default, but we convert it to `rse_point` to ensure consistency in
  the units of importance score.

- min_log_score:

  A numeric value specifying a minimum threshold for log scores for the
  `pmf` output to avoid issues with extremely low probabilities assigned
  to the true outcome, which can lead to undefined or negative infinite
  log scores. Any probability lower than this threshold will be adjusted
  to this minimum value. The default value is set to -10, which is an
  arbitrary choice. Users may choose a different value based on their
  practical needs.

- ...:

  Optional arguments passed to `ensemble_fun` when it is specified as
  `"simple_ensemble"`. See 'Details'.

## Value

A data.frame with columns `task_id`, `output_type`, `model_id`, (task
level) `importance`.

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
