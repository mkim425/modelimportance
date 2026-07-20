# Summary method for model importance score table

Summary method for model importance score table

## Usage

``` r
# S3 method for class 'model_imp_tbl'
summary(object, ...)
```

## Arguments

- object:

  An object of class `model_imp_tbl`.

- ...:

  Additional arguments passed to the print method.

## Value

A list of class `summary.model_imp_tbl` with four elements:

- all_models:

  A character vector of the unique model IDs present in `object`.

- all_tasks:

  A data frame with one row per unique prediction task (one column per
  task ID, e.g. `location`, `horizon`, `target_end_date`).

- model_summary:

  A data frame with one row per model and columns `model_id`, `n_tasks`
  (number of tasks the model was scored on), `min_importance`,
  `max_importance`, and `n_NA` (number of tasks with a missing
  importance score).

- task_winners:

  A data frame with one row per prediction task and columns for the task
  ID(s), `top_model` (the model with the highest importance score for
  that task), and `max_score` (that model's importance score).

This object is printed via
[`print.summary.model_imp_tbl`](https://mkim425.github.io/modelimportance/reference/print.summary.model_imp_tbl.md).
