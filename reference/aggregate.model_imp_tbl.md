# Aggregate model importance scores across tasks to compute overall importance for each model

Aggregate model importance scores across tasks to compute overall
importance for each model

## Usage

``` r
# S3 method for class 'model_imp_tbl'
aggregate(
  x,
  by = "model_id",
  na_action = c("drop", "worst", "average"),
  fun = mean,
  ...
)
```

## Arguments

- x:

  An object of class `model_imp_tbl`.

- by:

  A character vector with column names specifying the grouping
  variable(s) for summarization. Default is `"model_id"`, which
  summarizes importance scores for each model across all tasks.

- na_action:

  A character string specifying how to handle `NA` values generated
  during importance score calculation for each task, occurring when a
  model did not contribute to the ensemble prediction for a given task
  by missing its forecast submission. Three options are available:
  `c("drop", "worst", "average")`. For each specific prediction task,
  each option works as follows:

  - `"drop"` removes `NA`s.

  - `"worst"` replaces `NA`s with the smallest value among importance
    metrics of the other models.

  - `"average"` replaces `NA`s with the average value from the other
    models' importance metrics.

- fun:

  A function used to summarize importance scores. Default is
  [`mean()`](https://rdrr.io/r/base/mean.html)

- ...:

  Additional arguments passed to the summary function `fun`. (e.g.,
  `fun = quantile, probs = 0.25` for a quartile summary)

## Value

A data frame with columns `model_id` and `importance_score_<fun>`, where
`<fun>` is the name of the summary function used (e.g.,
`importance_score_mean` when `fun = mean`). The output is sorted in
descending order of the summary importance scores.

## Details

This method extends
[`stats::aggregate`](https://rdrr.io/r/stats/aggregate.html) for objects
of class `model_imp_tbl`.
