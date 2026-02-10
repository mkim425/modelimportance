# modelimportance

## Overview

The goal of modelimportance is to provide tools to quantify each
individual model’s contribution to an ensemble model’s predictive
performance. Importance scores for each ensemble member are computed
based on their impact on the ensemble’s accuracy, helping users
understand which models are most influential in improving the ensemble’s
predictions. The package is designed to work with the standard S3 class
model output format defined by the
[hubverse](https://docs.hubverse.io/en/latest/) convention.

## Installation

You can install the development version of modelimportance from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("mkim425/modelimportance")
```

## Getting started

``` r
library(modelimportance)
```

modelimportance provides two main functions:

- [`model_importance()`](https://mkim425.github.io/modelimportance/reference/model_importance.md)
  calculates the importance score for each model in an ensemble for
  individual prediction tasks.

- [`model_importance_summary()`](https://mkim425.github.io/modelimportance/reference/model_importance_summary.md)
  summarizes these importance scores across multiple tasks to provide an
  overall importance score for each model.
