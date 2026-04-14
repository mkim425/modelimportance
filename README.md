
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modelimportance

<!-- badges: start -->

[![R-CMD-check](https://github.com/mkim425/modelimportance/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mkim425/modelimportance/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

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

The main function in the package is `model_importance()`, which calculates the importance score for each model in an ensemble for individual prediction tasks.
The output of `model_importance()` is an S3 object of class `model_imp_tbl`. 
This object can be further analyzed and visualized using various methods such as `print()`, `summary()`, `plot()`, and `aggregate()`, which offer different ways to visualize and interpret the importance scores.

## Learn more

Learn more about this package and how to use it in the
[vignette](https://mkim425.github.io/modelimportance/articles/get-started.html).
The vignette provides detailed examples and theoretical background on
the algorithms implemented in the package.
