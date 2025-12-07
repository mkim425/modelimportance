# inst Directory

## `get-testdata` 

This directory contains scripts to generate test datasets used in the package's 
unit tests. The test datasets are stored in the sub-directories under 
`tests/testthat/testdata`, with each subdirectory named after the function being
tested.

* `complex_hub.R` generates test data for the `validate_input_data()` and 
`split_data_by_task()` functions.
* Test data sets for the `compute_importance()` function are generated from:
  * four top-level scripts matching the pattern `exp_imp_*_lomo.R`
  * scripts under `exp_imp_*_lasomo/` directories
  * a helper script, `helper-exp_imp.R`
* Test data for the `model_performance()` function is generated from: 
  * `forecast-target-datasets.R`
  * `exp_overall_imp.R`
  * a helper script, `helper-exp_overall_imp.R`
