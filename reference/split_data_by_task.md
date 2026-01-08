# Split the input data by a single task and make a list of data sets

This function splits the input data by a single task with a combination
of task IDs and returns a list of data sets, each corresponding to a
single task.

## Usage

``` r
split_data_by_task(valid_tbl)
```

## Arguments

- valid_tbl:

  A data.frame containing forecast data and target data, processed by
  the function `validate_input_data()`.

## Value

A list of data sets, each corresponding to a single task.
