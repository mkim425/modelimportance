library(dplyr)

# data for testing
target_data <- readRDS(
  testthat::test_path("testdata/flu_example_target_data.rds")
)

forecast_quantiles <- readRDS(
  testthat::test_path("testdata/flu_example_quantile_model_output.rds")
)

valid_tbl <- validate_input_data(forecast_quantiles, target_data)

## commonly required columns
required_cols <- c(
  "reference_date", "model_id", "output_type", "output_type_id", "value"
)
## task specific columns
split_cols <- setdiff(colnames(valid_tbl), required_cols)

test_that("split_data_by_task() groups data correctly for untrained ensemble", {
  result <- split_data_by_task(valid_tbl,
    weighted = FALSE,
    training_window_length = 0
  )

  # The result is a list
  expect_type(result, "list")

  # The list length matches the number of unique combinations of split_cols
  expect_equal(
    length(result),
    valid_tbl |>
      dplyr::select(all_of(split_cols)) |>
      dplyr::mutate(across(everything(), as.character)) |>
      dplyr::rowwise() |>
      dplyr::mutate(combined = paste(c_across(everything()), collapse = "")) |>
      dplyr::pull(combined) |>
      unique() |>
      length()
  )

  # Each element of the list is a data frame
  expect_true(all(sapply(result, is.data.frame)))

  # Each data set has unique task_id
  expect_true(all(
    sapply(
      result, function(x) {
        split_cols <- c("horizon", "location", "target_end_date")
        a <- x |>
          dplyr::select(all_of(split_cols)) |>
          dplyr::summarise_all(n_distinct) |>
          unlist() |>
          unname()
        setequal(a, rep(1, length(split_cols)))
      }
    )
  ))
})

test_that("split_data_by_task() groups data correctly for trained ensemble", {
  training_window_length <- 2
  result <- split_data_by_task(valid_tbl,
    weighted = TRUE,
    training_window_length
  )

  # The result is a list
  expect_type(result, "list")

  # Each element of the list is a data frame
  expect_true(all(sapply(result, is.data.frame)))

  # Check the number of dataset splits
  expect_equal(
    length(result),
    length(unique(valid_tbl$reference_date)) - training_window_length
  )
})

test_that("split_data_by_task() throws an error", {
  training_window_length <- 5
  expect_error(
    split_data_by_task(valid_tbl, weighted = TRUE, training_window_length),
    "The number of reference_date must greater than the training window
        length."
  )
})
