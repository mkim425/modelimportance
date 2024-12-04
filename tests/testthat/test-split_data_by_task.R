library(dplyr)

# data for testing
target_data <- readRDS(
  testthat::test_path("testdata/flu_example_target_data.rds")
)

forecast_quantiles <- readRDS(
  testthat::test_path("testdata/flu_example_quantile_model_output.rds")
)

valid_tbl <- validate_input_data(forecast_quantiles, target_data)
result <- split_data_by_task(valid_tbl)

split_cols <- c("horizon", "location", "target_end_date")

test_that("split_data_by_task() groups data correctly", {
  # The result is a list
  expect_type(result, "list")

  # The list length matches the number of unique combinations of split_cols
  expect_equal(
    length(result),
    valid_tbl |>
      dplyr::select(all_of(split_cols)) |>
      dplyr::reframe(across(everything(), as.character)) |>
      dplyr::summarise(n_distinct(do.call(paste, c(., sep = "")))) |>
      as.numeric()
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
