library(dplyr)

test_that("split_data_by_task() groups data correctly", {
  forecast_data <- readRDS(
    system.file("testdata",
      "simple_example_quantile_model_output.rds",
      package = "modelimportance"
    )
  ) |>
    valid_input_data()

  task_id_cols <- setdiff(
    colnames(forecast_data),
    c("model_id", "output_type", "output_type_id", "value")
  )

  result <- split_data_by_task(forecast_data)

  # The result is a list
  expect_type(result, "list")

  # The list length matches the number of unique combinations of task_ids
  expect_equal(
    length(result),
    forecast_data %>%
      dplyr::select(all_of(task_id_cols)) %>%
      dplyr::reframe(across(everything(), as.character)) %>%
      dplyr::summarise(n_distinct(do.call(paste, c(., sep = "")))) %>%
      as.numeric()
  )

  # Each element of the list is a data frame
  expect_true(all(sapply(result, is.data.frame)))

  # Each data set has unique task_id
  expect_true(all(
    sapply(
      result, function(x) {
        task_id <- setdiff(
          colnames(x),
          c("model_id", "output_type", "output_type_id", "value")
        )
        a <- x %>%
          dplyr::select(all_of(task_id)) %>%
          dplyr::summarise_all(n_distinct) %>%
          unlist() %>%
          unname()
        setequal(a, rep(1, length(task_id)))
      }
    )
  ))
})
