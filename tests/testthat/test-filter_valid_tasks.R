library(dplyr)

forecast_data <- hubExamples::forecast_outputs |>
  dplyr::filter(
    output_type %in% c("quantile"),
    location == "25",
    horizon == 1
  )
target_data <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    target_end_date %in% unique(forecast_data$target_end_date),
    location == "25"
  ) |>
  # Rename columns to match the oracle output format
  rename(
    oracle_value = observation
  )

valid_tbl <- validate_input_data(forecast_data, target_data)

test_that("filter_valid_tasks() works as expected", {
  # Case 1: All tasks valid
  df_list_by_task <- split_data_by_task(valid_tbl)
  result_all_valid <- filter_valid_tasks(df_list_by_task, min_models = 2)
  expect_equal(length(result_all_valid), length(df_list_by_task))

  # Case 2: Some tasks invalid
  # Create a modified df_list_by_task with one task having only one model
  modified_tbl_some_invalid <- valid_tbl |>
    filter(model_id != "PSI-DICE") |>
    filter(!(model_id == "MOBS-GLEAM_FLUH" & target_end_date == "2022-12-24"))
  modified_df_list_some_invalid <- split_data_by_task(modified_tbl_some_invalid)
  result_some_invalid <- filter_valid_tasks(modified_df_list_some_invalid,
    min_models = 2
  )
  expect_true(
    0 < length(result_some_invalid),
    length(result_some_invalid) < length(modified_df_list)
  )

  # Case 3: All tasks invalid
  # Create a modified df_list_by_task with all tasks having only one model
  modified_tbl_all_invalid <- valid_tbl |>
    filter(model_id != "PSI-DICE") |>
    filter(!(
      model_id == "MOBS-GLEAM_FLUH" &
        target_end_date == "2022-12-24"
    )) |>
    filter(!(
      model_id == "Flusight-baseline" &
        target_end_date == "2022-11-26"
    ))

  modified_df_list_all_invalid <- split_data_by_task(modified_tbl_all_invalid)
  expect_error(
    filter_valid_tasks(modified_df_list_all_invalid, min_models = 2),
    "No valid tasks with at least 2 distinct models to evaluate."
  )
})
