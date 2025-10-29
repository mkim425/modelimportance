## Get data for testing the score_untrained.R by output type
# load the package to make its internal functions available
devtools::load_all()

##### mean output ########################################
# target data
target_data_mean <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_target_data.rds"
  )
) |> mutate(target_end_date = as.Date(target_end_date))
# forecast data with mean output
forecast_mean <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_mean_model_output.rds"
  )
)
valid_tbl_mean <- validate_input_data(forecast_mean, target_data_mean)
mean_data_list <- split_data_by_task(valid_tbl_mean,
  weighted = FALSE,
  training_window_length = 0
)
# replace with simple values for easy calculation
dat_mean1 <- mean_data_list[[16]]
dat_mean <- rbind(dat_mean1, dat_mean1[1:2, ])
dat_mean$model_id <- c(dat_mean1$model_id, "fake-mod1", "fake-mod2")
dat_mean$value <- c(30, 12, 18, 6, 6)

# find index of target_data corresponding to dat
idx <- with(
  target_data_mean,
  target_end_date == unique(dat_mean$target_end_date) &
    output_type == unique(dat_mean$output_type) &
    location == unique(dat_mean$location)
)
# replace the target oracle value with a simple value
target_data_mean$oracle_value[idx] <- 10

# save data
saveRDS(dat_mean,
  file = "tests/testthat/testdata/for-score_untrained/dat_mean.rds"
)

saveRDS(
  target_data_mean |> filter(
    target_end_date %in% unique(dat_mean$target_end_date) &
      output_type == unique(dat_mean$output_type) &
      location %in% unique(dat_mean$location)
  ),
  file = "tests/testthat/testdata/for-score_untrained/target_mean.rds"
)

##### median output ########################################
# target data
target_data_median <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_target_data.rds"
  )
) |> mutate(target_end_date = as.Date(target_end_date))
# forecast data with median output
forecast_median <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_median_model_output.rds"
  )
)
valid_tbl_median <- validate_input_data(forecast_median, target_data_median)
median_data_list <- split_data_by_task(valid_tbl_median,
  weighted = FALSE,
  training_window_length = 0
)
# replace with simple values for easy calculation
dat_median1 <- median_data_list[[16]]
dat_median <- rbind(dat_median1, dat_median1[1:2, ])
dat_median$model_id <- c(dat_median1$model_id, "fake-mod1", "fake-mod2")
dat_median$value <- c(30, 12, 18, 6, 6)

# find index of target_data corresponding to dat
idx <- with(
  target_data_median,
  target_end_date == unique(dat_median$target_end_date) &
    output_type == unique(dat_median$output_type) &
    location == unique(dat_median$location)
)
# replace the target oracle value with a simple value
target_data_median$oracle_value[idx] <- 10
# save data
saveRDS(dat_median,
  file = "tests/testthat/testdata/for-score_untrained/dat_median.rds"
)

saveRDS(
  target_data_median |> filter(
    target_end_date %in% unique(dat_median$target_end_date) &
      output_type == unique(dat_median$output_type) &
      location %in% unique(dat_median2$location)
  ),
  file = "tests/testthat/testdata/for-score_untrained/target_median.rds"
)
##### quantile output ########################################
# target data
target_data_qntl <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_target_data.rds"
  )
) |> mutate(target_end_date = as.Date(target_end_date))

## forecast data with quantile output
forecast_qntl <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_quantile_model_output.rds"
  )
)
valid_tbl_qntl <- validate_input_data(forecast_qntl, target_data_qntl)
qntl_data_list <- split_data_by_task(valid_tbl_qntl,
  weighted = FALSE,
  training_window_length = 0
)
dat_qntl <- qntl_data_list[[16]]
# save data
saveRDS(dat_qntl,
  file = "tests/testthat/testdata/dat_qntl.rds"
)

saveRDS(
  target_data_qntl |> filter(
    target_end_date %in% unique(dat_qntl$target_end_date) &
      output_type == unique(dat_qntl$output_type) &
      location %in% unique(dat_qntl$location)
  ),
  file = "tests/testthat/testdata/for-score_untrained/target_qntl.rds"
)
##### pmf output ########################################
# target data
target_data_pmf <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_target_data.rds"
  )
) |> mutate(target_end_date = as.Date(target_end_date))
# forecast data with pmf output
forecast_pmf <- readRDS(
  testthat::test_path(
    "testdata/for-validate_input_data/flu_example_pmf_model_output.rds"
  )
)
valid_tbl_pmf <- validate_input_data(forecast_pmf, target_data_pmf)
pmf_data_list <- split_data_by_task(valid_tbl_pmf,
  weighted = FALSE,
  training_window_length = 0
)
dat_pmf <- pmf_data_list[[16]]

# save data
saveRDS(dat_pmf,
  file = "tests/testthat/testdata/for-score_untrained/dat_pmf.rds"
)
saveRDS(
  target_data_pmf |> filter(
    target_end_date %in% unique(dat_pmf$target_end_date) &
      output_type == unique(dat_pmf$output_type) &
      location %in% unique(dat_pmf$location)
  ),
  file = "tests/testthat/testdata/for-score_untrained/target_pmf.rds"
)
