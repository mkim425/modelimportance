## Data for testing the score_untrained example: mean output

## target data
target_data_mean <- readRDS(
  testthat::test_path("testdata/flu_example_target_data.rds")
) |> mutate(target_end_date = as.Date(target_end_date))

## forecast data with mean output
forecast_mean <- readRDS(
  testthat::test_path("testdata/flu_example_mean_model_output.rds")
)
valid_tbl_mean <- validate_input_data(forecast_mean, target_data_mean)
mean_data_list <- split_data_by_task(valid_tbl_mean,
  weighted = FALSE,
  training_window_length = 0
)
model_id_list <- unique(valid_tbl_mean$model_id)

# replace with simple values for easy calculation
dat_mean <- mean_data_list[[16]]
dat_mean$value <- c(30, 12, 18)
# find index of target_data corresponding to dat
idx <- with(
  target_data_mean,
  target_end_date == unique(dat_mean$target_end_date) &
    output_type == unique(dat_mean$output_type) &
    location == unique(dat_mean$location)
)
# replace the target oracle value with a simple value
target_data_mean$oracle_value[idx] <- 10

## Case 1: no missing data and 'linear pool' ensemble
# ensemble built from all models
ens_df <- linear_pool(dat_mean, model_id = "ens_all")
# construct ensembles without each model
model_names <- dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_mean[model_names != model_names[i], ]
  sub_ens <- linear_pool(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  ens_df <- bind_rows(ens_df, sub_ens)
}
# evaluate each ensemble
ens_df <- ens_df |>
  # calculate squared error
  mutate(se = (value - target_data_mean$oracle_value[idx])^2) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se - se[1]) |>
  filter(model_id != "ens_all")
ens_df$model_id <- sub("^ens_wo_", "", ens_df$model_id)
# expected importance scores with mean output and linear pool
exp_imp_mean1 <- ens_df |>
  select(model_id, importance) |>
  mutate(
    ens_fun = "linear_pool",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()

## Case 2: a missing data and 'linear pool' ensemble
# data with missing values
sub_dat_mean <- dat_mean[c(1, 3), ]
# ensemble built from all models
ens_df <- linear_pool(sub_dat_mean, model_id = "ens_all")
# construct ensembles without each model
model_names <- sub_dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- sub_dat_mean[model_names != model_names[i], ]
  sub_ens <- linear_pool(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  ens_df <- bind_rows(ens_df, sub_ens)
}
# evaluate each ensemble
ens_df <- ens_df |>
  # calculate squared error
  mutate(se = (value - target_data_mean$oracle_value[idx])^2) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se - se[1]) |>
  filter(model_id != "ens_all")
ens_df$model_id <- sub("^ens_wo_", "", ens_df$model_id)
exp_imp_mean2 <- data.frame(model_id = model_id_list) |>
  left_join(ens_df, by = "model_id") |>
  select(model_id, importance) |>
  mutate(
    ens_fun = "linear_pool",
    test_purp = "missing data"
  ) |>
  as.data.frame()

# combine the expected importance scores
exp_imp_mean <- rbind(exp_imp_mean1, exp_imp_mean2)
