## Data for testing the score_untrained example: median output

## target data
target_data_median <- readRDS(
  testthat::test_path("testdata/flu_example_target_data.rds")
) |> mutate(target_end_date = as.Date(target_end_date))

## forecast data with median output
forecast_median <- readRDS(
  testthat::test_path("testdata/flu_example_median_model_output.rds")
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

model_id_list <- unique(dat_median$model_id)
# find index of target_data corresponding to dat
idx <- with(
  target_data_median,
  target_end_date == unique(dat_median$target_end_date) &
    output_type == unique(dat_median$output_type) &
    location == unique(dat_median$location)
)
# replace the target oracle value with a simple value
target_data_median$oracle_value[idx] <- 10

## Case 1: no missing data and 'simple_ensemble' using agg_fun = mean
# ensemble built from all models
ens_df_simple <- simple_ensemble(
  dat_median,
  model_id = "ens_all", agg_fun = "mean"
)
# construct ensembles without each model
model_names <- dat_median$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_median[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "mean"
  )
  ens_df_simple <- bind_rows(ens_df_simple, sub_ens)
}
# evaluate each ensemble
ens_df_simple <- score_model_out(ens_df_simple,
  target_data_median,
  metrics = "ae_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = ae_point - ae_point[1]) |>
  filter(model_id != "ens_all")
ens_df_simple$model_id <- sub("^ens_wo_", "", ens_df_simple$model_id)
# expected importance scores with median output and linear pool
exp_imp_median1 <- ens_df_simple |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lomo",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()

## Case 2: no missing data and 'simple_ensemble' using agg_fun = median
# ensemble built from all models
ens_df_simple <- simple_ensemble(
  dat_median,
  model_id = "ens_all", agg_fun = "median"
)
# construct ensembles without each model
model_names <- dat_median$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_median[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "median"
  )
  ens_df_simple <- bind_rows(ens_df_simple, sub_ens)
}
# evaluate each ensemble
ens_df_simple <- score_model_out(ens_df_simple,
  target_data_median,
  metrics = "ae_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = ae_point - ae_point[1]) |>
  filter(model_id != "ens_all")
ens_df_simple$model_id <- sub("^ens_wo_", "", ens_df_simple$model_id)
# expected importance scores with median output and linear pool
exp_imp_median2 <- ens_df_simple |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "simple_ensemble-median",
    algorithm = "lomo",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()

## Case 3: a missing data and 'simple_mean' ensemble
# data with missing values
sub_dat_median <- dat_median |> filter(model_id %in% model_id_list[c(1, 3)])
# ensemble built from all models
ens_df <- simple_ensemble(sub_dat_median, model_id = "ens_all")
# construct ensembles without each model
model_names <- sub_dat_median$model_id
for (i in seq_along(model_names)) {
  sub_data <- sub_dat_median[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  ens_df <- bind_rows(ens_df, sub_ens)
}
# evaluate each ensemble
ens_df <- score_model_out(ens_df,
  target_data_median,
  metrics = "ae_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = ae_point - ae_point[1]) |>
  filter(model_id != "ens_all")
ens_df$model_id <- sub("^ens_wo_", "", ens_df$model_id)
exp_imp_median3 <- data.frame(model_id = model_id_list) |>
  left_join(ens_df, by = "model_id") |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "simple_mean",
    algorithm = "lomo",
    test_purp = "missing data"
  ) |>
  as.data.frame()

# combine the expected importance scores
exp_imp_median <- rbind(
  exp_imp_median1, exp_imp_median2, exp_imp_median3
)

# save data
saveRDS(dat_median,
  file = "tests/testthat/testdata/dat_median.rds"
)
saveRDS(exp_imp_median,
  file = "tests/testthat/testdata/exp_imp_median_untrained_lomo.rds"
)
saveRDS(target_data_median,
  file = "tests/testthat/testdata/target_median.rds"
)
