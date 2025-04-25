## Data for testing the score_untrained example: mean output
# load the package to make its internal functions available
devtools::load_all()
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

# replace with simple values for easy calculation
dat_mean1 <- mean_data_list[[16]]
dat_mean <- rbind(dat_mean1, dat_mean1[1:2, ])
dat_mean$model_id <- c(dat_mean1$model_id, "fake-mod1", "fake-mod2")
dat_mean$value <- c(30, 12, 18, 6, 6)

model_id_list <- unique(dat_mean$model_id)
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
ens_df <- score_model_out(ens_df,
  target_data_mean,
  metrics = "se_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se_point - se_point[1]) |>
  filter(model_id != "ens_all")
ens_df$model_id <- sub("^ens_wo_", "", ens_df$model_id)

# expected importance scores with mean output and linear pool
exp_imp_mean1 <- ens_df |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "linear_pool-NA",
    algorithm = "lomo",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()

## Case 2: no missing data and 'simple_ensemble' using agg_fun = mean
# ensemble built from all models
ens_df_simple <- simple_ensemble(
  dat_mean,
  model_id = "ens_all", agg_fun = "mean"
)
# construct ensembles without each model
model_names <- dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_mean[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "mean"
  )
  ens_df_simple <- bind_rows(ens_df_simple, sub_ens)
}
# evaluate each ensemble
ens_df_simple <- score_model_out(ens_df_simple,
  target_data_mean,
  metrics = "se_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se_point - se_point[1]) |>
  filter(model_id != "ens_all")
ens_df_simple$model_id <- sub("^ens_wo_", "", ens_df_simple$model_id)
# expected importance scores with mean output and simple mean ensemble
exp_imp_mean2 <- ens_df_simple |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lomo",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()

## Case 3: no missing data and 'simple_ensemble' using agg_fun = median
# ensemble built from all models
ens_df_simple <- simple_ensemble(
  dat_mean,
  model_id = "ens_all", agg_fun = "median"
)
# construct ensembles without each model
model_names <- dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_mean[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "median"
  )
  ens_df_simple <- bind_rows(ens_df_simple, sub_ens)
}
# evaluate each ensemble
ens_df_simple <- score_model_out(ens_df_simple,
  target_data_mean,
  metrics = "se_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se_point - se_point[1]) |>
  filter(model_id != "ens_all")
ens_df_simple$model_id <- sub("^ens_wo_", "", ens_df_simple$model_id)
# expected importance scores with mean output and simple median ensemble
exp_imp_mean3 <- ens_df_simple |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "simple_ensemble-median",
    algorithm = "lomo",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()


## Case 4: a missing data and 'simple_mean' ensemble
# data with missing values
sub_dat_mean <- dat_mean |> filter(model_id %in% model_id_list[c(1, 3)])
# ensemble built from all models
ens_df <- simple_ensemble(sub_dat_mean, model_id = "ens_all")
# construct ensembles without each model
model_names <- sub_dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- sub_dat_mean[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  ens_df <- bind_rows(ens_df, sub_ens)
}
# evaluate each ensemble
ens_df <- score_model_out(ens_df,
  target_data_mean,
  metrics = "se_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se_point - se_point[1]) |>
  filter(model_id != "ens_all")
ens_df$model_id <- sub("^ens_wo_", "", ens_df$model_id)
# expected importance scores with mean output and simple mean ensemble
exp_imp_mean4 <- data.frame(model_id = model_id_list) |>
  left_join(ens_df, by = "model_id") |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "simple_mean",
    algorithm = "lomo",
    test_purp = "missing data"
  ) |>
  as.data.frame()

# combine the expected importance scores
exp_imp_mean <- rbind(
  exp_imp_mean1, exp_imp_mean2, exp_imp_mean3, exp_imp_mean4
)

# save data
saveRDS(dat_mean,
  file = "tests/testthat/testdata/dat_mean.rds"
)
saveRDS(exp_imp_mean,
  file = "tests/testthat/testdata/exp_imp_mean_untrained_lomo.rds"
)
saveRDS(target_data_mean,
  file = "tests/testthat/testdata/target_mean.rds"
)
