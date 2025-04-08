## Data for testing the score_untrained example: quantile output

## target data
target_data_qntl <- readRDS(
  testthat::test_path("testdata/flu_example_target_data.rds")
) |> mutate(target_end_date = as.Date(target_end_date))

## forecast data with quantile output
forecast_qntl <- readRDS(
  testthat::test_path("testdata/flu_example_quantile_model_output.rds")
)
valid_tbl_qntl <- validate_input_data(forecast_qntl, target_data_qntl)
qntl_data_list <- split_data_by_task(valid_tbl_qntl,
  weighted = FALSE,
  training_window_length = 0
)

# select a dataset to test
dat_qntl <- qntl_data_list[[16]]
# model_id list
model_id_list <- unique(dat_qntl$model_id)

## Case 1: no missing data and 'linear pool' ensemble
# ensemble built from all models
ens_df <- linear_pool(dat_qntl, model_id = "ens_all")
# construct ensembles without each model
model_names <- unique(dat_qntl$model_id)
for (i in seq_along(model_names)) {
  sub_data <- dat_qntl |> filter(model_id != model_names[i])
  sub_ens <- linear_pool(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  ens_df <- bind_rows(ens_df, sub_ens)
}
# evaluate each ensemble
ens_df <- score_model_out(ens_df,
  target_data_qntl,
  metrics = "wis"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = wis - wis[1]) |>
  filter(model_id != "ens_all")
ens_df$model_id <- sub("^ens_wo_", "", ens_df$model_id)
# expected importance scores with quantile output and linear pool
exp_imp_qntl1 <- ens_df |>
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
  dat_qntl,
  model_id = "ens_all", agg_fun = "mean"
)
# construct ensembles without each model
model_names <- unique(dat_qntl$model_id)
for (i in seq_along(model_names)) {
  sub_data <- dat_qntl |> filter(model_id != model_names[i])
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "mean"
  )
  ens_df_simple <- bind_rows(ens_df_simple, sub_ens)
}
# evaluate each ensemble
ens_df_simple <- score_model_out(ens_df_simple,
  target_data_qntl,
  metrics = "wis"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = wis - wis[1]) |>
  filter(model_id != "ens_all")
ens_df_simple$model_id <- sub("^ens_wo_", "", ens_df_simple$model_id)
# expected importance scores with quantile output and simple mean ensemble
exp_imp_qntl2 <- ens_df_simple |>
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
  dat_qntl,
  model_id = "ens_all", agg_fun = "median"
)
# construct ensembles without each model
model_names <- unique(dat_qntl$model_id)
for (i in seq_along(model_names)) {
  sub_data <- dat_qntl |> filter(model_id != model_names[i])
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "median"
  )
  ens_df_simple <- bind_rows(ens_df_simple, sub_ens)
}
# evaluate each ensemble
ens_df_simple <- score_model_out(ens_df_simple,
  target_data_qntl,
  metrics = "wis"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = wis - wis[1]) |>
  filter(model_id != "ens_all")
ens_df_simple$model_id <- sub("^ens_wo_", "", ens_df_simple$model_id)
# expected importance scores with quantile output and simple median ensemble
exp_imp_qntl3 <- ens_df_simple |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "simple_ensemble-median",
    algorithm = "lomo",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()

## Case 4: a missing data and 'simple_mean' ensemble
# data with missing values
sub_dat_qntl <- dat_qntl |> filter(model_id %in% model_id_list[c(1, 3)])
# ensemble built from all models
ens_df <- simple_ensemble(sub_dat_qntl, model_id = "ens_all")
# construct ensembles without each model
model_names <- unique(sub_dat_qntl$model_id)
for (i in seq_along(model_names)) {
  sub_data <- sub_dat_qntl |> filter(model_id != model_names[i])
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  ens_df <- bind_rows(ens_df, sub_ens)
}
# evaluate each ensemble
ens_df <- score_model_out(ens_df, target_data_qntl, metrics = "wis") |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = wis - wis[1]) |>
  filter(model_id != "ens_all")
ens_df$model_id <- sub("^ens_wo_", "", ens_df$model_id)
# expected importance scores with quantile output and simple mean ensemble
exp_imp_qntl4 <- data.frame(model_id = model_id_list) |>
  left_join(ens_df, by = "model_id") |>
  select(model_id, importance) |>
  mutate(
    ens_mthd = "simple_mean",
    algorithm = "lomo",
    test_purp = "missing data"
  ) |>
  as.data.frame()

# combine the expected importance scores
exp_imp_qntl <- rbind(
  exp_imp_qntl1, exp_imp_qntl2, exp_imp_qntl3, exp_imp_qntl4
)

# save data
saveRDS(dat_qntl,
  file = "tests/testthat/testdata/dat_qntl.rds"
)
saveRDS(exp_imp_qntl,
  file = "tests/testthat/testdata/exp_imp_qntl_untrained_lomo.rds"
)
saveRDS(target_data_qntl,
  file = "tests/testthat/testdata/target_qntl.rds"
)
