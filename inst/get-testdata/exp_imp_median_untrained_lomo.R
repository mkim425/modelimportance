## Generate expected importance scores for the untrained ensemble models
## with median output in LOMO

# load the package to make its internal functions available
devtools::load_all()

# target data
target_data_median <- readRDS(
  testthat::test_path("testdata/target_median.rds")
)
# forecast data with mean output
dat_median <- readRDS(
  testthat::test_path("testdata/dat_median.rds")
)
model_id_list <- unique(dat_median$model_id)

## Case 1: no missing data and 'simple_ensemble' using agg_fun = mean
# create an ensemble using all models and store it in 'df_ensembles_simple'
df_ensembles_simple <- simple_ensemble(
  dat_median,
  model_id = "ens_all", agg_fun = "mean"
)
# create ensembles without each model and add them in the 'df_ensembles_simple'
model_names <- dat_median$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_median[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "mean"
  )
  df_ensembles_simple <- bind_rows(df_ensembles_simple, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(df_ensembles_simple,
  target_data_median,
  metrics = "ae_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = ae_point - ae_point[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with median output and linear pool
exp_imp_median1 <- model_imp_scores |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lomo",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()

## Case 2: no missing data and 'simple_ensemble' using agg_fun = median
# ensemble built from all models
df_ensembles_simple <- simple_ensemble(
  dat_median,
  model_id = "ens_all", agg_fun = "median"
)
# create ensembles without each model and add them in the 'df_ensembles_simple'
model_names <- dat_median$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_median[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "median"
  )
  df_ensembles_simple <- bind_rows(df_ensembles_simple, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(df_ensembles_simple,
  target_data_median,
  metrics = "ae_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = ae_point - ae_point[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with median output and linear pool
exp_imp_median2 <- model_imp_scores |>
  mutate(
    ens_mthd = "simple_ensemble-median",
    algorithm = "lomo",
    test_purp = "properly assigned"
  ) |>
  as.data.frame()

## Case 3: a missing data and 'simple_mean' ensemble
# data with missing values
sub_dat_median <- dat_median |> filter(model_id %in% model_id_list[c(1, 3)])
# create an ensemble using all models and store it in 'df_ensembles'
df_ensembles <- simple_ensemble(sub_dat_median, model_id = "ens_all")
# create ensembles without each model and add them in the 'df_ensembles'
model_names <- sub_dat_median$model_id
for (i in seq_along(model_names)) {
  sub_data <- sub_dat_median[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  df_ensembles <- bind_rows(df_ensembles, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(df_ensembles,
  target_data_median,
  metrics = "ae_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = ae_point - ae_point[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with mean output and simple mean ensemble
exp_imp_median3 <- data.frame(model_id = model_id_list) |>
  left_join(model_imp_scores, by = "model_id") |>
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
saveRDS(exp_imp_median,
  file = "tests/testthat/testdata/exp_imp_median_untrained_lomo.rds"
)
