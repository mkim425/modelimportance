## Generate expected importance scores for the untrained ensemble models
## with mean output in LOMO

# load the package to make its internal functions available
devtools::load_all()
# target data
target_data_mean <- readRDS(
  testthat::test_path("testdata/target_mean.rds")
)

# forecast data with mean output
dat_mean <- readRDS(
  testthat::test_path("testdata/dat_mean.rds")
)
model_id_list <- unique(dat_mean$model_id)

## Case 1: no missing data and 'linear pool' ensemble
# create an ensemble using all models and store it in 'df_ensembles' dataframe.
df_ensembles <- linear_pool(dat_mean, model_id = "ens_all")
# construct ensembles without each model and add them in the 'df_ensembles'
model_names <- dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_mean[model_names != model_names[i], ]
  sub_ens <- linear_pool(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  df_ensembles <- bind_rows(df_ensembles, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(df_ensembles,
  target_data_mean,
  metrics = "se_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se_point - se_point[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with mean output and linear pool
exp_imp_mean1 <- model_imp_scores |>
  mutate(
    ens_mthd = "linear_pool-NA",
    algorithm = "lomo",
    test_purp = "properly assigned"
  )

## Case 2: no missing data and 'simple_ensemble' using agg_fun = mean
# create an ensemble using all models and store it in 'df_ensembles_simple'
df_ensembles_simple <- simple_ensemble(
  dat_mean,
  model_id = "ens_all", agg_fun = "mean"
)
# create ensembles without each model and add them in the 'df_ensembles_simple'
model_names <- dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_mean[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "mean"
  )
  df_ensembles_simple <- bind_rows(df_ensembles_simple, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(df_ensembles_simple,
  target_data_mean,
  metrics = "se_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se_point - se_point[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with mean output and simple mean ensemble
exp_imp_mean2 <- model_imp_scores |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lomo",
    test_purp = "properly assigned"
  )

## Case 3: no missing data and 'simple_ensemble' using agg_fun = median
# create an ensemble using all models and store it in 'df_ensembles_simple'
df_ensembles_simple <- simple_ensemble(
  dat_mean,
  model_id = "ens_all", agg_fun = "median"
)
# create ensembles without each model and add them in the 'df_ensembles_simple'
model_names <- dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- dat_mean[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "median"
  )
  df_ensembles_simple <- bind_rows(df_ensembles_simple, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(df_ensembles_simple,
  target_data_mean,
  metrics = "se_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se_point - se_point[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with mean output and simple median ensemble
exp_imp_mean3 <- model_imp_scores |>
  mutate(
    ens_mthd = "simple_ensemble-median",
    algorithm = "lomo",
    test_purp = "properly assigned"
  )

## Case 4: a missing data and 'simple_mean' ensemble
# data with missing values
sub_dat_mean <- dat_mean |> filter(model_id %in% model_id_list[c(1, 3)])
# create an ensemble using all models and store it in 'df_ensembles'
df_ensembles <- simple_ensemble(sub_dat_mean, model_id = "ens_all")
# create ensembles without each model and add them in the 'df_ensembles'
model_names <- sub_dat_mean$model_id
for (i in seq_along(model_names)) {
  sub_data <- sub_dat_mean[model_names != model_names[i], ]
  sub_ens <- simple_ensemble(sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  df_ensembles <- bind_rows(df_ensembles, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(df_ensembles,
  target_data_mean,
  metrics = "se_point"
) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = se_point - se_point[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with mean output and simple mean ensemble
exp_imp_mean4 <- data.frame(model_id = model_id_list) |>
  left_join(model_imp_scores, by = "model_id") |>
  mutate(
    ens_mthd = "simple_mean",
    algorithm = "lomo",
    test_purp = "missing data"
  )

# combine the expected importance scores
exp_imp_mean <- rbind(
  exp_imp_mean1, exp_imp_mean2, exp_imp_mean3, exp_imp_mean4
) |> as_tibble()

# save data
saveRDS(exp_imp_mean,
  file = "tests/testthat/testdata/exp_imp_mean_untrained_lomo.rds"
)
