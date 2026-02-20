## Generate expected importance scores for the untrained ensemble models
## with pmf output in LOMO

# load the package to make its internal functions available
devtools::load_all()

# target data
target_data_pmf <- readRDS(
  testthat::test_path("testdata/for-compute_importance/target_pmf.rds")
)

# forecast data with pmf output
dat_pmf <- readRDS(
  testthat::test_path("testdata/for-compute_importance/dat_pmf.rds")
)
min_log_score <- -10
model_id_list <- unique(dat_pmf$model_id)

## Case 1: no missing data and 'linear pool' ensemble
# create an ensemble using all models and store it in 'df_ensembles' dataframe.
df_ensembles <- linear_pool(dat_pmf, model_id = "ens_all")
# create ensembles without each model and add them in the 'df_ensembles'
model_names <- unique(dat_pmf$model_id)
for (i in seq_along(model_names)) {
  sub_data <- dat_pmf |> filter(model_id != model_names[i])
  sub_ens <- linear_pool(sub_data, model_id = paste0("ens_wo_", model_names[i]))
  df_ensembles <- bind_rows(df_ensembles, sub_ens)
}

# evaluate each ensemble
ensemble_scores <- score_model_out(
  model_out_tbl = df_ensembles,
  oracle_output = target_data_pmf,
  metrics = "log_score"
) |>
  mutate(
    log_score = ifelse(
      .data$log_score > -min_log_score,
      -min_log_score,
      .data$log_score
    )
  ) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = log_score - log_score[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with pmf output and linear pool
exp_imp_pmf1 <- model_imp_scores |>
  mutate(
    ens_mthd = "linear_pool-NA",
    algorithm = "lomo",
    test_purp = "properly assigned"
  )

## Case 2: no missing data and 'simple_ensemble' using agg_fun = mean
# create an ensemble using all models and store it in 'df_ensembles_simple'
df_ensembles_simple <- simple_ensemble(
  dat_pmf,
  model_id = "ens_all",
  agg_fun = "mean"
)
# create ensembles without each model and add them in the 'df_ensembles_simple'
model_names <- unique(dat_pmf$model_id)
for (i in seq_along(model_names)) {
  sub_data <- dat_pmf |> filter(model_id != model_names[i])
  sub_ens <- simple_ensemble(
    sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "mean"
  )
  df_ensembles_simple <- bind_rows(df_ensembles_simple, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(
  df_ensembles_simple,
  target_data_pmf,
  metrics = "log_score"
) |>
  mutate(
    log_score = ifelse(
      .data$log_score > -min_log_score,
      -min_log_score,
      .data$log_score
    )
  ) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = log_score - log_score[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with pmf output and simple mean ensemble
exp_imp_pmf2 <- model_imp_scores |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lomo",
    test_purp = "properly assigned"
  )

## Case 3: no missing data and 'simple_ensemble' using agg_fun = median
# create an ensemble using all models and store it in 'df_ensembles_simple'
df_ensembles_simple <- simple_ensemble(
  dat_pmf,
  model_id = "ens_all",
  agg_fun = "median"
)
# create ensembles without each model and add them in the 'df_ensembles_simple'
model_names <- unique(dat_pmf$model_id)
for (i in seq_along(model_names)) {
  sub_data <- dat_pmf |> filter(model_id != model_names[i])
  sub_ens <- simple_ensemble(
    sub_data,
    model_id = paste0("ens_wo_", model_names[i]),
    agg_fun = "median"
  )
  df_ensembles_simple <- bind_rows(df_ensembles_simple, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(
  df_ensembles_simple,
  target_data_pmf,
  metrics = "log_score"
) |>
  mutate(
    log_score = ifelse(
      .data$log_score > -min_log_score,
      -min_log_score,
      .data$log_score
    )
  ) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = log_score - log_score[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with pmf output and simple median ensemble
exp_imp_pmf3 <- model_imp_scores |>
  mutate(
    ens_mthd = "simple_ensemble-median",
    algorithm = "lomo",
    test_purp = "properly assigned"
  )

## Case 4: a missing data and 'simple_mean' ensemble
# data with missing values
sub_dat_pmf <- dat_pmf |> filter(model_id %in% model_id_list[c(1, 3)])
# create an ensemble using all models and store it in 'df_ensembles'
df_ensembles <- simple_ensemble(sub_dat_pmf, model_id = "ens_all")
# create ensembles without each model and add them in the 'df_ensembles'
model_names <- unique(sub_dat_pmf$model_id)
for (i in seq_along(model_names)) {
  sub_data <- sub_dat_pmf |> filter(model_id != model_names[i])
  sub_ens <- simple_ensemble(
    sub_data,
    model_id = paste0("ens_wo_", model_names[i])
  )
  df_ensembles <- bind_rows(df_ensembles, sub_ens)
}
# evaluate each ensemble
ensemble_scores <- score_model_out(
  df_ensembles,
  target_data_pmf,
  metrics = "log_score"
) |>
  mutate(
    log_score = ifelse(
      .data$log_score > -min_log_score,
      -min_log_score,
      .data$log_score
    )
  ) |>
  # calculate importance scores: subtract the error of the ensemble-all
  mutate(importance = log_score - log_score[1]) |>
  filter(model_id != "ens_all")

# get dataframe including model_id and each model's importance score
model_imp_scores <- ensemble_scores |>
  select(model_id, importance) |>
  mutate(model_id = sub("^ens_wo_", "", model_id))

# expected importance scores with pmf output and simple mean ensemble
exp_imp_pmf4 <- data.frame(model_id = model_id_list) |>
  left_join(model_imp_scores, by = "model_id") |>
  mutate(
    ens_mthd = "simple_mean",
    algorithm = "lomo",
    test_purp = "missing data"
  )

# combine the expected importance scores
exp_imp_pmf <- rbind(
  exp_imp_pmf1,
  exp_imp_pmf2,
  exp_imp_pmf3,
  exp_imp_pmf4
) |>
  as_tibble()

# save data
saveRDS(
  exp_imp_pmf,
  file = paste0(
    "tests/testthat/testdata/for-compute_importance/",
    "exp_imp_pmf_lomo.rds"
  )
)
