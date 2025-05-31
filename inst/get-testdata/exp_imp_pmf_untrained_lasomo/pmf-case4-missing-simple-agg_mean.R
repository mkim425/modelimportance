## Generate expected importance scores for the untrained ensemble models
## with pmf output in LASOMO
## Case 4: missing data and 'simple_ensemble' using agg_fun = mean
# ----------------------------------------------------------------------------
# load the package to make its internal functions available
devtools::load_all()
# target data
target_data_pmf <- readRDS(
  testthat::test_path("testdata/target_pmf.rds")
)

# forecast data with pmf output
dat_pmf <- readRDS(
  testthat::test_path("testdata/dat_pmf.rds")
)
model_id_list <- unique(dat_pmf$model_id)

# data with missing values
sub_dat_pmf <- dat_pmf |> filter(model_id %in% model_id_list[c(1, 3)])
models <- unique(sub_dat_pmf$model_id)
# number of models
n <- length(models)
# Power set of {1,2,...,n} not including the empty set.
# We use this power set to get indices for subset of models
subsets <- lapply(1:n, function(x) combn(n, x, simplify = FALSE)) |>
  unlist(recursive = FALSE)
# data frame of all possible ensemble forecasts
dat_all_ens <- purrr::map_dfr(
  subsets,
  function(subset) {
    get_modelsubset <- models[subset]
    # index of the subsets list that is identical to the current subset, S
    i <- Position(function(x) identical(x, subset), subsets)
    # calculate the weight given to this subset when subset_wt="perm_based"
    weight_perm <- 1 / ((n - 1) * choose(n - 1, length(get_modelsubset)))
    # when the subset includes all indices, its weight is infinite
    # replace it with NA (this value won't be used)
    weight_perm <- ifelse(is.infinite(weight_perm), NA, weight_perm)
    # calculate the weight when subset_wt="equal"
    weight_eq <- 1
    # reduced data including the models in the subset S
    data_subset <- sub_dat_pmf |>
      filter(.data$model_id %in% get_modelsubset)
    # build an ensemble forecast using the models in the subset S
    ensemble_forecast <- simple_ensemble(data_subset,
      model_id = paste0("ensemble_", i),
      agg_fun = "mean"
    )
    # add index and weight to the ensemble forecast
    ens_dat <- ensemble_forecast |>
      mutate(
        subset_idx = i,
        subset_wt_perm = weight_perm,
        subset_wt_eq = weight_eq
      )
    ens_dat
  }
)

# score the ensemble forecasts
score_ens_all <- score_model_out(
  dat_all_ens |> select(-c(subset_idx, subset_wt_perm, subset_wt_eq)),
  target_data_pmf,
  metrics = "log_score"
) |>
  left_join(
    dat_all_ens |>
      select(c("model_id", "subset_wt_perm", "subset_wt_eq")) |>
      distinct(),
    by = "model_id"
  )

# calculate importance scores
model_imp_scores <- furrr::future_map_dfr(1:n, function(j) {
  # identify which in 'subsets' includes the value j, and return indices
  set_incl_j <- which(sapply(subsets, function(x) j %in% x))
  # find indices in 'subsets' list including more elements in addition to j
  set_incl_j_more <- set_incl_j[set_incl_j > n]

  cols <- c("subset_wt_perm", "subset_wt_eq")
  # weighted marginal contribution of the jth model to each subset
  scores_by_subset <- map(cols, function(col) {
    purrr::map_dbl(
      set_incl_j_more,
      function(k) {
        # get elements of the subset that includes j
        set_k <- subsets[[k]]
        # index in 'subsets' list that include elements of set_k except for j
        k1 <- which(sapply(subsets, setequal, set_k[set_k != j]))
        # log_score for the ensemble forecast including jth model
        score_incl_j <- score_ens_all$log_score[k]
        # log_score for the ensemble forecast not including jth model
        score_not_incl_j <- score_ens_all$log_score[k1]
        # get the subset weight for the current subset
        subset_weight <- score_ens_all[[col]][k1]
        # jth model's marginal contribution multiplied by the subset weight
        subset_weight * (-score_incl_j + score_not_incl_j)
      }
    )
  })

  names(scores_by_subset) <- cols
  # accumulate the scores calculated by subsets
  score <- lapply(scores_by_subset, sum)
  # store the importance score for the jth model
  map_dfr(cols, function(col) {
    data.frame(
      model_id = models[j],
      subset_wt = col,
      importance = score[[col]]
    )
  }) |>
    # importance score for the jth model depending on the subset_wt option
    mutate(
      subset_wt = sub("^subset_wt_", "", subset_wt),
      importance = ifelse(subset_wt == "perm", importance,
        importance / (2^(n - 1) - 1)
      )
    )
})

exp_imp_pmf_case4perm <- model_imp_scores |>
  filter(subset_wt == "perm") |>
  right_join(data.frame(model_id = model_id_list), by = "model_id") |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lasomo",
    test_purp = "missing data",
    subset_wt = "perm_based"
  ) |>
  select(model_id, importance, ens_mthd, algorithm, subset_wt, test_purp)

exp_imp_pmf_case4eq <- model_imp_scores |>
  filter(subset_wt == "eq") |>
  right_join(data.frame(model_id = model_id_list), by = "model_id") |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lasomo",
    test_purp = "missing data",
    subset_wt = "equal"
  ) |>
  select(model_id, importance, ens_mthd, algorithm, subset_wt, test_purp)
