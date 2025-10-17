## Generate expected importance scores for the untrained ensemble models
## with mean output in LASOMO
## Case 2: no missing data and 'simple_ensemble' using agg_fun = mean
# ----------------------------------------------------------------------------
# load the package to make its internal functions available
devtools::load_all()
source(system.file(
  "get-testdata/for-score_untrained-fn/helper-exp_imp-untrained.R",
  package = "modelimportance"
))
# target data
target_data_mean <- readRDS(
  testthat::test_path("testdata/target_mean.rds")
)

# forecast data with mean output
dat_mean <- readRDS(
  testthat::test_path("testdata/dat_mean.rds")
)

models <- dat_mean$model_id
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
    simple_ens_untrained_lasomo(models, subset, subsets, n,
      d = dat_mean,
      aggfun = "mean"
    )
  }
)

# score the ensemble forecasts
score_ens_all <- score_model_out(
  dat_all_ens |> select(-c(subset_idx, subset_wt_perm, subset_wt_eq)),
  target_data_mean,
  metrics = "se_point"
) |>
  left_join(dat_all_ens, by = "model_id")

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
      function(k) wtd_marginal_cntrbt_mean(k, j, score_ens_all, subsets, col)
    )
  })

  names(scores_by_subset) <- cols
  # accumulate the scores calculated by subsets
  score <- lapply(scores_by_subset, sum)
  # store the importance score for the jth model
  out <- df_score(cols, j, models, score)
  out
})

exp_imp_mean_case2perm <- model_imp_scores |>
  filter(subset_wt == "perm") |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lasomo",
    test_purp = "properly assigned",
    subset_wt = "perm_based"
  ) |>
  select(model_id, importance, ens_mthd, algorithm, subset_wt, test_purp)

exp_imp_mean_case2eq <- model_imp_scores |>
  filter(subset_wt == "eq") |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lasomo",
    test_purp = "properly assigned",
    subset_wt = "equal"
  ) |>
  select(model_id, importance, ens_mthd, algorithm, subset_wt, test_purp)
