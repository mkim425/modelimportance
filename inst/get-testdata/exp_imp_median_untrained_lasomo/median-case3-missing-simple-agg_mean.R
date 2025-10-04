## Generate expected importance scores for the untrained ensemble models
## with median output in LASOMO
## Case 3: missing data and 'simple_ensemble' using agg_fun = mean
# ----------------------------------------------------------------------------
# load the package to make its internal functions available
devtools::load_all()
source(system.file("get-testdata/helper-exp_imp-untrained.R",
  package = "modelimportance"
))
# target data
target_data_median <- readRDS(
  testthat::test_path("testdata/target_median.rds")
)

# forecast data with median output
dat_median <- readRDS(
  testthat::test_path("testdata/dat_median.rds")
)
model_id_list <- unique(dat_median$model_id)

# data with missing values
sub_dat_median <- dat_median |> filter(model_id %in% model_id_list[1:3])
models <- sub_dat_median$model_id

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
    simple_ens_untrained_lasomo(models, subset, subsets,
      d = sub_dat_median,
      aggfun = "mean"
    )
  }
)

# score the ensemble forecasts
score_ens_all <- score_model_out(
  dat_all_ens |> select(-c(subset_idx, subset_wt_perm, subset_wt_eq)),
  target_data_median,
  metrics = "ae_point"
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
      function(k) wtd_marginal_cntrbt_median(k, j, score_ens_all, subsets, col)
    )
  })

  names(scores_by_subset) <- cols
  # accumulate the scores calculated by subsets
  score <- lapply(scores_by_subset, sum)
  # store the importance score for the jth model
  out <- df_score(cols, j, models, score)
  out
})

exp_imp_median_case3perm <- model_imp_scores |>
  filter(subset_wt == "perm") |>
  right_join(data.frame(model_id = model_id_list), by = "model_id") |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lasomo",
    test_purp = "missing data",
    subset_wt = "perm_based"
  ) |>
  select(model_id, importance, ens_mthd, algorithm, subset_wt, test_purp)

exp_imp_median_case3eq <- model_imp_scores |>
  filter(subset_wt == "eq") |>
  right_join(data.frame(model_id = model_id_list), by = "model_id") |>
  mutate(
    ens_mthd = "simple_ensemble-mean",
    algorithm = "lasomo",
    test_purp = "missing data",
    subset_wt = "equal"
  ) |>
  select(model_id, importance, ens_mthd, algorithm, subset_wt, test_purp)
