## combined expected importance scores generated in each case
## into a single dataset by row-binding them
## for the untrained ensemble models with pmf output in LASOMO

file_names <- c(
  "pmf-case1-no_missing-linear_pool.R",
  "pmf-case2-no_missing-simple-agg_mean.R",
  "pmf-case3-no_missing-simple-agg_median.R",
  "pmf-case4-missing-simple-agg_mean.R"
)
root <- paste0(
  "./inst/get-testdata/",
  "for-score_untrained-fn/exp_imp_pmf_lasomo/"
)
for (i in 1:4) source(paste0(root, file_names[i]))

exp_imp_pmf <- rbind(
  exp_imp_pmf_case1perm, exp_imp_pmf_case1eq,
  exp_imp_pmf_case2perm, exp_imp_pmf_case2eq,
  exp_imp_pmf_case3perm, exp_imp_pmf_case3eq,
  exp_imp_pmf_case4perm, exp_imp_pmf_case4eq
) |> as_tibble()

# save data
saveRDS(exp_imp_pmf,
  file = paste0(
    "tests/testthat/testdata/for-score_untrained/",
    "exp_imp_pmf_lasomo.rds"
  )
)
