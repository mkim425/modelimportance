## combined expected importance scores generated in each case
## into a single dataset by row-binding them
## for the untrained ensemble models with median output in LASOMO

file_names <- c(
  "median-case1-no_missing-simple-agg_mean.R",
  "median-case2-no_missing-simple-agg_median.R",
  "median-case3-missing-simple-agg_mean.R"
)
root <- paste0(
  "./inst/get-testdata/",
  "for-score_untrained-fn/exp_imp_median_untrained_lasomo/"
)
for (i in 1:3) source(paste0(root, file_names[i]))

exp_imp_median <- rbind(
  exp_imp_median_case1perm, exp_imp_median_case1eq,
  exp_imp_median_case2perm, exp_imp_median_case2eq,
  exp_imp_median_case3perm, exp_imp_median_case3eq
) |> as_tibble()

# save data
saveRDS(exp_imp_median,
  file = paste0(
    "tests/testthat/testdata/for-score_untrained/",
    "exp_imp_median_untrained_lasomo.rds"
  )
)
