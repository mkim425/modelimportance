## combined expected importance scores generated in each case
## into a single dataset by row-binding them
## for the untrained ensemble models with mean output in LASOMO

file_names <- c(
  "mean-case1-no_missing-linear_pool.R",
  "mean-case2-no_missing-simple-agg_mean.R",
  "mean-case3-no_missing-simple-agg_median.R",
  "mean-case4-missing-simple-agg_mean.R"
)
root <- "./inst/get-testdata/exp_imp_mean_untrained_lasomo/"
for (i in 1:4) source(paste0(root, file_names[i]))

exp_imp_mean <- rbind(
  exp_imp_mean_case1perm, exp_imp_mean_case1eq,
  exp_imp_mean_case2perm, exp_imp_mean_case2eq,
  exp_imp_mean_case3perm, exp_imp_mean_case3eq,
  exp_imp_mean_case4perm, exp_imp_mean_case4eq
) |> as_tibble()

# save data
saveRDS(exp_imp_mean,
  file = "tests/testthat/testdata/exp_imp_mean_untrained_lasomo.rds"
)
