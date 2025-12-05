## combined expected importance scores generated in each case
## into a single dataset by row-binding them
## for the untrained ensemble models with quantile output in LASOMO

file_names <- c(
  "qntl-case1-no_missing-linear_pool.R",
  "qntl-case2-no_missing-simple-agg_mean.R",
  "qntl-case3-no_missing-simple-agg_median.R",
  "qntl-case4-missing-simple-agg_mean.R"
)
root <- paste0(
  "./inst/get-testdata/",
  "for-score_untrained-fn/exp_imp_qntl_lasomo/"
)
for (i in 1:4) source(paste0(root, file_names[i]))

exp_imp_qntl <- rbind(
  exp_imp_qntl_case1perm, exp_imp_qntl_case1eq,
  exp_imp_qntl_case2perm, exp_imp_qntl_case2eq,
  exp_imp_qntl_case3perm, exp_imp_qntl_case3eq,
  exp_imp_qntl_case4perm, exp_imp_qntl_case4eq
) |> as_tibble()

# save data
saveRDS(exp_imp_qntl,
  file = paste0(
    "tests/testthat/testdata/for-compute_importance/",
    "exp_imp_qntl_lasomo.rds"
  )
)
