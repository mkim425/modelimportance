# =============================================================================
# Helper functions to
# 1) build ensemble forecasts for untrained ensemble models using the LASOMO
# 2) calculate weighted marginal contributions of individual models
# 3) store the importance score for a specific model
# =============================================================================
library(dplyr)
library(hubEnsembles)

# 1-1) Build linear pool ensemble in LASOMO for untrained ensemble models
# Args:
#   models: model_id vector of all individual models
#   subset: indices of models included in the current subset, S
#   subsets: list of all possible subsets of models
#   d: data frame of forecasts from all individual models
#   n: number of individual models
# Returns:
#   data frame of all possible ensemble forecasts along with corresponding
#   subset weights
lp_ens_untrained_lasomo <- function(models, subset, subsets, n, d) {
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
  data_subset <- d |>
    dplyr::filter(.data$model_id %in% get_modelsubset)
  # build an ensemble forecast using the models in the subset S
  ensemble_forecast <- hubEnsembles::linear_pool(data_subset,
    model_id = paste0("ensemble_", i)
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


# 1-2) Build simple ensemble in LASOMO for untrained ensemble models
# Args:
#   models: model_id vector of all individual models
#   subset: indices of models included in the current subset, S
#   subsets: list of all possible subsets of models
#   d: data frame of forecasts from all individual models
#   n: number of individual models
# Returns:
#   data frame of all possible ensemble forecasts along with corresponding
#   subset weights
simple_ens_untrained_lasomo <- function(models, subset, subsets, n, d, aggfun) {
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
  data_subset <- d |>
    dplyr::filter(.data$model_id %in% get_modelsubset)
  # build an ensemble forecast using the models in the subset S
  ensemble_forecast <- hubEnsembles::simple_ensemble(data_subset,
    model_id = paste0("ensemble_", i),
    agg_fun = aggfun
  )
  # add index and weight to the ensemble forecast
  ens_dat <- ensemble_forecast |>
    dplyr::mutate(
      subset_idx = i,
      subset_wt_perm = weight_perm,
      subset_wt_eq = weight_eq
    )
  ens_dat
}

# ----------------------------------------------------------------------------
# 2) weighted marginal contribution of the jth model to each subset
# 2-1) output type: mean
wtd_marginal_cntrbt_mean <- function(k, j, score_ens_all, subsets, col) {
  # get elements of the subset that includes j
  set_k <- subsets[[k]]
  # index in 'subsets' list that include elements of set_k except for j
  k1 <- which(sapply(subsets, setequal, set_k[set_k != j]))
  # rse_point for the ensemble forecast including jth model
  score_incl_j <- score_ens_all$rse_point[k]
  # rse_point for the ensemble forecast not including jth model
  score_not_incl_j <- score_ens_all$rse_point[k1]
  # get the subset weight for the current subset
  subset_weight <- score_ens_all[[col]][k1]
  # jth model's marginal contribution multiplied by the subset weight
  subset_weight * (-score_incl_j + score_not_incl_j)
}

# 2-2) output type: median
wtd_marginal_cntrbt_median <- function(k, j, score_ens_all, subsets, col) {
  # get elements of the subset that includes j
  set_k <- subsets[[k]]
  # index in 'subsets' list that include elements of set_k except for j
  k1 <- which(sapply(subsets, setequal, set_k[set_k != j]))
  # ae_point for the ensemble forecast including jth model
  score_incl_j <- score_ens_all$ae_point[k]
  # ae_point for the ensemble forecast not including jth model
  score_not_incl_j <- score_ens_all$ae_point[k1]
  # get the subset weight for the current subset
  subset_weight <- score_ens_all[[col]][k1]
  # jth model's marginal contribution multiplied by the subset weight
  subset_weight * (-score_incl_j + score_not_incl_j)
}

# 2-3) output type: quantile
wtd_marginal_cntrbt_qntl <- function(k, j, score_ens_all, subsets, col) {
  # get elements of the subset that includes j
  set_k <- subsets[[k]]
  # index in 'subsets' list that include elements of set_k except for j
  k1 <- which(sapply(subsets, setequal, set_k[set_k != j]))
  # wis for the ensemble forecast including jth model
  score_incl_j <- score_ens_all$wis[k]
  # wis for the ensemble forecast not including jth model
  score_not_incl_j <- score_ens_all$wis[k1]
  # get the subset weight for the current subset
  subset_weight <- score_ens_all[[col]][k1]
  # jth model's marginal contribution multiplied by the subset weight
  subset_weight * (-score_incl_j + score_not_incl_j)
}

# 2-4) output type: pmf
wtd_marginal_cntrbt_pmf <- function(k, j, score_ens_all, subsets, col) {
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


# ----------------------------------------------------------------------------
# 3) store the importance score for the jth model
df_score <- function(cols, j, models, score) {
  purrr::map_dfr(cols, function(col) {
    data.frame(
      model_id = models[j],
      subset_wt = col,
      importance = score[[col]]
    )
  }) |>
    # importance score for the jth model depending on the subset_wt option
    dplyr::mutate(
      subset_wt = sub("^subset_wt_", "", .data$subset_wt),
      importance = ifelse(.data$subset_wt == "perm", .data$importance,
        .data$importance / (2^(n - 1) - 1)
      )
    )
}
