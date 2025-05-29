#' @title Calculate model importance for a single task when using an untrained
#' ensemble
#' @description
#' Evaluate the importance of ensemble component models by quantifying their
#' contribution to the prediction accuracy of an untrained ensemble for each
#' combination of model task.
#'
#' @inheritParams model_importance
#' @param single_task_data A data.frame with the predictions for a single
#' forecast task in a model_out_tbl format. The data must contain only one
#' `output_type`, which must be one of the following:
#' `mean`, `median`, `quantile`, or `pmf`.
#' @param model_id_list A list of all component model IDs to be used in the
#' ensemble. If a model is not present in the `single_task_data`, it means that
#' the model did not submit predictions for the given task.
#' This list is used to identify missing models in the ensemble.
#' @param metric A character string specifying the metric to be used for scoring
#' the model output. The metric is determined by the `output_type` and must be
#' one of the following: `se_point`, `ae_point`, `wis`, or `log_score`.
#'
#' @returns A data.frame with columns
#' `task_id`, `output_type`, `model_id`, `task_level_importance`.
#'
#' @import hubEnsembles
#' @import hubEvals
#' @importFrom utils getFromNamespace
#' @importFrom utils combn
#' @inherit model_importance details

score_untrained <- function(single_task_data, oracle_output_data, model_id_list,
                            ensemble_fun, importance_algorithm, subset_wt,
                            metric, ...) {
  # models in the single_task_data
  models <- unique(single_task_data$model_id)
  missing_model <- setdiff(model_id_list, single_task_data$model_id)

  # `ens_fun` is a function from hubEnsembles specified by `ensemble_fun`
  ens_fun <- getFromNamespace(ensemble_fun, ns = asNamespace("hubEnsembles"))

  # Compute importance score when importance_algorithm is 'lomo'
  if (importance_algorithm == "lomo") {
    # build an ensemble with the specified method using all models available
    ens_all <- ens_fun(single_task_data,
      weights = NULL,
      model_id = "ensemble-all",
      ...
    )
    # build ensemble forecasts by leaving one model out
    ens_lomo <- lapply(models, function(x) {
      single_task_data |>
        dplyr::filter(.data$model_id != x) |>
        ens_fun(
          weights = NULL,
          model_id = paste0("ens.wo.", x),
          ...
        )
    })
    # store all ensemble forecasts in a dataframe
    ensemble_data <- rbind(ens_all, dplyr::bind_rows(ens_lomo))
    # score the ensemble forecasts and add it to the input dataset
    score_ens_all <- score_model_out(ensemble_data,
      oracle_output_data,
      metrics = metric
    ) |>
      rename(calculated_metric = any_of(
        c("ae_point", "se_point", "wis", "log_score")
      )) |>
      left_join(ensemble_data, by = "model_id") |>
      as_tibble()
    # calculate importance scores
    ensemble_all_value <- score_ens_all |>
      dplyr::filter(.data$model_id == "ensemble-all") |>
      dplyr::pull(.data$calculated_metric)
    df_importance <- score_ens_all |>
      dplyr::mutate(
        importance = .data$calculated_metric - ensemble_all_value
      ) |>
      dplyr::filter(.data$model_id != "ensemble-all") |>
      dplyr::mutate(model_id = gsub("ens.wo.", "", .data$model_id)) |>
      dplyr::select(-"calculated_metric")
  } else {
    # check if the necessary packages are installed
    if (!requireNamespace("furrr", quietly = TRUE)) {
      stop("Please install `furrr` package for parallel execution in `lasomo`
           algorithm.")
    }
    if (!requireNamespace("future", quietly = TRUE)) {
      stop("Please install `future` package for parallel execution in `lasomo`
           algorithm.")
    }

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
        if (subset_wt == "perm_based") {
          # calculate the weight given to this subset
          weight <- 1 / ((n - 1) * choose(n - 1, length(get_modelsubset)))
          # when the subset includes all indices, its weight is infinite
          # replace it with NA (this value won't be used)
          weight <- ifelse(is.infinite(weight), NA, weight)
        } else {
          # when subset_wt="equal"
          weight <- 1
        }
        # reduced data including the models in the subset S
        data_subset <- single_task_data |>
          filter(.data$model_id %in% get_modelsubset)
        # build an ensemble forecast using the models in the subset S
        ensemble_forecast <- ens_fun(data_subset,
          model_id = paste0("ensemble_", i), ...
        )
        # add index and weight to the ensemble forecast
        ens_dat <- ensemble_forecast |>
          mutate(subset_idx = i, subset_weight = weight)
        ens_dat
      }
    )
    # score the ensemble forecasts
    score_ens_all <- score_model_out(
      dat_all_ens |> select(-c("subset_idx", "subset_weight")),
      oracle_output_data,
      metrics = metric
    ) |>
      rename(calculated_metric = any_of(
        c("ae_point", "se_point", "wis", "log_score")
      )) |>
      left_join(dat_all_ens, by = "model_id")

    ## Environment for parallel computation
    # store the original plan
    original_plan <- future::plan()
    # set parallel plan with a conservative number of workers
    future::plan(future::multisession,
      workers = min(4, parallel::detectCores() - 1)
    )
    # restore the original plan on exit
    on.exit(future::plan(original_plan), add = TRUE)

    ## parallel computation
    # calculate importance scores
    result <- furrr::future_map_dfr(1:n, function(j) {
      # identify which in 'subsets' includes the value j, and return indices
      set_incl_j <- which(sapply(subsets, function(x) j %in% x))
      # find indices in 'subsets' list including more elements in addition to j
      set_incl_j_more <- set_incl_j[set_incl_j > n]

      # weighted marginal contribution of the jth model to each subset
      scores_by_subset <- purrr::map_dbl(
        set_incl_j_more,
        function(k) {
          # get elements of the subset that includes j
          set_k <- subsets[[k]]
          # index in 'subsets' list that include elements of set_k except for j
          k1 <- which(sapply(subsets, setequal, set_k[set_k != j]))
          # calculated_metric for the ensemble forecast including jth model
          score_incl_j <- score_ens_all$calculated_metric[k]
          # calculated_metric for the ensemble forecast not including jth model
          score_not_incl_j <- score_ens_all$calculated_metric[k1]
          # get the subset weight for the current subset
          subset_weight <- score_ens_all$subset_weight[k1]
          # jth model's marginal contribution multiplied by the subset weight
          subset_weight * (-score_incl_j + score_not_incl_j)
        }
      )
      # accumulate the scores calculated by subsets
      score <- sum(scores_by_subset)
      # store the importance score for the jth model depending on the subset_wt option
      if (subset_wt == "perm_based") {
        data.frame(
          model_id = models[j],
          importance = score
        )
      } else {
        data.frame(
          model_id = models[j],
          importance = score / (2^(n - 1) - 1)
        )
      }
    })
    # add the scores column to the input dataset
    df_importance <- single_task_data |>
      left_join(result, by = "model_id")
  }
  # Insert NAs for missing models
  if (length(missing_model) > 0) {
    fixed_cols <- df_importance |>
      select(-c("model_id", "value", "importance")) |>
      distinct()
    missing_model_rows <- data.frame(
      model_id = missing_model, fixed_cols,
      value = NA, importance = NA
    )
    # bind the new rows to df_importance
    combined_df <- bind_rows(df_importance, missing_model_rows)
    # reorder the columns
    importance_scores <- data.frame(model_id = model_id_list) |>
      left_join(
        combined_df,
        by = "model_id"
      ) |>
      select(-c("output_type_id", "value")) |>
      distinct()
  } else {
    importance_scores <- df_importance |>
      select(-c("output_type_id", "value")) |>
      distinct()
  }
  importance_scores
}
