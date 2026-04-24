# This script is used to generate the runtime data for Computational complexity section in the article.
# The script measures the elapsed time for model_importance function with different numbers of models and tasks
# in both sequential and multisession plans, and
# then saves the results in a CSV file for further analysis and plotting.

library(readr)
library(future)
# Template for the fake data
forecast_data <- hubExamples::forecast_outputs |>
  dplyr::filter(
    output_type %in% c("median"),
    location == "25",
    horizon == 1,
    target_end_date == "2022-12-24"
  ) |>
  # select the first row
  slice(1) |>
  select(-model_id, -value)

target_data <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    target_end_date %in% unique(forecast_data$target_end_date),
    location == "25",
    target == "wk inc flu hosp",
    target_end_date == "2022-12-24"
  ) |>
  # Rename columns to match the oracle output format
  rename(oracle_value = observation)

# Create a fake data for 1000 target dates
set.seed(123)
target_data <- target_data |>
  slice(rep(seq_len(n()), each = 10000)) |>
  mutate(
    target_end_date = as.Date("2022-12-24") + (row_number() - 1),
    oracle_value = oracle_value + rnorm(n(), mean = 100, sd = 50)
  )

# Function to measure the elapsed time for model_importance with different numbers of models and tasks
elapsed_time <- function(
  n_mods,
  n_tasks,
  forecast_data,
  target_data,
  plan,
  exe_lasomo = TRUE
) {
  if (plan == "sequential") {
    future::plan(sequential)
  } else {
    future::plan(multisession, workers = 4)
  }

  set.seed(1234)
  fake_models <- paste0("model", 1:n_mods) # model names
  # Create a data frame with the required structure for model_importance
  fdat <- forecast_data |>
    slice(rep(seq_len(n()), each = n_tasks)) |>
    mutate(target_end_date = as.Date("2022-12-24") + (row_number() - 1)) |>
    slice(rep(seq_len(n()), each = n_mods)) |>
    mutate(
      model_id = rep(fake_models, times = n() / n_mods),
      value = rnorm(n(), mean = 500, sd = 100)
    )
  # Measure the elapsed time for both algorithms
  t_lomo <- system.time(
    model_importance(
      forecast_data = fdat,
      oracle_output_data = target_data,
      ensemble_fun = "simple_ensemble",
      importance_algorithm = "lomo",
      subset_wt = "equal"
    )
  )["elapsed"]
  if (exe_lasomo) {
    t_lasomo <- system.time(
      model_importance(
        forecast_data = fdat,
        oracle_output_data = target_data,
        ensemble_fun = "simple_ensemble",
        importance_algorithm = "lasomo",
        subset_wt = "equal"
      )
    )["elapsed"]
  } else {
    t_lasomo <- NA
  }

  data.frame(
    n_models = n_mods,
    n_tasks = n_tasks,
    elapsed_lomo = round(t_lomo, 3),
    elapsed_lasomo = round(t_lasomo, 3),
    plan = plan
  )
}

# Test the function with different numbers of models and tasks in both sequential and multisession plans,
# and save the results in a CSV file
plans <- c("sequential", "multisession")

n_mods <- 8
n_tasks <- 10
grid <- expand.grid(
  n_models = 2:n_mods,
  n_tasks = 1:n_tasks,
  plan = plans,
  stringsAsFactors = FALSE
)

out_file <- "inst/local/runtime_results.csv"
if (!file.exists(out_file)) {
  write_csv(
    data.frame(
      n_models = integer(),
      n_tasks = integer(),
      elapsed_lomo = numeric(),
      elapsed_lasomo = numeric(),
      plan = character()
    ),
    out_file
  )
}

pmap(
  grid,
  function(n_models, n_tasks, plan) {
    print(paste("Testing with", n_models, "models and", n_tasks, "tasks"))
    result <- elapsed_time(
      n_models,
      n_tasks,
      forecast_data = forecast_data,
      target_data = target_data,
      plan = plan
    )
    write_csv(result, out_file, append = TRUE)
  }
)

# =============================================================================
# After testing the initial combinations of models and tasks,
# we expand the grid to include more combinations for further testing,
# especially for the multisession plan which is expected to perform better with larger combinations.
existing_grid <- read_csv(out_file) |>
  select(n_models, n_tasks, plan)

n_mods_more <- 10
n_tasks_more <- 10
grid_full <- expand.grid(
  n_models = 2:n_mods_more,
  n_tasks = c(1:n_tasks_more, 20, 50, 100),
  plan = "multisession",
  stringsAsFactors = FALSE
)
grid_new <- grid_full |>
  anti_join(existing_grid, by = c("n_models", "n_tasks", "plan"))

pmap(
  grid_new,
  function(n_models, n_tasks, plan) {
    print(paste("Testing with", n_models, "models and", n_tasks, "tasks"))
    result <- elapsed_time(
      n_models,
      n_tasks,
      forecast_data = forecast_data,
      target_data = target_data,
      plan = plan
    )
    write_csv(result, out_file, append = TRUE)
  }
)


# =============================================================================
# Further testing for lomo only with more combinations of models and tasks,
# as lasomo is expected to be slower and may not complete in a reasonable time frame for larger combinations
existing_grid <- read_csv(out_file) |>
  select(n_models, n_tasks, plan)

n_mods_more <- 100
n_tasks_more <- 10
grid_full <- expand.grid(
  n_models = 2:n_mods_more,
  n_tasks = c(1:n_tasks_more, 20, 50, 100, 200, 500),
  plan = "multisession",
  stringsAsFactors = FALSE
)
grid_new <- grid_full |>
  anti_join(existing_grid, by = c("n_models", "n_tasks", "plan"))

pmap(
  grid_new,
  function(n_models, n_tasks, plan) {
    print(paste("Testing with", n_models, "models and", n_tasks, "tasks"))
    result <- elapsed_time(
      n_models,
      n_tasks,
      forecast_data = forecast_data,
      target_data = target_data,
      plan = plan,
      exe_lasomo = FALSE
    )
    write_csv(result, out_file, append = TRUE)
  }
)

# =============================================================================
# Further testing for lasomo only with more combinations of models and tasks,
# but only for a few fixed number of tasks
existing_grid <- read_csv(out_file) |>
  filter(!is.na(elapsed_lasomo)) |>
  select(n_models, n_tasks, plan)

n_mods_more <- 14
grid_full <- expand.grid(
  n_models = 2:n_mods_more,
  n_tasks = 5,
  plan = "multisession",
  stringsAsFactors = FALSE
)
grid_new <- grid_full |>
  anti_join(existing_grid, by = c("n_models", "n_tasks", "plan"))

purrr::pmap(
  grid_new,
  function(n_models, n_tasks, plan) {
    print(paste("Testing with", n_models, "models and", n_tasks, "tasks"))
    result <- elapsed_time(
      n_models,
      n_tasks,
      forecast_data = forecast_data,
      target_data = target_data,
      plan = plan
    ) |>
      mutate(elapsed_lomo = NA) # We only want to test lasomo here, so we set elapsed_lomo to NA
    write_csv(result, out_file, append = TRUE)
  }
)

# =============================================================================
# Further testing for lomo with thousand tasks, but only for a few fixed number of models
existing_grid <- read_csv(out_file) |>
  select(n_models, n_tasks, plan)

grid_full <- expand.grid(
  n_models = c(10, 20, 50, 100),
  n_tasks = c(1000, 2000, 5000, 10000),
  plan = "multisession",
  stringsAsFactors = FALSE
)
grid_new <- grid_full |>
  anti_join(existing_grid, by = c("n_models", "n_tasks", "plan"))

purrr::pmap(
  grid_new,
  function(n_models, n_tasks, plan) {
    print(paste("Testing with", n_models, "models and", n_tasks, "tasks"))
    result <- elapsed_time(
      n_models,
      n_tasks,
      forecast_data = forecast_data,
      target_data = target_data,
      plan = plan,
      exe_lasomo = FALSE
    )
    write_csv(result, out_file, append = TRUE)
  }
)

# =============================================================================
# Further testing for lomo with thousand tasks, but only for a few fixed number of models
existing_grid <- read_csv(out_file) |>
  select(n_models, n_tasks, plan)

grid_full <- expand.grid(
  n_models = 10,
  n_tasks = c(10, 20, 50, 100),
  plan = "multisession",
  stringsAsFactors = FALSE
)
grid_new <- grid_full |>
  anti_join(existing_grid, by = c("n_models", "n_tasks", "plan"))

purrr::pmap(
  grid_new,
  function(n_models, n_tasks, plan) {
    print(paste("Testing with", n_models, "models and", n_tasks, "tasks"))
    result <- elapsed_time(
      n_models,
      n_tasks,
      forecast_data = forecast_data,
      target_data = target_data,
      plan = plan
    ) |>
      mutate(elapsed_lomo = NA) # We only want to test lasomo here, so we set elapsed_lomo to NA
    write_csv(result, out_file, append = TRUE)
  }
)

# =============================================================================
# Further testing with sequential plan for a few fixed number of models and tasks
existing_grid <- read_csv(out_file) |>
  select(n_models, n_tasks, plan)

grid_full <- expand.grid(
  n_models = 2:10,
  n_tasks = c(5, 10, 20, 50, 100),
  plan = "sequential",
  stringsAsFactors = FALSE
)

grid_new <- grid_full |>
  anti_join(existing_grid, by = c("n_models", "n_tasks", "plan"))

purrr::pmap(
  grid_new,
  function(n_models, n_tasks, plan) {
    print(paste("Testing with", n_models, "models and", n_tasks, "tasks"))
    result <- elapsed_time(
      n_models,
      n_tasks,
      forecast_data = forecast_data,
      target_data = target_data,
      plan = plan
    )
    write_csv(result, out_file, append = TRUE)
  }
)
