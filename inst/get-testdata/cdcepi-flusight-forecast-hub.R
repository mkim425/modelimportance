## Get dataset for testing with trained ensembles using FluSight Forecasts
# 8 reference dates: "2025-04-12", "2025-04-19", "2025-04-26", "2025-05-03",
# "2025-05-10", "2025-05-17", "2025-05-24", "2025-05-31"
# 2 locations: "25" (MA), "48" (TX)
# 1 target: "wk inc flu hosp"
# 2 horizons: 1, 3
# 3 output types: "quantile", "pmf", median
# 4 model_id: "Flusight-baseline", "MOBS-GLEAM_FLUH", "UMass-flusion",
#             "GT-FluFNP"
# =============================================================================
# install hubData package if not already installed
install.packages("hubData", repos = c(
  "https://hubverse-org.r-universe.dev",
  "https://cloud.r-project.org"
))

library(dplyr)
library(hubData)

bucket_name <- "cdcepi-flusight-forecast-hub"
hub_bucket <- s3_bucket(bucket_name)
hub_con <- hubData::connect_hub(
  hub_bucket,
  file_format = "parquet", skip_checks = TRUE
)

ref_dates <- c(
  "2025-04-12", "2025-04-19", "2025-04-26", "2025-05-03",
  "2025-05-10", "2025-05-17", "2025-05-24", "2025-05-31"
)
models <- c(
  "JHUAPL-DMD", "MOBS-GLEAM_FLUH", "NIH-Flu_ARIMA", "UMass-flusion"
)
model_output <- hub_con |>
  dplyr::filter(
    reference_date %in% ref_dates,
    model_id %in% models,
    horizon %in% c(1, 3),
    target %in% c("wk inc flu hosp", "wk flu hosp rate change"),
    location %in% c("25", "48"),
    output_type %in% c("quantile", "pmf")
  ) |>
  hubData::collect_hub()

# datasets by output type
fdat_qntl_w_trainset <- model_output |>
  dplyr::filter(output_type == "quantile")

fdat_pmf_w_trainset <- model_output |>
  dplyr::filter(output_type == "pmf")

fdat_median_w_trainset <- model_output |>
  dplyr::filter(
    output_type == "quantile",
    output_type_id == "0.5"
  ) |>
  dplyr::mutate(
    output_type = "median",
    output_type_id = NA
  )


# Save datasets
saveRDS(fdat_qntl_w_trainset,
  file = "tests/testthat/testdata/for-score_trained/fdat_qntl_w_trainset.rds"
)

saveRDS(fdat_pmf_w_trainset,
  file = "tests/testthat/testdata/for-score_trained/fdat_pmf_w_trainset.rds"
)

saveRDS(fdat_median_w_trainset,
  file = "tests/testthat/testdata/for-score_trained/fdat_median_w_trainset.rds"
)
# =============================================================================
# target data
# directly download 'oracle-output.csv' from
# [https://github.com/cdcepi/FluSight-forecast-hub/target-data]
# save it as
# 'tests/testthat/testdata/for-score_trained/flusight_oracle_output.csv'
#
# Then read and save as RDS:
library(readr)
d <- read_csv(
  "tests/testthat/testdata/for-score_trained/flusight_oracle_output.csv"
) |>
  dplyr::filter(horizon == 0) |>
  dplyr::select(-c("as_of", "horizon")) |>
  distinct()

dd <- rbind(d, d |>
  dplyr::filter(output_type == "quantile") |>
  dplyr::mutate(
    output_type = "median",
    output_type_id = NA
  ))
saveRDS(
  dd,
  "tests/testthat/testdata/for-score_trained/flusight_oracle_output.rds"
)
