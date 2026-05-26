# Datasets sourced from hubExamples v1.0.0 (hubverse-org/hubExamples@v1.0.0)
# Run this script to regenerate data/ if hubExamples is updated.

library(hubExamples)

forecast_outputs <- hubExamples::forecast_outputs
forecast_target_ts <- hubExamples::forecast_target_ts
forecast_oracle_output <- hubExamples::forecast_oracle_output

usethis::use_data(
  forecast_outputs,
  forecast_target_ts,
  forecast_oracle_output,
  overwrite = TRUE
)
