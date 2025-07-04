if (requireNamespace("future", quietly = TRUE)) {
  os <- Sys.info()[["sysname"]]
  ci <- tolower(Sys.getenv("CI", "false")) == "true"

  if (ci) {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession)
  }
}
