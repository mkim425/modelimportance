if (requireNamespace("future", quietly = TRUE)) {
  if (Sys.getenv("CI") == "true" && Sys.info()["sysname"] == "Darwin") {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession)
  }
}
