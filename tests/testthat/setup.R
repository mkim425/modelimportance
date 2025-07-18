if (requireNamespace("future", quietly = TRUE)) {
  os <- Sys.info()[["sysname"]]
  ci <- tolower(Sys.getenv("CI", "false")) == "true"

  if (ci && os == "Darwin") {
    future::plan(future::sequential)
    message("CI macOS detected – using future::sequential")
  } else {
    future::plan(future::multisession)
    message("Using future::multisession")
  }
}

if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  future::plan("sequential")
  message("GITHUB_ACTIONS detected – using future::sequential")
}
