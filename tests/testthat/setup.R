if (requireNamespace("future", quietly = TRUE)) {
  os <- Sys.info()[["sysname"]]
  ci <- tolower(Sys.getenv("CI", "false")) == "true"

  if (ci) {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession)
  }
}

if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  future::plan("sequential")
}
