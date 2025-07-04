if (nzchar(Sys.getenv("CI"))) {
  future::plan("sequential")
}
