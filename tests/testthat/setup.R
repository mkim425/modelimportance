if (Sys.getenv("CI") == "true" && Sys.info()["sysname"] == "Darwin") {
        future::plan(sequential)
} else {
        future::plan(multisession)
}
