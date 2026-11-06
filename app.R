get_app_main_path <- function() {
  repo_path <- "inst/app/app_main.R"
  pkg_dir <- system.file("app", package = "EPAligit")
  pkg_path <- if (nzchar(pkg_dir) && dir.exists(pkg_dir)) file.path(pkg_dir, "app_main.R") else ""
  
  if (file.exists(repo_path)) return(repo_path)
  if (nzchar(pkg_path) && file.exists(pkg_path)) return(pkg_path)
  
  stop("app_main.R not found in repo or installed package.")
}

app_env <- new.env(parent = baseenv())
sys.source(get_app_main_path(), envir = app_env)
app_env$run_app()