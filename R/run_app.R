#' Run the EPA LIGIT R Shiny Tool
#'
#' @return No return value; launches the app.
#' @export
run_epaligt<- function() {
  get_app_main_path <- function() {
    repo_path <- "inst/app/app_main.R"
    pkg_dir <- system.file("app", package = "epaligt")
    pkg_path <- if (nzchar(pkg_dir) && dir.exists(pkg_dir)) file.path(pkg_dir, "app_main.R") else ""
    
    if (file.exists(repo_path)) return(repo_path)
    if (nzchar(pkg_path) && file.exists(pkg_path)) return(pkg_path)
    
    stop("app_main.R not found in repo or installed package.")
  }
  
  app_env <- new.env(parent = globalenv())
  sys.source(get_app_main_path(), envir = app_env)
  
  if (!exists("run_app", envir = app_env, mode = "function")) {
    stop("run_app() not found after sourcing app_main.R.")
  }
  
  app_env$run_app()
}