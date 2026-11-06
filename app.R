get_app_main_path <- function() {
  repo_path <- "inst/app/app_main.R"
  pkg_path <- system.file("app", "app_main.R", package = "epa_ligit")
  
  if (file.exists(repo_path)) return(repo_path)
  if (nzchar(pkg_path) && file.exists(pkg_path)) return(pkg_path)
  
  stop("app_main.R not found in repo or package.")
}

app_main_path <- get_app_main_path()
source(app_main_path, local = TRUE)
run_app()