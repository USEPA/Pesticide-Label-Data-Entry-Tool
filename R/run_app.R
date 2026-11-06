run_LIGIT <- function() {
  app_main_path <- system.file("app", "app_main.R", package = "epa_ligit")
  
  if (!nzchar(app_main_path) || !file.exists(app_main_path)) {
    stop("app_main.R not found in installed package.")
  }
  
  source(app_main_path, local = TRUE)
  run_app()
}