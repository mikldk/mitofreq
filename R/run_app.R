#' Run the shiny app MitoFREQ
#' 
#' @export
run_mitofreq_shiny_app <- function() {
  app_dir <- system.file("MitoFREQ", package = "mitofreq")
  
  if (app_dir == "") {
    stop("Could not find MitoFREQ Try re-installing `mitofreq` package.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}
