
#' @export
run_app <- function(){
  app_file <- system.file("PharmaReApCon-app/app.R", package = "PharmaReApCon")
  shiny::runApp(app_file, port = 3838)
}
