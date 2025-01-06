#' @name TemplateApp
#' @return
#' @examples
#' @export

TemplateApp <- function() {
  connection <- system.file("App.R", package = "GTableauPipeline")
  
  shiny::runApp(connection, display.mode = "normal")
}