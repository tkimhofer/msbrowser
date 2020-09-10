#' @title  Launch MSbrowser application
#'
#' @export startApp
#'
#' @return shiny application object
#' @examples
#' msbrowser::startApp()
#' @import shiny



startApp <- function() {
  shinyApp(ui = ui, server = server)
}
