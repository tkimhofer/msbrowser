#' @title  Launch MSbrowser application
#'
#' @export startApp
#'
#' @return shiny application object
#' @import shiny



startApp <- function() {
  shinyApp(ui = ui, server = server)
}
