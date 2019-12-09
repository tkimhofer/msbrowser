#' @title  launches the shinyAppDemo app
#'
#' @export startApp
#'
#' @return shiny application object
#' @import shiny


# wrapper for shiny::shinyApp()
startApp <- function() {
  shinyApp(ui = ui, server = server)
}
