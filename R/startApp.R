#' @title  launch the msbrowser app
#'
#' @export startApp
#'
#' @return shiny application object
#' @import shiny



startApp <- function() {
  shinyApp(ui = ui, server = server)
}
