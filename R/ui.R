# Shiny app UI function
# UI part of the app
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @importFrom shinybusy add_busy_bar
#' @importFrom plotly plotlyOutput
ui <- fluidPage(tags$body(tags$style(HTML("
      h3 {
        color: #22678D;
      }
    "))),
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Lobster|Roboto&display=swap');

      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 800;
        line-height: 1.1;
        font-size: 380%;
      }
    "))),
    tags$script(HTML("
    function doThis(el) {
    Shiny.setInputValue(\"clicked_text\", \"Torben\", {priority: \"event\"});
    };
     function doThat(el) {
    Shiny.setInputValue(\"clicked_target\", \"Torben\", {priority: \"event\"});
    }
    ")),
    titlePanel(title = h1(HTML("<span style=\"color: #FBDD00\">MS</span><span style=\"color: #00DBB0\">browser</span><span style=\"color: #00DBB0\">...</span>")),
        windowTitle = "MSbrowser"), fluidRow(column(4, offset = 0.2, div(id = "div_input",
        h3(a(href = "#", onclick = "doThis(this)", "1. Read-in LC-MS experiment")),
        div(id = "1ri", helpText("Choose an LC-MS data file in open data format (e.g., mzML)")),
        uiE_div_inp_col)), column(8, offset = 0.5, tabsetPanel(id = "msexpl"))),
    br(), fluidRow(br(), br(), br(), column(11, offset = 0.5, h5(a(id = "Info",
        icon("github"), "Report an issue or suggest to add a feature",
        href = "https://github.com/tkimhofer/msbrowser/issues", target = "_blank")),
        h5(a(id = "author", icon("external-link-alt"), "About the developer",
            href = "https://tkimhofer.com", target = "_blank"), align = "right")),
        align = "right"))
