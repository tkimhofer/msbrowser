#' @title  Shiny app server function
#' @import shiny
# @import shinyjs
# @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @importFrom shinybusy add_busy_bar
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput

logoF=list.files(system.file('www', package = "msbrowser"), pattern='logo')[1]
print(logoF)

ui <- fluidPage(
  #responsive = T,
  #title='MSbrowser',
  #useShinyjs(),  # Set up shinyjs
  tags$body(
    tags$style(HTML("

      h3 {
        color: #22678D;
      }

    "))
  ),


  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Indie+Flower|Cabin:400,700');

      h1 {
        font-family: 'Indie Flower', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
      }

    "))
  ),

  tags$script(
    HTML(
    '
    function doThis(el) {
    Shiny.setInputValue("clicked_text", "Torben", {priority: "event"});
    };
     function doThat(el) {
    Shiny.setInputValue("clicked_target", "Torben", {priority: "event"});
    }

    '
    )),


  #   "function doSomething(hd) {
  # Shiny.onInputChange('clicked_text', hd.innerHTML);
  #   }"
  # )),
  # tags$head(
  #   tags$style(HTML("hr {border-top: 1px solid #991500;}"))
  # ),
  #titlePanel(title=h1('MSbrowser')),
  headerPanel(title=div(a(img(src=logoF)), href='https://tkimhofer.com', target="_blank"), windowTitle='MSbrowser'),
  #titlePanel(title=div(id="MSbrowser_logo", img(height = 100, width = 100, src = "MSbrowser_logo_tricolour_alpha.png"))),

  fluidRow(column(4,offset=0.2,
                  #tags$img(src='MSbrowser_logo_tricolour_alpha.png')
                  #img(src='inst/extdata/MSbrowser_logo_tricolour_alpha.png', align='right'),
                  #style = "position:fixed;width:inherit;",

                  div(id ="div_input",
                      h3(a(href='#', onclick='doThis(this)', '1. Read-in LC-MS experiment')),
                      div(id='1ri', helpText('Choose an LC-MS full scan mode data file in open data format (e.g., mzML, netCDF)')),
                      uiE_div_inp_col)),

  column(8,offset=0.5,

         tabsetPanel(id='msexpl'
                     # div(id='tabs_in')
         )
  )
  ),
  br(),
  fluidRow(

    br(),
    hr(),
    br(),
    column(11, offset=0.5,
                  h5(a(id='Info', icon("github"), 'Report an issue or suggest to add a feature', href='https://github.com/tkimhofer/msbrowser/issues', target="_blank")), h5(a(id='author', icon("external-link-alt"), 'About the developer', href='https://tkimhofer.com', target="_blank"), align='right')), align='right')
)
