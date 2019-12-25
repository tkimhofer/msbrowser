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

  # tags$head(
  #   tags$style(HTML("hr {border-top: 1px solid #991500;}"))
  # ),
  #titlePanel(title=h1('MSbrowser')),
  headerPanel(title=div(a(img(src='www/MSbrowser_logo_tricolour_alpha.png')), href='https://tkimhofer.com', target="_blank"), windowTitle='MSbrowser'),
  #titlePanel(title=div(id="MSbrowser_logo", img(height = 100, width = 100, src = "MSbrowser_logo_tricolour_alpha.png"))),

  fluidRow(column(4,offset=0.2,
                  #tags$img(src='MSbrowser_logo_tricolour_alpha.png')
                  #img(src='inst/extdata/MSbrowser_logo_tricolour_alpha.png', align='right'),
                  #style = "position:fixed;width:inherit;",

                  div(id ="div_input",
                      h3('1. Read-in LC-MS experiment'),
                      helpText('Choose an LC-MS full scan mode data file in open data format (e.g., mzML, netCDF)'),
                      fluidRow(
                        column(12, offset = 0.7,
                               fluidRow(
                                 column(10,
                                        actionButton("filechoose", label = "Select file"), textOutput('bname', inline = T),
                                        actionButton("fileexample", label = "Load Example", inline=T),
                                        bsTooltip(id="filechoose", title="Choose an LC-MS data file in open data format (e.g., mzML, netCDF)",
                                                  placement="right", options = list(container = "body")),
                                        bsTooltip(id="fileexample", title="Use an example LC-MS file.",
                                                  placement="right", options = list(container = "body")))),
                               br(),
                               add_busy_bar(color = "#FBDD00")),
                        # div(id ="summary_file",
                        #     column(12, offset = -1, align='center',
                        #            textOutput('msfile'),
                        #            h4('Summary'),
                        #            tableOutput("datsum")
                        #            #withSpinner()
                        #     )
                        # ),
                        br(),
                        div(id ="div_xic"
                            #,
                            # div(id ="div_xic",
                            #     fluidRow(
                            #       column(6,  numericRangeInput(inputId='xic_ra', label=NA, value=NULL, separator = " to ", width='100%')),
                            #       column(6,  actionButton('go_xic', 'Go'))
                            #     ))

                        )


                      )),
                  br(),
                  #hr(),
                  br()





  ),

  column(8,offset=0.2,

         tabsetPanel(id='msexpl'
                     # div(id='tabs_in')
         ),


         h5(a(id='Info', icon("github"), 'Report an issue or suggest to add a feature', href='https://github.com/tkimhofer/msbrowser/issues', target="_blank")),
         h5(a(id='author', icon("external-link-alt"), 'About the creator', href='https://tkimhofer.com', target="_blank"))
  )




  )

)
