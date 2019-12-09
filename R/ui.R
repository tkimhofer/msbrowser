#' @title  Shiny app server function
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @importFrom shinybusy add_busy_bar
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput

ui <- fluidPage(
  #responsive = T,
  #title='MSbrowser',
  useShinyjs(),  # Set up shinyjs
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Pacifico&display=swap');
      h1 {
        font-family: 'Pacifico', cursive;
        font-weight: 900;
        line-height: 2;
        color: #FBDD00;
      }

    "))
  ),
  #fluidRow('MSbrowser'),

  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #991500;}"))
  ),
  #titlePanel(textOutput("title_panel")),
  headerPanel('MSbrowser...'),

  fluidRow(column(4,offset=0.2,

                  #style = "position:fixed;width:inherit;",

                  div(id ="div_input",
                      h3('1. Read-in LC-MS experiment'),
                      helpText('Choose an LC-MS data file in open data format (e.g., mzML, netCDF)'),
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
                               div(id ="summary_file",
                                   column(12, offset = -1, align='center',
                                          textOutput('msfile'),
                                          h4('Summary'),
                                          withSpinner(tableOutput("datsum"))
                                   ),
                                   br(),
                                   column(12, offset = 0.7, align='left',
                                          checkboxInput('imp_xic', 'Select XIC mass range manually', value = F),
                                          div(id ="div_xic",
                                              fluidRow(
                                                column(6,  numericRangeInput(inputId='xic_ra', label=NA, value=NULL, separator = " to ", width='100%')),
                                                column(6,  actionButton('go_xic', 'Go'))
                                              )) ),
                                   fluidRow(
                                     column(12, align='right',
                                            actionButton("move", 'Let\'s move on!', icon("thumbs-up"),
                                                         style="color: #fff; background-color: #33A2FF; border-color: #33A2FF")))
                               )
                        )),
                      br()),
                  hr(),
                  br(),

                  div(id ="div_target",
                      h3('2. Select target signal'),
                      helpText("Specify a spectral area either through manual entry of a rt and mz value, or use database-listed compound information."),
                      br(),
                      column(12, offset = 0.7,  align="center",
                             radioButtons('target_input', label = NULL, choices = c('Cursor selection'='click', 'Manual'='man', 'Database'='db'), inline = T, selected='click'),
                             conditionalPanel("input.target_input=='click'",
                                              textOutput('selection'))),
                      bsTooltip(id="target_input", title="Manual: enter mz and retention time manually, database: pre-entered values in file xxx.)",
                                placement="right", options = list(container = "body")),
                      column(12, offset = 0.7,
                             conditionalPanel("input.target_input=='man'",
                                              helpText('The chromatograms in the main panel can be used to identify target areas of high and low peak density.'),
                                              br(),
                                              fluidRow(
                                                column(12,
                                                       column(width=8, numericInput(inputId='in_rt','Retention time (s)', value=98.4)),
                                                       column(width=4, numericInput(inputId='in_rt_ws','window size (s)', value=25))
                                                )),

                                              fluidRow(
                                                column(12,
                                                       column(width=8, numericInput(inputId='in_mz','Mass to charge ratio', value=269.1109)),
                                                       column(width=4, numericInput(inputId='in_mz_ws','window size', value=10))
                                                ))

                             ),
                             conditionalPanel("input.target_input=='db'",
                                              helpText('Select assay type and use the list below as shortcut to compounds with database records. The list can be edited through the master csv file (see documentation).'),
                                              br(),
                                              fluidRow(
                                                column(12,
                                                       radioGroupButtons(inputId= "db_assays", label = "Assay type", choices = unique(icst$assay), direction = "horizontal"),
                                                       selectizeInput('in_icst', label='Compounds', choices = c('Select assay')),
                                                       textOutput('compound_info'), br()
                                                ))




                             )),
                      br(),
                      br(),
                      div(id ="div_vis",
                          br(),
                          fluidRow(
                            column(12, offset=0.7,
                                   column(width=3, numericInput(inputId='in_noisethr', label = 'Noise Threshold', value=100)),

                                   column(width=9, align='center', radioGroupButtons('raw_trans', label='Data Transformation', choices = list('None'='none', 'Squared'='sqrt', 'Square Root'='exp', 'Log 10'='log10', 'Logit'='logit','Reciprocal'='reciprocal'), selected = 'log10'))


                            )
                          )
                      ),
                      fluidRow(
                        column(2, offset=0.7, align='left',
                               actionButton('goImage', label='Generate plot', inline=T)),
                        column(10, align='left',
                               checkboxInput('imp_vis', 'Viz options', value = F))
                      ),
                      #helpText("To improve visualisation add an inital intensity value as noise threshold as well as data transformation (reflected in point size and colour scale, respectively). Initial specifications can be altered once a plot is generated."),

                      fluidRow(
                        column(12, align='right',
                               actionButton("move1", 'Perform Peak Picking', icon("thumbs-up"),
                                            style="color: #fff; background-color: #33A2FF; border-color: #33A2FF")))

                  ),
                  div(id ="div_ppick",
                      br(),
                      # hr(),
                      br(),
                      hr(),
                      br(),
                      h3('3. Perform peak picking'),
                      helpText("Select peak picking algorighm from the list below"),

                      fluidRow(align='center', offset=0.7,
                               column(12, offset=0.7,
                                      #wellPanel(

                                      br(),
                                      fluidRow(align='center',
                                               selectInput('in_pickMethod', label='Alogrithm', choices = c('Peak density and wavelet-based (centWave)'='centWave', 'Matched filter-based (matchedFilter)'='matchedFilter', 'DBSCAN'='dbscan'))
                                      ),

                                      conditionalPanel(
                                        condition="input.in_pickMethod=='centWave'",
                                        fluidRow(
                                          h4('Parameters'),
                                          helpText("The following peak picking parameters are the standard parameters defined by xcms - these nearly always require optimisation for each data set (most importantly: ppm, rt range and noise level)."),
                                          hr(),
                                          column(4, numericInput(inputId='in_mzdev', label=paste0('m/z deviation (ppm)'), value=25),
                                                 br(),
                                                 sliderInput('in_rtrange', 'Peakwidth: RT range (s)', min=1, max=100, step = 1, value= c(20, 50)),
                                                 br(),
                                                 numericInput('in_mzdiff', label = 'minimum diff m/z overlap', value = -0.001)
                                          ),
                                          column(4,
                                                 numericInput(inputId='in_noise', label='Noise', value=0),
                                                 br(),
                                                 numericInput(inputId='in_sn', label='Signal/Noise threshold', value=10),
                                                 br(),
                                                 selectInput('in_mzCentFun', label='m/z center function', choices = c('Weighted Mean'='wMean', 'Mean'='mean','Peak apex'='apex', 'Weighted mean of peak apex and neigbouring scans'='wMeanApex3', 'Mean of peak apex and neigbouring scans'='meanApex3'))
                                          ),
                                          column(4,
                                                 wellPanel(h5("Pre-filter"),
                                                           numericInput('in_prefilter_k', label = 'Number of consecutive scans...', min = 0, max=100, value = 3),
                                                           br(),
                                                           numericInput('in_prefilter_I', label = '...exceeding Intensity of', min = 0, max=10000000, value = 100)
                                                 ),
                                                 br(),
                                                 selectInput('in_integrate', label='Integration method', choices = c('Descend Mexican Hat'='1', 'Real MS data'='2'), selected='1')

                                          ))
                                        ),
                                      conditionalPanel(
                                        condition="input.in_pickMethod=='matchedFilter'",
                                        fluidRow(
                                          h4('Parameters'),
                                          helpText("The following peak picking parameters are the standard parameters defined by xcms - these nearly always require optimisation for each data set."),
                                          hr(),
                                          column(4, numericInput(inputId='in_fwhm', label=paste0('FWHM of matched filtration Gaussian'), value=30),
                                                 br(),
                                                 numericInput('in_sigma', 'SD of matched Gaussian', value= 2.3548)
                                          ),
                                          column(4,
                                                 numericInput(inputId='in_step', label='Bin width m/z dim.', value=0.1),
                                                 br(),
                                                 numericInput(inputId='in_steps', label='Bin to merge before filtration', value=2),
                                                 br(),
                                                 numericInput(inputId='in_mzdiff', label='Min. difference mz for peaks w overlapping rt\'s', value=0.8)
                                          ),
                                          column(4,
                                                 numericInput('in_max', label = 'Maximum  number expected peaks / slice', value = 5),
                                                 br(),
                                                 numericInput(inputId='in_snthres', label='S/N cutoff chromatogr. dim.', value=100)
                                          ))

                                        ),
                                      conditionalPanel(
                                        condition="input.in_pickMethod=='dbscan'",
                                        fluidRow(
                                          h4('Parameters'),
                                          helpText("The following are pre-defined paramter values, these should be tested for every data set."),
                                          hr(),
                                          column(4,
                                                 numericInput(inputId='in_ppm', label=paste0('inflate mz to accommodate detectors m/z accuracy (ppm in xcms)'), value=1e4),
                                                 numericInput(inputId='in_ppm_mztrans', label=paste0('transformation factor to match rt stepsize'), value=15)


                                          ),
                                          column(4,
                                                 numericInput(inputId='in_eps', label='Radius of neighbourhood', value=1),
                                                 br(),
                                                 numericInput('in_minPts', 'Minimum Number of points in each neighbourhood', value= 2)

                                          ),
                                          column(4,
                                                 numericInput(inputId='in_noise', label=paste0('noise threshold'), value=1),
                                                 br(),
                                                 numericInput(inputId='in_rttrans', label=paste0('rt trans (might not be needed)'), value=1)
                                          ))
                                        )


                               )#)
                      ),
                      actionButton('pickpeak1', label='Pick Peaks!'))


                  ),

           column(8,offset=0.2,

                  tabsetPanel(id='msexpl',
                              tabPanel("Chromatograms and mass spectrum",  value='ichron',
                                       br(),
                                       #conditionalPanel("input.chromtype=='tic'",
                                       withSpinner(plotlyOutput("tic_bpc"), type=4),
                                       withSpinner(plotlyOutput("xic"), type=4),
                                       withSpinner(plotlyOutput("barsScan"), type=4),
                                       add_busy_bar(color = "#FBDD00")
                              ),
                              # tabPanel("BPC",
                              #          br(),
                              #          #wellPanel(plotOutput("bpc"))
                              #          wellPanel(plotlyOutput("bpc"))
                              # ),
                              tabPanel(title="Raw Data",  value='rawData',
                                       br(),
                                       withSpinner(plotlyOutput('rawdd', width = "100%",
                                                                inline = T), type=4),
                                       add_busy_bar(color = "#FBDD00")
                              ),
                              tabPanel("Peak Picked",  value='ppick',
                                       withSpinner(plotlyOutput('pp1', width = "100%", inline = T), type=4),
                                       add_busy_bar(color = "#FBDD00")
                              ),
                              tabPanel("Peak Table",  value='peaks',
                                       column(8, align='center',
                                              br(),
                                              fluidRow(DTOutput("PeakTbl", width='auto', height='auto')),
                                              br(),
                                              fluidRow(actionButton(inputId="plotselection", 'Plot Peak Intensities'))
                                       ),
                                       column(4, align='center',
                                              plotlyOutput('peakplt',width='80%', height='auto',
                                                           inline = F),
                                              plotlyOutput('peakpltIso',width='80%', height='auto',
                                                           inline = F)
                                       ),
                                       add_busy_bar(color = "#FBDD00")

                              )
                  )

           )




           )


  )
