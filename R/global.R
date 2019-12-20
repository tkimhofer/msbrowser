#' @title UI elements for insertUI and removeUI commands
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinybusy add_busy_bar


icst=read.table('inst/extdata/signalDB.csv', sep=',', stringsAsFactors = F, comment.char ='#', blank.lines.skip = T,row.names = NULL, skip=1, col.names = c('assay', 'compound', 'mz', 'rt', 'info'))

icst=icst[icst$assay!='' & !is.na(icst$assay),]

#icst=dlply(icst, .(assay))




ui_par_centwave=fluidRow(
  h4('Parameters'),
  helpText("The following peak picking parameters are the standard parameters defined by xcms - these nearly always require optimisation for each data set."),
  hr(),
  column(4, numericInput(inputId='in_mzdev', label=paste0('m/z deviation (ppm)'), value=15),
         br(),
         sliderInput('in_rtrange', 'Peakwidth: RT range (s)', min=2, max=100, step = 1, value= c(20, 50)),
         br(),
         numericInput('in_mzdiff', label = 'minimum diff m/z overlap', value = 0.1)
  ),
  column(4,
         numericInput(inputId='in_noise', label='Noise', value=100),
         br(),
         numericInput(inputId='in_sn', label='Signal/Noise threshold', value=10),
         br(),
         selectInput('in_mzCentFun', label='m/z center function', choices = c('Weighted Mean'='wMean', 'Mean'='mean','Peak apex'='apex', 'Weighted mean of peak apex and neigbouring scans'='wMeanApex3', 'Mean of peak apex and neigbouring scans'='meanApex3'))
  ),
  column(4,
         wellPanel(h5("Pre-filter"),
                   numericInput('in_prefilter_k', label = 'Number of consecutive scans...', min = 3, max=10, value = 3),
                   br(),
                   numericInput('in_prefilter_I', label = '...exceeding Intensity of', min = 3, max=10, value = 3)
         ),
         br(),
         selectInput('in_integrate', label='Integration method', choices = c('Descend Mexican Hat'='1', 'MS data'='2'), selected='1')

  ))


ui_par_matchedFilter=fluidRow(
  h4('Parameters'),
  helpText("Specify the expected mass to charge ratio (m/s) and retention time in seconds (s) of a compound. Use the list below to select pre-defined internal chemical standards (ICS) for HILIC positive ionisation mode (v+)."),
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








uiT_ichron=tabPanel("Chromatograms and mass spectrum",  value='ichron',
                    br(),
                    #conditionalPanel("input.chromtype=='tic'",
                    withSpinner(plotlyOutput("tic_bpc"), type=8),
                    withSpinner(plotlyOutput("xic"), type=8),
                    withSpinner(plotlyOutput("barsScan"), type=8),
                    add_busy_bar(color = "#FBDD00")
)


uiT_rawData=tabPanel(title="Raw Data",  value='rawData',
                     br(),
                     withSpinner(plotlyOutput('rawdd', width = "100%",
                                              inline = T), type=4, color="#0dc5c1"),
                     add_busy_bar(color = "#FBDD00")
)

uiT_ppick=tabPanel("Peak Picked",  value='ppick',
                   withSpinner(plotlyOutput('pp1', width = "100%", inline = T), type=4),
                   add_busy_bar(color = "#FBDD00")
)

uiT_peaks=tabPanel("Peak Table",  value='peaks',
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

uiE_div_xic=div(id ="div_xic", fluidRow(
  column(8,  numericRangeInput(inputId='xic_ra', label=NA, value=c(500), separator = " to ", width='100%')),
  column(4,  actionButton('go_xic', 'Go'))
))

uiE_div_summary_file=div(id ="summary_file",
                         column(12, offset = -1, align='center',
                                br(),
                                h4('Summary'),
                                textOutput('msfile'),
                                br(),
                                withSpinner(tableOutput("datsum"), type=8)
                         ),
                         br(),
                         column(12, offset = 0.7, align='left',
                                checkboxInput('imp_xic', 'Select XIC mass range manually', value = F))
)


uiE_move=fluidRow(
  column(12, align='right',
         actionButton("move", 'Let\'s move on!', icon("thumbs-up"),
                      style="color: #fff; background-color: #33A2FF; border-color: #33A2FF")))

uiE_target=div(id ="div_target",
               h3('2. Select target signal'),
               helpText("Specify a spectral area either through manual entry of a rt and mz value, or use database-listed compound information."),
               br(),

               div(id='selectors',
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
                   br()),
               fluidRow(
                 column(12, align='right',
                        actionButton("move_picks", 'Generate plot', icon("thumbs-up"),
                                     style="color: #fff; background-color: #33A2FF; border-color: #33A2FF"))),
               fluidRow(
                 div(id='selectors1',
                     column(12, offset=0.7, align='left', checkboxInput('imp_vis', 'Viz options', value = F))))
               #helpText("To improve visualisation add an inital intensity value as noise threshold as well as data transformation (reflected in point size and colour scale, respectively). Initial specifications can be altered once a plot is generated."),



)


uiE_div_ppick=div(id ="div_ppick",
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
                                           selectInput('in_pickMethod', label='Algorithm', choices = c('CentWave'='centWave', 'Matched Filter'='matchedFilter', 'DBSCAN'='dbscan'))
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


                           )
                  ),
                  actionButton('pickpeak1', label='Pick Peaks!'))





# helper functions

xic_mzrange=function(xic_mz, ppm){

  mz_window = (xic_mz*(ppm/10^6))/2
  low=xic_mz-mz_window
  high=xic_mz+mz_window
  return(c(xic_mz-mz_window, xic_mz+mz_window, xic_mz))

}
