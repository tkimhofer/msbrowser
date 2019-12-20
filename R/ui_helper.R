# UI elements for insertUI and removeUI commands
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @import shiny


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
