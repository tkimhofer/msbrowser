#' @title UI elements used with insertUI and removeUI
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinybusy add_busy_bar
#' @importFrom shinyBS bsTooltip


# suppress warnings options(warn = -1)

icst <- read.table(file.path("inst", "extdata", "signalDB.csv", fsep = .Platform$file.sep), 
    sep = ",", stringsAsFactors = FALSE, comment.char = "#", blank.lines.skip = TRUE, 
    row.names = NULL, skip = 1, col.names = c("assay", "compound", "mz", 
        "rt", "info"))

icst <- icst[icst$assay != "" & !is.na(icst$assay), ]


ui_par_centwave <- fluidRow(column(12, offset = 0.2, h4("Parameterisation"), 
    helpText("The following peak picking parameters values are xcms pre-adjusted - these nearly always require optimisation for each instrumental setup.")), 
    hr(), column(4, numericInput(inputId = "in_mzdev", label = paste0("ppm"), 
        value = 15), bsTooltip("in_mzdev", "Maximal tolerated m/z deviation in consecutive scans in parts per million (ppm)"), 
        br(), sliderInput("in_rtrange", "peakwidth (s)", min = 2, max = 100, 
            step = 1, value = c(20, 50)), bsTooltip("in_rtrange", "Expected chromatographic peak width (elution/scan time). Given as range (min, max) in seconds"), 
        br(), numericInput("in_mzdiff", label = "mzdiff", value = 0.1), 
        bsTooltip("in_mzdiff", "Minimum closeness in m/z dimension for peaks with overlapping retention times; can take negative values, indicating a single data point can be allocated to two different features.")), 
    column(4, numericInput(inputId = "in_noise", label = "noise", value = 100), 
        br(), numericInput(inputId = "in_sn", label = "snthresh", value = 10), 
        br(), selectInput("in_mzCentFun", label = "mzCenterFun", choices = c(`Weighted Mean` = "wMean", 
            Mean = "mean", `Peak apex` = "apex", `Weighted mean of peak apex and neigbouring scans` = "wMeanApex3", 
            `Mean of peak apex and neigbouring scans` = "meanApex3"))), 
    column(4, wellPanel(h5("prefilter"), numericInput("in_prefilter_k", 
        label = "Number of consecutive scans...", min = 3, max = 10, value = 3), 
        br(), numericInput("in_prefilter_I", label = "...exceeding Intensity of", 
            min = 3, max = 10, value = 3)), br(), selectInput("in_integrate", 
        label = "integrate", choices = c(`Descend Mexican Hat` = "1", `MS data` = "2"), 
        selected = "1"), checkboxInput("in_fitgauss", label = "fitgauss", 
        value = FALSE)))


ui_par_matchedFilter <- fluidRow(h4("Parameterisation"), helpText("Specify the expected mass to charge ratio (m/s) and retention time in seconds (s) of a compound. Use the list below to select pre-defined internal chemical standards (ICS) for HILIC positive ionisation mode (v+)."), 
    hr(), column(4, numericInput(inputId = "in_fwhm", label = paste0("FWHM of matched filtration Gaussian"), 
        value = 30), br(), numericInput("in_sigma", "SD of matched Gaussian", 
        value = 2.3548)), column(4, numericInput(inputId = "in_step", label = "Bin width m/z dim.", 
        value = 0.1), br(), numericInput(inputId = "in_steps", label = "Bin to merge before filtration", 
        value = 2), br(), numericInput(inputId = "in_mzdiff", label = "Min. difference mz for peaks w overlapping rt's", 
        value = 0.8)), column(4, numericInput("in_max", label = "Maximum  number expected peaks / slice", 
        value = 5), br(), numericInput(inputId = "in_snthres", label = "S/N cutoff chromatogr. dim.", 
        value = 100)))


uiT_ichron <- tabPanel("Chromatograms and mass spectrum", value = "ichron", 
    br(), withSpinner(plotlyOutput("tic_bpc"), type = 8), withSpinner(plotlyOutput("xic"), 
        type = 8), withSpinner(plotlyOutput("ssms"), type = 8), add_busy_bar(color = "#FBDD00"))

uiT_rawData <- tabPanel(title = "Raw Data", value = "rawData", fluidRow(style = "height:1040px;", 
    withSpinner(plotlyOutput("rawdd", width = "100%", inline = TRUE), type = 4, 
        color = "#0dc5c1")), add_busy_bar(color = "#FBDD00"))

uiT_ppick <- tabPanel("Detected Features", value = "ppick", fluidRow(style = "height:1040px;", 
    withSpinner(plotlyOutput("pp1", width = "100%", inline = TRUE), type = 4)), 
    fluidRow(column(width = 11, wellPanel(span(h4("R console commands"), 
        style = "color: black"), span(textOutput("Rcode_loadp"), style = "font-family: Courier,courier; font-size:100%"), 
        br(), span(textOutput("Rcode_file"), style = "font-family: Courier,courier; font-size:100%"), 
        span(textOutput("Rcode_readIn"), style = "font-family: Courier,courier; font-size:100%"), 
        br(), span(textOutput("Rcode_ppick"), style = "font-family: Courier,courier; font-size:100%"))), 
        add_busy_bar(color = "#FBDD00")))

uiT_peaks <- tabPanel("Feature Table", value = "peaks", column(8, align = "center", 
    br(), fluidRow(DT::DTOutput("PeakTbl", width = "auto", height = "auto")), 
    br(), fluidRow(actionButton(inputId = "plotselection", "Plot Peak Intensities"))), 
    column(4, align = "center", plotlyOutput("peakplt", width = "80%", 
        height = "auto", inline = FALSE), plotlyOutput("peakpltIso", width = "80%", 
        height = "auto", inline = FALSE)), add_busy_bar(color = "#FBDD00"))



uiE_div_xic <- div(id = "div_xic", fluidRow(column(8, numericRangeInput(inputId = "xic_ra", 
    label = NA, value = c(200, 201), separator = " to ", width = "100%")), 
    column(2, actionButton("go_xic", "Go"))))

uiE_div_summary_file <- div(id = "summary_file", column(12, offset = -1, 
    align = "center", br(), textOutput("msfile"), br(), tableOutput("datsum")), 
    br(), column(12, offset = 0.7, align = "left", checkboxInput("imp_xic", 
        "Select m/z range for XIC manually", value = FALSE)), br(), div(id = "proceed", 
        br(), br(), column(12, offset = 0.7, align = "center", p(tags$strong(HTML("<span style=\"color:#33A2FF\">Click on signal in mass spectrum to proceed!</span>"))))))

uiE_div_inp_col <- div(id = "div_input_collapse", fluidRow(column(12, offset = 0.7, 
    fluidRow(column(10, actionButton("filechoose", label = "Select file"), 
        textOutput("bname", inline = TRUE), actionButton("fileexample", 
            label = "Load Example", inline = TRUE), bsTooltip(id = "filechoose", 
            title = "Choose an LC-MS data file in open data format (e.g., mzML)", 
            placement = "right", options = list(container = "body")), bsTooltip(id = "fileexample", 
            title = "Use an example LC-MS file.", placement = "right", 
            options = list(container = "body")))), br(), uiOutput("ss1"), 
    add_busy_bar(color = "#FBDD00"))))



uiE_move <- fluidRow(column(12, align = "right", actionButton("move", "Let's move on!", 
    icon("thumbs-up"), style = "color: #fff; background-color: #33A2FF; border-color: #33A2FF")))

uiE_target <- div(id = "div_target", h3(a(href = "#", onclick = "doThat(this)", 
    "2. Select target signal")), helpText("Specify a spectral area either through clicking in mass spectrum or by manual entry of a scantime and m/z value. Alternatively, select a compound listed in a database table."), 
    br())

uiE_div_tar_col <- div(id = "target_col", div(id = "selectors", column(12, 
    offset = 0.7, align = "center", radioButtons("target_input", label = NULL, 
        choices = c(`Cursor selection` = "click", Manual = "man", Database = "db"), 
        inline = TRUE, selected = "click"), conditionalPanel("input.target_input=='click'", 
        helpText("The chromatograms in the main panel can be used to identify target areas of high and low signal intensity."), 
        br(), textOutput("selection"), ), )), column(12, offset = 0.7, 
    conditionalPanel("input.target_input=='man'", helpText("Enter spectral region manually"), 
        br(), fluidRow(column(12, column(width = 8, numericInput(inputId = "in_rt", 
            "Retention time (s)", value = 98.4)), column(width = 4, numericInput(inputId = "in_rt_ws", 
            "window size (s)", value = 25)))), fluidRow(column(12, column(width = 8, 
            numericInput(inputId = "in_mz", "Mass to charge ratio", value = 269.1109)), 
            column(width = 4, numericInput(inputId = "in_mz_ws", "window size", 
                value = 10))))), conditionalPanel("input.target_input=='db'", 
        helpText("Compund values below are instrument and assay specific!", 
            tags$strong("Interprete carefully, since scan times vary!"), 
            "Please refer to the ", a("GitHub Wiki", href = "https://github.com/tkimhofer/msbrowser/wiki/Database-Table-Editing", 
                target = "_blank"), "for instructions on database personalisation."), 
        br(), fluidRow(column(4, radioGroupButtons(inputId = "db_assays", 
            label = "Assay type", choices = unique(as.character(icst$assay)), 
            direction = "horizontal")), column(8, selectizeInput("in_icst", 
            label = "Compounds", choices = c("Select assay")), textOutput("compound_info"), 
            br())), br()), br()), fluidRow(column(12, align = "right", 
    br(), actionButton("move_picks", "Generate plot", icon("thumbs-up"), 
        style = "color: #fff; background-color: #33A2FF; border-color: #33A2FF"))), 
    fluidRow(div(id = "selectors1", column(12, offset = 0.7, align = "left", 
        checkboxInput("imp_vis", "Viz options", value = FALSE)))))


uiE_div_ppick <- div(id = "div_ppick", h3("3. Perform peak picking"), helpText("Select peak picking algorighm from the list below"), 
    fluidRow(align = "center", offset = 0.7, column(12, offset = 0.7, br(), 
        fluidRow(align = "center", selectInput("in_pickMethod", label = "Algorithm", 
            choices = c(CentWave = "centWave", `Matched Filter` = "matchedFilter"))), 
        conditionalPanel(condition = "input.in_pickMethod=='centWave'", 
            fluidRow(h4("Parameters"), helpText("The following peak picking parameters are the standard parameters defined by xcms - these nearly always require optimisation for each data set (most importantly: ppm, rt range and noise level)."), 
                hr(), column(4, numericInput(inputId = "in_mzdev", label = paste0("m/z deviation [ppm]"), 
                  value = 25), br(), sliderInput("in_rtrange", "Elution time range (s) [peakwidth]", 
                  min = 1, max = 100, step = 1, value = c(20, 50)), br(), 
                  numericInput("in_mzdiff", label = "Minimum diff m/z overlap [mzdiff]", 
                    value = -0.001), br(), fluidRow(h4(a("Need parameter help?", 
                    href = "https://tkimhofer.github.io/msbrowser/articles/pars.html", 
                    target = "_blank")))), column(4, numericInput(inputId = "in_noise", 
                  label = "Noise", value = 0), br(), numericInput(inputId = "in_sn", 
                  label = "Signal/Noise threshold [snthresh]", value = 10), 
                  br(), selectInput("in_mzCentFun", label = "m/z center function", 
                    choices = c(`Weighted Mean` = "wMean", Mean = "mean", 
                      `Peak apex` = "apex", `Weighted mean of peak apex and neigbouring scans` = "wMeanApex3", 
                      `Mean of peak apex and neigbouring scans` = "meanApex3")), 
                  br(), selectInput("in_integrate", label = "Integration method [integrate]", 
                    choices = c(`1: Mexican Hat` = "1", `2: Real MS data` = "2"), 
                    selected = "1")), column(4, wellPanel(h5("Pre-filter"), 
                  numericInput("in_prefilter_k", label = "Number of scans [k]", 
                    min = 0, max = 100, value = 3), br(), numericInput("in_prefilter_I", 
                    label = "Intensity [I]", min = 0, max = 1e+07, value = 100)), 
                  br(), checkboxInput("in_fitgauss", label = "Fit Gaussian to each peak [fitgauss]", 
                    value = FALSE)))), conditionalPanel(condition = "input.in_pickMethod=='matchedFilter'", 
            fluidRow(h4("Parameters"), helpText("The following peak picking parameters are the standard parameters defined by xcms - these nearly always require optimisation for each data set."), 
                hr(), column(4, numericInput(inputId = "in_fwhm", label = paste0("FWHM of matched filtration Gaussian"), 
                  value = 30), br(), numericInput("in_sigma", "SD of matched Gaussian", 
                  value = 2.3548)), column(4, numericInput(inputId = "in_step", 
                  label = "Bin width m/z dim.", value = 0.1), br(), numericInput(inputId = "in_steps", 
                  label = "Bin to merge before filtration", value = 2), 
                  br(), numericInput(inputId = "in_mzdiff", label = "Min. difference mz for peaks w overlapping rt's", 
                    value = 0.8)), column(4, numericInput("in_max", label = "Maximum  number expected peaks / slice", 
                  value = 5), br(), numericInput(inputId = "in_snthres", 
                  label = "S/N cutoff chromatogr. dim.", value = 100)))), 
        conditionalPanel(condition = "input.in_pickMethod=='dbscan'", fluidRow(h4("Parameters"), 
            helpText("The following are pre-defined paramter values, these should be tested for every data set."), 
            hr(), column(4, numericInput(inputId = "in_ppm", label = paste0("inflate mz to accommodate detectors m/z accuracy (ppm in xcms)"), 
                value = 10000), numericInput(inputId = "in_ppm_mztrans", 
                label = paste0("transformation factor to match rt stepsize"), 
                value = 15)), column(4, numericInput(inputId = "in_eps", 
                label = "Radius of neighbourhood", value = 1), br(), numericInput("in_minPts", 
                "Minimum Number of points in each neighbourhood", value = 2)), 
            column(4, numericInput(inputId = "in_noise", label = paste0("noise threshold"), 
                value = 1), br(), numericInput(inputId = "in_rttrans", 
                label = paste0("rt trans (might not be needed)"), value = 1)))))), 
    actionButton("pickpeak1", label = "Pick Peaks!", icon("thumbs-up"), 
        style = "color: #fff; background-color: #33A2FF; border-color: #33A2FF"))
# helper functions
xic_mzrange <- function(xic_mz, ppm) {
    
    mz_window <- (xic_mz * (ppm/10^6))/2
    low <- xic_mz - mz_window
    high <- xic_mz + mz_window
    return(c(xic_mz - mz_window, xic_mz + mz_window, xic_mz))
    
}
