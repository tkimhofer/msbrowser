#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinybusy add_busy_bar
#' @importFrom shinyBS bsTooltip
#' @importFrom plotly plotlyOutput

options(shiny.maxRequestSize = 400 * 1024^2)  # gating upload files to 400 MB
globalVariables(c("peak"))

get_icst_path <- function() {
  system.file("extdata", "icst.rda", package = "msbrowser")
}

load_icst <- function() {
  f <- get_icst_path()
  if (!nzchar(f) || !file.exists(f)) {
    stop("Could not locate icst.rda")
  }
  load(f, envir = parent.frame())
}

load_icst()


get_example_df <- function() {
  f <- system.file("extdata", "HILIC_ESIpos_msLevel1_urine.rda", package = "msbrowser")
  if (!nzchar(f) || !file.exists(f)) {
    stop("Could not locate example dataset.")
  }

  e <- new.env(parent = emptyenv())
  load(f, envir = e)
  e$df_sub
}


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

uiT_ichron <- tabPanel(
  "Chromatograms and mass spectrum",
  value = "ichron",
  h4("TIC / BPC"),
  withSpinner(plotlyOutput("tic_bpc", height = 250), type = 8),
  h4("Extracted ion chromatogram"),
  withSpinner(plotlyOutput("xic", height = 250), type = 8),
  h4("Mass spectrum"),
  withSpinner(plotlyOutput("ssms", height = 320), type = 8)
)

uiT_rawData <- tabPanel(
  title = "Raw Data",
  value = "rawData",
  fluidRow(
    div(
      style = "position:relative;",

      div(
        id = "rawdd_loading_text",
        style = "
          position:absolute;
          top:10px;
          left:15px;
          z-index:10;
          font-size:14px;
          color:#22678D;
          background:rgba(255,255,255,0.85);
          padding:4px 8px;
          border-radius:6px;
        ",
        "Rendering raw LC-MS data ..."
      ),

      shinycssloaders::withSpinner(
        plotlyOutput("rawdd", width = "100%", height = "70vh"),
        type = 4,
        color = "#0dc5c1"
      )
    )
  )
)
#
# uiT_rawData <- tabPanel(title = "Raw Data", value = "rawData", fluidRow(
#         withSpinner(plotlyOutput("rawdd", width = "100%", height = "70vh"), type = 4,
#         color = "#0dc5c1")), add_busy_bar(color = "#FBDD00"))

uiT_ppick <- tabPanel(
  "Detected Features",
  value = "ppick",
  fluidRow(
    column(
      12,
      withSpinner(
        plotlyOutput("pp1", width = "100%", height = "70vh"),
        type = 4
      )
    )
  ),
  add_busy_bar(color = "#FBDD00")
)

uiE_issue_link <- div(
  id = "issue_link",
  style = "margin-top:14px; margin-bottom:6px; padding:10px 12px; background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px;",
  a(
    id = "Info",
    href = "https://github.com/tkimhofer/msbrowser/issues",
    target = "_blank",
    style = "color:#22678D; font-weight:600; text-decoration:none;",
    icon("github"),
    " Report issue or suggest feature"
  )
)

uiT_peaks <- tabPanel(
  "Feature Table",
  value = "peaks",

  div(
    style = "padding-top:10px;",

    fluidRow(
      column(
        12,
        div(
          style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:12px; padding:14px 16px; margin-bottom:16px;",
          div(
            style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap; margin-bottom:10px;",
            div(
              style = "font-weight:600; color:#22678D; font-size:18px;",
              "Detected features"
            ),
            actionButton(
              inputId = "plotselection",
              label = "Plot selected features",
              icon = icon("chart-bar"),
              style = "color:#fff; background-color:#22678D; border-color:#22678D; border-radius:10px; padding:8px 16px; font-weight:600;"
            )
          ),
          DT::DTOutput("PeakTbl", width = "100%", height = "auto")
        )
      )
    ),

    fluidRow(
      column(
        6,
        div(
          style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:12px; padding:14px 16px; margin-bottom:16px;",
          div(
            style = "font-weight:600; color:#22678D; font-size:16px; margin-bottom:10px;",
            "Intensity overview"
          ),
          plotlyOutput("peakplt", width = "100%", height = "320px")
        )
      ),
      column(
        6,
        div(
          style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:12px; padding:14px 16px; margin-bottom:16px;",
          div(
            style = "font-weight:600; color:#22678D; font-size:16px; margin-bottom:10px;",
            "Feature neighbourhood"
          ),
          plotlyOutput("peakpltIso", width = "100%", height = "420px")
        )
      )
    ),

    add_busy_bar(color = "#FBDD00")
  )
)

uiE_div_xic <- div(
  id = "div_xic",
  style = "margin-top:10px; padding:10px 12px; background:#f8fafc; border:1px solid #e8edf5; border-radius:10px;",

  fluidRow(
    column(
      9,
      numericRangeInput(
        inputId = "xic_ra",
        label = "XIC m/z range",
        value = c(200, 201),
        separator = " to ",
        width = "100%"
      )
    ),
    column(
      3,
      div(
        style = "padding-top:24px; display:flex; justify-content:flex-end;",
        actionButton(
          "go_xic",
          "Update",
          style = "color:#fff; background-color:#22678D; border-color:#22678D; border-radius:10px; font-weight:600;"
        )
      )
    )
  )
)


uiE_div_summary_file <- div(
  id = "summary_file",
  style = "padding:10px 0 6px 0;",

  fluidRow(
    column(
      12,
      align = "center",
      textOutput("msfile"),
      tableOutput("datsum")
    )
  ),

  fluidRow(
    column(
      12,
      checkboxInput("imp_xic", "Select m/z range for XIC manually", value = FALSE)
    )
  ),

  div(
    id = "proceed",
    fluidRow(
      column(
        12,
        align = "center",
        p(
          style = "margin-top:6px;",
          tags$strong(
            HTML("<span style=\"color:#33A2FF\">Click on a signal in the mass spectrum to proceed.</span>")
          )
        )
      )
    )
  )
)

uiE_div_inp_col <- div(
  id = "div_input_col",
  fluidRow(
    column(
      10, offset = 1,
      div(
        style = paste(
          "background:#ffffff;",
          "border:1px solid #e7edf3;",
          "border-radius:14px;",
          "padding:16px 18px 14px 18px;",
          "margin-bottom:14px;"
        ),
        div(
          style = "font-size:16px; font-weight:650; color:#22678D; margin-bottom:6px;",
          "Load LC-MS data"
        ),
        div(
          style = "color:#607086; font-size:13px; line-height:1.5; margin-bottom:12px;",
          "Choose an open-format raw data file such as mzML, mzXML or netCDF, or load the included example."
        ),
        fileInput("raw_file", "Select file"),
        div(
          style = "display:flex; gap:10px; align-items:center; margin-top:6px;",
          actionButton("fileexample", label = "Load Example"),
          div(
            style = "color:#5f6f86; font-size:13px;",
            textOutput("bname", inline = TRUE)
          )
        ),
        div(
          style = paste(
            "margin-top:12px;",
            "padding:10px 12px;",
            "background:#f8fafc;",
            "border:1px solid #edf2f7;",
            "border-radius:10px;",
            "color:#5f6f86;",
            "font-size:13px;"
          ),
          textOutput("file_status_text")
        )
      ),
      br(),
      uiOutput("ss1")
    )
  )
)

# uiE_div_inp_col <- div(id = "div_input_col", fluidRow(column(12, offset = 0.7,
#     fluidRow(column(10,
#                     fileInput("raw_file", "Select file"),
#                     verbatimTextOutput("info"),
#         textOutput("bname", inline = TRUE), actionButton("fileexample",
#             label = "Load Example", inline = TRUE), bsTooltip(id = "filechoose",
#             title = "Choose an LC-MS data file in open data format (e.g., mzML)",
#             placement = "right", options = list(container = "body")), bsTooltip(id = "fileexample",
#             title = "Use an example LC-MS file.", placement = "right",
#             options = list(container = "body")))), br(), uiOutput("ss1"),
#     add_busy_bar(color = "#FBDD00"))))

uiE_move <- fluidRow(
  column(
    12,
    div(
      style = "display:flex; justify-content:flex-end; margin-top:12px; margin-bottom:4px;",
      actionButton(
        "move",
        "Let's move on",
        icon("thumbs-up"),
        style = "color:#fff; background-color:#22678D; border-color:#22678D; border-radius:10px; padding:8px 16px; font-weight:600;"
      )
    )
  )
)


uiE_target <- div(
  id = "div_target",
  style = paste(
    "background:#ffffff;",
    "border:1px solid #e7edf3;",
    "border-radius:16px;",
    "padding:18px 20px 16px 20px;",
    "margin-top:18px;",
    "margin-bottom:18px;",
    "box-shadow:0 3px 10px rgba(25,42,70,0.05);"
  ),

  h3(
    style = "margin-top:0; margin-bottom:8px; font-size:21px; font-weight:650; color:#22678D;",
    a(
      href = "#",
      onclick = "toggleSection('div_target'); return false;",
      style = "color:#22678D; text-decoration:none;",
      "2. Select target signal"
    )
  ),

  div(
    style = "color:#607086; font-size:13px; line-height:1.55; margin-bottom:14px;",
    "Specify a target region by clicking in the plots, entering values manually, or using the database example."
  ),

  div(
    id = "div_target_col",

    div(
      id = "selectors",
      style = "margin-bottom:14px;",
      fluidRow(
        column(
          12,
          div(
            style = paste(
              "background:#f8fafc;",
              "border:1px solid #edf2f7;",
              "border-radius:12px;",
              "padding:10px 14px 2px 14px;"
            ),
            div(
              style = "font-size:13px; font-weight:600; color:#5f6f86; margin-bottom:8px;",
              "Target selection mode"
            ),
            radioButtons(
              "target_input",
              label = NULL,
              choices = c(`Click in plot` = "click", Manual = "man", `Database example` = "db"),
              inline = TRUE,
              selected = "click"
            )
          )
        )
      )
    ),

    conditionalPanel(
      "input.target_input=='click'",
      div(
        style = paste(
          "background:linear-gradient(180deg, #fbfdff 0%, #f6f9fc 100%);",
          "border:1px solid #e8edf5;",
          "border-radius:12px;",
          "padding:14px 16px 12px 16px;",
          "margin-bottom:14px;"
        ),
        div(
          style = "font-weight:600; color:#22678D; margin-bottom:6px;",
          "Select from the plots"
        ),
        div(
          style = "color:#607086; font-size:13px; line-height:1.5; margin-bottom:8px;",
          "Use the chromatograms and mass spectrum in the main panel to locate a signal region. Clicking near a signal updates the target values below."
        ),
        div(
          style = "background:#ffffff; border:1px dashed #d8e3ef; border-radius:10px; padding:10px 12px; min-height:44px;",
          textOutput("selection")
        )
      )
    ),

    conditionalPanel(
      "input.target_input=='man'",
      div(
        style = paste(
          "background:linear-gradient(180deg, #fbfdff 0%, #f6f9fc 100%);",
          "border:1px solid #e8edf5;",
          "border-radius:12px;",
          "padding:14px 16px 12px 16px;",
          "margin-bottom:14px;"
        ),
        div(
          style = "font-weight:600; color:#22678D; margin-bottom:6px;",
          "Enter target region manually"
        ),
        div(
          style = "color:#607086; font-size:13px; line-height:1.5; margin-bottom:12px;",
          "Specify the centre and search window for retention time and m/z."
        ),
        fluidRow(
          column(
            12,
            column(
              width = 8,
              numericInput(
                inputId = "in_rt",
                label = "Retention time (s)",
                value = 98.4,
                width = "100%"
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "in_rt_ws",
                label = "Window (s)",
                value = 25,
                width = "100%"
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            column(
              width = 8,
              numericInput(
                inputId = "in_mz",
                label = "m/z",
                value = 269.1109,
                width = "100%"
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "in_mz_ws",
                label = "Window",
                value = 10,
                width = "100%"
              )
            )
          )
        )
      )
    ),

    conditionalPanel(
      "input.target_input=='db'",
      div(
        style = paste(
          "background:linear-gradient(180deg, #fffdf8 0%, #fffaf0 100%);",
          "border:1px solid #f0e4c7;",
          "border-radius:12px;",
          "padding:14px 16px 12px 16px;",
          "margin-bottom:14px;"
        ),
        div(
          style = "font-weight:600; color:#8a6a14; margin-bottom:6px;",
          "Database example"
        ),
        div(
          style = "color:#6f6650; font-size:13px; line-height:1.55; margin-bottom:12px;",
          "This database option is included as an example of what can be connected to the app. The values shown here are assay- and instrument-specific and should not be expected to fit other experiments or data sets without adaptation."
        ),
        div(
          style = "color:#6f6650; font-size:13px; line-height:1.55; margin-bottom:12px;",
          tags$strong("Use with caution."),
          " Retention times and signal properties can shift across assays, instruments, and acquisition settings. See the ",
          a(
            "GitHub Wiki",
            href = "https://github.com/tkimhofer/msbrowser/wiki/Database-Table-Editing",
            target = "_blank"
          ),
          " for guidance on personalising this lookup table."
        ),
        fluidRow(
          column(
            4,
            radioGroupButtons(
              inputId = "db_assays",
              label = "Example assay",
              choices = unique(as.character(icst$assay)),
              direction = "horizontal"
            )
          ),
          column(
            8,
            selectizeInput(
              "in_icst",
              label = "Example compounds",
              choices = c("Select assay")
            ),
            div(
              style = "margin-top:4px; color:#5f6f86; font-size:13px;",
              textOutput("compound_info")
            )
          )
        )
      )
    ),

    fluidRow(
      column(
        12,
        div(
          style = "display:flex; justify-content:space-between; align-items:center; gap:12px; margin-top:6px; margin-bottom:8px;",
          div(
            style = "color:#6b7a90; font-size:13px;",
            "Continue once the target region looks sensible."
          ),
          actionButton(
            "move_picks",
            "Generate plot",
            icon("thumbs-up"),
            style = paste(
              "color:#fff;",
              "background-color:#22678D;",
              "border-color:#22678D;",
              "border-radius:10px;",
              "padding:8px 16px;",
              "font-weight:600;"
            )
          )
        )
      )
    ),

    fluidRow(
      div(
        id = "selectors1",
        column(
          12,
          align = "left",
          div(
            style = paste(
              "margin-top:4px;",
              "padding:10px 12px 2px 12px;",
              "background:#f8fafc;",
              "border:1px solid #edf2f7;",
              "border-radius:10px;"
            ),
            checkboxInput("imp_vis", "Show visualisation options", value = FALSE)
          )
        )
      )
    )
  )
)

uiE_div_tar_col <- function() {
  load_icst()

  div(
    id = "target_col",
    style = paste(
      "background:#ffffff;",
      "border:1px solid #e7edf3;",
      "border-radius:16px;",
      "padding:18px 20px 16px 20px;",
      "margin-bottom:18px;",
      "box-shadow:0 3px 10px rgba(25,42,70,0.05);"
    ),

    div(
      id = "selectors",
      style = "margin-bottom:14px;",
      fluidRow(
        column(
          12,
          div(
            style = paste(
              "background:#f8fafc;",
              "border:1px solid #edf2f7;",
              "border-radius:12px;",
              "padding:10px 14px 2px 14px;"
            ),
            div(
              style = "font-size:13px; font-weight:600; color:#5f6f86; margin-bottom:8px;",
              "Target selection mode"
            ),
            radioButtons(
              "target_input",
              label = NULL,
              choices = c(`Click in plot` = "click", Manual = "man", `Database example` = "db"),
              inline = TRUE,
              selected = "click"
            )
          )
        )
      )

    ),

    conditionalPanel(
      "input.target_input=='click'",
      div(
        style = paste(
          "background:linear-gradient(180deg, #fbfdff 0%, #f6f9fc 100%);",
          "border:1px solid #e8edf5;",
          "border-radius:12px;",
          "padding:14px 16px 12px 16px;",
          "margin-bottom:14px;"
        ),
        div(
          style = "font-weight:600; color:#22678D; margin-bottom:6px;",
          "Select from the plots"
        ),
        div(
          style = "color:#607086; font-size:13px; line-height:1.5; margin-bottom:8px;",
          "Use the chromatograms and mass spectrum in the main panel to locate a signal region. Clicking near a signal updates the target values below."
        ),
        div(
          style = "background:#ffffff; border:1px dashed #d8e3ef; border-radius:10px; padding:10px 12px; min-height:44px;",
          textOutput("selection")
        )
      )
    ),

    conditionalPanel(
      "input.target_input=='man'",
      div(
        style = paste(
          "background:linear-gradient(180deg, #fbfdff 0%, #f6f9fc 100%);",
          "border:1px solid #e8edf5;",
          "border-radius:12px;",
          "padding:14px 16px 12px 16px;",
          "margin-bottom:14px;"
        ),
        div(
          style = "font-weight:600; color:#22678D; margin-bottom:6px;",
          "Enter target region manually"
        ),
        div(
          style = "color:#607086; font-size:13px; line-height:1.5; margin-bottom:12px;",
          "Specify the centre and search window for retention time and m/z."
        ),
        fluidRow(
          column(
            12,
            column(
              width = 8,
              numericInput("in_rt", "Retention time (s)", value = 98.4, width = "100%")
            ),
            column(
              width = 4,
              numericInput("in_rt_ws", "Window (s)", value = 25, width = "100%")
            )
          )
        ),
        fluidRow(
          column(
            12,
            column(
              width = 8,
              numericInput("in_mz", "m/z", value = 269.1109, width = "100%")
            ),
            column(
              width = 4,
              numericInput("in_mz_ws", "Window", value = 10, width = "100%")
            )
          )
        )
      )
    ),

    conditionalPanel(
      "input.target_input=='db'",
      div(
        style = paste(
          "background:linear-gradient(180deg, #fffdf8 0%, #fffaf0 100%);",
          "border:1px solid #f0e4c7;",
          "border-radius:12px;",
          "padding:14px 16px 12px 16px;",
          "margin-bottom:14px;"
        ),
        div(
          style = "font-weight:600; color:#8a6a14; margin-bottom:6px;",
          "Database example"
        ),
        div(
          style = "color:#6f6650; font-size:13px; line-height:1.55; margin-bottom:12px;",
          "This database option is included as an example of what can be connected to the app. The values shown here are assay- and instrument-specific and should not be expected to fit other experiments or data sets without adaptation."
        ),
        div(
          style = "color:#6f6650; font-size:13px; line-height:1.55; margin-bottom:12px;",
          tags$strong("Use with caution."),
          " Retention times and signal properties can shift across assays, instruments, and acquisition settings. See the ",
          a(
            "GitHub Wiki",
            href = "https://github.com/tkimhofer/msbrowser/wiki/Database-Table-Editing",
            target = "_blank"
          ),
          " for guidance on personalising this lookup table."
        ),
        fluidRow(
          column(
            4,
            radioGroupButtons(
              inputId = "db_assays",
              label = "Example assay",
              choices = unique(as.character(icst$assay)),
              direction = "horizontal"
            )
          ),
          column(
            8,
            selectizeInput(
              "in_icst",
              label = "Example compounds",
              choices = c("Select assay")
            ),
            div(
              style = "margin-top:4px; color:#5f6f86; font-size:13px;",
              textOutput("compound_info")
            )
          )
        )
      )
    ),

    fluidRow(
      column(
        12,
        div(
          style = "display:flex; justify-content:space-between; align-items:center; gap:12px; margin-top:6px; margin-bottom:8px;",
          div(
            style = "color:#6b7a90; font-size:13px;",
            "Continue once the target region looks sensible."
          ),
          actionButton(
            "move_picks",
            "Generate plot",
            icon("thumbs-up"),
            style = paste(
              "color:#fff;",
              "background-color:#22678D;",
              "border-color:#22678D;",
              "border-radius:10px;",
              "padding:8px 16px;",
              "font-weight:600;"
            )
          )
        )
      )
    ),

    fluidRow(
      div(
        id = "selectors1",
        column(
          12,
          align = "left",
          div(
            style = paste(
              "margin-top:4px;",
              "padding:10px 12px 2px 12px;",
              "background:#f8fafc;",
              "border:1px solid #edf2f7;",
              "border-radius:10px;"
            ),
            checkboxInput("imp_vis", "Show visualisation options", value = FALSE)
          )
        )
      )
    )
  )
}

uiE_div_ppick <- div(
  id = "div_ppick",
  style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:14px; padding:18px 20px; margin-top:18px; margin-bottom:18px; box-shadow:0 2px 8px rgba(25,42,70,0.05);",

  h3(
    class = "section-title",
  # h3(
  #   style = "margin-top:0; margin-bottom:10px; font-size:20px; font-weight:650; color:#22678D;",
    "3. Peak picking"
  ),

  fluidRow(
    column(
      12,

      div(
        style = "background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px; padding:14px 16px;",

        # -------- Algorithm selector --------
        selectInput(
          "in_pickMethod",
          label = "Algorithm",
          choices = c(CentWave = "centWave", `Matched Filter` = "matchedFilter")
        ),

        # ================= CENTWAVE =================
        conditionalPanel(
          condition = "input.in_pickMethod=='centWave'",

          div(
            style = "margin-top:6px; margin-bottom:10px;",
            strong("CentWave parameters "),
            a(
              "Need parameter help?",
              href = "https://tkimhofer.github.io/msbrowser/articles/pars.html",
              target = "_blank"
            )
          ),

          fluidRow(
            column(
              4,
              numericInput("in_mzdev", "ppm", value = 25),
              sliderInput("in_rtrange", "peakwidth (s)", min = 1, max = 100, step = 1, value = c(20, 50)),
              numericInput("in_mzdiff", "mzdiff", value = -0.001)
            ),
            column(
              4,
              numericInput("in_noise", "noise", value = 0),
              numericInput("in_sn", "snthresh", value = 10),
              selectInput(
                "in_mzCentFun",
                "mzCenterFun",
                choices = c(
                  `Weighted Mean` = "wMean",
                  Mean = "mean",
                  `Peak apex` = "apex",
                  `Weighted mean apex +/- scans` = "wMeanApex3",
                  `Mean apex +/- scans` = "meanApex3"
                )
              ),
              selectInput(
                "in_integrate",
                "integrate",
                choices = c(`1: Mexican Hat` = "1", `2: Real MS data` = "2"),
                selected = "1"
              )
            ),
            column(
              4,
              div(
                style = "background:#ffffff; border:1px solid #e8edf5; border-radius:10px; padding:10px;",
                strong("prefilter"),
                numericInput("in_prefilter_k", "k", min = 0, max = 100, value = 3),
                numericInput("in_prefilter_I", "I", min = 0, max = 1e+07, value = 100)
              ),
              checkboxInput("in_fitgauss", "fitgauss", value = FALSE)
            )
          )
        ),

        # ================= MATCHED FILTER =================
        conditionalPanel(
          condition = "input.in_pickMethod=='matchedFilter'",

          div(
            style = "margin-top:6px; margin-bottom:10px;",
            strong("Matched Filter parameters")
          ),

          fluidRow(
            column(
              4,
              numericInput("in_fwhm", "fwhm", value = 30),
              numericInput("in_sigma", "sigma", value = 2.3548)
            ),
            column(
              4,
              numericInput("in_step", "step", value = 0.1),
              numericInput("in_steps", "steps", value = 2),
              numericInput("in_mzdiff", "mzdiff", value = 0.8)
            ),
            column(
              4,
              numericInput("in_max", "max", value = 5),
              numericInput("in_snthres", "snthres", value = 100)
            )
          )
        )

      )
    )
  ),

  div(
    style = "display:flex; justify-content:flex-end; margin-top:10px;",
    actionButton(
      "pickpeak1",
      label = "Pick peaks",
      icon("thumbs-up"),
      style = "color:#fff; background-color:#22678D; border-color:#22678D; border-radius:10px; padding:8px 16px; font-weight:600;"
    )
  )
)

xic_mzrange <- function(xic_mz, ppm) {

    mz_window <- (xic_mz * (ppm/10^6))/2
    low <- xic_mz - mz_window
    high <- xic_mz + mz_window
    return(c(xic_mz - mz_window, xic_mz + mz_window, xic_mz))

}
