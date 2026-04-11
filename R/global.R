#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinybusy add_busy_bar
#' @importFrom shinyBS bsTooltip

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

load_icst
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

uiT_rawData <- tabPanel(title = "Raw Data", value = "rawData", fluidRow(
        withSpinner(plotlyOutput("rawdd", width = "100%", height = "70vh"), type = 4,
        color = "#0dc5c1")), add_busy_bar(color = "#FBDD00"))

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

# uiE_div_summary_file <- div(
#   id = "summary_file",
#   style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:12px; padding:14px 16px; margin-top:12px; margin-bottom:14px;",
#
#   fluidRow(
#     column(
#       12,
#       align = "center",
#       div(
#         style = "font-weight:600; margin-bottom:10px;",
#         textOutput("msfile")
#       ),
#       tableOutput("datsum")
#     )
#   ),
#
#   fluidRow(
#     column(
#       12,
#       align = "left",
#       div(
#         style = "margin-top:10px;",
#         checkboxInput(
#           "imp_xic",
#           "Select m/z range for XIC manually",
#           value = FALSE
#         )
#       )
#     )
#   ),
#
#   div(
#     id = "proceed",
#     fluidRow(
#       column(
#         12,
#         align = "center",
#         div(
#           style = "margin-top:8px; padding:10px 12px; background:#f7fbff; border:1px solid #d8ebfb; border-radius:10px;",
#           p(
#             style = "margin:0;",
#             tags$strong(
#               HTML("<span style=\"color:#33A2FF\">Click on a signal in the mass spectrum to proceed.</span>")
#             )
#           )
#         )
#       )
#     )
#   )
# )


uiE_div_inp_col <- div(id = "div_input_collapse", fluidRow(column(12, offset = 0.7,
    fluidRow(column(10, actionButton("filechoose", label = "Select file"),
        textOutput("bname", inline = TRUE), actionButton("fileexample",
            label = "Load Example", inline = TRUE), bsTooltip(id = "filechoose",
            title = "Choose an LC-MS data file in open data format (e.g., mzML)",
            placement = "right", options = list(container = "body")), bsTooltip(id = "fileexample",
            title = "Use an example LC-MS file.", placement = "right",
            options = list(container = "body")))), br(), uiOutput("ss1"),
    add_busy_bar(color = "#FBDD00"))))

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
      onclick = "doThat(this)",
      style = "color:#22678D; text-decoration:none;",
      "2. Select target signal"
    )
  ),

  div(
    style = "color:#607086; font-size:13px; line-height:1.55; margin-bottom:14px;",
    "Specify a target region by clicking in the plots, entering values manually, or using the database example."
  ),

  div(
    id = "target_col",

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

# uiE_target <- div(
#   id = "div_target",
#   style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:14px; padding:18px 20px 12px 20px; margin-top:18px; margin-bottom:16px; box-shadow:0 2px 8px rgba(25,42,70,0.05);",
#   h3(
#     style = "margin-top:0; margin-bottom:8px; font-size:21px; font-weight:650; color:#22678D;",
#     a(
#       href = "#",
#       onclick = "doThat(this)",
#       style = "color:#22678D; text-decoration:none;",
#       "2. Select target signal"
#     )
#   ),
#   div(
#     style = "color:#5f6f86; font-size:14px; line-height:1.5;",
#     "Specify a spectral area by clicking in the mass spectrum, entering scan time and m/z manually, or selecting a compound from the database."
#   ),
#   br()
# )

uiE_div_tar_col <- div(
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


# uiE_div_tar_col <- div(
#   id = "target_col",
#   style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:14px; padding:18px 20px 16px 20px; margin-bottom:18px; box-shadow:0 2px 8px rgba(25,42,70,0.05);",
#
#   div(
#     id = "selectors",
#     fluidRow(
#       column(
#         12,
#         align = "center",
#         div(
#           style = "margin-bottom:12px;",
#           radioButtons(
#             "target_input",
#             label = NULL,
#             choices = c(`Cursor selection` = "click", Manual = "man", Database = "db"),
#             inline = TRUE,
#             selected = "click"
#           )
#         ),
#         conditionalPanel(
#           "input.target_input=='click'",
#           div(
#             style = "background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px; padding:12px 14px; margin-bottom:12px; text-align:left;",
#             helpText("Use the chromatograms and mass spectrum in the main panel to identify a signal region of interest."),
#             textOutput("selection")
#           )
#         )
#       )
#     )
#   ),
#
#   conditionalPanel(
#     "input.target_input=='man'",
#     div(
#       style = "background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px; padding:14px 16px; margin-bottom:14px;",
#       helpText("Enter the spectral region manually."),
#       fluidRow(
#         column(
#           12,
#           column(
#             width = 8,
#             numericInput(
#               inputId = "in_rt",
#               label = "Retention time (s)",
#               value = 98.4,
#               width = "100%"
#             )
#           ),
#           column(
#             width = 4,
#             numericInput(
#               inputId = "in_rt_ws",
#               label = "Window size (s)",
#               value = 25,
#               width = "100%"
#             )
#           )
#         )
#       ),
#       fluidRow(
#         column(
#           12,
#           column(
#             width = 8,
#             numericInput(
#               inputId = "in_mz",
#               label = "Mass-to-charge ratio (m/z)",
#               value = 269.1109,
#               width = "100%"
#             )
#           ),
#           column(
#             width = 4,
#             numericInput(
#               inputId = "in_mz_ws",
#               label = "Window size",
#               value = 10,
#               width = "100%"
#             )
#           )
#         )
#       )
#     )
#   ),
#
#   conditionalPanel(
#     "input.target_input=='db'",
#     div(
#       style = "background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px; padding:14px 16px; margin-bottom:14px;",
#       helpText(
#         "Compound values below are instrument- and assay-specific. ",
#         tags$strong("Interpret carefully, since scan times can vary."),
#         " Please refer to the ",
#         a(
#           "GitHub Wiki",
#           href = "https://github.com/tkimhofer/msbrowser/wiki/Database-Table-Editing",
#           target = "_blank"
#         ),
#         " for instructions on database personalisation."
#       ),
#       fluidRow(
#         column(
#           4,
#           radioGroupButtons(
#             inputId = "db_assays",
#             label = "Assay type",
#             choices = unique(as.character(icst$assay)),
#             direction = "horizontal"
#           )
#         ),
#         column(
#           8,
#           selectizeInput(
#             "in_icst",
#             label = "Compounds",
#             choices = c("Select assay")
#           ),
#           textOutput("compound_info")
#         )
#       )
#     )
#   ),
#
#   fluidRow(
#     column(
#       12,
#       div(
#         style = "display:flex; justify-content:flex-end; margin-top:8px; margin-bottom:10px;",
#         actionButton(
#           "move_picks",
#           "Generate plot",
#           icon("thumbs-up"),
#           style = "color:#fff; background-color:#22678D; border-color:#22678D; border-radius:10px; padding:8px 16px; font-weight:600;"
#         )
#       )
#     )
#   ),
#
#   fluidRow(
#     div(
#       id = "selectors1",
#       column(
#         12,
#         align = "left",
#         div(
#           style = "padding-top:4px;",
#           checkboxInput("imp_vis", "Visualisation options", value = FALSE)
#         )
#       )
#     )
#   )
# )


#
# uiE_div_ppick <- div(
#   id = "div_ppick",
#   style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:14px; padding:18px 20px 18px 20px; margin-top:18px; margin-bottom:18px; box-shadow:0 2px 8px rgba(25,42,70,0.05);",
#
#   h3(
#     style = "margin-top:0; margin-bottom:8px; font-size:21px; font-weight:650; color:#22678D;",
#     "3. Perform peak picking"
#   ),
#   div(
#     style = "color:#5f6f86; font-size:14px; line-height:1.5; margin-bottom:14px;",
#     "Select a peak-picking algorithm and adjust parameters as needed for your data set."
#   ),
#
#   fluidRow(
#     column(
#       12,
#       div(
#         style = "background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px; padding:14px 16px; margin-bottom:16px;",
#         selectInput(
#           "in_pickMethod",
#           label = "Algorithm",
#           choices = c(CentWave = "centWave", `Matched Filter` = "matchedFilter")
#         )
#       )
#     )
#   ),
#
#   conditionalPanel(
#     condition = "input.in_pickMethod=='centWave'",
#     div(
#       style = "background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px; padding:16px 18px; margin-bottom:16px;",
#       h4(
#         style = "margin-top:0; margin-bottom:8px; color:#22678D;",
#         "CentWave parameters"
#       ),
#       helpText("These are the standard xcms parameters. In practice, ppm, peak width, and noise settings usually require optimisation for each data set."),
#       hr(style = "margin-top:10px; margin-bottom:14px;"),
#
#       fluidRow(
#         column(
#           4,
#           numericInput(
#             inputId = "in_mzdev",
#             label = "m/z deviation [ppm]",
#             value = 25,
#             width = "100%"
#           ),
#           sliderInput(
#             "in_rtrange",
#             "Elution time range (s) [peakwidth]",
#             min = 1,
#             max = 100,
#             step = 1,
#             value = c(20, 50),
#             width = "100%"
#           ),
#           numericInput(
#             "in_mzdiff",
#             label = "Minimum m/z difference for overlap [mzdiff]",
#             value = -0.001,
#             width = "100%"
#           ),
#           div(
#             style = "margin-top:10px;",
#             h4(
#               style = "font-size:15px; margin:0;",
#               a(
#                 "Need parameter help?",
#                 href = "https://tkimhofer.github.io/msbrowser/articles/pars.html",
#                 target = "_blank"
#               )
#             )
#           )
#         ),
#
#         column(
#           4,
#           numericInput(
#             inputId = "in_noise",
#             label = "Noise",
#             value = 0,
#             width = "100%"
#           ),
#           numericInput(
#             inputId = "in_sn",
#             label = "Signal-to-noise threshold [snthresh]",
#             value = 10,
#             width = "100%"
#           ),
#           selectInput(
#             "in_mzCentFun",
#             label = "m/z center function",
#             choices = c(
#               `Weighted Mean` = "wMean",
#               Mean = "mean",
#               `Peak apex` = "apex",
#               `Weighted mean of peak apex and neighbouring scans` = "wMeanApex3",
#               `Mean of peak apex and neighbouring scans` = "meanApex3"
#             )
#           ),
#           selectInput(
#             "in_integrate",
#             label = "Integration method [integrate]",
#             choices = c(`1: Mexican Hat` = "1", `2: Real MS data` = "2"),
#             selected = "1"
#           )
#         ),
#
#         column(
#           4,
#           div(
#             style = "background:#ffffff; border:1px solid #e8edf5; border-radius:10px; padding:12px 14px;",
#             h5(
#               style = "margin-top:0; margin-bottom:10px; color:#22678D;",
#               "Pre-filter"
#             ),
#             numericInput(
#               "in_prefilter_k",
#               label = "Number of scans [k]",
#               min = 0,
#               max = 100,
#               value = 3,
#               width = "100%"
#             ),
#             numericInput(
#               "in_prefilter_I",
#               label = "Intensity [I]",
#               min = 0,
#               max = 1e+07,
#               value = 100,
#               width = "100%"
#             )
#           ),
#           div(
#             style = "margin-top:12px;",
#             checkboxInput(
#               "in_fitgauss",
#               label = "Fit Gaussian to each peak [fitgauss]",
#               value = FALSE
#             )
#           )
#         )
#       )
#     )
#   ),
#
#   conditionalPanel(
#     condition = "input.in_pickMethod=='matchedFilter'",
#     div(
#       style = "background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px; padding:16px 18px; margin-bottom:16px;",
#       h4(
#         style = "margin-top:0; margin-bottom:8px; color:#22678D;",
#         "MatchedFilter parameters"
#       ),
#       helpText("These are the standard xcms parameters. They usually need tuning for the instrument and data set in use."),
#       hr(style = "margin-top:10px; margin-bottom:14px;"),
#
#       fluidRow(
#         column(
#           4,
#           numericInput(
#             inputId = "in_fwhm",
#             label = "FWHM of matched filtration Gaussian",
#             value = 30,
#             width = "100%"
#           ),
#           numericInput(
#             "in_sigma",
#             "SD of matched Gaussian",
#             value = 2.3548,
#             width = "100%"
#           )
#         ),
#         column(
#           4,
#           numericInput(
#             inputId = "in_step",
#             label = "Bin width in m/z dimension",
#             value = 0.1,
#             width = "100%"
#           ),
#           numericInput(
#             inputId = "in_steps",
#             label = "Bins to merge before filtration",
#             value = 2,
#             width = "100%"
#           ),
#           numericInput(
#             inputId = "in_mzdiff",
#             label = "Minimum m/z difference for overlapping peaks",
#             value = 0.8,
#             width = "100%"
#           )
#         ),
#         column(
#           4,
#           numericInput(
#             "in_max",
#             label = "Maximum expected peaks per slice",
#             value = 5,
#             width = "100%"
#           ),
#           numericInput(
#             inputId = "in_snthres",
#             label = "S/N cutoff in chromatographic dimension",
#             value = 100,
#             width = "100%"
#           )
#         )
#       )
#     )
#   ),
#
#   conditionalPanel(
#     condition = "input.in_pickMethod=='dbscan'",
#     div(
#       style = "background:#f7f9fc; border:1px solid #e8edf5; border-radius:10px; padding:16px 18px; margin-bottom:16px;",
#       h4(
#         style = "margin-top:0; margin-bottom:8px; color:#22678D;",
#         "DBSCAN parameters"
#       ),
#       helpText("These are predefined values and should be evaluated carefully for each data set."),
#       hr(style = "margin-top:10px; margin-bottom:14px;"),
#
#       fluidRow(
#         column(
#           4,
#           numericInput(
#             inputId = "in_ppm",
#             label = "Inflate m/z to accommodate detector accuracy (ppm in xcms)",
#             value = 10000,
#             width = "100%"
#           ),
#           numericInput(
#             inputId = "in_ppm_mztrans",
#             label = "Transformation factor to match rt step size",
#             value = 15,
#             width = "100%"
#           )
#         ),
#         column(
#           4,
#           numericInput(
#             inputId = "in_eps",
#             label = "Radius of neighbourhood",
#             value = 1,
#             width = "100%"
#           ),
#           numericInput(
#             "in_minPts",
#             "Minimum number of points in each neighbourhood",
#             value = 2,
#             width = "100%"
#           )
#         ),
#         column(
#           4,
#           numericInput(
#             inputId = "in_noise",
#             label = "Noise threshold",
#             value = 1,
#             width = "100%"
#           ),
#           numericInput(
#             inputId = "in_rttrans",
#             label = "RT transformation",
#             value = 1,
#             width = "100%"
#           )
#         )
#       )
#     )
#   ),
#
#   div(
#     style = "display:flex; justify-content:flex-end; margin-top:8px;",
#     actionButton(
#       "pickpeak1",
#       label = "Pick peaks",
#       icon("thumbs-up"),
#       style = "color:#fff; background-color:#22678D; border-color:#22678D; border-radius:10px; padding:8px 16px; font-weight:600;"
#     )
#   )
# )
uiE_div_ppick <- div(
  id = "div_ppick",
  style = "background:#ffffff; border:1px solid #e6ebf2; border-radius:14px; padding:18px 20px; margin-top:18px; margin-bottom:18px; box-shadow:0 2px 8px rgba(25,42,70,0.05);",

  h3(
    style = "margin-top:0; margin-bottom:10px; font-size:20px; font-weight:650; color:#22678D;",
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
                  `Weighted mean apex ± scans` = "wMeanApex3",
                  `Mean apex ± scans` = "meanApex3"
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

# uiE_move <- fluidRow(column(12, align = "right", actionButton("move", "Let's move on!",
#     icon("thumbs-up"), style = "color: #fff; background-color: #33A2FF; border-color: #33A2FF")))
#
# uiE_target <- div(id = "div_target", h3(a(href = "#", onclick = "doThat(this)",
#     "2. Select target signal")), helpText("Specify a spectral area either through clicking in mass spectrum or by manual entry of a scantime and m/z value. Alternatively, select a compound listed in a database table."),
#     br())
#
# uiE_div_tar_col <- div(id = "target_col", div(id = "selectors", column(12,
#     offset = 0.7, align = "center", radioButtons("target_input", label = NULL,
#         choices = c(`Cursor selection` = "click", Manual = "man", Database = "db"),
#         inline = TRUE, selected = "click"), conditionalPanel("input.target_input=='click'",
#         helpText("The chromatograms in the main panel can be used to identify target areas of high and low signal intensity."),
#         br(), textOutput("selection"), ), )), column(12, offset = 0.7,
#     conditionalPanel("input.target_input=='man'", helpText("Enter spectral region manually"),
#         br(), fluidRow(column(12, column(width = 8, numericInput(inputId = "in_rt",
#             "Retention time (s)", value = 98.4)), column(width = 4, numericInput(inputId = "in_rt_ws",
#             "window size (s)", value = 25)))), fluidRow(column(12, column(width = 8,
#             numericInput(inputId = "in_mz", "Mass to charge ratio", value = 269.1109)),
#             column(width = 4, numericInput(inputId = "in_mz_ws", "window size",
#                 value = 10))))), conditionalPanel("input.target_input=='db'",
#         helpText("Compund values below are instrument and assay specific!",
#             tags$strong("Interprete carefully, since scan times vary!"),
#             "Please refer to the ", a("GitHub Wiki", href = "https://github.com/tkimhofer/msbrowser/wiki/Database-Table-Editing",
#                 target = "_blank"), "for instructions on database personalisation."),
#         br(), fluidRow(column(4, radioGroupButtons(inputId = "db_assays",
#             label = "Assay type", choices = unique(as.character(icst$assay)),
#             direction = "horizontal")), column(8, selectizeInput("in_icst",
#             label = "Compounds", choices = c("Select assay")), textOutput("compound_info"),
#             br())), br()), br()), fluidRow(column(12, align = "right",
#     br(), actionButton("move_picks", "Generate plot", icon("thumbs-up"),
#         style = "color: #fff; background-color: #33A2FF; border-color: #33A2FF"))),
#     fluidRow(div(id = "selectors1", column(12, offset = 0.7, align = "left",
#         checkboxInput("imp_vis", "Viz options", value = FALSE)))))
#
# uiE_div_ppick <- div(id = "div_ppick", h3("3. Perform peak picking"), helpText("Select peak picking algorighm from the list below"),
#     fluidRow(align = "center", offset = 0.7, column(12, offset = 0.7, br(),
#         fluidRow(align = "center", selectInput("in_pickMethod", label = "Algorithm",
#             choices = c(CentWave = "centWave", `Matched Filter` = "matchedFilter"))),
#         conditionalPanel(condition = "input.in_pickMethod=='centWave'",
#             fluidRow(h4("Parameters"), helpText("The following peak picking parameters are the standard parameters defined by xcms - these nearly always require optimisation for each data set (most importantly: ppm, rt range and noise level)."),
#                 hr(), column(4, numericInput(inputId = "in_mzdev", label = paste0("m/z deviation [ppm]"),
#                 value = 25), br(), sliderInput("in_rtrange", "Elution time range (s) [peakwidth]",
#                 min = 1, max = 100, step = 1, value = c(20, 50)), br(),
#                 numericInput("in_mzdiff", label = "Minimum diff m/z overlap [mzdiff]",
#                 value = -0.001), br(), fluidRow(h4(a("Need parameter help?",
#                 href = "https://tkimhofer.github.io/msbrowser/articles/pars.html",
#                 target = "_blank")))), column(4, numericInput(inputId = "in_noise",
#                 label = "Noise", value = 0), br(), numericInput(inputId = "in_sn",
#                 label = "Signal/Noise threshold [snthresh]", value = 10),
#                 br(), selectInput("in_mzCentFun", label = "m/z center function",
#                 choices = c(`Weighted Mean` = "wMean", Mean = "mean",
#                 `Peak apex` = "apex", `Weighted mean of peak apex and neigbouring scans` = "wMeanApex3",
#                 `Mean of peak apex and neigbouring scans` = "meanApex3")),
#                 br(), selectInput("in_integrate", label = "Integration method [integrate]",
#                 choices = c(`1: Mexican Hat` = "1", `2: Real MS data` = "2"),
#                 selected = "1")), column(4, wellPanel(h5("Pre-filter"),
#                 numericInput("in_prefilter_k", label = "Number of scans [k]",
#                 min = 0, max = 100, value = 3), br(), numericInput("in_prefilter_I",
#                 label = "Intensity [I]", min = 0, max = 1e+07, value = 100)),
#                 br(), checkboxInput("in_fitgauss", label = "Fit Gaussian to each peak [fitgauss]",
#                 value = FALSE)))), conditionalPanel(condition = "input.in_pickMethod=='matchedFilter'",
#             fluidRow(h4("Parameters"), helpText("The following peak picking parameters are the standard parameters defined by xcms - these nearly always require optimisation for each data set."),
#                 hr(), column(4, numericInput(inputId = "in_fwhm", label = paste0("FWHM of matched filtration Gaussian"),
#                   value = 30), br(), numericInput("in_sigma", "SD of matched Gaussian",
#                   value = 2.3548)), column(4, numericInput(inputId = "in_step",
#                   label = "Bin width m/z dim.", value = 0.1), br(), numericInput(inputId = "in_steps",
#                   label = "Bin to merge before filtration", value = 2),
#                   br(), numericInput(inputId = "in_mzdiff", label = "Min. difference mz for peaks w overlapping rt's",
#                     value = 0.8)), column(4, numericInput("in_max", label = "Maximum  number expected peaks / slice",
#                   value = 5), br(), numericInput(inputId = "in_snthres",
#                   label = "S/N cutoff chromatogr. dim.", value = 100)))),
#         conditionalPanel(condition = "input.in_pickMethod=='dbscan'", fluidRow(h4("Parameters"),
#             helpText("The following are pre-defined paramter values, these should be tested for every data set."),
#             hr(), column(4, numericInput(inputId = "in_ppm", label = paste0("inflate mz to accommodate detectors m/z accuracy (ppm in xcms)"),
#                 value = 10000), numericInput(inputId = "in_ppm_mztrans",
#                 label = paste0("transformation factor to match rt stepsize"),
#                 value = 15)), column(4, numericInput(inputId = "in_eps",
#                 label = "Radius of neighbourhood", value = 1), br(), numericInput("in_minPts",
#                 "Minimum Number of points in each neighbourhood", value = 2)),
#             column(4, numericInput(inputId = "in_noise", label = paste0("noise threshold"),
#                 value = 1), br(), numericInput(inputId = "in_rttrans",
#                 label = paste0("rt trans (might not be needed)"), value = 1)))))),
#     actionButton("pickpeak1", label = "Pick Peaks!", icon("thumbs-up"),
#         style = "color: #fff; background-color: #33A2FF; border-color: #33A2FF"))

xic_mzrange <- function(xic_mz, ppm) {

    mz_window <- (xic_mz * (ppm/10^6))/2
    low <- xic_mz - mz_window
    high <- xic_mz + mz_window
    return(c(xic_mz - mz_window, xic_mz + mz_window, xic_mz))

}
