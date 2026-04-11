#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @importFrom shinybusy add_busy_bar
#' @importFrom plotly plotlyOutput

ui <- fluidPage(
  tags$head(
    tags$title("MSbrowser"),
    tags$style(HTML("
      body {
        padding-top: 15px;
        background-color: #f6f8fb;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        color: #243447;
      }

      .container-fluid {
        max-width: 1500px;
        padding-left: 24px;
        padding-right: 24px;
      }

      .app-header {
        background: #ffffff;
        border-bottom: 1px solid #e6ebf2;
        padding: 18px 24px 14px 24px;
        margin: -20px -15px 24px -15px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }

      .app-title {
        margin: 0;
        font-size: 30px;
        font-weight: 700;
        line-height: 1.2;
        letter-spacing: 0.2px;
      }

      .app-subtitle {
        margin-top: 6px;
        margin-bottom: 0;
        color: #6b7a90;
        font-size: 14px;
      }

      .brand-ms {
        color: #d4aa00;
      }

      .brand-browser {
        color: #00a98f;
      }

      .panel-card {
        background: #ffffff;
        border: 1px solid #e6ebf2;
        border-radius: 14px;
        padding: 18px 18px 16px 18px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(25, 42, 70, 0.05);
      }

      .section-title {
        margin-top: 0;
        margin-bottom: 8px;
        font-size: 20px;
        font-weight: 650;
        color: #22678D;
      }

      .section-help {
        color: #6b7a90;
        font-size: 13px;
        margin-bottom: 14px;
      }

      .main-panel .tabbable > .nav > li > a {
        font-weight: 600;
      }

      .footer-links {
        margin-top: 8px;
        padding: 6px 2px 20px 2px;
        color: #6b7a90;
        font-size: 14px;
      }

      .footer-links a {
        color: #22678D;
        text-decoration: none;
      }

      .footer-links a:hover {
        text-decoration: underline;
      }

      .shiny-input-container {
        width: 100% !important;
      }
    "))
  ),

  add_busy_bar(color = "#22678D"),

  div(
    class = "app-header",
    div(
      class = "app-title",
      HTML("<span class='brand-ms'>MS</span><span class='brand-browser'>browser</span>")
    ),
    p(
      class = "app-subtitle",
      "LC–MS data exploration and feature analysis"
    )
  ),

  tags$script(HTML("
    function doThis(el) {
      Shiny.setInputValue('clicked_text', 'Torben', {priority: 'event'});
    };
    function doThat(el) {
      Shiny.setInputValue('clicked_target', 'Torben', {priority: 'event'});
    }
  ")),

  fluidRow(
    column(
      width = 4,
      div(
        id = "div_input",
        class = "panel-card",
        h3(
          class = "section-title",
          a(
            href = "#",
            onclick = "doThis(this)",
            "1. Read in LC-MS experiment"
          )
        ),
        div(
          id = "1ri",
          class = "section-help",
          "Choose an LC-MS experiment file in an open data format such as mzML."
        ),
        uiE_div_inp_col
      ),
      uiE_issue_link
    ),

    column(
      width = 8,
      div(
        class = "panel-card main-panel",
        tabsetPanel(id = "msexpl")
      )
    )
  )
)
