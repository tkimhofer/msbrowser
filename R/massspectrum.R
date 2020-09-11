#  Plotting function for single scan mass spectrum
# param df data.frame with scantime, m/z value and intensity
# param pars plotting parameters (shiny reactive values)
# eturn plotly object
#' @import shiny
#' @import plyr
#' @importFrom ggplot2 ggplot aes aes_string geom_point geom_rect geom_text theme_bw labs scale_colour_gradientn scale_x_continuous sec_axis
#' @importFrom colorRamps matlab.like2
#' @importFrom reshape2 melt
#' @importFrom plotly renderPlotly plot_ly add_trace layout event_register event_data add_segments add_text ggplotly hide_colorbar
#' @importFrom magrittr %>%

massspectrum <- function(df, pars) {
    message("mass spectrum")

    scantime <- pars$mspec_scant

    df_scan <- df[df$scantime == scantime, ]

    # label top 5 points
    df_scan$lab <- NA
    idx <- order(df_scan$Int, decreasing = TRUE)[seq_len(5)]
    df_scan$lab[idx] <- round(df_scan$mz[idx], 4)
    # hoverlabel
    df_scan$hover <- paste0("m/z: ", round(df_scan$mz, 4), "<br>", "rt: ",
        round(scantime, 2), " s")
    # scaling
    df_scan$Int <- df_scan$Int/max(df_scan$Int) * 100
    # plotiting
    g1 <- plot_ly(data = df_scan, source = "pc") %>% add_segments(x = ~mz,
        xend = ~mz, y = 0, yend = ~Int, name = paste("<b>Mass spectrum</b><br>Single scan at",
            round(scantime, 2), "s"), line = list(color = ~"black", width = 0.8),
        text = ~hover, hoverinfo = "text") %>% add_text(data = df_scan[idx,
        ], x = ~mz, y = ~Int, text = ~lab, textposition = "top right",
        showlegend = FALSE) %>% layout(showlegend = TRUE, legend = list(x = 0.7,
        y = 0.99), xaxis = list(title = "m/z", autorange = TRUE), yaxis = list(title = "Counts (%)",
        autorange = TRUE, zeroline = TRUE, showgrid = FALSE, showticklabels = TRUE)) %>%
        event_register(event = "plotly_click")

    return(g1)
}
