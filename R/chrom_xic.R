# Plotting function for eXtracted Ion Chromatogram (XIC)
# @param df data.frame with scantime, m/z value and intensity
# @param pars plotting parameters (shiny reactive values)
# return plotly object
# @import shiny
#' @importFrom plyr ddply
#' @importFrom plotly renderPlotly plot_ly add_trace layout event_register
#' @importFrom magrittr %>%

chrom_xic <- function(df, pars) {
    message("xic")
    # filter for mz value
    ds <- df[df$mz >= pars$xic_ra[1] & df$mz <= pars$xic_ra[2], ]
    if (nrow(ds) == 0) {
        return(NULL)
    }

    xic <- ddply(ds, as.quoted("scantime"), function(x) sum(x$Int))
    idx_lab <- which.max(xic$V1)  # maximum intensity of sum


    annot_max <- list(x = xic$scantime[idx_lab], y = xic$V1[idx_lab] +
        (xic$V1[idx_lab] * 0.05), text = paste(round(xic$scantime[idx_lab],
        2), "s"), showarrow = FALSE, align = "right")
    xic$hoverlab <- paste(round(xic$scantime, 2), "s")

    if (length(pars$xic_ra) == 3) {
        tit <- paste0("<b>Extracted ion chromatogram</b><br> m/z ", round(pars$xic_ra[3],
            4), " (&plusmn;", pars$ppm_change/2, " ppm)")
    } else {
        tit <- paste0("<b>Extracted ion chromatogram</b><br>", round(pars$xic_ra[1],
            3), "-", round(pars$xic_ra[2], 3), " m/z")
    }

    g1 <- plot_ly(source = "pb") %>% add_trace(data = xic, x = ~scantime,
        y = ~V1, type = "scatter", mode = "lines", line = list(color = "rgba(255, 88, 120,1)",
            width = 1), name = tit, hoverinfo = "text", text = ~hoverlab) %>%
        layout(legend = list(x = 0.7, y = 0.99), showlegend = TRUE, annotations = annot_max,
            xaxis = list(title = "Scan time (s)", showgrid = TRUE, showticklabels = TRUE,
                showspikes = TRUE, spikedash = "solid"), yaxis = list(title = "Counts",
                zeroline = FALSE, showgrid = FALSE, showticklabels = TRUE)) %>%
        event_register("plotly_click")

    return(g1)
}
