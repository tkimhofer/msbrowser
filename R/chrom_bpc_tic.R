# Plotting BPC and TIC
# m/z value and intensity
# param pars plotting pa# rameters (shiny reactive values)
#' @importFrom plyr ddply
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom magrittr %>%

chrom_bpc_tic <- function(df, pars) {
    message("bpc & tic")
    # filter for mz value
    tic <- ddply(df, as.quoted("scantime"), function(x) sum(x$Int))
    bpc <- ddply(df, as.quoted("scantime"), function(x) {
        idx <- which.max(x$Int)[1]
        c(x$Int[idx], x$mz[idx])
    })

    pa <- plot_ly(source = "pa") %>% add_trace(data = tic, x = ~scantime,
        y = ~V1, type = "scatter", mode = "lines",
        line = list(color = "rgba(0, 0, 0,0.7)", width = 0.8),
        text = ~paste(scantime, "s"),
        name = "<b>Total ion chromatogram</b>") %>%
        add_trace(data = bpc, x = ~scantime, y = ~V1, type = "scatter",
            mode = "lines",
            line = list(color = "rgba(0, 215, 167,1)", width = 1.1),
            text = ~paste(round(V2, 4), "m/z"), name = "<b>Base peak chromatogram</b>") %>%
        layout(legend = list(x = 0.7, y = 0.99), xaxis = list(title = "Scan time (s)",
            range = c(0, max(df$scantime)), showspikes = TRUE, spikemode = "toaxis+across",
            spikesnap = "data", showline = FALSE, zeroline = FALSE, spikedash = "solid",
            showgrid = TRUE), yaxis = list(title = "Counts", showgrid = FALSE,
            showticklabels = TRUE, zeroline = FALSE, showline = FALSE),
            hovermode = "x", showlegend = TRUE) %>% event_register("plotly_click")

    return(pa)

}
