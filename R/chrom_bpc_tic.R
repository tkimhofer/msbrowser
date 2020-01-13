#' @title  Plotting function for base peak and total ion chromatrogram in a single plot
#' @param df data.frame with scantime, m/z value and intensity
#' @param pars plotting parameters (shiny reactive values)
#' @import shiny
#' @importFrom ggplot2 ggplot aes aes_string geom_point geom_rect geom_text theme_bw labs scale_colour_gradientn scale_x_continuous sec_axis
#' @import plyr
#' @importFrom colorRamps matlab.like2
#' @importFrom reshape2 melt
#' @importFrom plotly renderPlotly plot_ly add_trace layout event_register event_data add_segments add_text ggplotly hide_colorbar
#' @importFrom magrittr %>%

chrom_bpc_tic=function(df=raw_data()[[1]], pars){
  message('bpc & tic')
  # filter for mz value
  tic=ddply(df, as.quoted('scantime'),function(x) sum(x$Int))
  bpc=ddply(df, as.quoted('scantime'),function(x) {idx=which.max(x$Int)[1]; c(x$Int[idx], x$mz[idx])})

  pa=plot_ly(source='pa') %>%
    add_trace(data=tic, x = ~scantime, y = ~V1, type = 'scatter', mode = 'lines', line=list(color='rgba(0, 0, 0,0.7)', width=0.8),
              text=~paste(scantime, 's'), name='<b>Total ion chromatogram</b>') %>%
    add_trace(data=bpc, x = ~scantime, y = ~V1, type = 'scatter', mode = 'lines', line=list(color='rgba(0, 215, 167,1)', width=1.1),
              text=~paste(round(V2, 4), 'm/z'), name='<b>Base peak chromatogram</b>') %>%
    layout(legend = list(x = 0.7, y = 0.99),
           xaxis = list(title='Scan time (s)',
                        range = c(0, max(df$scantime)),
                        showspikes = TRUE,
                        spikemode  = 'toaxis+across',
                        spikesnap = 'data',
                        showline=F,
                        zeroline = F,
                        spikedash = 'solid',
                        showgrid=TRUE),
           yaxis = list(title='Counts',
                        showgrid = F,
                        showticklabels = T,
                        zeroline = FALSE,
                        showline=F),
           hovermode  = 'x',
           showlegend = TRUE) %>%
    event_register('plotly_click')

 return(pa)

}
