#' @title  Shiny app server function
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session session ID
#' @import shiny
#' @import shinyjs
# @importFrom shinyjs hide hideElement show showElement toggle toggleElement onclick
#' @importFrom ggplot2 ggplot aes geom_point geom_rect geom_text theme_bw labs scale_colour_gradientn
#' @import plyr
#' @importFrom xcms xcmsRaw findPeaks.centWave findPeaks.matchedFilter
#' @importFrom colorRamps matlab.like2
#' @importFrom dbscan dbscan
#' @importFrom DT renderDT
#' @importFrom reshape2 melt
#' @importFrom shinyBS bsTooltip
#' @importFrom stats median quantile sd
#' @importFrom plotly renderPlotly plot_ly add_trace layout event_register event_data add_segments add_text ggplotly hide_colorbar
#' @importFrom magrittr %>%
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons


#source('R/ui_helper.R')



server <- function(input, output, session) {
  indic=reactiveVal(0)
  tab_ind=reactiveValues('ichron'=0, 'rawData'=0, 'ppick'=0, 'peaks'=0)
  plot_reg=reactiveValues('pa'=0, 'pb'=0, 'pc'=0)
  
  pars=reactiveValues(
    msfile=NA,
    noise98p=NA,
    Imax_xic_mz=NA,
    Imax_xic_scant=NA,
    massSpec_scantime=NA,
    ppm_change=50,
    mzra=NA,
    xic_ra=NA
  )
  
  
  # SELECT FILE AND READ DATA
  {
    
    observeEvent(input$filechoose, {
      msfile1=tryCatch(file.choose(),  error = function(e) 'no selection')
      if(grepl('\\.mzxml$|\\.mzml$|\\.cdf$|\\.netcdf$', msfile1, ignore.case = T)){
        removeNotification(id='nofile')
        output$msfile <- renderText({
          msfile1
        })
        pars$msfile=msfile1
        message('Got file!')
      } else{showNotification(ui="Accepted file formats are netCDF, mzXML, mzData and mzML. ", duration=NULL, closeButton = T, type='error', id='nofile')}
    }, ignoreNULL = T, ignoreInit = T)
    
    observeEvent(input$fileexample, {
      removeNotification(id='nofile')
      output$msfile <- renderText({
        'Example: HILIC-ESI(+)-MS of a urine sample'
      })
      pars$msfile='inst/extdata/mzXML/Urine_HILIC_ESIpos.mzXML'
      message('Got example file!')
    }, ignoreNULL = T, ignoreInit = T)
  }
  
  raw_data<-eventReactive(pars$msfile, {
    message('Reading file')
    raw_xcms=xcmsRaw(pars$msfile, profstep = 0, includeMSn = F, mslevel = 1)
    # extract raw data from xcms object
    df_xcms=data.frame(mz=raw_xcms@env$mz, Int=raw_xcms@env$intensity)
    # extract intensities per scan and scantime
    ndp=length(raw_xcms@env$mz)
    idx.trans=which(raw_xcms@env$mz[-1]<raw_xcms@env$mz[-ndp]) # every new scan starts with low mz
    idx.scans=cbind(c(0, idx.trans)+1, c(idx.trans, ndp)) # create df of indices (one scan per row)
    df_xcms$scan=unlist(sapply(1:nrow(idx.scans), function(i){ rep(i, diff(idx.scans[i,])+1) }))
    df_xcms$scantime=raw_xcms@scantime[df_xcms$scan] # add scantime (s)
    # create summary stats
    pars$noise98p=round(quantile(df_xcms$Int,probs = 0.98))
    output$datsum<-renderTable(
      data.frame(Descr=c('Scantime range', 'Scan frequency', 'Mass range', 'Median ion count of all scans', 'Intensity at p(x<=X)=0.98'),
                 Value=c(paste(paste(round(range(raw_xcms@scantime)), collapse = '-'), 's'),
                         paste(round(1/median(diff(raw_xcms@scantime[order(raw_xcms@scanindex)]))), 'scans per s'), paste(paste(round(raw_xcms@mzrange), collapse = '-'), 'm/z'),
                         paste(format(median(raw_xcms@tic), scientific = T, digits = 3), 'AU'),
                         paste(pars$noise98p, 'AU'))
      ) , rownames=F, colnames=F, spacing='xs')
    
    insertUI(
      selector = "#fileexample",
      where = "afterEnd",
      ui = uiE_div_summary_file,
      immediate=F
    )
    
    df=df_xcms
    tic=ddply(df, as.quoted('scantime'), function(x) sum(x$Int))
    bpc=ddply(df,  as.quoted('scantime'), function(x) {idx=which.max(x$Int)[1]; c(x$Int[idx], x$mz[idx])})
    # Get maximumn signal and scantime of that signal
    pars$Imax_xic_mz=df$mz[which.max(df_xcms$Int)]
    pars$Imax_xic_scant=df$scantime[which.max(df_xcms$Int)]
    message('Read-in completed, showing files')
    
    pars$mzra=range(df$mz)
    pars$scantimera=range(df$scantime)
    
    return(df_xcms)
  }, ignoreNULL = T, ignoreInit = T)
  
  
  print('okay')
  
  
  # GENERATE CHROMATOGRAMS AND SINGLE SCAN MASS SPECTRUM
  {
    #observeEvent({raw_data()},{
    # bpc triggered when data is loaded
    output$tic_bpc <- renderPlotly({
      df=raw_data()
      message('Generate BPC')
      tic=ddply(df, as.quoted('scantime'),function(x) sum(x$Int))
      bpc=ddply(df, as.quoted('scantime'),function(x) {idx=which.max(x$Int)[1]; c(x$Int[idx], x$mz[idx])})
      
      plot_ly(source='pa') %>%
        add_trace(data=tic, x = ~scantime, y = ~V1, type = 'scatter', mode = 'lines', line=list(color='rgba(0, 0, 0,0.7)', width=0.8),
                  text=~paste(scantime, 's'), name='<b>Total ion chromatogram</b>') %>%
        add_trace(data=bpc, x = ~scantime, y = ~V1, type = 'scatter', mode = 'lines', line=list(color='rgba(0, 215, 167,1)', width=1.1),
                  text=~paste(round(V2, 4), 'm/z'), name='<b>Base peak chromatogram</b>') %>%
        layout(legend = list(x = 0.7, y = 0.99), xaxis = list(title='Scantime (s)', range = c(0, max(df$scantime)),
                                                              showspikes = TRUE,
                                                              spikemode  = 'toaxis+across',
                                                              spikesnap = 'data',
                                                              showline=TRUE,
                                                              spikedash = 'solid',
                                                              showgrid=TRUE),
               yaxis=list(title='Intensity (AU)',   showgrid = F,showticklabels = T),
               hovermode  = 'x', showlegend = TRUE) %>%
        event_register('plotly_click')
      
      
    })
    
    
    pa_act=eventReactive(event_data("plotly_click", source='pa'),{
      print('pa_act activated')
      print(event_data("plotly_click", source='pa'))
      df=raw_data()
      event.data <- event_data("plotly_click", source='pa')
      sdif=abs(df$scantime-event.data$x[1])
      scantime=df$scantime[which(sdif==min(sdif))[1]]
      pars$massSpec_scantime=scantime
      # mzrange for xic
      sub=df[which(sdif==min(sdif)),]
      xic_mz=sub$mz[which.max(sub$Int)]
      ppm_change=50
      mz_window = (xic_mz*(pars$ppm_change/10^6))/2
      low=xic_mz-mz_window
      high=xic_mz+mz_window
      print(pars$xic_ra)
      pars$xic_ra=c(low, high, xic_mz)
      print(pars$xic_ra)
      event.data
      
    }, ignoreInit = T, ignoreNULL = T)
    
    observeEvent(pa_act(), print(pa_act()))
    
    
    # XIC and MASS SPECTRUM PLOT TRIGGERS / REACTIVITY
    # an XIC can be triggered by 3 events:
    # 1. new data
    # 2. click on BPC/TIC
    # 3. manual range selection
    
    # an singel scan mass spectrum (ssms) can be triggered by 4 events:
    # 1. new data
    # 2. click on BPC/TIC
    # 3. manual range selection
    # 4. click on XIC
    # create observers for each one and output is scantime and xic_ra to create xic and ssms plots
    # variable initialistion for XIC and single scan mass spectrum (ssms)
    
    # TRIGGERS
    # 1. new data
    observeEvent(raw_data(), {
      if(tab_ind$ichron==0){
        prependTab(
          inputId='msexpl',
          tab=uiT_ichron,
          select=T
        )
      }
      # scantime for single scan mass spectrum
      # mzrange for xic
      xic_mz=pars$Imax_xic_mz
      ppm_change=50
      mz_window = (xic_mz*(ppm_change/10^6))/2
      low=xic_mz-mz_window
      high=xic_mz+mz_window
      pars$xic_ra=c(low, high, xic_mz)
      massSpec_scantime=pars$Imax_xic_scant
    }, ignoreNULL = T)
    
    # 2. click on BPC/TIC
    observeEvent(pa_act(), {
      print('trigger pa')
      df=raw_data()
      event.data <- pa_act()
      # scantime for single scan mass spectrum
      sdif=abs(df$scantime-event.data$x[1])
      scantime=df$scantime[which(sdif==min(sdif))[1]]
      pars$massSpec_scantime=scantime
      # mzrange for xic
      sub=df[which(sdif==min(sdif)),]
      xic_mz=sub$mz[which.max(sub$Int)]
      ppm_change=50
      mz_window = (xic_mz*(ppm_change/10^6))/2
      low=xic_mz-mz_window
      high=xic_mz+mz_window
      pars$xic_ra=c(low, high, xic_mz)
    }, ignoreNULL = T, ignoreInit = T)
    
    
    # 3. manual range selection
    observeEvent(xic_range(), {
      # scantime for single scan mass spectrum
      df=raw_data()
      ds=df[df$mz>=xic_range()[1] & df$mz<=xic_range()[2],]
      scantime=ds$scantime[which.max(ds$Int)]
      pars$massSpec_scantime=scantime
      # mzrange for xic
      pars$xic_ra=xic_range()
    }, ignoreNULL = T, ignoreInit = T)
    
    # 4. click on XIC triggers ssms change
    observeEvent(pb_act(), {
      # scantime for single scan mass spectrum
      df=raw_data()
      event.data <- pb_act()
      sdif=abs(df$scantime-event.data$x[1])
      scantime=df$scantime[which(sdif==min(sdif))[1]]
      pars$massSpec_scantime=scantime
    }, ignoreNULL = T, ignoreInit = T)
    
    # Generate xic plot based on trigger event
    # observeEvent({xic_ra()},{
    #
    #     print('trigger done')
    output$xic <- renderPlotly({
      df=raw_data()
      
      browser()
      print('xic1 ')
      low=pars$xic_ra[1]
      high=pars$xic_ra[2]
      ds=df[df$mz>=low & df$mz<=high,]
      print('generate xic')
      xic=ddply(ds, as.quoted('scantime'), function(x) sum(x$Int))
      print(head(xic))
      print(dim(xic))
      idx_lab=which.max(xic$V1)
      print('generate xic annot')
      annot_max <- list(
        x = xic$scantime[idx_lab],
        y = xic$V1[idx_lab]+(xic$V1[idx_lab]*0.05),
        text = paste(round(xic$scantime[idx_lab], 2), 's'),
        showarrow = F,
        align='right'
      )
      print('generate xic annot dibe')
      
      xic$hoverlab=paste(round(xic$scantime, 2), 's')
      if(length(pars$xic_ra)==3){
        print('generate xic title, le ra==3')
        print(pars$xic_ra)
        tit=paste0('<b>Extracted ion chromatogram</b> m/z ', round(pars$xic_ra[3], 4),
                   ' (&plusmn;', pars$ppm_change/2, ' ppm)<br>(Exact mass range:',
                   round(low, 3), '-', round(high, 3), ' m/z)')
      }else{
        print('generate xic title, le ra==2')
        print(pars$xic_ra)
        tit=paste0('<b>Extracted ion chromatogram</b><br>(Exact mass range:',
                   round(low, 3), '-', round(high, 3), ' m/z)')
      }
      print('xic plot')
      g1=plot_ly(source='pb') %>%
        add_trace(data=xic, x = ~scantime, y = ~V1, type = 'scatter', mode = 'lines', line=list(color='rgba(255, 88, 120,1)', width=1),
                  name=tit,
                  hoverinfo='text', text=~hoverlab ) %>%
        layout(legend = list(x = 0.7, y = 0.99),
               showlegend = TRUE,
               annotations =annot_max,
               xaxis=list(
                 title='Scantime (s)',
                 showgrid = T,
                 showticklabels = T,
                 showspikes = TRUE,
                 spikedash = 'solid'),
               yaxis = list(
                 title='Intensity (AU)',
                 zeroline = FALSE,
                 showgrid = F,
                 showticklabels = T)
        ) %>% event_register('plotly_click')
      print('done plotting')
      return(g1)
    })
    #}, ignoreNULL = T, ignoreInit = T)
    
    pb_act=eventReactive(event_data("plotly_click", source='pb'),
                         {event_data("plotly_click", source='pb')
                           
                         }, ignoreInit = T, ignoreNULL = T)
    
    
    # Generate ssms plot based on trigger event
    output$barsScan <- renderPlotly({
      print('barsc1 ')
      scantime=pars$massSpec_scantime
      df=raw_data()
      df_scan=df[df$scantime == scantime,]
      df_scan$lab=NA
      idx=order(df_scan$Int, decreasing = T)[1:5]
      df_scan$lab[idx]=round(df_scan$mz[idx],4)
      df_scan$hover=paste0('m/z: ', round(df_scan$mz,4),  '<br>', 'rt: ', round(scantime, 2), ' s')
      df_scan$Int=df_scan$Int/max(df_scan$Int)*100
      
      
      
      # pc_act(data.frame(x=df_scan$mz[idx[1]], y=NA))
      
      plot_ly(data=df_scan, source='pc') %>%
        add_segments(x = ~mz, xend = ~mz, y = 0, yend = ~Int,
                     name=paste('<b>Mass spectrum</b> (single scan at', round(scantime, 2), 's)'),
                     line=list(color=~'black', width=0.8),
                     text=~hover,  hoverinfo = 'text') %>%
        add_text(data=df_scan[idx,], x = ~mz, y=~Int, text = ~lab, textposition = "top right", showlegend=F) %>%
        layout(
          showlegend = TRUE,
          legend = list(x = 0.7, y = 0.99),
          xaxis = list(title='m/z', autorange=TRUE),
          yaxis = list(title='Intensity (%)', autorange=TRUE,  zeroline = FALSE,
                       showgrid = F,
                       showticklabels = T)
        ) %>%
        event_register(event = 'plotly_click')
    })
    
    
    # 
    # observeEvent(event_data("plotly_click", source='pc'),{
    #   
    #   pc_act(event_data("plotly_click", source='pc'))
    # })
    pc_act=eventReactive(event_data("plotly_click", source='pc'),
                         {event_data("plotly_click", source='pc')}, ignoreInit = T, ignoreNULL = T)
    
    
    # update mz values in
    observeEvent(raw_data(),{
      updateNumericInput(session, inputId='in_mz', value=round(pars$Imax_xic_mz , 2))
      updateNumericInput(session, inputId='in_rt', value=round(pars$Imax_xic_scant))
      updateNumericInput(session, 'in_noisethr', value=as.numeric(pars$noise98p))
      output$selection <- renderText({ paste('Signal:', round(pars$Imax_xic_scant), 's', round(pars$Imax_xic_mz, 2), 'm/z') })
      insertUI(
        selector='#div_xic',
        where='afterEnd',
        ui= uiE_move
      )
    })
  }
  
  
  
  
  # expand fields for manual entering mz values for xic
  observeEvent(input$imp_xic,{
    if(input$imp_xic){
      insertUI(
        selector = "#imp_xic",
        where = "afterEnd",
        ui = uiE_div_xic
      )    } else{
        removeUI(
          selector = "#div_xic"
        )
      }
  }, ignoreInit = T, ignoreNULL = T)
  
  # clean-up / validate manually entered mz range for xic
  xic_range=reactiveVal(NULL)
  
  observeEvent(input$go_xic, {
    #browser()
    xic_ra=input$xic_ra
    mz_ra=pars$mzra
    if(all(is.numeric(xic_ra))){
      xic_ra=sort(xic_ra)
      if(length(xic_ra)==2 & xic_ra[1]>=mz_ra[1] & xic_ra[2]<=mz_ra[2] &  xic_ra[2]!= xic_ra[1]){
        removeNotification(id='xic_notnumeric')
        xic_range(c(xic_ra, mz_ra))
        print('following is manually set xic_range')
        print(xic_range())
      }
    } else{
      showNotification(ui="Check entered mass range for XIC!", duration=NULL, closeButton = T, type='warning', id='xic_notnumeric')
    }
  }, ignoreNULL = T, ignoreInit = T)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # transition to next step (select region for vis 3d raw data)
  # either by clicking on next (move)
  # or by clicking in mass spectrum plot, then peak picking paramters are automatically shown
  
  # onclick('h3test', alert(date()))
  
  observeEvent(input$move, {
    
    print(input$move)
    insertUI(
      selector='#div_input',
      where='afterEnd',
      ui=uiE_target
    )
    
    removeUI(
      selector = '#move',
    )
    
    
    # hideElement(id='move')
    # shinyjs::hide(id = "div_vis")
    # shinyjs::show(id = "div_target")
  })
  
  # update ui for internal standards (depending on assay selection) and change mz / rt according to db entry
  # internal standards / db entries
  {
    observeEvent(input$"db_assays", {
      # icst_cname=icst[icst$assay %in% input$"db_assays",]
      # updateSelectizeInput(session, "in_icst",
      #                      label = "Compound",
      #                      choices = dlply(icst_cname, as.quoted('info'), function(x) {x[,2]}),
      #                      selected=F)
      # output$compound_info=renderText({''})
    })
    
    observeEvent(input$in_icst, {
      # icst_cname=icst[icst$assay %in% input$"db_assays",]
      # idx=match(input$in_icst, icst_cname$compound)
      # if(length(idx)>0 & !is.na(idx)){
      #   updateNumericInput(session, 'in_mz', value = icst_cname$mz[idx])
      #   updateNumericInput(session, 'in_rt', value = icst_cname$rt[idx])
      #   output$compound_info=renderText({paste0('rt=',icst_cname$rt[idx], ' s, m/z=', icst_cname$mz[idx])})
      # }
    }, ignoreNULL = T, ignoreInit = T)
  }
  
  
  
  
  
  
  
  
  #
  #   observeEvent(pc_act(),{
  #     print('click on mass spectrum')
  #     updateRadioButtons(session, 'target_input',  selected='click')
  #     dats=pc_act()
  #     print(dats)
  #     output$selection <- renderText({
  #       paste('Signal:', raw_data()[[4]], 's', round(dats$x[1], 4), 'm/z')
  #     })
  #     updateNumericInput(session, inputId='in_rt', value=raw_data()[[4]])
  #     updateNumericInput(session, inputId='in_mz', value=round(dats$x[1], 4))
  #     updateNumericInput(session, inputId='in_noisethr', value=round(as.numeric(raw_data()[[5]])))
  #     shinyjs::show("div_target")
  #     shinyjs::hide(id = "div_vis")
  #   }, ignoreInit = T, ignoreNULL = T)
  
  observeEvent(input$'imp_vis',{
    #shinyjs::toggle(id = "div_vis")
    
    print(input$'imp_vis')
    if(input$'imp_vis'==T){
      insertUI(
        selector='#selectors',
        where='afterEnd',
        ui= div(id ="#div_vis",
                br(),
                fluidRow(
                  column(12, offset=0.7,
                         column(width=3, numericInput(inputId='in_noisethr', label = 'Noise Threshold', value=pars$noise98p)),
                         
                         column(width=9, align='center', radioGroupButtons('raw_trans', label='Data Transformation', choices = list('None'='none', 'Squared'='sqrt', 'Square Root'='exp', 'Log 10'='log10', 'Logit'='logit','Reciprocal'='reciprocal'), selected = 'log10'))
                         
                         
                  )
                )
        )
      )
    }else{
      print('imp viz remove')
      removeUI(
        selector = '#div_vis'
      )
    }
  })
  
  
  
  # observeEvent(input$in_noisethr, {
  #   browser()
  #   raw_data()[[5]]=input$in_noisethr
  # })
  # 
  # hide create image button when cursor selection is enabled
  observeEvent(input$target_input,{
    if(input$target_input=='click') {hideElement(id='goImage')}else{
      showElement(id='goImage')
    }
  })
  
  
  # extracting raw data for plotting
  ttt=eventReactive(
    { input$move_picks>0 | input$goImage>0},
    {
      browser()
      
      if(tab_ind$rawData==0){
        insertTab(
          inputId='msexpl',
          tab=uiT_rawData, 
          target='ichron',
          position='after',
          select=T
        )
        
        tab_ind$rawData=1
      }
      
      
      print('click on mass spectrum')
      updateRadioButtons(session, 'target_input',  selected='click')
      
      if(!is.null(pc_act())){dats=pc_act()}else{
        dats=data.frame(x=550)
      }
      
      print(dats)
      output$selection <- renderText({
        paste('Signal:', round(pars$Imax_xic_scant,2), 's', round(dats$x[1], 4), 'm/z')
      })
      
      
      print('updating feature information')
      updateNumericInput(session, inputId='in_rt', value=pars$Imax_xic_scant)
      updateNumericInput(session, inputId='in_mz', value=round(dats$x[1], 4))
      updateNumericInput(session, inputId='in_noisethr', value=round(as.numeric(pars$noise98p)))
      
      
      print('get feature information')
      
      mf=raw_data()
      target.rt=as.numeric(input$in_rt)
      target.mz=as.numeric(input$in_mz)
      # create sub with fixed equidist window size from target signal
      wind.rt=input$in_rt_ws/2
      wind.mz=input$in_mz_ws/2
      ra.rt=c(max(target.rt-wind.rt, min(mf$scantime)), min(target.rt+wind.rt, max(mf$scantime)))
      ra.mz=c(max(target.mz-wind.mz, min(mf$mz)), min(target.mz+wind.mz, max(mf$mz)))
      idx=which(mf$mz>ra.mz[1] & mf$mz<ra.mz[2] & mf$scantime>ra.rt[1] & mf$scantime<ra.rt[2])
      print(length(idx))
      sub=mf[idx,]
      # plot raw data
      showTab(inputId = "msexpl", target = "rawData", select = T)
      output$rawdd <- renderPlotly({
        noi=input$in_noisethr
        print(noi)
        df=sub
        g1=ggplot()+
          geom_point(data=subset(df, Int<=noi), aes(scantime, mz, colour=Int), size=0.1)+
          geom_point(data=subset(df, Int>noi), aes(scantime, mz, colour=Int), size=1)+
          theme_bw()+
          labs(x='Scantime (s)', y='m/z', colour='Int',  caption='Raw Data')
        print(input$raw_trans)
        
        if(is.null(input$raw_trans)){
          g1=g1+scale_colour_gradientn(colours=matlab.like2(10))
        }else{
          if(input$raw_trans!='none'){g1= g1+scale_colour_gradientn(colours=matlab.like2(10), trans=input$raw_trans)} else{
            g1=g1+scale_colour_gradientn(colours=matlab.like2(10))}
        }
        
        ggplotly(g1, height=1000, width=1100, dynamicTicks=T)
      })
      
      
      if(indic()==0){
        insertUI(
          selector='#div_target',
          where='afterEnd',
          ui=uiE_div_ppick
        )
        
        removeUI(
          selector = '#move1',
        )
        indic(1)
      }
      return(sub)
    }, ignoreNULL = T, ignoreInit = T)
  
  # observeEvent(input$target_input=='click', {
  #   showTab(inputId = "msexpl", target = "ichron", select=T)
  # })
  
  observeEvent(ttt(), {
    print(head(ttt()))
  })
  #
  #   observeEvent({
  #     #input$move1
  #     ttt()
  #     }, {
  #     # hideElement(id='move1')
  #     # shinyjs::show(id = "div_ppick")
  #     # if(input$msexpl != 'rawData') {
  #     #   indic(1)
  #     #   shinyjs::click('goImage')}
  #
  #     insertUI(
  #       selector='#div_target',
  #       where='afterEnd',
  #       ui=uiE_div_ppick
  #     )
  #
  #   })
  
  # peak picking
  fgt <- eventReactive(input$pickpeak1, {
    showTab(inputId = "msexpl", target = "ppick", select=T)
    message('Peak picking')
    removeNotification('nopeaks')
    raw_xcms=raw_data() # this is xcms object
    target.rt=as.numeric(input$in_rt)
    target.mz=as.numeric(input$in_mz)
    wind.rt=input$in_rt_ws/2
    wind.mz=input$in_mz_ws/2
    mf=ttt()
    switch(input$in_pickMethod,
           'centWave'={
             peaktbl=findPeaks.centWave(raw_xcms,
                                        ppm=as.numeric(input$in_mzdev),
                                        peakwidth=input$in_rtrange,
                                        snthresh=as.numeric(input$in_sn),
                                        prefilter=c(input$in_prefilter_k, as.numeric(input$in_prefilter_I)),
                                        mzCenterFun=input$in_mzCentFun,
                                        integrate=as.numeric(input$in_integrate),
                                        mzdiff=input$in_mzdiff,
                                        fitgauss=F,
                                        noise=as.numeric(input$in_noise),
                                        scanrange=range(mf$scan))
             peaktbl=as.data.frame(peaktbl)
           },
           'matchedFilter'={
             peaktbl=findPeaks.matchedFilter(raw_xcms,
                                             fwhm=as.numeric(input$in_fwhm),
                                             sigma=as.numeric(input$in_sigma),
                                             max=as.numeric(input$in_max),
                                             snthresh=as.numeric(input$in_snthres),
                                             step=as.numeric(input$in_step),
                                             steps=as.numeric(input$in_steps),
                                             #sleep=F,
                                             scanrange=range(mf$scan))
             peaktbl=as.data.frame(peaktbl)
           },
           'dbscan'={
             # # noise=10
             # ppm=60
             #fe=1e4
             exp_fact=as.numeric(input$in_ppm_mztrans)
             mz_cor=((1/as.numeric(input$ppm))*exp_fact)
             rt_fac=as.numeric(input$in_rttrans)
             # mzp=280
             # rtp=247
             # noise=1000
             
             #idx=which(df_xcms$mz>800 & df_xcms$mz<820 & df_xcms$scantime>320 & df_xcms$scantime<370)
             #df_xcms[which.min(df_xcms$mz-802),]
             
             # define df and noise
             #browser()
             df=ttt()
             
             dcl=df[df$Int>as.numeric(input$in_noise),c(1,3)]
             dcl$mz=dcl$mz*mz_cor
             dcl$scan=dcl$scan*scan_cor
             
             # calculate distance matrix and check ditances for a signal manually
             # tt=dist(dcl)
             # ttm=melt(tt)
             # which(ttm$value<1 & ttm$value>0)
             
             dbclust=dbscan(dcl, eps=3, minPts = 2, search='kdtree', borderPoints = T); test;
             dcl$cl=dbclust$cluster
             
             clb=ddply(dcl, as.quoted('cl'), function(x){
               if(nrow(x)<5){return(NULL)}
               c(range(x$mz), range(x$scanrange), max(x$Int), sd(x$Int))
             })
             
             peaktbl=clb
             
           }
    )
    
    
    
    message('Peak picking done!')
    
    # create sub with fixed window around target
    ra.rt=c(max(target.rt-wind.rt, min(mf$scantime)), min(target.rt+wind.rt, max(mf$scantime)))
    ra.mz=c(max(target.mz-wind.mz, min(mf$mz)), min(target.mz+wind.mz, max(mf$mz)))
    idx=which(peaktbl$mz>=ra.mz[1] & peaktbl$mz<=ra.mz[2] & peaktbl$rt>=ra.rt[1] & peaktbl$rt<=ra.rt[2])
    # if(length(idx)==0){return(NULL)}
    
    if(length(idx)>0){
      
      ptbl=peaktbl[idx,]
      ptbl=ptbl[order(ptbl$maxo, decreasing = T),]
      ptbl$roi=as.character(1:nrow(ptbl))
      showTab(inputId = "msexpl", target = "ppick", select=T)
      showTab(inputId = "msexpl", target = "peaks", select = F)
      
      
      print('okay7')
      output$pp1 <- renderPlotly({
        
        # raw data
        noi=as.numeric(input$in_noisethr)
        # small points when not belonging to signal
        idc=unlist(dlply(ptbl, as.quoted('roi'), function(peak, ds=mf){
          which(ds$mz>=peak$mzmin & ds$mz<=peak$mzmax & ds$scantime >= peak$rtmin & ds$scantime <= peak$rtmax)
        }))
        df=mf
        df$peak='No'
        df$peak[idc]='Yes'
        g2=ggplot()+
          geom_point(data=subset(df, peak=='No'), aes(scantime, mz, colour=Int), size=0.1)+
          geom_rect(data=ptbl, aes(xmin=rtmin, xmax=rtmax, ymin=mzmin, ymax=mzmax),  size=1 , color='darkgrey', fill='darkgrey')+
          geom_point(data=subset(df, peak=='Yes'), aes(scantime, mz, colour=Int), size=1)+
          geom_text(data=ptbl, aes(x=rtmax, y=mzmin, label=roi), colour='red', size=5, hjust=0, vjust=0)+
          theme_bw()+
          labs(x='Scantime (s)', y='m/z', colour='Int')
        if(input$raw_trans!='none'){
          g2=g2+
            scale_colour_gradientn(colours=matlab.like2(10), trans=input$raw_trans)
        }else{
          g2=g2+
            scale_colour_gradientn(colours=matlab.like2(10))
        }
        ggplotly(g2, height=1000, width=1100, dynamicTicks=T)
      })
      return(ptbl)
    }else{
      message('No peaks detected!');
      showNotification(ui="No peaks detected!", duration=NULL, closeButton = T, type='error', id='nopeaks');
      return(NULL)
    }
  }, ignoreNULL = T)
  
  observeEvent(fgt(), {
    print(head(fgt()))
  })
  
  peakTbl <- observeEvent(fgt(), {
    out=fgt()[,c(11, 1:10)]
    rownames(out)=NULL
    idx=grep('mz', colnames(out))
    out[,idx]=apply(out[,idx], 2, round, 4)
    idx=grep('rt', colnames(out))
    out[,idx]=apply(out[,idx], 2, round, 2)
    idx=grep('int|max0', colnames(out))
    out[,idx]=apply(out[,idx], 2, round, 0)
    
    output$PeakTbl=renderDT(out, selection = 'multiple', escape = FALSE, options = list(
      searchHighlight = TRUE,
      pageLength = 20,
      rownames= FALSE
    ),
    rownames= FALSE)
  }, ignoreNULL = T, ignoreInit = F)
  print('okay8')
  observeEvent(input$plotselection, {
    idx=input$PeakTbl_rows_selected
    if(length(idx)>0){
      removeNotification('norows')
      df=fgt()[idx,c(11, 1:10)]
      df$roi=paste('ROI', df$roi)
      df$roi=factor(df$roi, levels=df$roi)
      output$peakplt <- renderPlotly({
        df$text_hover=paste0('m/z=', round(df$mz, 4), '<br />', 'rt=', round(df$rt))
        plot_ly(df, x = ~roi, type = 'bar', hoverinfo = 'text', y = ~into, name = 'into: Signal integration', text = ~paste0('into<br />', text_hover), marker = list(color = 'rgba(255,88,120,,0.8)')) %>%
          add_trace(y = ~intb, name = 'intb: Signal integration after baseline correction', text = ~paste0('intb<br />', text_hover), marker = list(color = 'rgba(255,213,117,,0.8)')) %>%
          add_trace(y = ~maxo, name = 'maxo: Maximum signal intensity', text = ~paste0('maxo<br />', text_hover), marker = list(color = 'rgba(0, 214, 167,0.8)')) %>%
          layout(yaxis = list(title = 'Intensity (AU)', showgrid = T), barmode = 'group', xaxis = list(title = ''), legend=list(x=0.15, y=-0.1, orientation='h'))
      })
      output$peakpltIso <- renderPlotly({
        int_max=max(df$maxo)
        df$maxo_norm=(df$maxo/int_max)*100
        idx_maxo=which.max(df$maxo_norm)
        ra_rt=range(c(df$rtmin-1, df$rtmax+1))
        ra_mz=range(c(df$mzmin-0.1, df$mzmax+0.1))
        
        add=dlply(df, as.quoted('roi'), function(x) {
          melt(x, id.vars=colnames(x)[!colnames(x) %in% c('mzmin', 'mzmax', 'rtmin', 'rtmax')])
        })
        idx_rt=grep('rt', add[[1]]$variable)
        df$text_annot=paste0(df$roi, '<br />', paste('mz', round(df$mz, 4)), '<br />', paste('rt', round(df$rt, 2)), '<br />', paste0('Intensity is ',round(df$maxo_norm), '% of ', df$roi[idx_maxo]) )
        df$text_annot[idx_maxo]=paste0(df$roi[idx_maxo], '<br />', paste('mz', round(df$mz[idx_maxo], 4)), '<br />', paste('rt', round(df$rt[idx_maxo], 2)), '<br />', paste0('Intensity set to 100%') )
        
        g1=plot_ly() %>%
          layout(scene = list(xaxis = list(title = "rt (s)", range = ra_rt), yaxis = list(title = "m/z", range = ra_mz), zaxis = list(title = "Intensity (AU)")),  legend=list(x=0.15, y=-0.1, orientation='h'))  %>%
          add_trace(data = df, x = ~rt, y = ~mz, z=~maxo, type = 'scatter3d', mode='markers', color=~sn, hoverinfo='text', marker=list(size=20, color='rgba(166, 143, 195, 1)'), showlegend =F, hoverinfo = 'text', text = ~df$text_annot) %>%
          add_trace(data = df,x = ~rt, y = ~mz, z=~maxo, type = 'scatter3d', mode = 'lines', line = list(width = 10, color='rgba(166, 143, 195, 0.8)'), showlegend = F, name='Min-Max of ROI')# %>%
        
        for(i in 1:length(add)){
          if(i==1){ g1= g1 %>% add_trace(data=add[[i]][idx_rt,], x = ~value, y = ~mz, z=~maxo, type='scatter3d', mode='lines',  line = list(color = 'black', width=5, showscale = F),  name='Signal range in rt dimension', showlegend=T) %>% add_trace(data=add[[i]][-idx_rt,], x = ~value, y = ~mz, z=~maxo, type='scatter3d', mode='lines',  line = list(color = 'red', width=5, showscale = F),  name='Signal range in m/z dimension', showlegend=T)
          }else{
            g1= g1 %>% add_trace(data=add[[i]][idx_rt,], x = ~value, y = ~mz, z=~maxo, type='scatter3d', mode='lines',  line = list(color = 'black',  width=5),  name='', showlegend=F) %>% add_trace(data=add[[i]][-idx_rt,], x = ~value, y = ~mz, z=~maxo, type='scatter3d', mode='lines',  line = list(color = 'red', width=5),  name='', showlegend=F)
          }
        }
        # add rois that fall into the specified spectral region
        ds=fgt()[-idx,c(11, 1:10)]
        idx_other=which(ds$mz>ra_mz[1] & ds$mz<ra_mz[2] & ds$rt>ra_rt[1] & ds$rt<ra_rt[2] & ds$maxo<=int_max)
        if(length(idx_other)>0){
          ds=ds[which(ds$mz>ra_mz[1] & ds$mz<ra_mz[2] & ds$rt>ra_rt[1] & ds$rt<ra_rt[2] & ds$maxo<=int_max),]
          ds$roi=paste('ROI', ds$roi)
          ds$maxo=ds$max/int_max*100
          g1 %>% add_trace(data = ds, x = ~rt, y = ~mz, z=~maxo,  type = 'scatter3d', mode='markers', showlegend =F, hoverinfo='text', marker=list(size=5, color='rgba(0, 214, 167,0.4)', showscale = F), text = ~ds$roi) %>%
            hide_colorbar()
        }else{
          g1 %>% hide_colorbar()
        }
      })
    }else{
      showNotification(ui="Select rows in the peak table", duration=NULL, closeButton = T, type='warning', id='norows')
    }
    
  })
  
  
  session$allowReconnect(TRUE)
}






