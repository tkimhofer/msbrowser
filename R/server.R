#' @title  Shiny app server function
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session session ID
#' @import shiny
# @import shinyjs
# @importFrom shinyjs hide hideElement show showElement toggle toggleElement onclick
#' @importFrom ggplot2 ggplot aes aes_string geom_point geom_rect geom_text theme_bw labs scale_colour_gradientn scale_x_continuous sec_axis
#' @import plyr
#' @importFrom xcms xcmsRaw findPeaks.centWave findPeaks.matchedFilter
#' @importFrom colorRamps matlab.like2
#' @importFrom dbscan dbscan
#' @importFrom DT renderDataTable
#' @importFrom reshape2 melt
#' @importFrom shinyBS bsTooltip
#' @importFrom stats median quantile sd
#' @importFrom plotly renderPlotly plot_ly add_trace layout event_register event_data add_segments add_text ggplotly hide_colorbar
#' @importFrom magrittr %>%
#' @importFrom shinyWidgets numericRangeInput radioGroupButtons
#' @importFrom plyr dlply
#' @importFrom utils unzip


server <- function(input, output, session) {

  # initialise parameters
  pars=reactiveValues(
    msfile=NA,
    noise98p=NA,
    noise_plot=NA,
    trans_plot='log10',
    Imax_xic_mz=NA,
    Imax_xic_scant=NA,
    xic_mz=NA,
    xic_ra=NA, # calculated mz window for xic, using xic_mz and ppm_change
    mspec_scant=NA,
    massSpec_scantime=NA,
    ppm_change=50,
    mzra=NA,
    xic_ra=NA,
    pp.mz=NA,
    pp.rt=NA,
    pp.mz.ra=5,
    pp.rt.ra=10
  )

  ui_ind=reactiveValues(
    ichron=0,
    rawData=0,
    ppick=0,
    peaks=0,
    summarytbl=0,
    xicra=0,
    target=0,
    ppdiv=0,
    impvis=0,
    peaktbl=0,
    div_input_collapse=1,
    div_target_collapse=1,
    nopeaks=0
    # pa=0,
    # bp=0,
    # pc=0
  )

  dat_pl=reactiveVal()

  # # suppress warnings
  # storeWarn<- getOption("warn")
  # options(warn = -1)


  observeEvent(input$clicked_text, {
    #print('clicked')
    if(ui_ind$div_input_collapse==1){
      #print('remove UI')
      removeUI('#div_input_collapse')
      ui_ind$div_input_collapse=0
    }else{
      #print('insert UI')
      insertUI('#1ri', 'afterEnd', ui=uiE_div_inp_col)
      ui_ind$div_input_collapse=1
    }

  }, ignoreInit = T, ignoreNULL = T)



  observeEvent(input$clicked_target, {
    #print('clicked')
    if(ui_ind$div_target_collapse==1){
      #print('remove UI')
      removeUI('#target_col')
      ui_ind$div_target_collapse=0
    }else{
      #print('insert UI')
      insertUI('#div_target', 'afterEnd', ui=uiE_div_tar_col)
      ui_ind$div_target_collapse=1
    }

  }, ignoreInit = T, ignoreNULL = T)





  # SELECT FILE
  {
    observeEvent(input$filechoose, {
      msfile1=tryCatch(file.choose(),  error = function(e) 'no selection')
      if(grepl('\\.mzxml$|\\.mzml$|\\.cdf$|\\.netcdf$', msfile1, ignore.case = T)){
        removeNotification(id='nofile')
        output$msfile <- renderText({
          msfile1
        })

        # remove ui's when new file is read-in
        pars$msfile=msfile1
        message(paste0('Selected file: ', pars$msfile))

        updateTabsetPanel(session, inputI='msexpl', selected = 'ichron')

        # remove all ui's that have been included with other files
        removeTab('msexpl', 'ichron')
        ui_ind$ichron=0

        removeTab('msexpl', 'rawData')
        ui_ind$rawData=0

        removeTab('msexpl', 'ppick')
        ui_ind$ppick=0

        removeTab('msexpl', 'peaks')
        ui_ind$peaks=0

        removeUI('#uiE_div_summary_file')
        ui_ind$summarytbl=0

        removeUI('#div_target')
        ui_ind$target=0

        removeUI('#div_ppick')
        ui_ind$ppdiv=0


        # cat('Reading user file...')
      } else{showNotification(ui="Accepted file formats are CDF, netCDF, mzXML, mzData and mzML. Check out ProteoWizard for conversion software. ", duration=NULL, closeButton = T, type='error', id='nofile')}
    }, ignoreNULL = T, ignoreInit = T)

    observeEvent(input$fileexample, {
      # print(logoF)
      removeNotification(id='nofile')

      # check if file was unzipped previously
      exF=system.file(file.path('extdata', 'mzXML', 'Urine_HILIC_ESIpos.mzXML', fsep = .Platform$file.sep), package='msbrowser')
      # print(exF)
      output$msfile <- renderText({
        'Example file: HILIC-ESI(+)-MS of a urine sample'
      })
      # in case it can't find exF (hasn't been unzipped)
      if(exF=='') {
        # locate file
        zipF=file.path('extdata', 'mzXML', 'Urine_HILIC_ESIpos.mzXML.zip', fsep = .Platform$file.sep)
        exFzip=system.file(zipF, package = "msbrowser")
        if(exFzip==''){message('No example file installed'); }

        # cat('Unzipping LC-MS example file ...')
        unzip(exFzip, exdir=dirname(exFzip))
        pars$msfile=gsub('\\.zip', '', exFzip)
        # cat('...done!\n')
        # cat('Reading example file...')
        message(paste0('Selected file: ', pars$msfile))
      }else{pars$msfile=exF}
    }, ignoreNULL = T, ignoreInit = T)
  }

  # AND READ DATA
  raw_data<-eventReactive(pars$msfile, {

    raw_xcms=xcmsRaw(pars$msfile, profstep = 0, includeMSn = F, mslevel = 1)
    df_xcms=xcms_df(raw_xcms)

    #browser()
    # create summary stats and insert into side bar
    output$datsum<-renderTable(
      {data.frame(Descr=c('Scan time range', 'Scan frequency', 'Mass range', 'Median ion count across scans', 'Counts ECDF(x<=X), p=0.98'),
                  Value=c(paste(paste(round(range(raw_xcms@scantime)), collapse = '-'), 's'),
                          paste(round(1/median(diff(raw_xcms@scantime[order(raw_xcms@scanindex)]))), 'scans per s'), paste(paste(round(raw_xcms@mzrange), collapse = '-'), 'm/z'),
                          paste(format(median(raw_xcms@tic), scientific = T, digits = 3)),
                          paste(pars$noise98p))
      )} , rownames=F, colnames=F, spacing='xs', caption = "Summary", caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))


    if(ui_ind$summarytbl==0){
      # instert summary file
      # insertUI(
      #   selector = "#fileexample",
      #   where = "afterEnd",
      #   ui = uiE_div_summary_file,
      #   immediate=F
      # )
      ui_ind$summarytbl=1
      output$ss1=renderUI(uiE_div_summary_file)
    }


    # expand fields for manual entering mz values for xic
    observeEvent(input$imp_xic,{
      if(input$imp_xic & ui_ind$xicra==0){
        insertUI(
          selector = "#imp_xic",
          where = "afterEnd",
          ui = uiE_div_xic
        )
        ui_ind$xicra=1
      }


      if(!input$imp_xic  & ui_ind$xicra==1){
        removeUI(
          selector = "#div_xic"
        )
        ui_ind$xicra=0
      }
    }, ignoreInit = T, ignoreNULL = T)



    # get parameter values for plotting
    pars$noise98p=round(quantile(df_xcms$Int,probs = 0.98))
    pars$Imax_xic_mz=df_xcms$mz[which.max(df_xcms$Int)]
    pars$Imax_xic_scant=df_xcms$scantime[which.max(df_xcms$Int)]
    pars$xic_mz=df_xcms$mz[which.max(df_xcms$Int)]
    pars$mspec_scant=df_xcms$scantime[which.max(df_xcms$Int)]
    pars$mzra=range(df_xcms$mz)
    pars$scantimera=range(df_xcms$scantime)
    #cat('completed!\nPlotting chromatograms and mass spectrum...')
    return(list(df_xcms, raw_xcms))
  }, ignoreNULL = T, ignoreInit = T)



  # GENERATE CHROMATOGRAMS AND SINGLE SCAN MASS SPECTRUM
  {
    #observeEvent({raw_data()},{
    # bpc triggered when data is loaded
    output$tic_bpc <- renderPlotly({
      pa=chrom_bpc_tic(df=raw_data()[[1]], pars)
      pars$pa=1
      return(pa)
    })

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
      # add tab showing chromatograms
      if(ui_ind$ichron==0){
        prependTab(
          inputId='msexpl',
          tab=uiT_ichron,
          select=T
        )
        ui_ind$ichron=1
      }

      # calc mzrange for xic
      pars$xic_ra=xic_mzrange(pars$xic_mz, pars$ppm_change)
      # scantime for single scan mass spectrum
      pars$mspec_scant=pars$Imax_xic_scant
    }, ignoreNULL = T)


    # 2. click on BPC/TIC
    observeEvent(

      {
        req(pars$pa)
        event_data("plotly_click", source='pa')
      }
      ,{
        df=raw_data()[[1]]
        event.data <- event_data("plotly_click", source='pa')

        # which scan xic is closest to click, save scan mspec
        sdif=abs(df$scantime-event.data$x[1])
        pars$mspec_scant=df$scantime[which(sdif==min(sdif))[1]]

        # get max mz for respective scantime
        sub=df[which(df$scantime==pars$mspec_scant),]

        # save parameters for XIC plotting
        pars$xic_mz=sub$mz[which.max(sub$Int)]
        pars$xic_ra=xic_mzrange(pars$xic_mz, pars$ppm_change)

      }, ignoreInit = T, ignoreNULL = T)


    # pb_act=observeEvent(
    observeEvent({
      req(pars$pb)
      event_data("plotly_click", source='pb')
      },{
        df=raw_data()[[1]]
        event.data <- event_data("plotly_click", source='pb')
        # which scan xic is closest to click, save scan mspec
        sdif=abs(df$scantime-event.data$x[1])
        pars$mspec_scant=df$scantime[which(sdif==min(sdif))[1]]
      }, ignoreInit = T, ignoreNULL = T)

    # clean-up / validate manually entered mz range for xic


    observeEvent(input$go_xic, {
      updateTabsetPanel(session, inputI='msexpl', selected = 'ichron')
      xic_ra=sort(input$xic_ra)
      if(all(is.numeric(xic_ra))){
        if(length(xic_ra)==2 & all(!is.na(xic_ra)) &  xic_ra[2]!=xic_ra[1]){
          removeNotification(id='xic_notnumeric')
          pars$xic_ra=xic_ra
        }
      } else{
        showNotification(ui="Check entered mass range for XIC!", duration=NULL, closeButton = T, type='warning', id='xic_notnumeric')
      }
    }, ignoreNULL = T, ignoreInit = T)


    # Generate xic plot based on trigger event
    observeEvent(
      {pars$xic_ra},{
        output$xic <- renderPlotly({
          #browser()
          pb=chrom_xic(df=raw_data()[[1]], pars)
          pars$pb=1
          return(pb)
        })
      }, ignoreNULL = T, ignoreInit = T)

    observeEvent(
      {pars$xic_ra
        pars$mspec_scant},{
        #print('xic plot now generated')
        # Generate ssms plot based on trigger event
        output$ssms <- renderPlotly({
          pc=massspectrum(df=raw_data()[[1]], pars)
          pars$pc=1
          return(pc)
          #browser()
          # pc<<-pc
          # pc
        })
      }, ignoreNULL = T, ignoreInit = T)



    observeEvent({
      req(pars$pc)
      event_data("plotly_click", source='pc')
      },{
        event.data <- event_data("plotly_click", source='pc')
        #browser()
        pars$pp.mz=as.numeric(event.data$x[1])
        pars$pp.rt=as.numeric(pars$mspec_scant)

       #print('check 1')
        output$selection <- renderText({
          paste('Selected signal: scan time', round(as.numeric(pars$pp.rt),2), 's, m/z', round(as.numeric(pars$pp.mz), 4))
        })

        removeUI('#proceed')

        if(ui_ind$target==0){
          insertUI(
            selector='#div_input',
            where='afterEnd',
            ui=uiE_target
          )
          ui_ind$target=1

          insertUI(
            selector='#div_target',
            where='afterEnd',
            ui=uiE_div_tar_col
          )


          ui_ind$div_target_collapse=1
        }

        updateNumericInput(session, inputId='in_rt', value=as.numeric(pars$pp.rt))
        updateNumericInput(session, inputId='in_mz', value=round(as.numeric(pars$pp.mz), 4))
        updateNumericInput(session, inputId='in_noisethr', value=round(pars$noise98p))

      }, ignoreInit = T, ignoreNULL = T)


    observeEvent(input$'imp_vis',{
      if(input$'imp_vis'==T & ui_ind$impvis==0){
        insertUI(
          selector='#selectors1',
          where='afterEnd',
          ui= div(id ="div_vis",
                  br(),
                  fluidRow(
                    column(12, offset=0.7,
                           column(width=3, numericInput(inputId='in_noisethr', label = 'Noise Threshold', value=as.numeric(pars$noise98p))),
                           column(width=9, align='center', radioGroupButtons('raw_trans', label='Data Transformation', choices = list('None'='none', 'Squared'='sqrt', 'Square Root'='exp', 'Log 10'='log10', 'Reciprocal'='reciprocal'), selected = 'log10'))
                    )
                  )
          )
        )
        ui_ind$impvis=1
      }

      if(input$'imp_vis'==F & ui_ind$impvis==1){
        removeUI(
          selector = '#div_vis'
        )
        ui_ind$impvis=0
      }
    })


    observeEvent({
      req(input$raw_trans)
      input$raw_trans}, {
      pars$trans_plot=input$raw_trans
    }, ignoreNULL = T, ignoreInit = T)

    observeEvent({
      req(input$in_noisethr)
      input$in_noisethr}, {
      pars$noise_plot=input$in_noisethr
    })


    ttt=eventReactive(
      { input$move_picks}, # 'Generate plot' has been clicked
      {
        if(ui_ind$rawData==0){
          insertTab(
            inputId='msexpl',
            tab=uiT_rawData,
            target='ichron',
            position='after',
            select=T
          )
          ui_ind$rawData=1
        }else{
          updateTabsetPanel(session, inputI='msexpl', selected = 'rawData')
        }

        if(input$target_input=='man'){
          pars$pp.mz=input$in_mz
          pars$pp.mz.ra=input$in_mz_ws

          pars$pp.rt=input$in_rt
          pars$pp.rt.ra=input$in_rt_ws
        }

        # collapse side 1 and 2
        removeUI('#target_col')
        ui_ind$div_target_collapse=0

        removeUI('#div_input_collapse')
        ui_ind$div_input_collapse=0

        mf=raw_data()[[1]]
        message('Generating spectral area plot (scan time vs mz).')
        target.rt=as.numeric(input$in_rt)
        target.mz=as.numeric(input$in_mz)
        # create sub with fixed equidist window size from target signal
        wind.rt=input$in_rt_ws/2
        wind.mz=input$in_mz_ws/2
        ra.rt=c(max(target.rt-wind.rt, min(mf$scantime)), min(target.rt+wind.rt, max(mf$scantime)))
        ra.mz=c(max(target.mz-wind.mz, min(mf$mz)), min(target.mz+wind.mz, max(mf$mz)))
        idx=which(mf$mz>ra.mz[1] & mf$mz<ra.mz[2] & mf$scantime>ra.rt[1] & mf$scantime<ra.rt[2])
        sub=mf[idx,]
        dat_pl(sub)
        # plot raw data

        output$rawdd <- renderPlotly({
          if(is.na(pars$noise_plot)){
            pars$noise_plot=pars$noise98p
          }

          noi=pars$noise_plot
          idx=sub$Int<=noi
          df=transf(sub, pars$trans_plot)

          g1=ggplot()+
            geom_point(data=df[idx,], aes(scantime, mz, colour=Int), size=0.1)+
            geom_point(data=df[!idx,], aes(scantime, mz, colour=Int), size=1)+
            #scale_x_continuous(sec.axis = sec_axis(trans=~./60, name='Scantime (min)'))+
            theme_bw()+
            scale_colour_gradientn(colours=matlab.like2(10))+
            labs(x='Scan time (s)', y='m/z', colour='Counts',  caption='Raw Data')
#
#           if(pars$trans_plot=='none'){
#             g1=g1+scale_colour_gradientn(colours=matlab.like2(10))
#           }else{
#             g1= g1+scale_colour_gradientn(colours=matlab.like2(10), trans=pars$trans_plot)
#           }

          ggplotly(g1, height=1000, width=1100, dynamicTicks=T)
        })


        if(ui_ind$ppdiv==0){
          insertUI(
            selector='#div_target',
            where='afterEnd',
            ui=uiE_div_ppick
          )
          ui_ind$ppdiv=1

        }
        return(sub)
      }, ignoreNULL = T, ignoreInit = T)

    observeEvent(ttt(), {message(paste0('Number of data points ', nrow(ttt()), '.'))})

    # transition to next step (select region for vis 3d raw data)
    # either by clicking on next (move)
    # or by clicking in mass spectrum plot, then peak picking paramters are automatically shown

    # onclick('h3test', alert(date()))



    # update ui for internal standards (depending on assay selection) and change mz / rt according to db entry
    # internal standards / db entries
    {
      observeEvent(input$"db_assays", {
        icst_cname=icst[icst$assay %in% input$"db_assays",]
        updateSelectizeInput(session, "in_icst",
                             label = "Compound",
                             choices = dlply(icst_cname, as.quoted('info'), function(x) {x[,2]}),
                             selected=F)
        output$compound_info=renderText({''})
      })

      observeEvent(input$in_icst, {
        icst_cname=icst[icst$assay %in% input$"db_assays",]
        idx=match(input$in_icst, icst_cname$compound)
        if(length(idx)>0 & !is.na(idx)){
          updateNumericInput(session, 'in_mz', value = as.numeric(icst_cname$mz[idx]))
          updateNumericInput(session, 'in_rt', value = as.numeric(icst_cname$rt[idx]))
          output$compound_info=renderText({paste0('Scan time=', as.numeric(icst_cname$rt[idx]), ' s, m/z=', as.numeric(icst_cname$mz[idx]))})

          pars$pp.mz=icst_cname$mz[idx]
          #pars$pp.mz.ra=input$in_mz_ws

          pars$pp.rt=icst_cname$rt[idx]
          #pars$pp.rt.ra=input$in_rt_ws

        }
      }, ignoreNULL = T, ignoreInit = T)
    }

    # peak picking
    fgt <- eventReactive(input$pickpeak1, {
      #browser()

      #showTab(inputId = "msexpl", target = "ppick", select=T)
      updateTabsetPanel(session, inputI='msexpl', selected = 'ppick')
      if(ui_ind$nopeaks==1){
        removeNotification('nopeaks')
        ui_ind$nopeaks=0
      }
      raw_xcms=raw_data()[[2]] # this is xcms object
      target.rt=as.numeric(input$in_rt)
      target.mz=as.numeric(input$in_mz)
      wind.rt=input$in_rt_ws/2
      wind.mz=input$in_mz_ws/2
      mf=dat_pl()
      #if(exists(mf))
      switch(input$in_pickMethod,
             'centWave'={
               message('Performing peak picking (centWave)')
               peaktbl=findPeaks.centWave(raw_xcms,
                                          ppm=as.numeric(input$in_mzdev),
                                          peakwidth=input$in_rtrange,
                                          snthresh=as.numeric(input$in_sn),
                                          prefilter=c(input$in_prefilter_k, as.numeric(input$in_prefilter_I)),
                                          mzCenterFun=input$in_mzCentFun,
                                          integrate=as.numeric(input$in_integrate),
                                          mzdiff=input$in_mzdiff,
                                          fitgauss=as.logical(input$in_fitgauss),
                                          noise=as.numeric(input$in_noise),
                                          scanrange=range(mf$scan)
               )
               peaktbl=as.data.frame(peaktbl)

               codeF=paste0('msfile=\"', pars$msfile, '\"\n')
               codeRI=paste0('xcms_data=xcmsRaw(filename=msfile , profstep = 0, includeMSn = F, mslevel = 1)\n')
               codePP=paste0('xcms_ppick=findPeaks.centWave(xcms_data, ', 'ppm=', as.numeric(input$in_mzdev), ' , peakwidth=c(', input$in_rtrange[1], ', ', input$in_rtrange[2] ,'), snthresh=', as.numeric(input$in_sn),', prefilter=c(', input$in_prefilter_k, ', ', as.numeric(input$in_prefilter_I), '), mzCenterFun="', input$in_mzCentFun, '", integrate=', as.numeric(input$in_integrate), ', mzdiff=', input$in_mzdiff, ', fitgauss=', input$in_fitgauss, ', noise=', as.numeric(input$in_noise), ')\n')

               # cat(codeRI, '\n')
               # cat(codePP, '\n')


               # output$centwave_pars=renderText({paste0('ppm=', as.numeric(input$in_mzdev), ' , peakwidth=c(', input$in_rtrange[1], ', ', input$in_rtrange[2] ,'), snthresh=', as.numeric(input$in_sn),', prefilter=c(', input$in_prefilter_k, ', ', as.numeric(input$in_prefilter_I), '), mzCenterFun=\"', input$in_mzCentFun, '\", integrate=', as.numeric(input$in_integrate), ', mzdiff=', input$in_mzdiff, ', fitgauss=', input$in_fitgauss, ', noise=', as.numeric(input$in_noise), ')')})

               output$Rcode_loadp<-renderText({'library(xcms)\n'})
               output$Rcode_file<-renderText({codeF})
               output$Rcode_readIn<-renderText({codeRI})
               output$Rcode_ppick<-renderText({codePP})



             },
             'matchedFilter'={
               message('Performing peak picking (matchedFilter)')
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
             }
             # ,
             # 'dbscan'={
             #   # # noise=10
             #   # ppm=60
             #   #fe=1e4
             #   exp_fact=as.numeric(input$in_ppm_mztrans)
             #   mz_cor=((1/as.numeric(input$ppm))*exp_fact)
             #   rt_fac=as.numeric(input$in_rttrans)
             #   # mzp=280
             #   # rtp=247
             #   # noise=1000
             #
             #   #idx=which(df_xcms$mz>800 & df_xcms$mz<820 & df_xcms$scantime>320 & df_xcms$scantime<370)
             #   #df_xcms[which.min(df_xcms$mz-802),]
             #
             #   # define df and noise
             #   #browser()
             #   df=ttt()
             #
             #   dcl=df[df$Int>as.numeric(input$in_noise),c(1,3)]
             #   dcl$mz=dcl$mz*mz_cor
             #   dcl$scan=dcl$scan*scan_cor
             #
             #   # calculate distance matrix and check ditances for a signal manually
             #   # tt=dist(dcl)
             #   # ttm=melt(tt)
             #   # which(ttm$value<1 & ttm$value>0)
             #
             #   dbclust=dbscan(dcl, eps=3, minPts = 2, search='kdtree', borderPoints = T); test;
             #   dcl$cl=dbclust$cluster
             #
             #   clb=ddply(dcl, as.quoted('cl'), function(x){
             #     if(nrow(x)<5){return(NULL)}
             #     c(range(x$mz), range(x$scanrange), max(x$Int), sd(x$Int))
             #   })
             #
             #   peaktbl=clb
             #
             # }
      )



      message('done!\n')

      # create sub with fixed window around target
      ra.rt=c(max(target.rt-wind.rt, min(mf$scantime)), min(target.rt+wind.rt, max(mf$scantime)))
      ra.mz=c(max(target.mz-wind.mz, min(mf$mz)), min(target.mz+wind.mz, max(mf$mz)))
      idx=which(peaktbl$mz>=ra.mz[1] & peaktbl$mz<=ra.mz[2] & peaktbl$rt>=ra.rt[1] & peaktbl$rt<=ra.rt[2])
      # if(length(idx)==0){return(NULL)}

      if(length(idx)>0){

        if(ui_ind$ppick==0){
          insertTab(
            inputId='msexpl',
            tab=uiT_ppick,
            target='rawData',
            position='after',
            select=T
          )

          insertTab(
            inputId='msexpl',
            tab=uiT_peaks,
            target='ppick',
            position='after',
            select=F
          )


          ui_ind$ppick=1
          ui_ind$peaktbl=1
        }

        #cat('Vis peaks.\n')
        ptbl=peaktbl[idx,]
        ptbl=ptbl[order(ptbl$maxo, decreasing = T),]
        ptbl$feature=as.character(1:nrow(ptbl))
        output$pp1 <- renderPlotly({

          # raw data
          # small points when not belonging to signal
          idc=unlist(dlply(ptbl, as.quoted('feature'), function(peak, ds=mf){
            which(ds$mz>=peak$mzmin & ds$mz<=peak$mzmax & ds$scantime >= peak$rtmin & ds$scantime <= peak$rtmax)
          }))
          df=transf(mf, pars$trans_plot)
          df$peak='No'
          df$peak[idc]='Yes'
          g2=ggplot()+
            geom_point(data=subset(df, peak=='No'), aes(scantime, mz, colour=Int), size=0.1)+
            geom_rect(data=ptbl, aes(xmin=rtmin, xmax=rtmax, ymin=mzmin, ymax=mzmax),  size=1 , color='darkgrey', fill='darkgrey')+
            geom_point(data=subset(df, peak=='Yes'), aes(scantime, mz, colour=Int), size=1)+
            geom_text(data=ptbl, aes(x=rtmax, y=mzmin, label=feature), colour='red', size=5, hjust=0, vjust=0)+
            theme_bw()+
            scale_colour_gradientn(colours=matlab.like2(10))+
            scale_x_continuous(sec.axis = sec_axis(trans=~./60, name='Scan time (min)'))+
            labs(x='Scan time (s)', y='m/z', colour='Counts')

          # if(pars$trans_plot=='none'){
          #   g2=g2+scale_colour_gradientn(colours=matlab.like2(10))
          # }else{
          #   g2= g2+scale_colour_gradientn(colours=matlab.like2(10), trans=pars$trans_plot)
          # }

          ggplotly(g2, height=1000, width=1100, dynamicTicks=T)
        })
        return(ptbl)
      }else{
        message('No peaks detected- change parameter values!');
        ui_ind$nopeaks=1
        showNotification(ui="No peaks detected!", duration=NULL, closeButton = T, type='error', id='nopeaks');
        return(NULL)
      }
    }, ignoreNULL = T)

    observeEvent(fgt(), {
      message(paste('Features:', nrow(fgt())))
    })

    peakTbl <- observeEvent(fgt(), {
      out=fgt()[,c(11, 1:10)]
      rownames(out)=NULL
      #cat('Ouputting peak table.\n')
      idx=grep('mz', colnames(out))
      out[,idx]=apply(out[,idx], 2, round, 4)
      idx=grep('rt', colnames(out))
      out[,idx]=apply(out[,idx], 2, round, 2)
      idx=grep('int|max0', colnames(out))
      out[,idx]=apply(out[,idx], 2, round, 0)

      # browser()
      output$PeakTbl=DT::renderDataTable(
        out, server=F,
        selection = 'multiple',
        escape = FALSE,
        extensions = 'Buttons',
        options = list(
          dom='Bfrtips',
          buttons=list('copy', 'csv', 'excel', 'print'),
          searchHighlight = TRUE,
          pageLength = 30,
          rownames= FALSE
        ),
        rownames= FALSE)
    }, ignoreNULL = T, ignoreInit = F)

    observeEvent(input$plotselection, {
      idx=input$PeakTbl_rows_selected
      if(length(idx)>0){
        removeNotification('norows')
        df=fgt()[idx,c(11, 1:10)]
        df$feature=paste('Feat.', df$feature)
        df$feature=factor(df$feature, levels=df$feature)
        output$peakplt <- renderPlotly({
          df$text_hover=paste0('m/z=', round(df$mz, 4), '<br />', 'rt=', round(df$rt))
          plot_ly(df, x = ~feature, type = 'bar', hoverinfo = 'text', y = ~into, name = 'into: Signal integration', text = ~paste0('into<br />', text_hover), marker = list(color = 'rgba(255,88,120,,0.8)')) %>%
            add_trace(y = ~intb, name = 'intb: Signal integration after baseline correction', text = ~paste0('intb<br />', text_hover), marker = list(color = 'rgba(255,213,117,,0.8)')) %>%
            add_trace(y = ~maxo, name = 'maxo: Maximum signal intensity', text = ~paste0('maxo<br />', text_hover), marker = list(color = 'rgba(0, 214, 167,0.8)')) %>%
            layout(yaxis = list(title = 'Intensity', showgrid = T), barmode = 'group', xaxis = list(title = ''), legend=list(x=0.15, y=-0.1, orientation='h'))
        })
        output$peakpltIso <- renderPlotly({
          int_max=max(df$maxo)
          df$maxo_norm=(df$maxo/int_max)*100
          idx_maxo=which.max(df$maxo_norm)
          ra_rt=range(c(df$rtmin-1, df$rtmax+1))
          ra_mz=range(c(df$mzmin-0.1, df$mzmax+0.1))

          add=dlply(df, as.quoted('feature'), function(x) {
            melt(x, id.vars=colnames(x)[!colnames(x) %in% c('mzmin', 'mzmax', 'rtmin', 'rtmax')])
          })
          idx_rt=grep('rt', add[[1]]$variable)
          df$text_annot=paste0(df$feature, '<br />', paste('mz', round(df$mz, 4)), '<br />', paste('rt', round(df$rt, 2)), '<br />', paste0('Intensity is ',round(df$maxo_norm), '% of ', df$feature[idx_maxo]) )
          df$text_annot[idx_maxo]=paste0(df$feature[idx_maxo], '<br />', paste('mz', round(df$mz[idx_maxo], 4)), '<br />', paste('rt', round(df$rt[idx_maxo], 2)), '<br />', paste0('Intensity set to 100%') )

          g1=plot_ly() %>%
            layout(scene = list(xaxis = list(title = "scan time (s)", range = ra_rt), yaxis = list(title = "m/z", range = ra_mz), zaxis = list(title = "Counts")),  legend=list(x=0.15, y=-0.1, orientation='h'))  %>%
            add_trace(data = df, x = ~rt, y = ~mz, z=~maxo, type = 'scatter3d', mode='markers', color=~sn, hoverinfo='text', marker=list(size=20, color='rgba(166, 143, 195, 1)'), showlegend =F, hoverinfo = 'text', text = ~df$text_annot) %>%
            add_trace(data = df,x = ~rt, y = ~mz, z=~maxo, type = 'scatter3d', mode = 'lines', line = list(width = 10, color='rgba(166, 143, 195, 0.8)'), showlegend = F, name='Min-Max of Feat.')# %>%

          for(i in 1:length(add)){
            if(i==1){ g1= g1 %>% add_trace(data=add[[i]][idx_rt,], x = ~value, y = ~mz, z=~maxo, type='scatter3d', mode='lines',  line = list(color = 'black', width=5, showscale = F),  name='Signal width in rt dimension', showlegend=T) # %>% add_trace(data=add[[i]][-idx_rt,], x = ~value, y = ~mz, z=~maxo, type='scatter3d', mode='lines',  line = list(color = 'red', width=5, showscale = F),  name='Signal range in m/z dimension', showlegend=T)
            }else{
              g1= g1 %>% add_trace(data=add[[i]][idx_rt,], x = ~value, y = ~mz, z=~maxo, type='scatter3d', mode='lines',  line = list(color = 'black',  width=5),  name='', showlegend=F) #%>% add_trace(data=add[[i]][-idx_rt,], x = ~value, y = ~mz, z=~maxo, type='scatter3d', mode='lines',  line = list(color = 'red', width=5),  name='', showlegend=F)
            }
          }
          # add rois that fall into the specified spectral region
          ds=fgt()[-idx,c(11, 1:10)]
          idx_other=which(ds$mz>ra_mz[1] & ds$mz<ra_mz[2] & ds$rt>ra_rt[1] & ds$rt<ra_rt[2] & ds$maxo<=int_max)
          if(length(idx_other)>0){
            ds=ds[which(ds$mz>ra_mz[1] & ds$mz<ra_mz[2] & ds$rt>ra_rt[1] & ds$rt<ra_rt[2] & ds$maxo<=int_max),]
            ds$feature=paste('Feature', ds$feature)
            ds$maxo=ds$max/int_max*100
            g1 %>% add_trace(data = ds, x = ~rt, y = ~mz, z=~maxo,  type = 'scatter3d', mode='markers', showlegend =F, hoverinfo='text', marker=list(size=5, color='rgba(0, 214, 167,0.4)', showscale = F), text = ~ds$feature) %>%
              hide_colorbar()
          }else{
            g1 %>% hide_colorbar()
          }
        })
      }else{
        showNotification(ui="Select rows in the peak table", duration=NULL, closeButton = T, type='warning', id='norows')
      }

    })

  }

}





