#' @title  Read-in LC-MS file (xcms) and extract raw scan data and create dataframe
#' @param raw_xcms xcms data object
#' @importFrom xcms xcmsRaw

xcms_df=function(raw_xcms){
  message('extract xcms raw data')
  df_xcms=data.frame(mz=raw_xcms@env$mz, Int=raw_xcms@env$intensity)
  ndp=length(raw_xcms@env$mz)
  idx.trans=which(raw_xcms@env$mz[-1]<raw_xcms@env$mz[-ndp]) # every new scan starts with low mz
  idx.scans=cbind(c(0, idx.trans)+1, c(idx.trans, ndp)) # create df of indices (one scan per row)
  df_xcms$scan=unlist(sapply(1:nrow(idx.scans), function(i){ rep(i, diff(idx.scans[i,])+1) }))
  df_xcms$scantime=raw_xcms@scantime[df_xcms$scan] # add scantime (s)
  return(df_xcms)
}
