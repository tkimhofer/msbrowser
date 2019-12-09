

icst=read.table('inst/extdata/signalDB.csv', sep=',', stringsAsFactors = F, comment.char ='#', blank.lines.skip = T,row.names = NULL, skip=1, col.names = c('assay', 'compound', 'mz', 'rt', 'info'))

icst=icst[icst$assay!='' & !is.na(icst$assay),]




