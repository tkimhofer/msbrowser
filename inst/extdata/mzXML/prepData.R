raw_xcms=xcms::xcmsRaw('inst/extdata/mzXML/Urine_HILIC_ESIpos.mzXML', profstep = 0, includeMSn = FALSE, mslevel = 1)

xraw=raw_xcms[raw_xcms@scantime >= 400 & raw_xcms@scantime <= 405]

save(xraw, file='xred.rda')

library(xml2)
f=read_xml('inst/extdata/mzXML/Urine_HILIC_ESIpos.mzXML')

# xml_find_all(f, "//dataProcessing")
#
# xml_attr(f, 'num')
#
# xml_contents(f)
ch=xml_children(f)[[1]]

te=xml_children(ch)[4:10870] #:10870

msl=sapply(te, function(x){
  xml_attr(x, 'msLevel')
})
table(msl)

ii=xml_find_all(f, '//*[@msLevel=\'2\']')
xml_remove(ii, free = TRUE)


ii=xml_find_all(f, '//*[@num<5000 or @num>5400]')
xml_remove(ii, free = TRUE)

write_xml(x = f, file = 'test1.mzML')

rx=xcms::xcmsRaw('test1.mzML', profstep = 0, includeMSn = FALSE, mslevel = 1)

xcms::plotChrom(raw_xcms)
df=xcms_df(raw_xcms)
save(df, file='test1.rda')






