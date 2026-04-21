# preparing reduced example LC-MS data
xeF <- system.file(file.path("extdata", "Urine_HILIC_ESIpos_msLevel1.mzML.zip",
                             fsep = .Platform$file.sep), package = "lcmsData")

tmp_dir <- tempdir()
mzml <- unzip(xeF, exdir = tmp_dir)
raw_xcms <- xcmsRaw(mzml, profstep = 0, includeMSn = FALSE, mslevel = 1)

df <- xcms_df(raw_xcms)
idc = df$scantime > 100 & df$scantime < 160
df_sub = df[idc,]
dim(df_sub)

save(df_sub, file = "inst/extdata/HILIC_ESIpos_msLevel1_urine.rda")



library(MSnbase)
xeF <- system.file(file.path("extdata", "Urine_HILIC_ESIpos_msLevel1.mzML.zip",
                             fsep = .Platform$file.sep), package = "lcmsData")
tmp_dir <- tempdir()
mzml <- unzip(xeF, exdir = tmp_dir)

x <- readMSData(mzml, mode = "onDisk")
x_sub <- filterRt(x, c(110, 160))   # seconds, if your file uses seconds
writeMSData(x_sub, file = "inst/extdata/HILIC_ESIpos_msLevel1_urine_red.mzML")
