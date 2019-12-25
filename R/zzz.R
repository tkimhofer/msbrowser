# add www folder to resource path
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the MSbrowser! If you encounter any issues or have queries please visit https://github.com/tkimhofer/msbrowser/issues or send a report to msbrowser@tkimhofer.com.")
  addResourcePath('www',  system.file("www", package = "msbrowser"))

  invisible(suppressPackageStartupMessages(
    sapply(c('xcms', 'mzR', 'shinyjs', 'methods', 'shiny'),
           requireNamespace, quietly = TRUE)
  )
  )


}
