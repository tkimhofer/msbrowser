# add www folder to resource path
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the MSbrowser! If you encounter any issues or have queries please log @ https://github.com/tkimhofer/msbrowser/issues.")
  #addResourcePath('www',  system.file("www", package = "msbrowser"))

  invisible(suppressPackageStartupMessages(
    sapply(c('xcms', 'mzR', 'methods', 'shiny'),
           requireNamespace, quietly = TRUE)
  )
  )


}


.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c('xcms', 'mzR', 'methods', 'shiny'),
           requireNamespace, quietly = TRUE)
  )
  )
}

