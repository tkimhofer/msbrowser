# add www folder to resource path
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the MSbrowser! If you encounter any issues or have queries please log @ https://github.com/tkimhofer/msbrowser/issues.")
  #addResourcePath('www',  system.file("www", package = "msbrowser"))

  invisible(suppressPackageStartupMessages(
    vapply(c('xcms', 'mzR', 'methods', 'shiny'),
           requireNamespace, quietly = TRUE, FUN.VALUE = NULL)
  )
  )


}


.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    vapply(c('xcms', 'mzR', 'methods', 'shiny'),
           requireNamespace, quietly = TRUE, FUN.VALUE = NULL)
  )
  )
}

