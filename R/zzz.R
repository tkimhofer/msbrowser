# .onLoad <- function(libname, pkgname) {
#   shiny::addResourcePath(
#     "msbrowser",
#     system.file("www", package = pkgname)
#   )
# }

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste(
      "MSbrowser loaded",
      "Report issues:",
      "https://github.com/tkimhofer/msbrowser/issues",
      sep = "\n"
    )
  )
}
