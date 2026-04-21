# Launch the MSbrowser application

Starts the interactive MSbrowser graphical user interface built with
shiny. The application provides tools for visualisation, inspection, and
processing of LC-MS data, including raw data exploration, target
selection, and peak picking workflows.

## Usage

``` r
startApp()
```

## Value

A [shinyApp](https://rdrr.io/pkg/shiny/man/shinyApp.html) object. The
function is primarily called for its side effect of launching the
interactive application.

## Details

This function initializes the Shiny application by loading the user
interface and server logic defined within the package. The app will open
in the default web browser or in the RStudio Viewer, depending on the
environment.

## Examples

``` r
if (FALSE) { # \dontrun{
msbrowser::startApp()
} # }
```
