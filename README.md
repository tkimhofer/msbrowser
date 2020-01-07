![](www/MSbrowser_logo_tricolour_alpha.png)

[![HitCount](http://hits.dwyl.io/tkimhofer/msbrowser.svg)](http://hits.dwyl.io/tkimhofer/msbrowser)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/rstudio/rmarkdown.svg?branch=master)](https://travis-ci.org/rstudio/rmarkdown)

**MSbrowser** is an R package providing a web-browser-based application for **parameter fine-tuning** of **xcms peak picking algorighms**. 

The app is designed for fast and easy xcms parameter testing and peak picking performance optimisation. Different visualisation allow to gain insight into the LC-MS data structure and the effects different peak picking algorithm parameters have on peak picking performance. 

MSbrowser is implemented in a user-friendly web-application framework that can be executed on a local computer running R or assessed via a web-link.

# Installation and app launching
Installation is performed with the following R commands

``
if(!'devtools' %in% installed.packages()[,1]) install.packages('devtools')
devtools::install_github('tkimhofer/msbrowser')

msbrowser::startApp()
``


# Documentation
Provided is a short tutorial that highlights the different functionalities and provides a step-by-step guidance for less experienced users.


# Support
