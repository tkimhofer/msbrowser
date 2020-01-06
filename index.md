![MSbrowser Logo](www/MSbrowser_logo_tricolour_alpha.png)

[![HitCount](http://hits.dwyl.io/tkimhofer/msbrowser.svg)](http://hits.dwyl.io/tkimhofer/msbrowser)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**MSbrowser** is an R package providing a web-browser-based application for **parameter fine-tuning** of **xcms peak picking algorithms**. 

The app is designed for fast and easy xcms parameter testing and peak picking performance optimisation. Interactive visualisations allow to gain insight into the LC-MS data structure and the effects different peak picking algorithm parameters have on peak picking performance. 

**CentWave parameters:** *ppm, peakwidth, snthresh, prefilter, mzCenterFun, integrate, mzdiff, fitgauss, scanrange, noise.*  All of these parameters require fine-tuning for each assay type and mass analyser. Too stringet parameter values (e.g., ppm to low) can lead to missing out on singals or to peak splitting, the latter leads to quantification errors, which has obvious implications for the statistical analysis. Peak detection errors are not always obvious when using PCA for quality control assessment.



| Parameter | Explanation | Effect |
|-----------|-------------|--------|
|ppm| signal deviation im m/z dimension| |
|peakwidht | range peak elution times in seconds | |
| snthresh | signals below this locally defined value are not considered as signals| |
|pre-filter| Peak definition: how many data points (*n*) should be above a certain intensity threshold (*I*)| |
|mzCenterFun| | |
|integrate| | |
|mzdiff| Closeness of two signals in mz dimension| |
|fitgauss| Peak fitting using Gauss distribution| |
|scanrange| limit peak detection to a certain scanrange (in seconds) | |
|noise| Intensity threshold, values below are considered instrument noise | |



parameter values can have profound negative impact on data analysis. Find out yourself, perform a peak picking quality assessment with MSbrowser!

MSbrowser is implemented in a user-friendly web-application framework that can be executed on a local computer running R or assessed via a web-link.

# Installation and app launching
MSbrowser is a simple R package that can be installed with the following R commands.

`
if(!'devtools' %in% installed.packages()[,1]) install.packages('devtools')

devtools::install_github('tkimhofer/msbrowser')

msbrowser::startApp()
`


# Documentation
Provided is a short tutorial that highlights the different functionalities and provides a step-by-step guidance for less experienced users.


# Support