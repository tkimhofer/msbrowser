![](www/MSbrowser_logo_tricolour_alpha.png)

[![HitCount](http://hits.dwyl.io/tkimhofer/msbrowser.svg)](http://hits.dwyl.io/tkimhofer/msbrowser)
[![Build Status](https://travis-ci.org/rstudio/rmarkdown.svg?branch=master)](https://travis-ci.org/rstudio/rmarkdown)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**MSbrowser** is an R package providing a web-browser-based application for **parameter fine-tuning** of **xcms peak picking algorighms**. 

The app is designed for fast and easy xcms parameter testing and peak picking performance optimisation. Different visualisation allow to gain insight into the LC-MS data structure and the effects different peak picking algorithm parameters have on peak picking performance. 

MSbrowser is implemented in a user-friendly web-application framework that can be executed on a local computer running R or assessed via a web-link.

# Installation and launch
Installation is performed with the following R commands

```R
# install devtools (if not already installed)
if(!'devtools' %in% installed.packages()[,1]) install.packages('devtools')

# install MSbroswer
devtools::install_github('tkimhofer/msbrowser')
```

Once successfully installed, MSbrowswer can be started as follows
```R
msbrowser::startApp()
```
A new web-browser window opens with the MSbrowser user interface.


# Documentation
Provided is a short video tutorial that walks through the different functionalities of MS browser, provides a step-by-step guidance for first time user.

Descriptions of all *centWave* peak picking paramters can be found under the **xcms CentWave** option in the menu above. 
