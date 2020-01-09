![](www/MSbrowser_logo_tricolour_alpha.png)

[![HitCount](http://hits.dwyl.io/tkimhofer/msbrowser.svg)](http://hits.dwyl.io/tkimhofer/msbrowser)
[![Build Status](https://travis-ci.org/rstudio/rmarkdown.svg?branch=master)](https://travis-ci.org/rstudio/rmarkdown)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**MSbrowser** is web-browser-based application for **parameter fine-tuning** of **xcms peak picking algorithms**. 

The app is designed for fast and easy parameter testing to optimise xcms peak picking performance. Different visualisations allow to gain insight into the LC-MS data structure and the effects different peak picking parameter values have on peak picking performance. 

MSbrowser is implemented in a user-friendly web-application framework that can be installed as an simple R package.

# Installation and launch
Installation is performed with the following R commands:

```R
# install devtools (if not already installed)
if(!'devtools' %in% installed.packages()[,1]) install.packages('devtools')

# install MSbroswer
devtools::install_github('tkimhofer/msbrowser')
```

Once successfully installed, MSbrowser can be launched with the following R-terminal commands:

```R
msbrowser::startApp()
```
A new web-browser window opens with the **MSbrowser** user interface.


# Documentation
Descriptions of all *centWave* peak picking parameters can be found under the **centWave Resources** option in the menu above.
