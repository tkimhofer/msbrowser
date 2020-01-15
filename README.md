![](imgs/logo.png)

# Welcome!

[![Build Status](https://travis-ci.org/rstudio/rmarkdown.svg?branch=master)](https://travis-ci.org/rstudio/rmarkdown)
[![HitCount](http://hits.dwyl.io/tkimhofer/msbrowser.svg)](http://hits.dwyl.io/tkimhofer/msbrowser)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


**MSbrowser is web-browser-based application for parameter fine-tuning of xcms peak picking algorithms**. 


The app is designed for fast and easy parameter testing to optimise xcms peak picking performance. Different visualisations allow to gain insight into the LC-MS data structure and the effects different peak picking parameter values have on peak picking performance. 

MSbrowser is implemented in a user-friendly web-application framework that can be installed as an simple R package.

![](imgs/GUI.png)


# App installation and launch

The MSbrowser package is hosted on [GitHub](https://github.com/) and is build with [xcms](https://bioconductor.org/packages/release/bioc/html/xcms.html) version 3.6 (or higher) to perform peak picking. The following R code can be used to install necessary R dependencies on your computer.

```R
# install dependencies from CRAN (devtools and BiocManager)
deps=c('devtools', 'BiocManager')
id=deps %in% installed.packages()[,1]
if(any(!id)) install.packages(deps[id])

# install xcms
if(!requireNamespace("xcms", versionCheck=list(op = ">=", version = "3.6"), quietly = T)){BiocManager::install('xcms')}
```

Now you're ready to go for the installation of MSbrowser:
```
# install MSbroswer
devtools::install_github('tkimhofer/msbrowser')
```
If prompted by the command line, perform necessary package updates.

MSbrowser can be launched with the following R-terminal commands:

```R
msbrowser::startApp()
```
A new web-browser window opens with the **MSbrowser** user interface.



# Documentation 
MSbrowser has an intuitive workflow, with help text placed in the user interface. A descriptions of all *centWave* peak picking parameters can be found under the **centWave Resources** option in the page menu above. All open data file formats are accepted. File conversion from vendor format can be performed with [ProteoWizard](http://proteowizard.sourceforge.net/).

# Example Data
The application comes with HILIC-MS example data acquired using a urine sample, so that one can familiarise oneself with MSbrowser's functionalities before using own data.


# Feedback
Got questions or suggestions? Log an [issue on GitHub](https://github.com/tkimhofer/msbrowser/issues) (requires login) or [drop me an email](mailto:torben.kimhofer@murdoch.edu.au?subject=[MSbrwoser]%20Source%20Han%20Sans)!
