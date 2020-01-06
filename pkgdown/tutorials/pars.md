# Explanation of centWave Parameters


**CentWave parameters:** *ppm, peakwidth, snthresh, prefilter, mzCenterFun, integrate, mzdiff, fitgauss, scanrange, noise.*  


All centWave parameters require fine-tuning for each assay type and mass analyser. Too stringet parameter values (e.g., ppm to low) can result in missing singals or to peak splitting, the latter leads to quantification errors, which has obvious implications for the statistical analysis. Peak detection errors are not obvious further downstream, e.g., when using PCA for quality control assessment.


Explanation of parameters:

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


