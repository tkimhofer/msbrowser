# Strategy centWave
![](www/MSbrowser_logo_tricolour_alpha.png)
Many factors influence the result of a peak detection routine, these are instrument and assay specific (e.g., mass analyser accuracy, number of scans per second on MS 1 level), as well as the noise structure and mass analyser error rate.

The current strategy for untargeted metabolic profiling experiements is to use less stringent parameter sets for centWave peak picking, in order to pick all signals (even low intensity ones). To remove false positives (no real signals), we apply stringent filtering procedures that include i) 



| Parameter | Value | Effect |
|-----------|-------------|--------|
|ppm| 30 | |
|peakwidht | 1-20 | |
| snthresh | 0| |
|pre-filter| n=3, I=100 | This is stongly instrument and assay specific! |
|mzCenterFun|  | Maximum rather than parametric model |
|integrate|1 | Integrate peak area and not parametric model |
| mzdiff | 0.2 | close but no peak overlap in m/z dimension tolerated |
|fitgauss| FALSE | No peak fitting |
|scanrange| [entire scan range] | |
|noise  | 100 | This is stongly instrument and assay specific! |


