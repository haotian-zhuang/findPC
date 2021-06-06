# findPC
## Overview
findPC is a software tool including multiple metrics to automatically determine the optimal number of principal components to retain based on the standard deviations explained by each PC. A major advantage of findPC is that the only information required in it is a series of standard deviations explained by each PC.

## findPC Installation
findPC software can be installed via Github. Users should have R installed on their computer before installing findPC. R can be downloaded here: http://www.r-project.org/. To install the latest version of findPC package via Github, run following commands in R:

`if (!require("devtools"))
  
install.packages("devtools")
  
devtools::install_github("haotian-zhuang/findPC")`
