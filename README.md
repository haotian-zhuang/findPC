# findPC: An R package to automatically select number of principal components in single-cell analysis
## Overview
findPC is a software tool including six methods to automatically select the number of principal components to retain based on the standard deviations explained by each PC. A major advantage of findPC is that the only information required is a numeric vector of standard deviations explained by each PC.

## findPC Installation
findPC software can be installed via Github. Users should have R installed on their computer before installing findPC. R can be downloaded here: http://www.r-project.org/. To install the latest version of findPC package via Github, run following commands in R:
```
if (!require("devtools"))
install.packages("devtools")
devtools::install_github("haotian-zhuang/findPC")
```
## User Manual
Check the following page for the user manual:
http://htmlpreview.github.io/?https://github.com/haotian-zhuang/findPC/blob/main/vignettes/UserManual.html

## Citation
Please cite the following paper: Zhuang, H., & Ji, Z. (2021). findPC: An R package to automatically select number of principal components in single-cell analysis. bioRxiv.

## Contact the Author
Author: Haotian Zhuang, Zhicheng Ji

Report bugs and provide suggestions by sending email to:

Maintainer: Haotian Zhuang (haotian.zhuang@duke.edu)

Or open a new issue on this Github page
