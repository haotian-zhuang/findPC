# FindPC: An R package to automatically select number of principal components in single-cell analysis
## Overview
FindPC is a software tool including six methods to automatically select the number of principal components to retain based on the standard deviations explained by each PC. A major advantage of FindPC is that the only information required is a numeric vector of standard deviations explained by each PC.

## FindPC Installation
FindPC software can be installed via Github. Users should have R installed on their computer before installing FindPC. R can be downloaded here: http://www.r-project.org/. To install the latest version of FindPC package via Github, run following commands in R:
```
if (!require("devtools"))
install.packages("devtools")
devtools::install_github("haotian-zhuang/FindPC")
```
## User Manual
Check the following page for PDF version of the user manual:
https://github.com/haotian-zhuang/findPC/blob/main/findPC-manual.pdf

## Citation
Please cite the following paper: Zhuang, H., & Ji, Z. (2021). findPC: An R package to automatically select number of principal components in single-cell analysis. bioRxiv.

## Contact the Author
Author: Haotian Zhuang, Zhicheng Ji

Report bugs and provide suggestions by sending email to:

Maintainer: Haotian Zhuang (haotian.zhuang@duke.edu)

Or open a new issue on this Github page
