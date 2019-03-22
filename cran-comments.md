## Test environments
* local Windows 10 (64-bit) install, R 3.5.3, RStudio 1.1.463
* win-builder: R Under development (unstable) (2019-03-19 r76252)
* win-builder: R version 3.5.3 (2019-03-11)
* win-builder: R version 3.4.4 (2018-03-15)
* R-hub (Windows Server 2008, Ubuntu Linux 16.04, Fedora Linux)

## R CMD check results
There were no ERRORs or WARNINGs.
There was one NOTE under R version 3.4.4 about possible mis-spelled words in DESCRIPTION - These are correct.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of strvalidator:
There are currently only one dependent package.
pcrsim version 1.0.2 passed the check.