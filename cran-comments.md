## Test environments
* local Windows 10 (64-bit) install, R 4.0.2, RStudio 1.3.959
* win-builder: R Under development (unstable) (2020-07-08 r78794)
* win-builder: R version 4.0.2 (2020-06-22)
* win-builder: R version 3.6.3 (2020-02-29)
* R-hub (Windows Server 2008 R2 SP1, Ubuntu Linux 16.04 LTS, Fedora Linux)

## R CMD check results
There was an ERROR under R-hub Windows Server 2008 R2 SP1 "Package required but not available: 'data.table'" - 'data.table' is in imports, so not sure why it fails.
There were no other ERRORs or WARNINGs.

## Downstream dependencies
There are currently no dependent packages.
