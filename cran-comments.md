## Test environments
* local Windows 10 (64-bit) install, R version 4.2.1 (2022-06-23 ucrt), RStudio 2022.07.1+554 * win-builder: R Under development (unstable) (2022-09-21 r82893 ucrt)
* win-builder: R version 4.2.1 (2022-06-23 ucrt)
* win-builder: R version 4.1.3 (2022-03-10)
* R-hub: Windows Server 2022, R-devel, 64 bit
* R-hub: Fedora Linux, R-devel, clang, gfortran
* R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC

## R CMD check results
There were no ERRORs or WARNINGs. There was a NOTE for Windows Server 2022 about a 'lastMiKTXException' in a temp directory. I can't find this locally and think it is safe to ignore. There was a NOTE for Fedora about skipping checking HTML validation (no command 'tidy') and math rendering (package 'V8' unavailable). These are unfamiliar to me, and I could not find useful information online. I assume it is safe to ignore. There was a NOTE for R version 4.1.3 about possibly mis-spelled words and possibly invalid URLs. The words are spelled correctly and the URLs are working. 

## Downstream dependencies
There are currently no dependent packages.
