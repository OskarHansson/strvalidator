## Test environments
* local Windows 10 (64-bit) install, R version 4.3.0 (2023-04-21 ucrt), RStudio RStudio 2023.06.0+421
* win-builder: R Under development (unstable) (2023-07-09 r84667 ucrt)
* win-builder: R version 4.3.1 (2023-06-16 ucrt)
* win-builder: R version 4.2.3 (2023-03-15 ucrt)
* R-hub: Windows Server 2022, R-devel, 64 bit
* R-hub: Fedora Linux, R-devel, clang, gfortran
* R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC

## R CMD check results
There were no ERRORs or WARNINGs. There was a NOTE for Windows Server 2022 about a 'lastMiKTXException' in a temp directory. I can't find this locally and think it is safe to ignore. There was a NOTE for Fedora and Ubuntu about skipping checking HTML validation (no command 'tidy') and math rendering (package 'V8' unavailable). These are unfamiliar to me, and I could not find useful information online. I assume it is safe to ignore. There was a NOTE for R version 4.2.3 about possibly mis-spelled words. The words are spelled correctly. 

## Downstream dependencies
There are currently no dependent packages.
