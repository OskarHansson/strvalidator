## Resubmission
This is a resubmission. In this version I have:

* Removed the 'revdep' folder that was created by running 'revdep_check' after the CRAN check.
* The package passed a new CRAN check.

* There is an ERROR on the CRAN Package Check Results page for flavour 'r-release-osx-x86_64-mavericks'.
I don't know how to fix this and have no access to an OSX system.

## Test environments
* local Windows 7 (64-bit) install, R 3.3.1, RStudio 0.99.902
* win-builder: devel 2016-06-30 r70858

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

Maintainer: 'Oskar Hansson <oskar.hansson@fhi.no>'

* Possibly mis-spelled words in DESCRIPTION:
  ENFSI (20:68)
  STR (4:60, 18:13)
  SWGDAM (22:6)

Response: Abbreviations are spelled out.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of strvalidator:
There are currently only one dependent package. It passed the check.