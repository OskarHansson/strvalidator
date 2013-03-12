###############################################################################
#' Internal validation of forensic STR kits made easy with strValidator.
#'
#' strValidator provide tools that make it easier to perform an internal
#' validation of a forensic short tandem repeat (STR) kit for human 
#' identification. The tools are developed to provide all necessary data
#' to conform with the guidelines in REF... 
#' More information about each function can be found in 
#' its help documentation.
#'
#' \tabular{ll}{
#' Package: \tab strValidator\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2013-02-06\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @title Simplifies internal validation of forensic STR typing kits
#' @docType package
#' @name strValidator-package
#' @aliases strValidator
#' @author Oskar Hansson \email{oskar.hansson@@fhi.no}
#' @import ggplot2, data.table
#' @section Warning: This package is experimental and has not been thoroughly validated.
#' @keywords package
NULL

#' ESX17 Positive Control Profile
#' 
#' A dataset in 'GeneMaper' format containing the DNA profile of
#' the ESX17 positive control sample with homozygotes as one entry.
#' 
#' @docType data
#' @keywords datasets
#' @name ref1
#' @usage data(ref1)
#' @format A data frame with 17 rows and 4 variables
NULL

#' ESX17 Positive Control Profile
#' 
#' A dataset in 'GeneMaper' format containing the DNA profile of
#' the ESX17 positive control sample with homozygotes as two entries.
#' 
#' @docType data
#' @keywords datasets
#' @name ref11
#' @usage data(ref11)
#' @format A data frame with 17 rows and 4 variables
NULL


#' Typing data in 'GeneMapper' format
#' 
#' A dataset containing ESX17 genotyping result for 8 replicates
#' of the positive control sample, a negative control and ladder.
#' 
#' @docType data
#' @keywords datasets
#' @name set1
#' @usage data(set1)
#' @format A data frame with 170 rows and 13 variables
NULL