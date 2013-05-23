###############################################################################
#' Internal validation of forensic STR kits made easy with strValidator.
#'
#' strValidator provide tools that make it easier to perform an internal
#' validation of a forensic short tandem repeat (STR) kit for human 
#' identification. The tools are developed to provide all necessary data
#' to conform with the ENFSI guidelines in
#' "Recommended Minimum Criteria for the Validation of Various 
#' Aspects of the DNA Profiling Process - New Multiplex Kit"
#' More information about each function can be found in 
#' its help documentation. The package is still in an early developmental
#' stage and things will likely change in coming versions. 
#' Manually check the results thoroughly, the software has not been validated.
#' Please report bugs to: https://github.com/OskarHansson/strvalidator/issues  
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

#' ESX17 example data
#' 
#' A slimmed reference dataset containing an arbitrary ESX17 DNA profile.
#' 
#' @docType data
#' @keywords datasets
#' @name ref2
#' @usage data(ref2)
#' @format A data frame with 16 rows and 3 variables
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

#' ESX17 example data
#' 
#' A slimmed dataset containing ESX17 genotyping result for 2 replicates
#' of 'sampleA'.
#' 
#' @docType data
#' @keywords datasets
#' @name set2
#' @usage data(set2)
#' @format A data frame with 32 rows and 5 variables
NULL
