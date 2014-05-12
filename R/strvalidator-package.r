###############################################################################
# Internal validation of forensic STR kits made easy with strValidator.
#
# STR validator is a package intended for validation of
# forensic short tandem repeat (STR)  DNA typing kit.
# This graphical user interface make it very easy to
# analyse data from internal validations.
# The code has been extensively tested in order to assure correct results.
# However, some bugs might still persist, so check the result carefully.
# 
# Created by:
# Oskar Hansson, Department of Forensic Biology (NIPH, Norway)
# 
# General information and tutorials:
# "https://sites.google.com/site/forensicapps/strvalidator
# 
# Please report bugs to:
# https://github.com/OskarHansson/strvalidator/issues
# 
# The source is hosted at GitHub:
# https://github.com/OskarHansson/strvalidator
#
#' @title Simplifies internal validation of forensic STR typing kits
#' @docType package
#' @name strValidator-package
#' @aliases strValidator
#' @author Oskar Hansson \email{oskar.hansson@@fhi.no}
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

#' SGMPlus example data
#' 
#' A slimmed reference dataset containing an arbitrary SGMPlus DNA profile.
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

#' SGMPlus example data
#' 
#' A slimmed dataset containing SGM Plus genotyping result for 2 replicates
#' of 'sampleA'.
#' 
#' @docType data
#' @keywords datasets
#' @name set2
#' @usage data(set2)
#' @format A data frame with 32 rows and 5 variables
NULL

#' ESX17 example data for dropout analysis.
#' 
#' Data from dilution experiment for dropout analysis.
#' Text file with exported GeneMapper genotypes table.
#' 
#' @docType data
#' @keywords datasets
#' @name set3
#' @method set3 <- import(fileName=system.file("extdata", "set3.txt", package = "strvalidator"))
#' @format ASCII text file
NULL

#' ESX17 example data for dropout analysis.
#' 
#' Reference profiles for source samples.
#' Text file in GeneMapper format.
#' 
#' @docType data
#' @keywords datasets
#' @name ref3
#' @method ref3 <- import(fileName=system.file("extdata", "ref3.txt", package = "strvalidator"))
#' @format ASCII text file
NULL

#' ESX17 example data for dropout analysis.
#' 
#' A slimmed dataset containing data from
#' dilution experiment for dropout analysis (from set3).
#' One sample replicate has lower case sample name (bc9).
#' 
#' @docType data
#' @keywords datasets
#' @name set4
#' @usage data(set4)
#' @format A data frame with 1609 rows and 5 variables
NULL

#' ESX17 example data for dropout analysis.
#' 
#' A slimmed dataset containing reference profiles for source samples in set4.
#' Reference 'A2' has douoble entries for homozygotes.
#' Reference 'F2' has single entries for homozygotes.
#' Reference 'bc' has douoble entries for homozygotes, and lower case sample name. 
#' 
#' @docType data
#' @keywords datasets
#' @name ref4
#' @usage data(ref4)
#' @format A data frame with 98 rows and 3 variables
NULL
