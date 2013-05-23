################################################################################
# TODO LIST
# TODO: sort - sort alleles after size (and rename allele columns).
# TODO: unique - keep only unique alleles.

################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 01: First version.

#' @title Combine reference samples
#'
#' @description
#' \code{combineRef} Combines two reference samples into one.
#'
#' @details
#' Combines two data frames with reference samples into one data frame. 
#'  
#' @param ref1 a data frame containing reference sample 1.
#' @param ref2 a data frame containing reference sample 2.
#' @param sort logical for sorting. Not implemented.
#' @param unique logical for keeping only unique alleles. Not implemented.
#' 
#' @return data.frame with columns 'Min', 'Max', 'Mean', n', and
#' 'Sd' for each target column.
#' 


combineRef <- function(ref1, ref2, sort=FALSE, unique=FALSE){

	# Get sample names.
	s1 <- ref1$Sample.Name[1]
	s2 <- ref2$Sample.Name[1]

	# Merge data.
	res <- merge(ref1, ref2,
			by="Marker", sort=FALSE,
			all=TRUE,
			suffixes = c(paste(".", s1, sep=""), 
					paste(".", s2, sep="")))

	# Remove old sample name columns.
	res <- res[ , !grepl("Sample.Name",names(res))]

	# Create sample name.
	res$Sample.Name <- paste(s1, ".", s2, sep="")

	# Make 'Sample.Name' column 1.
	res <- data.frame("Sample.Name"=res[ , length(res)], 
		res[ , 1:length(res)-1]) 

	return (res)

}
