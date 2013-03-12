################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 05: Roxygenized.
# 04: new name 'trimData' -> 'trim'
# 03: remove NA/empty cols.
# 02: handle no sample/ no columns.

#' @title Trim data
#'
#' @description
#' \code{trim} Extracts data from a larger data set.
#'
#' @details
#' Simplifies extraction of specific data from a larger set of typing data.
#' 
#' @param data data frame with genotype data in 'slim' format.
#' @param samples string giving sample names separated by pipe.
#' @param columns string giving column names separated by pipe.
#' @param word logical indicating if a word boundary should be added to 
#'  \code{samples} and \code{columns}.
#' @param ignoreCase logical, TRUE ignore case in sample names.
#' @param invertS logical, FALSE samples given will be removed from 'data'
#' while TRUE will remove samples NOT given.
#' @param invertC logical, FALSE columns given will be removed from 'data'
#' while TRUE will remove columns NOT given.
#' @param rmNaCol logical, TRUE columns with only NA are removed from 'data'
#' while FALSE will preserve the columns.
#' @param rmEmptyCol logical, TRUE columns with no values are removed from 'data'
#' while FALSE will preserve the columns.
#' @param missing value to replace missing values with.
#' 
#' @return data.frame with extracted result.
#' 


trim <- function(data, samples=NULL, columns=NULL, 
	word=FALSE, ignoreCase=TRUE, invertS=FALSE, invertC=FALSE,
	rmNaCol=TRUE, rmEmptyCol=TRUE, missing=NA){

	# Add word anchor.
	if(word){
		samples <- gsub("|", "\\b|\\b", samples, fixed=TRUE)
		samples <- paste("\\b", samples, "\\b", sep="")
		columns <- gsub("|", "\\b|\\b", columns, fixed=TRUE)
		columns <- paste("\\b", columns, "\\b", sep="")
	}

	# Grab rows.
	sampleNames <- data$Sample.Name
	if(is.null(samples)){
		# Default is to keep all samples.
		rows <- rep(TRUE, length(sampleNames))
	} else {
		rows <- grepl(samples, sampleNames, ignore.case=ignoreCase)
	}

	# Grab columns.
	columnNames <- names(data)
	if(is.null(columns)){
		# Default is to keep all columns.
		columns <- rep(TRUE, length(columnNames))
	} else {
		columns <- grepl(columns, columnNames, ignore.case=ignoreCase)
	}

	# Invert selection of samples.
	if(invertS){
		rows <- !rows
	}
	# Invert selection of columns.
	if(invertC){
		columns <- !columns
	}

	# Trim data.
	data <- data[rows,columns]

	if(!is.null(missing)){
		data[data==""] <- missing
	}

	if(rmEmptyCol){
		data <- data[ , colSums(data=="") != nrow(data) | colSums(is.na(data))>0]
	}

	if(rmNaCol){
		data <- data[,colSums(is.na(data))<nrow(data)]
	}

	return(data)
}