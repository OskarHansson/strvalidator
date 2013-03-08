################################################################################
# CHANGE LOG
# 06: Roxygenized.
# 05: Corrected missing variable in calculateZvalue, ref --> data.

################################################################################
# TODO: Test if parameter homoCols is useful or not.
# Better to remove NA's from: alleles <- length(unique(as.character(ref[r,col.a])))
# If use ref names output will be ref names and not sample name!
# NB! TODO (in addZvalue): This adds number of peaks, NOT z-value?? 

#' @title Adds all kit information
#'
#' @description
#' \code{calculateZvalue} Indicates heterozygosity (1) or homozygosity (2) 
#' for reference data.
#'
#' @details
#' 'data' contains short tandem repeat (STR) marker/locus names in the required
#'   column. Based on the provided kit name other information is attached to
#'   'data' in new columns.
#'   
#' @param data Data frame containing at least a column named 'Marker'.
#' @param homoCols '1' if homozygotes are given as a single allele name (e.g. 16)
#' '2' if homozygotes are given as a double allele names (e.g. 16/16)
#' 
#' @return data.frame the original data frame containing additional columns.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' print("Example will come later")


calculateZvalue <- function(data, homoCols=2){

	data$Z<-NA

	if(homoCols==1) {
		col.a <- grepl("Allele", names(data))
		for(r in 1:nrow(data)){
			data$Z[r] <- sum(!is.na(data[r,col.a]))

		}
	}
	if(homoCols==2) {
		col.a <- grepl("Allele", names(data))
		for(r in 1:nrow(data)){
			alleles <- length(unique(as.character(data[r,col.a])))
			data$Z[r] <- if(alleles==1){2} else {1}

		}
	}
	return(data)
}