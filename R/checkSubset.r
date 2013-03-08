################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 03: Roxygenized.
# 02: Works with atomic vector.

#' @title check subset
#'
#' @description
#' \code{checkSubset} checks the result of subsetting
#'
#' @details
#' Check if ref and sample names are unique for subsetting.
#' Prints the result to the R-prompt.
#'  
#' @param data a data frame in GeneMapper format containing column 'Sample.Name'.
#' @param ref a data frame in GeneMapper format containing column 'Sample.Name', 
#'  OR an atomic vector e.g. a single sample name string.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' #checkSubset(data=data.frame,ref=ref.data.frame)
#' #checkSubset(data=data.frame,ref="SampleName")

checkSubset <- function(data, ref){

	# Get reference name(s).
	if(is.atomic(ref)){
		ref.names <- ref
	} else {
		ref.names <- unique(ref$Sample.Name)
	}

	# Subset 'data$Sample.Name' using 'ref.name'.
	for(n in seq(along=ref.names)){

		print(c("Reference name:",ref.names[n]))
		print(c("Subsetted samples:"))
		print(grep(ref.names[n],unique(data$Sample.Name), value=TRUE))

	}
  
}