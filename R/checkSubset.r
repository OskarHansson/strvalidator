################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 04: Added option 'console'.
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
#' @param console logical, if TRUE result is printed to R console,
#' if FALSE a string is returned. 
#' 

checkSubset <- function(data, ref, console=TRUE){

	# Get reference name(s).
	if(is.atomic(ref)){
		ref.names <- ref
	} else {
		ref.names <- unique(ref$Sample.Name)
	}

  res <- list()
	
	# Subset 'data$Sample.Name' using 'ref.name'.
	for(n in seq(along=ref.names)){

    cRef <- ref.names[n]
		cSamples <- grep(ref.names[n],unique(data$Sample.Name), value=TRUE)
    res[n] <- paste("Reference name: ", cRef, "\n",
                    "Subsetted samples: ", paste(cSamples, collapse=", "), "\n\n", sep="")
    
    

	}
  
  if(console){
    cat(unlist(res))
  } else {
    return(unlist(res))
  }
  
}
