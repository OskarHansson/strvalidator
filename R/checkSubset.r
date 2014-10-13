################################################################################
# TODO LIST
# TODO: Add option for exact match.

################################################################################
# CHANGE LOG (last 20 changes)
# 25.07.2013: Added 'debug' option.
# 25.07.2013: Fixed bug option 'word' was not correctly implemented.
# 15.07.2013: Added parameter 'ingoreCase' and 'fixed'.
# <15.07.2013: Added option 'console'.
# <15.07.2013: Roxygenized.
# <15.07.2013: Works with atomic vector.

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
#' @param ignoreCase logical, if TRUE case insesitive matching is used.
#' @param word logical, if TRUE only exact match.
#' @param debug logical indicating printing debug information.
#' 
#' @export
#' 

checkSubset <- function(data, ref, console=TRUE, ignoreCase=TRUE, word=FALSE, debug=FALSE){

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Get reference name(s).
	if(is.atomic(ref)){
		ref.names <- ref
	} else {
		ref.names <- unique(ref$Sample.Name)
	}

  res <- list()
  
  samples <- unique(data$Sample.Name)
	
	# Subset 'data$Sample.Name' using 'ref.name'.
	for(n in seq(along=ref.names)){

    cRef <- ref.names[n]

    # Add word anchor.
    if(word){
      cRef <- paste("\\b", cRef, "\\b", sep="")
    }
    
    if(debug){
      print("cRef")
      print(cRef)
      print("samples")
      print(samples)
      print("ignoreCase")
      print(ignoreCase)
      print("word")
      print(word)
    }
    
    cSamples <- grep(cRef, samples,
                     value = TRUE, fixed = FALSE, ignore.case = ignoreCase)
    
    res[n] <- paste("Reference name: ", ref.names[n], "\n",
                    "Subsetted samples: ", paste(cSamples, collapse=", "), "\n\n", sep="")
    
    if(debug){
      print("cSamples")
      print(cSamples)
    }
	}
  
  if(console){
    cat(unlist(res))
  } else {
    return(unlist(res))
  }
  
}
