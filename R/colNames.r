################################################################################
# TODO LIST
# TODO: Make base name defining string optional instead of always "."

################################################################################
# CHANGE LOG
# 15.12.2013: Fixed cropping column names with multiple periods.
# 25.05.2013: Fixed returning original column names for single occuring names.
# 24.05.2013: First version.

#' @title colNames
#'
#' @description
#' \code{colNames} is an internal helper function.
#'
#' @details
#' \code{colNames} takes a data frame as input and return either column names
#' occuring once or multiple times. Matching is done by the 'base name'
#' (the substring to the left of the last period, if any). The return type
#' is a string vector by default, or a single string of colum names separated
#' by a string 'concatenate' (see 'collapse' in \code{paste} for details).
#' 
#' @param df data.frame.
#' @param slim logical, TRUE returns column names occuring once,
#' FALSE returns column names occuring multiple times.
#' @param concatenate string, if not NULL returns a single string with column
#' names concatenated by the provided string instead of a vector.
#' @param debug logical indicating printing debug information.
#' 
#' @return character, vector or string.
#' 

colNames <- function(df, slim=TRUE, concatenate=NULL, debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("df")
    print(str(df))
    print("slim")
    print(slim)
    print("concatenate")
    print(concatenate)
  }
  
  ret <- NA
  colNames <- names(df)
  
  if(debug){
    print("colNames")
    print(colNames)
  }
  
  if(!is.null(colNames)){
  
    # Guess base names i.e. "Allele.1" -> "Allele"
    # To the first dot.
    #baseNames <- unique(gsub("(\\w*)\\..*", "\\1", colNames))
    # To the last dot.
    baseNames <- unique(gsub("(.*)\\.(.*)", "\\1", colNames))
    
    if(debug){
      print("baseNames")
      print(baseNames)
    }
    
    matches<-vector()
    for(v in seq(along=baseNames)){
      
      # Find number of occurences.
      matches[v] <-(length(grep(pattern=baseNames[v], x=colNames, value = FALSE, fixed = FALSE)))
      
    }
    
    if(slim){
      # Return column names matching 'base name' that occured once.
      
      # Get 'base name' that occured once.
      singleNames <- baseNames[matches == 1]
      matchingNames <- vector()

      # Get original column names matching 'base name'.
      for(s in seq(along=singleNames)){
        matchingNames[s] <- grep(pattern=singleNames[s], x=colNames, value = TRUE, fixed = FALSE)
      }
      
      if(is.null(concatenate)){
        # Return as vector.
        ret <- matchingNames
      } else {
        # Return concatenated as single string.
        ret <- paste(matchingNames, collapse=concatenate)
      }
      
    } else {
      # Return column names matching 'base name' that occured multiple times.
      if(is.null(concatenate)){
        # Return as vector.
        ret <- baseNames[matches > 1]
      } else {
        # Return concatenated as single string.
        ret <- paste(baseNames[matches > 1], collapse=concatenate)
      }
    }
  }
  
  if(length(ret) == 0){
    ret <- NA
  }
    
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  return(ret)
  
}
