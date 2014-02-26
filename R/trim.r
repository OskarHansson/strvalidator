################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 14.01.2014: Support dataframes without a 'Sample.Name' column.
# 27.10.2013: Fixed bug when 'samples'=NULL and 'invertS'=TRUE.
# 27.04.2013: Fixed error when result is only 1 column.
# <27.04.2013: Roxygenized.
# <27.04.2013: new name 'trimData' -> 'trim'
# <27.04.2013: remove NA/empty cols.
# <27.04.2013: handle no sample/ no columns.

#' @title Trim data
#'
#' @description
#' \code{trim} Extracts data from a larger data set.
#'
#' @details
#' Simplifies extraction of specific data from a larger set of typing data.
#' 
#' @param data data frame with genotype data.
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
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with extracted result.
#' 


trim <- function(data, samples=NULL, columns=NULL, 
	word=FALSE, ignoreCase=TRUE, invertS=FALSE, invertC=FALSE,
	rmNaCol=TRUE, rmEmptyCol=TRUE, missing=NA, debug=FALSE){

  colNames <- columns
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("data:")
    print(head(data))
    print("samples:")
    print(samples)
    print("columns:")
    print(columns)
    print("word:")
    print(word)
    print("ignoreCase:")
    print(ignoreCase)
    print("invertS:")
    print(invertS)
    print("invertC:")
    print(invertC)
    print("rmNACol:")
    print(rmNaCol)
    print("rmEmptyCol:")
    print(rmEmptyCol)
    print("missing:")
    print(missing)
  }
  
	# Add word anchor.
	if(word){
		samples <- gsub("|", "\\b|\\b", samples, fixed=TRUE)
		samples <- paste("\\b", samples, "\\b", sep="")
		columns <- gsub("|", "\\b|\\b", columns, fixed=TRUE)
		columns <- paste("\\b", columns, "\\b", sep="")
    
		if(debug){
      print("After adding word anchor.")
		  print("samples:")
		  print(samples)
		  print("columns:")
		  print(columns)
		}
		
	}

  # Check if column 'Sample.Name' exist.
  if("Sample.Name" %in% names(data)){
    
    # Grab rows.
    sampleNames <- data$Sample.Name
    if(is.null(samples)){
      
      # Default is to keep all samples.
      rows <- rep(TRUE, length(sampleNames))
      
    } else {
      
      # Get matching rows.
      rows <- grepl(samples, sampleNames, ignore.case=ignoreCase)
      
      # Invert selection of samples.
      if(invertS){
        rows <- !rows
      }
      
    }
    
  } else {
    
    # Keep all rows.
    rows <- rep(TRUE, nrow(data))
    
  }

	# Grab columns.
	columnNames <- names(data)
	if(is.null(columns)){
    
		# Default is to keep all columns.
		columns <- rep(TRUE, length(columnNames))
    
	} else {
    
    # Get matching columns.
		columns <- grepl(columns, columnNames, ignore.case=ignoreCase)
    
		# Invert selection of columns.
		if(invertC){
		  columns <- !columns
		}
		
	}

  if(debug){
    print(paste("Grab columns:", paste(names(data)[columns], collapse=",")))
  }
  
	# Trim data.
	data <- data[rows,columns]

	if(!is.null(missing)){
		data[data==""] <- missing
	}

  if(rmEmptyCol){
    if(!is.null(ncol(data))){
		  data <- data[ , colSums(data=="") != nrow(data) | colSums(is.na(data))>0]
    }
	}

	if(rmNaCol){
	  if(!is.null(ncol(data))){
	    data <- data[,colSums(is.na(data))<nrow(data)]
	  }
	}

  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }

  if(is.null(ncol(data))){
    data <- data.frame(Name=data)
    names(data) <- colNames
  }
  
    return(data)
}
