################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 28.04.2014: More robust and handles '+' and '-' in sample names.
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

  # Variables.
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

  # Ignore case. NB! Must be before add word boundary.
  if(ignoreCase){

    # Convert to upper case.
    samples <- toupper(samples)
    columns <- toupper(columns)
    
    if(length(samples) == 0){
      samples <- NULL
    }
    
    if(length(columns) == 0){
      columns <- NULL
    }
    
    if(debug){
      print("After ignoreCase.")
      print("samples:")
      print(samples)
      print("columns:")
      print(columns)
    }
    
  }

  # Add word boundary. NB! Must be after ignore case.
  if(word){
    
    if(!is.null(samples)){
      samples <- gsub("|", "\\b|\\b", samples, fixed=TRUE)
      samples <- paste("\\b", samples, "\\b", sep="")
    }
    if(!is.null(columns)){
      columns <- gsub("|", "\\b|\\b", columns, fixed=TRUE)
      columns <- paste("\\b", columns, "\\b", sep="")
    }

    if(debug){
      print("After adding word boundary.")
      print("samples:")
      print(samples)
      print("columns:")
      print(columns)
    }
    
  }
  
  # Check for and escape '+' and '-' in sample names.
  if(any(grepl("+", samples, fixed=TRUE))){
    samples <- gsub("+", "\\+", samples, fixed=TRUE)
    message("'+' in sample names escaped")
  }
  if(any(grepl("-", samples, fixed=TRUE))){
    samples <- gsub("-", "\\-", samples, fixed=TRUE)
    message("'-' in sample names escaped")
  }
  
  # Grab samples --------------------------------------------------------------
  
  # Check if column 'Sample.Name' exist.
  if("Sample.Name" %in% names(data)){
    
    # Grab rows.
    if(ignoreCase){
      sampleNames <- toupper(as.character(data$Sample.Name))
    } else {
      sampleNames <- as.character(data$Sample.Name)
    }

    if(debug){
      print("Pattern for samples")
      print(head(samples))
      print("String")
      print(head(sampleNames))
    }
    
    if(is.null(samples)){
      
      # Default is to keep all samples.
      rows <- rep(TRUE, length(sampleNames))
      
    } else {
      
      # Get matching rows.
      rows <- grepl(samples, sampleNames, fixed=FALSE)
      
      # Invert selection of samples.
      if(invertS){
        rows <- !rows
      }
      
    }
    
  } else {
    
    # Keep all rows.
    rows <- rep(TRUE, nrow(data))
    
  }
  
  if(debug){
    print(paste("Grab samples:", paste(sampleNames[rows], collapse=",")))
  }
  
  # Grab columns --------------------------------------------------------------
  
  if(ignoreCase){
    columnNames <- toupper(names(data))
  } else {
    columnNames <- names(data)
  }

  if(debug){
    print("Pattern for columns")
    print(head(columns))
    print("String")
    print(head(columnNames))
  }
  
  
	if(is.null(columns)){
    
		# Default is to keep all columns.
		columns <- rep(TRUE, length(columnNames))
    
	} else {
    
    # Get matching columns.
		columns <- grepl(columns, columnNames, fixed=FALSE)
    
		# Invert selection of columns.
		if(invertC){
		  columns <- !columns
		}
		
	}

  if(debug){
    print(columns)
    print(paste("Grab columns:", paste(names(data)[columns], collapse=",")))
  }
  
	# Trim data.
	data <- data[rows, columns]

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
