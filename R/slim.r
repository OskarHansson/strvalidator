################################################################################
# TODO LIST
# TODO: option to drop/keep unspecified columns.

################################################################################
# CHANGE LOG
# 06: Renamed parameters (slim.col -> stack / fix.col -> fix (as earlier)
# 05: Fixed returned factor levels. Added as.matrix to return value.
# 05: Renamed parameters (slim -> slim.col / fixed -> fix.col
# 05: to avoid function/parameter slim to crash. 
# 04: Roxygenized.
# 03: new parameter 'keepAllFixed' - WORKING for two key kolumns, but not for unslim
# 02: new name flattenGM() -> slim(), new parameter names.

#' @title Slim data frames
#'
#' @description
#' \code{slim} slims data frames with GeneMapper data.
#'
#' @details
#' \code{slim} repeated columns into single columns, also see \code{unslim()}.
#' For example, the following data frame:
#'  Sample.Name|Marker|Allele.1|Allele.2|Size.1|Size.2|Data.Point..
#' using this command:
#'  slim(data, fix=c("Sample.Name","Marker"), stack=c("Allele","Size"))
#' would result in this data frame (NB! 'Data.Point' is lost):
#'  Sample.Name|Marker|Allele|Size
#' 
#' @param data data frame.
#' @param fix vector of strings with colum names to keep fixed.
#' @param stack vector of strings with colum names to slim.
#' @param keepAllFixed logical, keep a rows even if no data.
#' @param debug logical indicating printing debug information.
#' 
#' @return list with simulation results.
#' 


slim <- function(data, fix=NULL, stack=NULL, 
                 keepAllFixed=TRUE, debug=FALSE){

  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("data")
    print(names(data))
    print("fix")
    print(fix)
    print("stack")
    print(stack)
    print("keepAllFixed")
    print(keepAllFixed)
  }
  
  # Initiate result row counter.
  resRow <- 0
  
  # Create a data frame for the result.
  N <- 500  # Number of rows to pre-allocate.
  res <- data.frame(matrix(NA, N, length(fix) + length(stack)))
  names(res) <- paste(c(fix,stack))  # Add new column names.
  
  # Get columns to slim.
  slimCols <- NULL
  for(c in seq(along=stack)){
    slimCols[c] <- list(grepl(stack[c], names(data)))
  }
  
  # Get fixed indexes.
  fixedIndex <- vector()
  for(k in seq(along=fix)){
    fixedIndex[k] <- grep(fix[k], names(data))
  }

  # Loop over all rows
  for(r in 1:nrow(data)){
    
    # Get fixed.
    fixedVal <- NULL
    for(k in seq(along=fix)){
      fixedVal[k] <- as.matrix(data[r,fixedIndex[k]])
    }
    
    # Get values to slim.
    value <- NULL
    for(c in seq(along=slimCols)){
      value[c] <- list(as.matrix(data[r, slimCols[[c]]]))
      # Replace empty values with NA.
      value[[c]][value[[c]]==""]<-NA
      # Grab values (exclude NA).
      value[[c]] <- value[[c]][!is.na(value[[c]])]
    }
    
    # Put in an NA for missing data.
    if(keepAllFixed){
      for(v in seq(along=value)){
        if(length(value[[v]])==0){
          value[[v]] <- NA
        }
      }
    }
    
    # Loop over all alleles.
    for(a in seq(along=value[[1]])){
      
      # Increase result row counter.
      resRow <- resRow + 1
      
      # Check if more rows are needed.
      if(resRow > nrow(res)){
        tmp <- data.frame(matrix(NA, N, length(fix) + length(stack)))
        names(tmp) <- paste(c(fix, stack))  # Add correct names.
        res <- rbind(res, tmp)  # Add to result data frame.
      }
      
      # Pull values from list.
      aValue <- NULL
      for(i in seq(along=value)){
        aValue <- c(aValue, value[[i]][a])
      }

      # Save in data frame.
      res[resRow, ] <- c(fixedVal, aValue)
    }
    
  }
  
  # Remove empty rows and return data frame.
  res <- res[!is.na(res[1]), ]
  return(res)
  
}
