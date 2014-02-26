################################################################################
# TODO LIST
# TODO: option to drop/keep unspecified columns.
# TODO: option to drop/keep 'OL'.

################################################################################
# CHANGE LOG
# 23.01.2014: Fixed bug when only one column in 'fix'.
# 13.01.2014: Completely re-written for improved performance.
# <13.01.2014: Renamed parameters (slim.col -> stack / fix.col -> fix (as earlier)
# <13.01.2014: Fixed returned factor levels. Added as.matrix to return value.
# <13.01.2014: Renamed parameters (slim -> slim.col / fixed -> fix.col
# <13.01.2014: to avoid function/parameter slim to crash. 
# <13.01.2014: Roxygenized.
# <13.01.2014: new parameter 'keepAllFixed' - WORKING for two key kolumns, but not for unslim
# <13.01.2014: new name flattenGM() -> slim(), new parameter names.

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
    print(str(data))
    print("fix")
    print(fix)
    print("stack")
    print(stack)
    print("keepAllFixed")
    print(keepAllFixed)
  }
  
  # Get columns to slim.
  slimCols <- NULL
  for(c in seq(along=stack)){
    slimCols[c] <- list(grepl(stack[c], names(data)))
  }
  
  # Number of columns to slim
  nbSlimCol <- unlist(lapply(slimCols, sum))
  nbCol <- unique(nbSlimCol)
  # Check if equal.
  if(length(nbCol) != 1){
    stop(paste("Columns to stack must have equal number of columns each!",
               paste(paste(stack, nbSlimCol, sep=":"), collapse="\n"), sep="\n"))
  }

  # Get fixed indexes.
  fixedIndex <- vector()
  for(k in seq(along=fix)){
    fixedIndex[k] <- grep(fix[k], names(data))
  }
  
  # Get fixed data.
  fixedData <- as.data.frame(data[,fixedIndex])
  names(fixedData) <- fix
  
  # Make a list of matrixes for columns to stack.
  listStack <- list()
  listRep <- vector()
  for(c in seq(along=slimCols)){
    
    # Get columns for current stack key.
    matrixStack <- data[,slimCols[[c]]]
    
    # Count number of values.
    values <- rowSums(!is.na(matrixStack))
    
    # Transpose and vectorize.
    vectorStack <- c(t(matrixStack))
    
    if(keepAllFixed) {
      
      # Keep one value per fixed row.
      values <- replace(values, values==0, 1)
      
      # Create a boolean vector with values to keep.
      bolVec <- rep(c(TRUE,FALSE), nrow(data))
      bolTimes <- vector()
      bolTimes[seq(from=1, to=length(values)*2, by=2)] <- values
      bolTimes[seq(from=2, to=length(values)*2, by=2)] <- nbCol - values
      keep <- rep(bolVec, times=bolTimes)
      
      # Extract values to keep.
      vectorStack <- vectorStack[keep]
      
    } else {

      # Extract values to keep.
      vectorStack <- vectorStack[!is.na(vectorStack)]
      
    }
    
    # Add values to list.
    listRep[c] <- list(values)
    
    # Transpose and vectorize matrix, and put in list.
    listStack[c] <- list(vectorStack)
    
  }
  
  # Check if all listRep's are equal.
  for(i in seq(along=listRep)){

    # Compare lists.
    if(!identical(listRep[i], listRep[1])){

      # Convert mismatch to vectors.
      testA <- unlist(listRep[1])
      testB <- unlist(listRep[i])
      
      # Find row causing the error.
      for(e in seq(along=testA)){
        
        # Compare elements.
        if(testA[e] != testB[e]){
          
          stop(paste("Different repeat patterns detected for stacked columns!\n",
                     "Caused by: ",
                     paste(paste(names(fixedData), ":", fixedData[e,], sep=""),
                           collapse=", ") ,"\n",
                     "Please fix and try again!"), sep="")

        }
        
      }
      
    }
  }
  
  # Loop over columns in fixed data.
  fixedDataExt <- list()
  for(k in seq(along=fix)){

    # Repeat each 'row' to fit the stacked data.
    fixedDataExt[k] <- list(rep(fixedData[,k], times=unlist(listRep[1])))
    
  }
  
  # Get number of rows.
  numberOfRows <- length(fixedDataExt[[1]])
  # Create a data frame for the result.
  res <- data.frame(matrix(NA, numberOfRows, length(fix) + length(stack)))
  # Add new column names.
  names(res) <- paste(c(fix,stack))

  # Combine fixed and stacked list into one.
  listRes <- append(fixedDataExt, listStack)
  for(i in seq(along=listRes)){
    res[,i] <- as.character(listRes[[i]])
  }  
  
  if(debug){
    print(head(res))
    print(str(res))
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  return(res)
  
}
