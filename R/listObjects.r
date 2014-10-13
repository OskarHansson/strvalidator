################################################################################
# TODO LIST
# TODO: Option to return other info e.g. size. Return dataframe instead of vector.
#       Workaround: unname(sapply(listObjects(), function(x) object.size(get(x, envir = baseenv()))))

################################################################################
# CHANGE LOG (last 20 changes)
# 26.07.2013: 'objClass' can now be a vector.
# 17.05.2013: New parameters 'objClass', 'debug'.
# 17.05.2013: Made general. Changed name from listDataFrames -> listObjects.
# <17.05.2013: First version.

#' @title List objects
#'
#' @description
#' \code{listObjects} returns a list of objects.
#'
#' @details
#' Internal helper function to retrieve a list of objects from a workspace.
#' Take an environment as argument and optionally an object class.
#' Returns a list of objects of the specified class in the environment.
#' 
#' @param env environment in wich to search for objects.
#' @param objClass character string or vector specifying the object class.
#' @param debug logical indicating printing debug information.
#' 
#' @return character vector with the object names.
#' 
#' @examples
#' \dontrun{
#' # List data frames in the workspace.
#' listObjects(objClass="data.frame")
#' # List functions in the workspace.
#' listObjects(objClass="function")
#' }

listObjects <- function(env=parent.frame(), objClass=NULL, debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  # Result vector.
  res <- character()
  
  # List objects in environment.
  wsObj <- ls(env)

  if(debug){
    print("Objects:")
    print(wsObj)
  }
  
  # Check if specified object class.
  if(!is.null(objClass)){

    classes <- list()

    # Loop to save all class information.
    for(i in seq(along=wsObj)){
      obj <- get(wsObj[i], envir=env)
      classes[i] <- list(class(obj))
      
    }
    
    # Filter objects with specified classes.
    for(c in seq(along=objClass)){
      for(i in seq(along=classes)){
        if(objClass[c] %in% classes[[i]]){
          res <- c(res, wsObj[i])
        }
      }
    }
    
  } else {

    # Return all objects.
    res <- wsObj
    
  }

  if(debug){
    print("Returned objects:")
    print(res)
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  return(res)
  
}
