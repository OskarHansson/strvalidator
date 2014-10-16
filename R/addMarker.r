################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 15.01.2014: Added message to show progress.
# 30.11.2013: Specified package for function in 'plyr' -> 'plyr::rbind.fill'
# 27.11.2013: Fixed uppercase marker names when ignoreCase=TRUE.
# 10.11.2013: First version.

#' @title Add missing markers.
#'
#' @description
#' \code{addMarker} Add missing markers to a dataset given a kit.
#'
#' @details
#' Given a dataset or a vector with sample names the function loops through
#' each sample and add any missing markers.
#' Returns a dataframe where each sample have at least one row per marker in
#' the specified marker vector. Use \code{\link{sortMarker}} to sort the markers
#' according to a specified kit.
#' Required columns are: 'Sample.Name'.
#' 
#' @param data data.frame or vector with sample names.
#' @param marker vector with marker names.
#' @param ignoreCase logical. TRUE ignores case in marker names.
#' @param debug logical indicating printing debug information.
#' 
# @importFrom plyr rbind.fill
#' 
#' @export
#' 
#' @return data.frame.
#' 

addMarker <- function(data, marker, ignoreCase=FALSE, debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("data")
    print(head(data))
    print("marker")
    print(marker)
    print("ignoreCase")
    print(ignoreCase)
  }
  
  # Initiate variables.
  res <- data[0,]
  vectorFlag <- FALSE
  markerName <- marker # 'marker' might be converted to uppercase.
  
  # CHECK DATA ----------------------------------------------------------------

  if(is.data.frame(data)){
    # Check dataset.
    if(!"Sample.Name" %in% names(data)){
      stop("'data' must contain a column 'Sample.Name'",
           call. = TRUE)
    }
  } else if(is.vector(data)){
    vectorFlag <- TRUE
  } else {
    stop("'data' must be a vector of sample names or a data.frame containing a column 'Sample.Name'",
         call. = TRUE)
  }

  if(!is.vector(marker)){
      stop("'marker' must be a vector", call. = TRUE)
  }
  
  # PREPARE -------------------------------------------------------------------

  if(vectorFlag){
    # Create data frame.
    data <- data.frame(Sample.Name=data)
  }
  
  if(ignoreCase){
    marker <- toupper(marker)
  }
  
  if(!"Marker" %in% names(data)){
    data$Marker <- NA
    message("Column 'Marker' added to 'data'")
  }
  
  # METHOD --------------------------------------------------------------------
  
  # Get sample names.
  sample <- unique(data$Sample.Name)
  
  # Loop through all samples.
  for(s in seq(along=sample)){

    # Reset vector.
    missingMarkers <- NULL
    
    # Get current data subset.
    cSample <- data[data$Sample.Name == sample[s], ]
    
    # Get current sample marker names.
    cMarker <- unique(cSample$Marker)
    if(ignoreCase){
      cMarker <- toupper(cMarker)
    }

    # Loop over provided marker vector.
    for(m in seq(along=marker)){
      
      if(!marker[m] %in% cMarker){
        # Add missing marker to data set.
        missingMarkers <- c(missingMarkers, markerName[m])
      }
      
    }
    
    if(!is.null(missingMarkers)){
      
      # Show progress.
      message(paste("Adding missing markers to sample (",
                    s, " of ", length(sample), "): ", sample[s], sep=""))

      # Add missing markers to current sample.
      new <- plyr::rbind.fill(cSample, data.frame(Sample.Name=sample[s],
                                            Marker=missingMarkers,
                                            stringsAsFactors=FALSE))
      # Add to result.
      res <- plyr::rbind.fill(res, new)
      
    } else {
      
      # Add current sample to res.
      res <- rbind(res, cSample)
      
    }
    
  }
  
  # RETURN --------------------------------------------------------------------
  
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  return(res)
  
}