################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 01: First version.

#' @title Calculate drop-in peaks
#'
#' @description
#' \code{calculateDropin} calculates number of drop-in peaks
#'
#' @details
#' Not sure if this function is working (might be a copy of countPeaks as a draft...)
#'  
#' @param data a data frame containing at least 'Sample.Name'.
#' 
#' @return data.frame  data frame with 'Sample.Name','Marker','Allele','Peaks') 
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' print("Example will come later")


countDropin <- function(data, col="Height", unique=TRUE){
  
  # Create empty result data frame with NAs.
  res <- data.frame(t(rep(NA,4)))
  # Add column names.
  names(res) <- c("Sample.Name","Marker","Allele","Peaks")
  # Remove all NAs
  res <- res[-1,]
  
  # Get the sample names.
  sampleNames <- unique(data$Sample.Name)
  
  # Loop through all samples.
  for (s in seq(along = sampleNames)) {
    
    # Get sample name.
    currentSampleName <- sampleNames[s]
    
    # Subset sample data.
    currentSampleRows <- data$Sample.Name == currentSampleName
    currentData <- data[currentSampleRows,]
    
    # Subset height data.
    valueColumns <- grepl(col, names(currentData))
    valueData <- currentData[ , valueColumns]
    
    # Check option.
    if(unique){
      
      # Sum number of unique values per marker.
      observedPeaks <- apply(valueData, 1,
                             function(x) sum(!is.na(unique(as.character(x))),na.rm=T))
      
    } else {
      
      # Sum number of values per marker.
      observedPeaks <- apply(valueData, 1,
                             function(x) sum(!is.na(x),na.rm=T))
      
    }
    
    # Get markers.
    markers <- currentData$Marker
    
    # Save result in temporary data frame.
    tmp <- data.frame(Sample.Name = currentSampleName,
                      Marker = markers,
                      Peaks = observedPeaks)
    
    # Add result to data frame.
    res <- rbind(res, tmp)
    
  }
  
  # Return result.
  return(res)
  
}
