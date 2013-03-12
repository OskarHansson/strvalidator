################################################################################
# CHANGE LOG
# 06: Roxygenized.
# 05: Corrected missing variable in calculateZvalue, ref --> data.

#' @title Adds all kit information
#'
#' @description
#' \code{addZvalue} Applies the Z value to dna results.
#'
#' @details Takes (GM-formatted) data for samples and references
#' (with Z column) as input. Adds a column 'Z' to the data.frame.
#'   
#' @param data Data frame containing at least a column named 'Marker'.
#' @param ref ...
#' @param byName if TRUE matching of samples and ref by name,
#'  if FALSE by position.
#' @param matchSource if 'ref' sample names from 'ref' is used as source for
#' matching. Allowes for partial matching of sample names
#' (e.g. 'AB' matches '01_ABc'). NB! names in 'ref' must be unique to sample
#' names refering to a single DNA source. If 'data' sample names from 
#' 'data' is used as source for matching.  Only identical sample names match.
#' @param ignoreCase If TRUE name matching is case insensitive.
#' 
#' @return data.frame the original data frame containing an additional column.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' # Get marker names for Promega PowerPlex ESX 17.
#' x <- data.frame(Marker = getKit("ESX17")$locus)
#' # Get other kit information using string name.
#' y <- addKitInfo(data=x, kit="ESX17")
#' print(x)
#' print(y)

addZvalue <- function(data, ref, byName=TRUE, ignoreCase=TRUE, matchSource="ref"){
  
  res <- data
  res$Z <- NA
  
  # If Z column does not exist. Call function and add a Z column.
  # NB! TODO: This adds number of peaks, NOT z-value??
  if(!'Z' %in% colnames(ref)){
    refZ <- countPeaks(data=ref, col="Allele", unique=TRUE)
    ref$Z<- refZ$Peaks
  }

  # Get the sample names.
  if(matchSource=="data"){
    names <- unique(data$Sample.Name)
  }
  if(matchSource=="ref"){
    names <- unique(ref$Sample.Name)
  }
  
  # Loop through all samples.
  for (s in seq(along = names)) {
    
    # Get sample name.
    current.name <- names[s]
    
    # Subset sample data.
    if(matchSource=="data"){
      if(ignoreCase){
        current.sample.rows <- toupper(data$Sample.Name) == toupper(current.name)
      } else {
        current.sample.rows <- data$Sample.Name == current.name
      }
      current.sample.data <- data[current.sample.rows,]
    }
    if(matchSource=="ref"){
      if(ignoreCase){
        current.sample.rows <- grepl(toupper(current.name), toupper(data$Sample.Name))
      } else {
        current.sample.rows <- grepl(current.name, data$Sample.Name)
      }
      current.sample.data <- data[current.sample.rows,]
    }
    
    # Subset reference data.
    if(byName==TRUE) {
      if(matchSource=="data"){
        if(ignoreCase){
          current.reference.rows <- toupper(ref$Sample.Name) == toupper(current.name)
        } else {
          current.reference.rows <- ref$Sample.Name == current.name
        }
        current.reference.data <- ref[current.reference.rows,]
      }
      if(matchSource=="ref"){
        if(ignoreCase){
          current.reference.rows <- grepl(toupper(current.name), toupper(ref$Sample.Name))
        } else {
          current.reference.rows <- grepl(current.name, ref$Sample.Name)
        }
        current.reference.data <- ref[current.reference.rows,]
      }
    } else if (byName==FALSE){
      current.reference.data <- ref[current.sample.rows,]
    }
    
    # Get the marker names.
    marker.names <- unique(current.sample.data$Marker)
    
    # Loop through all markers.
    for (m in seq(along = marker.names)) {
      
      # Get marker name.
      current.marker <- marker.names[m]
      
      # Get correct z value.
      res[current.sample.rows & current.sample.data$Marker == current.marker,]$Z <- 
        current.reference.data[current.reference.data$Marker == current.marker,]$Z 
    }
    # Get markers.
    markers <- marker.names
    
  }
  
  # Return result.
  return(res)
  
}