################################################################################
# TODO LIST
# TODO: Option to calculate size (of any allele) instead of matching to allele in bins.

################################################################################
# CHANGE LOG
# 11.09.2014: First version.

#' @title Add color information.
#'
#' @description
#' \code{addSize} add size information to alleles.
#'
#' @details
#' Adds a column 'Size' with estimated size in base pair (bp)
#' for alleles from kit. 
#' 
#' @param data data.frame with at least columns 'Marker' and 'Allele'.
#' @param kit data.frame with columns 'Marker', 'Allele', and 'Size'.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with additional columns for added colors, 
#' or vector with converted values.
#' 

addSize <- function(data, kit=NA, debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("PARAMETERS:")
    print("data:")
    print(str(data))
    print(head(data))
    print("kit:")
    print(str(kit))
    print(head(kit))
  }
  
  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!"Marker" %in% names(data)){
    stop("'data' must contain a column 'Marker'",
         call. = TRUE)
  }
  
  if(!"Allele" %in% names(data)){
    stop("'data' must contain a column 'Allele'",
         call. = TRUE)
  }
  
  # Check kit.
  if(!"Marker" %in% names(kit)){
    stop("'kit' must contain a column 'Marker'",
         call. = TRUE)
  }
  
  if(!"Allele" %in% names(kit)){
    stop("'kit' must contain a column 'Allele'",
         call. = TRUE)
  }
  
  if(!"Size" %in% names(kit)){
    stop("'kit' must contain a column 'Size'",
         call. = TRUE)
  }
  
  # Check if character data.
  if(!is.character(data$Allele)){
    message("'Allele' must be character. 'data' converted")
    data$Allele <- as.character(data$Allele)
  }
  
  # PREPARE -----------------------------------------------------------------
  
  # Get size of kit alleles.
  kit <- getKit("ESX17", what="Size")
  
  # Check for column 'Size'
  if("Size" %in% names(data)){
    
    warning(paste("'data' already contain a column 'Size'\n",
                  "Size will be overwritten!"),
         call. = TRUE)
    
  }
  
  # Add a column 'Size'
  data$Size <- NA
  
  # ADD SIZE ------------------------------------------------------------------

  # Get markers in dataset.
  marker <- unique(data$Marker)
  
  if(debug){
    print("Markers:")
    print(marker)
  }

  # Loop over markers.
  for(m in seq(along=marker)){
    
    if(debug){
      print("Current marker:")
      print(marker[m])
    }
    
    # Select rows for current marker.
    cMarker <- data$Marker == marker[m]

    # Get alleles for current marker.
    allele <- unique(data$Allele[cMarker])
    # Remove NAs.
    allele <- allele[!is.na(allele)]

    if(debug){
      print("Alleles:")
      print(allele)
    }
    
    # Loop over allele in current marker.
    for(a in seq(along=allele)){
      
      # Select rows for current allele.
      cAllele <- data$Allele == allele[a]
      
      # Combine selections.
      selection <- cMarker & cAllele
      
      # Get size.
      size <- kit$Size[kit$Marker== marker[m] & kit$Allele == allele[a]]
      
      # Store size for current allele in current marker.
      if(length(size) != 0){
        
        data$Size[selection] <- size
        
      } else {
        
        warning(paste("Allele", allele[a],
                      "for marker", marker[m], "not in kit definition file."))
        
      }
      
    }
    
  }
  
  if(debug){
    print("Return:")
    print(str(data))
    print(head(data))
  }
  
  return(data)
  
}
