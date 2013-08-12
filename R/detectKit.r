################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 05.06.2013: Added debug option.
# 03.06.2013: Distinguish equal scores by marker position.
# 28.04.2013: Best match from proportion instead of number of matching markers.
# <28.04.2013: First version

#' @title Detect kit
#'
#' @description
#' \code{detectKit} finds the most likely STR kit for a dataset.
#'
#' @details
#' The function looks at the markers in the dataset and returns the kit(s)
#' with the highest proportions of matching markers.
#' 
#' @param data data frame.
#' @param index logical, returns kit index if TRUE or short name if FALSE.
#' @param debug logical, prints debug information if TRUE.
#' 
#' @return integer or string indicating the detected kit.
#' 

detectKit <- function(data, index=TRUE, debug=FALSE){
  
  if(!'Marker' %in% colnames(data)){
    stop("Data frame must contain a column 'Marker'")
  }
  
  markers <- unique(data$Marker)
  
  # Get available kits.
  kits <- getKit()

  kitMarkers <- list()
  score <- vector()
  
  # Get markers for available kits.
  for(k in seq(along=kits)){
    
    kitMarkers[[k]] <- getKit(kits[k])$locus
      
  }

  for(k in seq(along=kitMarkers)){
    
    score[k] <- sum(markers %in% kitMarkers[[k]])
    score[k] <- score[k] / length(kitMarkers[[k]])
    
  }

  # Get kit index.
  bestFit <- max(score)
  detectedKit <- which(score %in% bestFit)

  # Get number of candidate kit.
  candidates <- length(detectedKit)

  if(debug){
    print("Detected kit:")
    print(detectedKit)
  }
  
  # Check if more than one.
  if(candidates > 1){

    if(debug){
      print("Multiple kits with equal score!")
      print("Trying to resolve by marker order.")
    }
    
    # Try to distinguish based on marker order.
    posMatch <- vector()
    markers<-unique(data$Marker)

    # Loop over all candidate kits.
    for(c in seq(along=detectedKit)){
      
      # Sum number of markers in matching position.
      posMatch[c]<- sum(kitMarkers[[detectedKit[c]]] == markers)
      
    }

    # Get kit index.
    bestFit <- max(posMatch)
    kitIndex <- which(posMatch %in% bestFit)
    detectedKit <- detectedKit[kitIndex]

    if(debug){
      print("Detected kit:")
      print(detectedKit)
    }
    
  }  

  # Get number of candidate kit.
  candidates <- length(detectedKit)
  
  # Check if more than one.
  if(candidates > 1){
    
    if(debug){
      print("Still multiple kits with equal score!")
      print("Trying to resolve by sub string matching.")
    }
    
    # Try to distinguish based on sub string matching.
    posMatch <- vector()
    markerString <- paste(unique(data$Marker), collapse="")
    
    # Loop over all candidate kits.
    for(c in seq(along=detectedKit)){
      
      kitString <- paste(kitMarkers[[detectedKit[c]]], collapse="")

      # Search for substring.
      posMatch[c]<- grepl(markerString, kitString, fixed=TRUE)
      
    }
    
    # Get kit index.
    bestFit <- TRUE
    kitIndex <- which(posMatch %in% bestFit)
    
    if(length(kitIndex) > 0){
      detectedKit <- detectedKit[kitIndex]
    
      if(debug){
        print("Detected kit:")
        print(detectedKit)
      }
      
    } else {

      if(debug){
        print("Could not resolve any further.")
      }
      
    }
    
  }  
  
  if(!index){
    # Get kit name.
    detectedKit <- getKit(detectedKit)$shortName
  }
  
  return(detectedKit)
  
}
