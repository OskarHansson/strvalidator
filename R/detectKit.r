################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
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
#' 
#' @return integer or string indicating the detected kit.
#' 


detectKit <- function(data, index=TRUE){
  
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
  
  if(!index){
    # Get kit name.
    detectedKit <- getKit(detectedKit)$shortName
  }
  
  return(detectedKit)
  
}
