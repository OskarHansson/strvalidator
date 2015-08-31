################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 28.08.2015: Added importFrom.
# 15.12.2014: Changed parameter names to format: lower.case
# 09.12.2014: Derived from PCRsim.

#' @title Compact Profile
#'
#' @description
#' Adds Identical Peaks.
#'
#' @details
#' Add peak heights of identical alleles together.
#' 
#' @param data data frame with at least 'Sample.Name', 'Marker', 'Allele', and 'Height' columns.
#' @param per.sample logical TRUE compact per sample, FALSE for entire dataset (in which case
#' 'Sample.Name' is set to 'Profile').
#' @param col character string to specify which column to compact.
#' @param debug logical for printing debug information.
#' 
#' @return data.frame with columns 'Sample.Name', 'Marker', 'Allele', and 'Height'.
#' 
#' @importFrom utils flush.console str head tail
#' 
#' @keywords internal
#' 


compact <- function(data, per.sample=TRUE, col="Height", debug=FALSE){
  
  message("COMPACT PROFILE")
  
  # Debug info.
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    flush.console()
  } else {}
  
  if(length(grep("Allele", names(data))) > 1){
    
    stop("'data' must be in 'slim' format.",
         call. = TRUE)
    
  }
  
  if(!"Allele" %in% names(data)){
    stop("'data' must contain a column 'Allele'.",
         call. = TRUE)
  }
  
  if(!"Marker" %in% names(data)){
    stop("'data' must contain a column 'Marker'",
         call. = TRUE)
  }
  
  if(!"Sample.Name" %in% names(data)){
    stop("'data' must contain a column 'Sample.Name'",
         call. = TRUE)
  }
  
  
  # Prepare -------------------------------------------------------------------
  
  # Check if numeric.
  if(!is.numeric(data[[col]])){
    data[[col]] <- as.numeric(data[[col]])
    message(paste("Column", col, "converted to numeric."))
  }
  
  # Remove NA rows.
  if(any(is.na(data$Allele))){
    data <- data[!is.na(data$Allele),]
    message("'NA' alleles removed.")
  }
  
  # Remove "OL" rows.
  if(any(data$Allele=="OL")){
    data <- data[!data$Allele=="OL",]
    message("'OL' alleles removed.")
  }
  
  # COMPACT PROFILE ###########################################################
  
  if(per.sample){
    
    # Get samples.    
    sample <- unique(data$Sample.Name)
    
    # Pre-allocate arrays for result.
    lenArr <- nrow(data)
    sampleArr <- rep(NA, lenArr)
    markerArr <- rep(NA, lenArr)
    alleleArr <- rep(NA, lenArr)
    valueArr <- rep(NA, lenArr)
    
    # Initiate variables.
    i <- 1
    
    for(s in seq(along=sample)){
      
      # Progress.
      message(paste("Sample", s, "of", length(sample)))
      
      # Subset current sample.
      tmpS <- data[data$Sample.Name==sample[s], ]
      
      # Get markers.
      marker <- unique(tmpS$Marker)
      
      for(m in seq(along=marker)){
        
        # Subset current marker.
        tmpM <- tmpS[tmpS$Marker==marker[m], ]
        
        # Get alleles.
        allele <- unique(tmpM$Allele)
        
        for(a in seq(along=allele)){
          
          # Select allele.
          sel <- tmpM$Allele==allele[a]
          
          # Sum heights.
          value <- sum(tmpM[[col]][sel])
          
          # Save data in arrays.
          sampleArr[i] <- sample[s]
          markerArr[i] <- marker[m]
          alleleArr[i] <- allele[a]
          valueArr[i] <- value
          i <- i + 1
          
        }
        
      }    
      
    }
    
  } else {
    # For the entire dataset.
    
    # Pre-allocate arrays for result.
    lenArr <- nrow(data)
    markerArr <- rep(NA, lenArr)
    alleleArr <- rep(NA, lenArr)
    valueArr <- rep(NA, lenArr)
    
    # Initiate variables.
    sel <- TRUE
    i <- 1
    
    # Get markers.
    marker <- unique(data$Marker)
    
    for(m in seq(along=marker)){
      
      # Subset current marker.
      tmpM <- data[data$Marker==marker[m], ]
      
      # Get alleles.
      allele <- unique(tmpM$Allele)
      
      for(a in seq(along=allele)){
        
        # Select allele.
        sel <- tmpM$Allele==allele[a]
        
        # Sum heights.
        value <- sum(tmpM[[col]][sel])
        
        # Save data in arrays.
        markerArr[i] <- marker[m]
        alleleArr[i] <- allele[a]
        valueArr[i] <- value
        i <- i + 1
        
      }
      
    }    
    
    sampleArr <- "Profile"    
  }
  
  # Create result and finalise ------------------------------------------------
  
  # Create result.
  res <- data.frame(Sample.Name=sampleArr, Marker=markerArr,
                    Allele=alleleArr, Value=valueArr,
                    stringsAsFactors=FALSE)
  
  # Add correct names.  
  names(res) <- c("Sample.Name", "Marker", "Allele", col)
  
  # Remove NA rows.
  res <- res[!is.na(res[[col]]), ]
  
  # Debug info.
  if(debug){
    print("Result:")
    print(str(res))
    print(head(res))
    print(tail(res))
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  return (res)
  
}
