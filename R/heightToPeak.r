################################################################################
# TODO LIST
# TODO: Handle factors...

################################################################################
# CHANGE LOG
# 15.12.2014: Changed parameter names to format: lower.case
# 09.12.2014: Moved from PCRsim.

#' @title Height To Peak.
#'
#' @description
#' Internal helper function to convert a peak into a plottable polygon.
#'
#' @details
#' Converts a single height and size value to a plottable 0-height-0 triangle/peak value.
#' Makes 3 data points from each peak size for plotting a polygon representing a peak.
#' Factors in other columns might get converted to factor level.
#' 
#' @param data data frame containing at least columns 'Height' and 'Size'.
#' @param width numeric specifying the width of the peak in bp.
#' @param keep.na logical, TRUE keep empty markers.
#' 
#' @return data.frame with new values.
#' 
#' @keywords internal

heightToPeak <- function(data, width=1, keep.na=TRUE, debug=FALSE){
  
  # Debug info.
  if(debug){
    print("ENTER: heightToPeak")
    print("data:")
    print(data)
    print(str(data))
  }
  
  # CHECK ARGUMENTS -----------------------------------------------------------
  
  if(!is.data.frame(data)){
    stop("'data' must be a data frame.",
         call. = TRUE)
  }
  
  if(!any(grepl("Height",names(data)))){
    stop("Data frame 'data' must contain a column 'Height'.",
         call. = TRUE)
  }
  
  if(!any(grepl("Size",names(data)))){
    stop("Data frame 'data' must contain a column 'Size'.",
         call. = TRUE)
  }
  
  if(!is.numeric(width)){
    stop("Peak width 'width' must be a numeric value.",
         call. = TRUE)
  }
  
  if(!is.logical(keep.na)){
    stop("'keep.na' must be a logical value.",
         call. = TRUE)
  }
  
  if("Size" %in% names(data)){
    if(!is.numeric(data$Size)){
      data$Size <- as.numeric(data$Size)
      message("'Size' must be numeric. Data converted!")
    }
  }
  
  # FUNCTION ------------------------------------------------------------------
  
  if(!keep.na){
    # Remove all rows with no height.
    data <- data[!is.na(data$Height),]
  } else if (keep.na){
    # Replace all NAs with 0s.
    data$Height[is.na(data$Height)] <- 0
  }
  
  # Create an empty data frame 3 times the length of 'data'.
  newData <- data.frame(matrix(NA,nrow(data)*3,ncol(data)))
  
  # Add column names.
  names(newData) <- names(data)
  
  # Initiate row counter.
  r <- 1
  
  # Do not enter loop if no rows.
  if(nrow(data) > 0){
    
    # Loop through the data frame.
    for(i in 1:nrow(data)){
      
      # Copy data and change value of x1 and y1.
      newData[r,] <- data[i,]
      newData$Size[r] <- newData$Size[r] - width/2
      newData$Height[r] <- 0
      r <- r + 1
      
      # Copy data for x2 and y2.
      newData[r,] <- data[i,]
      r <- r + 1
      
      # Copy data and change value of x3 and y3.
      newData[r,] <- data[i,]
      newData$Size[r] <- newData$Size[r] + width/2
      newData$Height[r] <- 0
      r <- r + 1
      
    }
  }
  
  # Debug info.
  if(debug){
    print("EXIT: heightToPeak")
    print("newData:")
    print(newData)
    print(str(newData))
    flush.console()
  }
  
  return(newData)
  
}
