################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 06.08.2017: Added audit trail.
# 08.02.2017: First version.

#' @title Add Marker Order.
#'
#' @description
#' Add marker order to data frame containing a column 'Marker'.
#'
#' @details
#' Markers in a kit appear in a certain order. Not all STR-validator functions
#' keep the original marker order in the result. A column indicating the marker
#' order is added to the dataset. This is especially useful when exporting the
#' data to an external spread-sheet software and allow to quickly sort the data
#' in the correct order.
#' 
#' @param data data frame or vector.
#' @param kit string representing the forensic STR kit used.
#' Default is NULL and automatic detection of kit will be attempted.
#' @param overwrite logical if TRUE and column exist it will be overwritten.
#' @param ignore.case logical if TRUE case in marker names will be ignored.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with additional numeric column 'Order'.
#' 
#' @export
#' 
#' @importFrom utils str
#' 
#' @examples
#' # Load a dataset containing two samples.
#' data("set2")
#' # Add marker order when kit is known.
#' addOrder(data = set2, kit = "SGMPlus")

addOrder <- function(data, kit=NULL, overwrite=FALSE, ignore.case=FALSE,
                     debug=FALSE){
  
  # Parameters that are changed by the function must be saved first.
  attr_data <- substitute(data)
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  if(debug){
    print("data")
    print(str(data))
    print("kit")
    print(kit)
    print("overwrite")
    print(overwrite)
    print("ignore.case")
    print(ignore.case)
  }
  
  # Initialise 'ok' to add marker order.
  ok <- NULL

  if("ORDER" %in% toupper(names(data))){
  
    # Check if overwrite.
    if(overwrite){

      message("Column 'Order' will be overwritten!")
      ok <- TRUE
      
      # Remove and re-add column (will add column to the right).
      data$Order <- NULL
      data$Order <- as.numeric(NA)

    } else {
        
      message("A column 'Order' already exist! Data returned unchanged.")
      message("Remove 'Order' or use option overwrite=TRUE to update.")
      ok <- FALSE
        
    }
    
  } else {
    
    # Add column.
    data$Order <- as.numeric(NA)
    ok <- TRUE
    
  }
  
  if(ok){
    # Go ahead and add marker order.

    # Check kit and auto detect if not provided. 
    if(is.null(kit)){
      kit <- detectKit(data = data)[1]
      message("No kit provided. Detected: ", kit)
    }
    
    # Get markers and their color.
    kitInfo <- getKit(kit, what="Color")
    marker <- kitInfo$Marker
    
    message("Adding marker order according to kit: ", kit)
    
    # Loop over all markers.
    for(m in seq(along=marker)){
      
      if(ignore.case){
        
        # Add order by marker.
        data$Order[toupper(data$Marker) == toupper(marker[m])] <- m
        
      } else {
        
        # Add new column and colors per marker.
        data$Order[data$Marker == marker[m]] <- m
        
      }
      
    }
    
    # Add attributes to result.
    attr(data, which="kit") <- kit
    
    # Update audit trail.
    data <- auditTrail(obj = data, f.call = match.call(), package = "strvalidator")

  }

  if(debug){
    print("Return")
    print(str(data))
  }
  
  return(data)
  
}
