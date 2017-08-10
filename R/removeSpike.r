################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 07.08.2017: Added audit trail.
# 28.04.2016: First version.

#' @title Remove Spikes
#'
#' @description
#' Remove spikes from data.
#'
#' @details
#' Removes identified spikes from the dataset. Spikes are identified using the
#' function \code{\link{calculateSpike}} and provided as a separate dataset.
#' NB! Samples must have unique identifiers.
#' Some laboratories use non-unique names for e.g. negative controls. To allow
#' identification of specific samples when multiple batches are imported into
#' one dataset an id is automatically created by combining the sample name and
#' the file name. This work well as long as there is at most 1 identically
#' named sample in each file (batch). To enable multiple identically named
#' samples in one file, the sample names can be prefixed with the lane or well
#' number before importing them to STR-validator.
#' 
#' @param data data.frame with data to remove spikes from.
#' @param spike data.frame with list of spikes.
#' @param invert logical FALSE to remove spikes, TRUE to keep spikes.
#' @param debug logical indicating printing debug information.
#' 
#' @export
#' 
#' @importFrom utils str
#' 
#' @return data.frame with spikes removed.
#' 

removeSpike <- function(data, spike, invert=FALSE, debug=FALSE){
  
  # Parameters that are changed by the function must be saved first.
  attr_data <- substitute(data)
  attr_spike <- substitute(spike)

  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("data:")
    print(str(data))
    print("spike:")
    print(str(spike))
    print("invert:")
    print(invert)
  }
  
  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!"Sample.Name" %in% names(data)){
    stop("'data' must contain a column 'Sample.Name'",
         call. = TRUE)
  }
  
  if(!"Marker" %in% names(data)){
    stop("'data' must contain a column 'Marker'",
         call. = TRUE)
  }
  
  if(!"Allele" %in% names(data)){
    stop("'data' must contain a column 'Allele'",
         call. = TRUE)
  }
  
  if(!"File.Name" %in% names(data)){
    stop("'data' must contain a column 'File.Name'",
         call. = TRUE)
  }
  
  # Check reference dataset.
  if(!"Sample.Name" %in% names(spike)){
    stop("'spike' must contain a column 'Sample.Name'",
         call. = TRUE)
  }
  if(!"Marker" %in% names(spike)){
    stop("'spike' must contain a column 'Marker'",
         call. = TRUE)
  }
  if(!"Allele" %in% names(spike)){
    stop("'spike' must contain a column 'Allele'",
         call. = TRUE)
  }
  
  if(sum(grepl("Allele", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
  
  # Check logical flags.
  if(!is.logical(invert)){
    stop("'invert' must be logical", call. = TRUE)
  }
  
  # PREPARE -------------------------------------------------------------------
  
  # Check if character data.
  if(!is.character(data$Allele)){
    
    message("'Allele' must be character. 'data' converted")
    
    data$Allele <- as.character(data$Allele)
    
  }

  if(!is.character(spike$Allele)){
    
    message("'Allele' must be character. 'spike' converted")
    
    spike$Allele <- as.character(spike$Allele)
    
  }

  # FILTER --------------------------------------------------------------------
  
  # Check if column exist.
  if("Remove" %in% names(data)){
    message("The column 'Remove' will be overwritten!")
  }
  # Add new flag column and initialise.
  if(invert){
    data$Remove <- TRUE
  } else {
    data$Remove <- FALSE
  }
  
  # Check if column exist.
  if(!"Id" %in% names(data)){
    
    # Create id from sample name and file name.
    data$Id <- paste(data$Sample.Name, data$File.Name)
    
    message("'Id' column created.")
  }
  
  # Get id's from filter list.
  ids <- unique(spike$Id)
  
  # Loop over id's
  for(i in seq(along=ids)){
    
    # Print progress.
    message("Sample ", i, " of (", length(ids), ")")

    # Select samples matching id.    
    selS <- data$Id==ids[i]
    
    # Get markers for current id.
    marker <- unique(spike[spike$Id==ids[i],]$Marker)
    
    # Loop through the markers.
    for(m in seq(along=marker)){
      
      # Select matching markers.
      selM <- data$Marker==marker[m]
      
      # Get size.
      size <- unique(spike[spike$Id==ids[i] & spike$Marker==marker[m],]$Size)  
      
      # Loop through size.
      for(b in seq(along=size)){
        
        # Select matchin size.
        selB <- data$Size==size[b]

        # Combine all matches.        
        selection <- selS & selM & selB
        
        # Set flag.
        if(invert){
          data[selection, ]$Remove <- FALSE 
        } else {
          data[selection, ]$Remove <- TRUE 
        }
        
      } # End size loop.
      
    } # End marker loop.
    
  } # End id loop.
  
  # Remove flagged data.
  n1 <- nrow(data)
  res <- data[!data$Remove, ]
  n2 <- nrow(res)

  # Calculate and print number of rows removed.
  print(paste(n1 - n2, "rows removed!"))

  # Remove column
  res$Remove <- NULL
  
  # Update audit trail.
  res <- auditTrail(obj = res, f.call = match.call(), package = "strvalidator")
  
  # RETURN --------------------------------------------------------------------
  
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  return(res)
  
}