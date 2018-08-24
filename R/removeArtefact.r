################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 24.08.2018: Removed unused variables.
# 07.08.2017: Added audit trail.
# 28.04.2016: First version.

#' @title Remove Artefacts
#'
#' @description
#' Remove artefact peaks from data.
#'
#' @details
#' Removes identified artefacts from the dataset. Likely artefacts can be
#' identified using the function \code{\link{calculateAllele}}. The output
#' should then be provided to the 'artefact'. Alternatively known artefacts
#' can be provided using the 'marker' and 'allele' arguments.
#' 
#' @param data data.frame with data to remove spikes from.
#' @param artefact data.frame that lists artefacts in columns 'Marker',
#'  'Allele', optionally with 'Allele.Proportion'.
#'  Alternatively artefacts can be provided using 'marker' and 'allele'.
#' @param marker character vector with marker names paired with values in 'allele'.
#' @param allele character vector with allele names paired with values in 'marker'.
#' @param threshold numeric value defining a minimum proportion for artefacts.
#'  Requires 'artefacts' including the column 'Allele.Proportion'.
#' @param na.rm logical TRUE to preserve Allele=NA in 'data'.
#' @param debug logical indicating printing debug information.
#' 
#' @export
#' 
#' @importFrom utils str
#' 
#' @return data.frame with spikes removed.
#' 

removeArtefact <- function(data, artefact=NULL, marker=NULL, allele=NULL,
                           threshold=NULL, na.rm=FALSE, debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("data:")
    print(str(data))
    print("artefact:")
    print(str(artefact))
    print("marker:")
    print(marker)
    print("allele:")
    print(allele)
    print("threshold:")
    print(threshold)
  }
  
  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!"Marker" %in% names(data)){
    stop("'data' must contain a column 'Marker'", call. = TRUE)
  }
  
  if(!"Allele" %in% names(data)){
    stop("'data' must contain a column 'Allele'", call. = TRUE)
  }

  if(sum(grepl("Allele", names(data))) > 1){
    stop("'data' must be in 'slim' format", call. = TRUE)
  }
  
  # Check artefact dataset.
  if(!is.null(artefact)){
    if(!"Marker" %in% names(artefact)){
      stop("'artefact' must contain a column 'Marker'", call. = TRUE)
    }
    if(!"Allele" %in% names(artefact)){
      stop("'artefact' must contain a column 'Allele'", call. = TRUE)
    }
  }
  
  # Check threshold.
  if(!is.null(threshold)){
    if(!is.numeric(threshold)){
      stop("'threshold' must be numeric {0-1}", call. = TRUE)
    }
    if(!"Allele.Proportion" %in% names(artefact)){
      stop(paste("To use 'threshold' the 'artefact' dataset must",
                 "contain a column 'Allele.Proportion'."), call. = TRUE)
    }
  }

  # Check marker.
  if(!is.null(marker)){
    if(!is.character(marker)){
      stop("'marker' must be a character vector.", call. = TRUE)
    }
    if(length(marker) != length(allele)){
      stop("'marker' and 'allele' must be vectors of equal length.", call. = TRUE)
    }
  }

  # Check allele.
  if(!is.null(allele)){
    if(!is.character(allele)){
      stop("'allele' must be a character vector.", call. = TRUE)
    }
    if(length(marker) != length(allele)){
      stop("'marker' and 'allele' must be vectors of equal length.", call. = TRUE)
    }
  }
  
  # Check that enough information is provided.
  if(is.null(artefact) && any(is.null(marker), is.null(allele))){
    stop("Either 'artefact' or both 'marker' and 'allele' must be provided!",
         call. = TRUE)
  }
  
  if(!is.logical(na.rm)){
    stop("'na.rm' must be logical.", call. = TRUE)
  }
  
  # PREPARE -------------------------------------------------------------------
  
  # Check if character data.
  if(!is.character(data$Allele)){
    
    message("'Allele' must be character. 'data' converted")
    
    data$Allele <- as.character(data$Allele)
    
  }

  if(!is.null(artefact)){
    
    if(!is.character(artefact$Allele)){
      
      message("'Allele' must be character. 'artefact' converted")
      
      artefact$Allele <- as.character(artefact$Allele)
      
    }
    
  }

  # If threshold is provided.
  if(!is.null(threshold)){
    message("Extracting artefacts observed with a proportion >=", threshold)
    
    # Extract artefacts passing threshold.
    artefact <- artefact[artefact$Allele.Proportion >= threshold, ]

    message(nrow(artefact), " artefacts passing 'threshold'.")
    
  }
  
  # Get artefacts.
  if(!is.null(artefact)){
    message("Argument 'artefact' provided and will be used.")
    
    marker <- artefact$Marker
    allele <- artefact$Allele
    proportion <- artefact$Allele.Proportion # NULL if not provided.
    
  } else {
    
    message("Arguments 'marker' and 'allele' provided and will be used.")

  }
  
  # Check if NA's should be removed.
  if(na.rm){
    
    n1 <- nrow(data)
    data <- data[!is.na(data$Allele), ]
    n2 <- nrow(data)
    
    message("Removed ", n1-n2, " rows where Allele=NA.")
    
  }
  
  # FILTER --------------------------------------------------------------------

  # Loop over marker.  
  for(m in seq(along=marker)){

    # Save number of rows.
    n1 <- nrow(data)

    # Remove all matching alleles for the current marker, while preserving 'NA' rows.
    data <- data[is.na(data$Allele) | !(data$Marker==marker[m] & data$Allele==allele[m]), ]
      
    # Save number of rows.
    n2 <- nrow(data)
    
    # Calculate and print number of rows removed.
    message("Removed ", n1-n2, " artefacts (", marker[m], ":", allele[m], ").")
    
  }
  
  # RETURN --------------------------------------------------------------------
  
  # Update audit trail.
  data <- auditTrail(obj = data, f.call = match.call(), package = "strvalidator")

  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  return(data)
  
}