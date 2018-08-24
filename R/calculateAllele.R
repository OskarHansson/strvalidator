################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 24.08.2018: Removed unused variables.
# 06.08.2017: Added audit trail.
# 02.05.2016: Added parameters 'sex.rm' and 'kit'.
# 02.05.2016: Implemented calculation of proportion and frequency.
# 29.04.2016: Added more attributes.
# 13.10.2015: First version.

#' @title Calculate Allele
#'
#' @description
#' Calculates summary statistics for alleles per marker over the entire dataset.
#'
#' @details Creates a table of the alleles in the dataset sorted by number of
#' observations.For each allele the proportion of total observations is
#' calculated. Using a threshold this can be used to separate likely artefacts
#' from likely drop-in peaks. In addition the observed allele frequency is
#' calculated. If columns 'Height' and/or 'Size' are available summary
#' statistics is calculated. 
#' NB! The function removes NA's and OL's prior to analysis.
#' 
#' @param data data.frame including columns 'Marker' and 'Allele', and
#'  optionally 'Height' and 'Size'.
#' @param threshold numeric if not NULL only peak heights above 'threshold'
#'  will be considered.
#' @param sex.rm logical TRUE removes all sex markers. Requires 'kit'.
#' @param kit character for the DNA typing kit defining the sex markers.
#' @param debug logical indicating printing debug information.
#' 
#' @export
#' 
#' @importFrom data.table data.table := .N
#' 
#' @return data.frame with columns 'Marker', 'Allele', 'Peaks', 'Size.Min',
#' 'Size.Mean', 'Size.Max', 'Height.Min', 'Height.Mean', 'Height.Max',
#' 'Total.Peaks', 'Allele.Proportion', 'Sum.Peaks', and 'Allele.Frequency'.

#' 
#' @seealso \code{\link{data.table}}


calculateAllele <- function(data, threshold=NULL, sex.rm=FALSE, kit=NULL,
                            debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("str(data):")
    print(str(data))
    print("threshold:")
    print(threshold)
  }
  
  # Check data ----------------------------------------------------------------
  
  # Columns:
  if(is.null(data$Marker)){
    stop("'Marker' does not exist!")
  }
  
  if(is.null(data$Allele)){
    stop("'Allele' does not exist!")
  }
  
  # Check if slim format:
  if(sum(grepl("Allele", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
  
  if(sum(grepl("Height", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
  
  if(sum(grepl("Size", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }

  # Check logical.
  if(!is.logical(sex.rm)){
    stop("'sex.rm' must be logical!")
  }

  # Check character.  
  if(is.character(kit)){
    stop("'kit' must be character!")
  }
  
  
  # Prepare -------------------------------------------------------------------

  # Check data type:
  if(!is.null(data$Height)){
    if(!is.numeric(data$Height)){
      data$Height <- as.numeric(data$Height)
      message("'Height' not numeric! 'data' converted.")
    }
  }

  # Check data type:
  if(!is.null(data$Size)){
    if(!is.numeric(data$Size)){
      data$Size <- as.numeric(data$Size)
      message("'Size' not numeric! 'data' converted.")
    }
  }

  if(sex.rm){
    # Remove sex markers.
    
    if(is.null(kit)){
      # Kit is not provided.
      
      message("'kit' is not provided. Autodetect kit...")
      kit <- detectKit(data = data, index = FALSE, debug = debug)[1]
      message("Using first detected kit '", kit, "' to define sex markers.")
      
    }
    
    # Get sex markes.
    sexMarker <- getKit(kit = kit, what = "Sex.Marker")
    message("Defined markers: ", paste(sexMarker, collapse=", "))
    
    for(m in seq(along=sexMarker)){
      
      n1 <- nrow(data)
      data <- data[!data$Marker==sexMarker[m], ]
      n2 <- nrow(data)
      message("Removed ", n1-n2, " rows with sex marker ", sexMarker[m])
      
    }
    
  } # End if remove sex markers.
  
  # Convert to data.table.  
  DT <- data.table::data.table(data)
  
  # Remove NA's.
  if(any(is.na(DT$Allele))){
    n1 <- nrow(DT)
    DT <- DT[!is.na(DT$Allele), ]
    n2 <- nrow(DT)
    message("Removed ", n1-n2, " rows with Allele=NA.")
  }

  # Remove OL's.
  if(any("OL" %in% DT$Allele)){
    n1 <- nrow(DT)
    DT <- DT[!DT$Allele=="OL", ]
    n2 <- nrow(DT)
    message("Removed ", n1-n2, " rows with Allele=OL.")
  }

  # Remove peaks below threshold.
  if(!is.null(threshold)){
    if("Height" %in% names(DT)){
      n1 <- nrow(DT)
      DT <- DT[DT$Height >= threshold, ]
      n2 <- nrow(DT)
      message("Removed ", n1-n2, " rows with peaks below ", threshold, " RFU.")
    } else {
      message("Option 'threshold' ignored since column 'Height' is missing.")
    }
  }
  
  
  # Analyse -------------------------------------------------------------------

  if(all(c("Size", "Height") %in% names(DT))){
    message("Counts number of peaks for each allele by marker.")
    message("Calculates summary statistics for both 'Size' and 'Height'.")
    
    res <- DT[, list("Peaks"=.N, "Size.Min"=min(Size), "Size.Mean"=mean(Size),
                         "Size.Max"=max(Size), "Height.Min"=min(Height),
                         "Height.Mean"=mean(Height), "Height.Max"=max(Height)),
                  by=c("Marker", "Allele")]
    
  } else if ("Height" %in% names(DT)) {
    message("Counts number of peaks for each allele by marker.")
    message("Calculates summary statistics for 'Height'.")
    
    res <- DT[, list("Peaks"=.N, "Height.Min"=min(Height),
                         "Height.Mean"=mean(Height), "Height.Max"=max(Height)),
                  by=c("Marker", "Allele")]
    
  } else if ("Size" %in% names(DT)) {
    message("Counts number of peaks for each allele by marker.")
    message("Calculates summary statistics for 'Size'.")
    
    res <- DT[, list("Peaks"=.N, "Size.Min"=min(Size),
                     "Size.Mean"=mean(Size), "Size.Max"=max(Size)),
              by=c("Marker", "Allele")]
    
  } else {
    message("Counts number of peaks for each allele by marker.")
    
    res <- DT[, list("Peaks"=.N), by=c("Marker", "Allele")]
    
  }
  
  if(debug){
    print("str(DT)")
    print(str(DT))
  }
  
  message("Calculates proportion of the total for each allele.")
  res[, Total.Peaks:=sum(Peaks)]
  res[, Allele.Proportion:=Peaks/Total.Peaks]
  
  message("Calculates frequency of each allele.")
  res[, Sum.Peaks:=sum(Peaks), by=c("Marker")]
  res[, Allele.Frequency:=Peaks/Sum.Peaks]
  
  # Sort table with the most frequent peak at top.
  data.table::setorder(res, -"Peaks", "Marker", "Allele")
  
  # Convert to data.frame.
  res <- as.data.frame(res)
  
  # Add attributes to result.
  attr(res, which="kit") <- kit
  
  # Update audit trail.
  res <- auditTrail(obj = res, f.call = match.call(), package = "strvalidator")
  
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  return(res)

}