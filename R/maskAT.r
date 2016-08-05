################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 05.08.2016: Removed Allele rows with NA from reference dataset.
# 20.05.2016: File name changed from blockAT.r to maskAT.r.
# 20.05.2016: 'Block*' changed to 'mask*' throughout.
# 28.08.2015: Added importFrom
# 26.06.2015: Fixed hard-coded kit/dye set.
# 05.05.2015: First version.

#' @title Mask And Prepare Data To Analyze Analytical Threshold
#'
#' @description
#' Break-out function to prepare data for the function \code{calculateAT}.
#'
#' @details
#' Prepares the 'SamplePlotSizingTable' for analysis of analytical threshold. It is needed
#' by the plot functions for control of masking. The preparation consist of 
#' converting the 'Height' and 'Data.Point' column to numeric (if needed), then
#' dye channel information is extracted from the 'Dye.Sample.Peak' column and
#' added to its own 'Dye' column, known fragments of the internal lane standard
#' (marked with an asterisk '*') is flagged as 'TRUE' in a new column 'ILS'.
#' 
#' @param data a data frame containing at least 'Dye.Sample.Peak',
#'  'Sample.File.Name', 'Marker', 'Allele', 'Height', and 'Data.Point'.
#' @param ref a data frame containing at least
#'  'Sample.Name', 'Marker', 'Allele'.
#' @param mask.height logical to indicate if high peaks should be masked.
#' @param height integer for global lower peak height threshold for peaks
#' to be excluded from the analysis. Active if 'mask.peak=TRUE.
#' @param mask.sample logical to indicate if sample allelic peaks should be masked.
#' @param per.dye logical TRUE if sample peaks should be masked per dye channel.
#' FALSE if sample peaks should be masked globally across dye channels.
#' @param range.sample integer to specify the masking range in (+/-) data points.
#' Active if mask.sample=TRUE.
#' @param mask.ils logical to indicate if internal lane standard peaks should be masked.
#' @param range.ils integer to specify the masking range in (+/-) data points.
#' Active if mask.ils=TRUE.
#' @param ignore.case logical to indicate if sample matching should ignore case.
#' @param word logical to indicate if word boundaries should be added before sample matching.
#' @param debug logical to indicate if debug information should be printed.
#' 
#' @return data.frame with added columns 'Dye' and 'ILS'.
#' 
#' @export
#' 
#' @importFrom utils str head
#' 
#' @seealso \code{\link{calculateAT}}


maskAT <- function(data, ref=NULL, mask.height=TRUE, height=500,
                    mask.sample=TRUE, per.dye = TRUE, range.sample=20,
                    mask.ils=TRUE, range.ils=10,
                    ignore.case=TRUE, word=FALSE, debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("data")
    print(str(data))
    print("ref")
    print(str(ref))
    print("mask.sample")
    print(mask.sample)
    print("per.dye")
    print(per.dye)
    print("range.sample")
    print(range.sample)
    print("mask.ils")
    print(mask.ils)
    print("range.ils")
    print(range.ils)
    print("ignore.case")
    print(ignore.case)
    print("word")
    print(word)
  }
  
  # Check data ----------------------------------------------------------------
  
  # Check data.
  if(is.null(data$Dye.Sample.Peak)){
    stop("'data' must contain a column 'Dye.Sample.Peak'")
  }
  
  if(is.null(data$Height)){
    stop("'data' must contain a column 'Height'")
  }
  
  if(is.null(data$Data.Point)){
    stop("'data' must contain a column 'Data.Point'")
  }
  
  if(sum(grepl("Height", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
  
  if(sum(grepl("Data.Point", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }

  # Check ref.
  if(!is.null(ref)){

    if(is.null(ref$Sample.Name)){
      stop("'ref' must contain a column 'Sample.Name'")
    }

    if(is.null(ref$Marker)){
      stop("'ref' must contain a column 'Marker'")
    }

    if(is.null(ref$Allele)){
      stop("'ref' must contain a column 'Allele'")
    }
    
    # Check if slim format.  
    if(sum(grepl("Allele", names(ref))) > 1){
      stop("'ref' must be in 'slim' format",
           call. = TRUE)
    }
    
  }
  
  # Check parameters.  
  if(!is.logical(mask.height)){
    stop("'mask.height' must be logical",
         call. = TRUE)
  }

  if(!is.numeric(height)){
    stop("'height' must be numeric",
         call. = TRUE)
  }

  if(!is.logical(mask.sample)){
    stop("'mask.sample' must be logical",
         call. = TRUE)
  }

  if(!is.logical(per.dye)){
    stop("'per.dye' must be logical",
         call. = TRUE)
  }

  if(!is.numeric(range.sample)){
    stop("'range.sample' must be numeric",
         call. = TRUE)
  }

  if(!is.logical(mask.ils)){
    stop("'mask.ils' must be logical",
         call. = TRUE)
  }

  if(!is.numeric(range.ils)){
    stop("'range.ils' must be numeric",
         call. = TRUE)
  }

  if(!is.logical(ignore.case)){
    stop("'ignore.case' must be logical",
         call. = TRUE)
  }

  if(!is.logical(word)){
    stop("'word' must be logical",
         call. = TRUE)
  }

  if(!is.logical(debug)){
    stop("'debug' must be logical",
         call. = TRUE)
  }
  
  # Prepare -------------------------------------------------------------------

  # Check data type.
  if(typeof(data$Height)!="integer" & typeof(data$Height)!="double" ){
    message("'Height' not numeric. Converting to numeric.")
    # Convert to numeric.
    data$Height <- suppressWarnings(as.numeric(data$Height))
  }
  
  # Check data type.
  if(typeof(data$Data.Point)!="integer" & typeof(data$Data.Point)!="double" ){
    message("'Data.Point' not numeric. Converting to numeric.")
    # Convert to numeric.
    data$Data.Point <- suppressWarnings(as.numeric(data$Data.Point))
  }
  
  if(!is.numeric(height) & mask.height){
    mask.height=FALSE
    message(paste("No valid threshold for peak height was provided (",
                  height, ")\n",
                  "Setting mask.height to FALSE"))
  }
  
  # Split information in Dye.Sample.Peak column.

  # Get dye letter (leftmost character).
  data$Dye <- substr(data$Dye.Sample.Peak, 1, 1)
  
  # Mark all peaks in the internal lane standard (marked with *).
  data$ILS <- grepl(pattern="*", x=data$Dye.Sample.Peak, fixed=TRUE)

  # Get all dyes.
  dyes <- as.character(unique(data$Dye))
  dyeILS <- unique(data$Dye[data$ILS])
  dyesKit <- setdiff(dyes, dyeILS)
  
  # Get the data sample names.
  sample <- unique(data$Sample.File.Name)

  # Only if ref is provided.
  if(!is.null(ref)){
    
    # Check for NA alleles (Y markers in female references).
    if(any(is.na(ref$Allele))){
      
      # Remove NA rows.
      tmp1 <- nrow(ref)
      ref <- ref[!is.na(ref$Allele), ]
      tmp2 <- nrow(ref)
      message("Removed ", tmp1 - tmp2,
              " rows with NA in 'Allele' column in reference dataset.")
      
    }
    
    # Get the reference sample names.
    refNames <- unique(ref$Sample.Name)
    
    # Create match vector.
    if(word){
      # Add word anchor.
      grepNames <- paste("\\b", refNames, "\\b", sep="")
    } else {
      # Use reference sample names.
      grepNames <- refNames
    }
    
  }

  # Add columns for mask status for ILS and sample.
  data$I.Mask <- FALSE # Mask ILS peak position.
  data$S.Mask <- FALSE # Mask sample allele positions.
  data$H.Mask <- FALSE # Mask according to height.
  
  # Mask ---------------------------------------------------------------------
  
  # Mask ILS peak range in all dyes per sample.
  if(mask.ils){

    message(paste("Masking internal lane standard (ILS) +/-",
                  range.ils, "data points."))

    # Add columns for masking range.
    data$ILS.Min <- NA
    data$ILS.Max <- NA
    
    # Calculate and add range.
    data[data$ILS,]$ILS.Min <- data[data$ILS,]$Data.Point - range.ils
    data[data$ILS,]$ILS.Max <- data[data$ILS,]$Data.Point + range.ils 
    
    # Loop over all samples.
    for(s in seq(along=sample)){
      
      message(paste("Masking ILS in sample", sample[s]))
      
      # Select start and end data point for current samples ILS.
      selection <- data$Sample.File.Name==sample[s] 
      start <- data[selection,]$ILS.Min
      end <- data[selection,]$ILS.Max
      # Remove NA.
      start <- start[!is.na(start)]
      end <- end[!is.na(end)]
      
      # Select data points for current sample profile.
      dp <- data[selection,]$Data.Point
      
      # Loop over all elements.
      for(e in seq(along=start)){
        
        # Create a sequence of data points to search for pull-up within.
        seqVec <- seq(start[e],end[e])
        
        # Check if any overlap in mask range.
        maskVec <- dp %in% seqVec
        
        # Mark masked data points.
        data$I.Mask[selection] <- data$I.Mask[selection] | maskVec
        
      } # End element loop.
      
    } # End sample loop.
    
  } # End mask.ils if.
  
  # Mask sample peak range per sample.
  if(mask.sample & !is.null(ref)){
    
    message(paste("Masking sample alleles +/-", range.sample, "data points."))
    
    # Get data into temporary vectors.
    sVec <- data$Sample.File.Name
    dVec <- data$Dye
    pVec <- data$Data.Point
    mVec <- data$Marker
    aVec <- data$Allele
    minVec <- rep(NA, length(sVec))
    maxVec <- rep(NA, length(sVec))
    
    # Loop over all reference samples.
    for(r in seq(along=grepNames)){
      
      # Select samples containing reference name.
      selSample <- grepl(grepNames[r], sVec, ignore.case=ignore.case)
      
      # Show progress.
      message(paste("Masking", length(unique(sVec[selSample])),
                    "samples matching reference", grepNames[r]))
      
      # Select current reference sample.
      selRef <- ref$Sample.Name==grepNames[r]
      
      # Get markers for current reference sample.
      marker <- unique(ref$Marker)
      
      # Loop over reference markers.
      for(m in seq(along=marker)){
        
        # Select current marker in reference samples.
        selRefMarker <- ref$Marker==marker[m]
        
        # Combine selection.
        selectionRef <- selRefMarker & selRef
        
        # Get reference alleles for current marker.
        alleles <- ref[selectionRef,]$Allele
        
        # Select current marker.
        selMarker <- mVec==marker[m]
        
        # Get sample data points for the matching alleles.
        selAlleles <- aVec %in% alleles
        
        # Combine selection.
        sel <- selSample & selMarker & selAlleles
        
        # Calculate min and max data point to mask.
        minVec[sel] <- pVec[sel] - range.sample
        maxVec[sel] <- pVec[sel] + range.sample
        
      } # End marker loop.
      
    } # End reference loop.

    # Add columns with mask range.
    data$Min <- minVec
    data$Max <- maxVec

    # Loop over all samples and mask sample peaks.
    for(s in seq(along=sample)){
      
      # Select current sample.
      selSample <- data$Sample.File.Name==sample[s]
      
      if(debug){
        print("head(data[selSample,])")
        print(head(data[selSample,]))
      }
      
      if(per.dye){
        # Mask sample peaks per dye.
        
        for(d in seq(along=dyesKit)){
          
          # Select current dye.
          selDye <- data$Dye==dyesKit[d]
          
          # Combine selection.
          selection <- selSample & selDye

          # Select start and end data point for current samples peaks.
          start <- data[selection,]$Min
          end <- data[selection,]$Max
          # Remove NA.
          start <- start[!is.na(start)]
          end <- end[!is.na(end)]
          
          # Select data points for current sample profile.
          dp <- data[selection,]$Data.Point
          
          # Loop over all elements.
          for(e in seq(along=start)){
            
            # Create a sequence of data points to search for overlap within.
            seqVec <- seq(start[e],end[e])
            
            # Check if any overlap in mask range.
            maskVec <- dp %in% seqVec
            
            # Mark masked data points.
            data$S.Mask[selection] <- data$S.Mask[selection] | maskVec

          } # End element loop.
          
        } # End dye loop.

      } else {
        # Mask sample peaks per sample
        
        # Select start and end data point for current samples peaks.
        start <- data[selSample,]$Min
        end <- data[selSample,]$Max
        # Remove NA.
        start <- start[!is.na(start)]
        end <- end[!is.na(end)]
        
        # Select data points for current sample profile.
        dp <- data[selSample,]$Data.Point
        
        # Loop over all elements.
        for(e in seq(along=start)){
          
          # Create a sequence of data points to search for overlap within.
          seqVec <- seq(start[e],end[e])
          
          # Check if any overlap in mask range.
          maskVec <- dp %in% seqVec
          
          # Mark masked data points.
          data$S.Mask[selSample] <- data$S.Mask[selSample] | maskVec
          
        } # End element for loop.
        
      } # End mask sample if.

    } # End sample loop.    
    
  } # End mask.sample if.
  
  # Mask sample peak range per sample.
  if(mask.height){
    
    message(paste("Masking peaks >", height, "RFU."))
    
    # Mask peaks.
    data$H.Mask <- data$Height > height
    
  }    
  
  # Mark masked data points.
  data$Masked <- data$S.Mask | data$I.Mask | data$H.Mask

  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  # Return result.
  return(data)
  
}