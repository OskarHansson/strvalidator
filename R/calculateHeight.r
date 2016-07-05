################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 25.01.2016: Fixed save attribute saves dataset.
# 09.01.2016: Added more attributes to result.
# 06.01.2016: Added attributes to result.
# 12.10.2014: Fixed bug when NA in Allele column.
# 26.09.2014: Accept vector for 'exclude'.
# 12.09.2014: Included 'exclude' parameter.
# 10.09.2014: Included total peak height in result.
# 04.03.2014: Fixed bug when no NA and NA!=NULL.
# 25.02.2014: Option to add directly to dataset.
# 25.02.2014: Option to replace NAs.
# 13.04.2013: Rewrote the function to work with 'slim' data.
# 11.04.2013: Changed 'Z' to 'Heterozygous' (het/hom now indicated by 1/0,
#             but changed to 2 in function)

#' @title Calculate Peak Height.
#'
#' @description
#' Calculate peak height metrics for samples.
#'
#' @details
#' Calculates the average peak height (H) and/or the total peak height (TPH) for each sample.
#' To enable calculation of H the sample data must contain a column "Heterozygous",
#' where 1 = heterozygous loci, and 0 = homozygous loci as known from the reference sample.
#' Calculates H according to the formula:
#' \eqn{H = sum(peak heights)/(n[het] + 2n[hom]}
#' Where:
#' n[het] = number of observed heterozygous alleles
#' n[hom] = number of observed homozygous alleles
#' 
#' @param data data.frame with at least columns 'Sample.Name' and 'Height'.
#' @param na replaces NA values.
#' @param exclude character vector (case sensitive) e.g. "OL" excludes rows with
#'  "OL" in the 'Allele' column.
#' @param add logical default is TRUE which will add/overwrite columns
#' 'H', 'TPH', and 'Peaks' in the provided 'data'.
#' @param sex.rm logical, default FALSE to include sex markers in the analysis.
#' @param qs.rm logical, default TRUE to exclude quality sensors from the analysis.
#' @param kit character, required if sex.rm=TRUE or qs.rm=TRUE to define the kit.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with with at least columns 'Sample.Name', 'TPH', and 'Peaks'.
#' 
#' @export
#' 
#' @references
#' Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
#'  Journal of the Royal Statistical Society: Series C (Applied Statistics),
#'  Volume 59, Issue 5, 2010,
#'  Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
#' \url{http://dx.doi.org/10.1111/j.1467-9876.2010.00722.x}

calculateHeight <- function(data, na=NULL, add=TRUE, exclude=NULL,
                            sex.rm=FALSE, qs.rm=FALSE, kit=NULL, debug=FALSE){
  
  # Parameters that are changed by the function must be saved first.
  attr_data <- substitute(data)

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  # Flag to calculate average peak height.
  flagH <- TRUE
  
  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!any(grepl("Sample.Name", names(data)))){
    stop("'data' must contain a column 'Sample.Name'.",
         call. = TRUE)
  }

  if(!any(grepl("Heterozygous", names(data), fixed = TRUE))){
    message("'data' does not contain a column 'Heterozygous'.",
            "Average peak height 'H' will not be calculated.")
    flagH <- FALSE
  }
  
  if(!any(grepl("Height", names(data)))){
    stop("'data' must contain a column 'Height'.",
         call. = TRUE)
  }
  
  # Check if slim format.
  if(sum(grepl("Height", names(data))>1)){
    stop("'data' must be in 'slim' format.",
         call. = TRUE)
  }
  
  # Check if character data.
  if(!is.numeric(data$Height)){
    message("'Height' must be numeric. 'data' converted.")
    data$Height <- as.numeric(data$Height)
  }

  # Check na.
  if(length(na) > 1){
    stop("'na' must be of length 1.",
         call. = TRUE)
  }

  # Check logical arguments.
  if(!is.logical(add)){
    stop("'add' must be logical.", call. = TRUE)
  }
  if(!is.logical(sex.rm)){
    stop("'sex.rm' must logical.", call. = TRUE)
  }
  if(!is.logical(qs.rm)){
    stop("'qs.rm' must be logical.", call. = TRUE)
  }

  # Check kit.  
  if(!is.null(kit)){
    if(is.na(getKit(kit = kit))){
      stop("'kit' was not found in the kit definition file.")
    }
  }

  # Check dependencies.  
  if(sex.rm){
    if(is.null(kit)){
      stop("'kit' can't be NULL if sex.rm=TRUE.")
    }
  }
  if(qs.rm){
    if(is.null(kit)){
      stop("'kit' can't be NULL if qs.rm=TRUE.")
    }
  }
  
  # PREPARE -----------------------------------------------------------------

  # Remove sex markers.
  if(sex.rm){
    
    message("Removing sex markers defined in kit: ", kit)
    
    # Get sex markers.    
    sexMarkers <- getKit(kit = kit, what = "Sex.Marker")
    
    if(debug){
      print("Sex markers:")
      print(sexMarkers)
    }
    
    # Loop through and remove all sex markers in data.
    message("Removing sex markers in 'data':")
    for(i in seq(along = sexMarkers)){
      
      tmp1 <- nrow(data)
      
      data <- data[data$Marker != sexMarkers[i],]
      
      tmp2 <- nrow(data)
      
      message("Removed ", tmp1 - tmp2, " rows with marker ", sexMarkers[i])
      
    }
    
  }
  
  # Remove quality sensors. 
  if(qs.rm){
    
    message("Removing quality sensors defined in kit: ", kit, ".")
    
    # Get quality sensors.
    qsMarkers <- getKit(kit = kit, what = "Quality.Sensor")
    
    if(debug){
      print("Quality sensors:")
      print(qsMarkers)
    }
    
    # Loop through and remove all quality sensors in data.
    message("Removing quality sensors in 'data':")
    for(i in seq(along = qsMarkers)){
      
      tmp1 <- nrow(data)
      
      data <- data[data$Marker != qsMarkers[i],]
      
      tmp2 <- nrow(data)
      
      message("Removed ", tmp1 - tmp2,
              " rows with quality sensor ", qsMarkers[i], ".")
      
    }
    
  }
  
  if(!is.null(exclude)){
    
    if("Allele" %in% names(data)){

      # Loop over all elements in exclude and remove matching rows.
      for(i in seq(along=exclude)){
        
        tmp1 <- nrow(data)
        data <- data[data$Allele != exclude[i] | is.na(data$Allele), ]
        tmp2 <- nrow(data)
        
        message(paste(tmp1-tmp2, "Allele rows =", exclude[i],
                      "removed from dataset."))
        
      }
      
    }
    
  }
  
  if(add){

    if(flagH){
      # Check if columns exist.
      if("H" %in% names(data)){
        message("Column 'H' will be overwritten.")
      }
      # Add columns for result.
      data$H <- NA
    }
    
    if("TPH" %in% names(data)){
      message("Column 'TPH' will be overwritten.")
    }
    if("Peaks" %in% names(data)){
      message("Column 'Peaks' will be overwritten.")
    }
    
    # Add columns for result.
    data$TPH <- NA
    data$Peaks <- NA
    
  } else {

    resSample <- NULL
    resH <- NULL
    resTPH <- NULL
    resPeaks <- NULL
    
  }
  
  # ANALYSE -------------------------------------------------------------------

  # Create a vector 'Z', where 1 is heterozygous and 2 is homozygous.
  if(flagH){
    Z <- data$Heterozygous
    Z <- as.numeric(Z)
    Z[Z == 0] <- 2
  }

	# Get the sample names.
	sampleNames <- unique(data$Sample.Name)
  
	# Loop through all samples.
	for (s in seq(along = sampleNames)) {
	
		# Get sample name.
		cSampleName <- sampleNames[s]

		# Subset sample data.
		cSampleRows <- data$Sample.Name == cSampleName
		cSampleData <- data[cSampleRows, ]
		if(flagH){
		  cZ <- Z[cSampleRows]
		}
		
		# Sum all peak heights.
		totalPeakHeight <- sum(cSampleData$Height, na.rm=TRUE)
    
		# Sum number of peaks.
		totalObservedPeaks <- sum(!is.na(cSampleData$Height))

		if(flagH){
		  
      # Number of peaks adjusted for 'invisible' homozygotes.
  		totalAdjustedPeaks <- sum(cZ[!is.na(cSampleData$Height)])
  
      # Calculate the average peak height.
  		avgPeakHeight <- totalPeakHeight / totalAdjustedPeaks 
      
		}

    if(add){
      
      if(flagH){
        # Add result to dataframe.
        data[cSampleRows, ]$H <- avgPeakHeight
      }        
      
      # Add result to dataframe.
      data[cSampleRows, ]$TPH <- totalPeakHeight
      data[cSampleRows, ]$Peaks <- totalObservedPeaks
      
    } else {

      if(flagH){
        # Add to result.
        resH <- c(resH, avgPeakHeight)
      }
      
      # Add to result.
      resSample <- c(resSample, cSampleName)
      resTPH <- c(resTPH, totalPeakHeight)
      resPeaks <- c(resPeaks, totalObservedPeaks)
      
    }
    
	}

  # Copy to return data or create a new dataframe.
  if(add){
    # Add result to data.
    res <- data
    
  } else {
    # Create new data frame.
    
    if(flagH){
      res <- data.frame(Sample.Name=resSample, H=resH, TPH=resTPH, Peaks=resPeaks)
    } else {
      res <- data.frame(Sample.Name=resSample, TPH=resTPH, Peaks=resPeaks)
    }
    
  }
  
  # Replace NA:s
  if(!is.null(na)){
    # Check if NA:s and change to 'na'.
    
    if(flagH){
      if(any(is.na(res$H))){
        n <- sum(is.na(res$H))
        res[is.na(res$H), ]$H <- na
        message(paste(n, " NA's in 'H' replaced with '", na, "'.", sep=""))
      }
    }
    
    if(any(is.na(res$TPH))){
      n <- sum(is.na(res$TPH))
      res[is.na(res$TPH), ]$TPH <- na
      message(paste(n, " NA's in 'TPH' replaced with '", na, "'.", sep=""))
    }
    
  }
	
	# Add attributes to result.
	attr(res, which="kit") <- kit
	attr(res, which="calculateHeight, strvalidator") <- as.character(utils::packageVersion("strvalidator"))
	attr(res, which="calculateHeight, call") <- match.call()
	attr(res, which="calculateHeight, date") <- date()
	attr(res, which="calculateHeight, data") <- attr_data
	attr(res, which="calculateHeight, na") <- na
	attr(res, which="calculateHeight, add") <- add
	attr(res, which="calculateHeight, exclude") <- exclude
	attr(res, which="calculateHeight, sex.rm") <- sex.rm
	attr(res, which="calculateHeight, qs.rm") <- qs.rm
	
	# Return result.
	return(res)

}
