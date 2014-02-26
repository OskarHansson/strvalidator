################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 25.02.2014: Option to add directly to dataset.
# 25.02.2014: Option to replace NAs.
# 13.04.2013: Rewrote the function to work with 'slim' data.
# 11.04.2013: Changed 'Z' to 'Heterozygous' (het/hom now indicated by 1/0,
#             but changed to 2 in function)

#' @title Calculate average peak height.
#'
#' @description
#' \code{calculateH} calculates the average peak height for a sample.
#'
#' @details
#' Calculates the average peak height (H) for each sample.
#' Sample data must contain a column "Heterozygous", where 1 = heterozygous loci,
#' and 0 = homozygous loci as known from the reference sample.
#' Calculates H according to the formula:
#' \eqn{H = sum(peak heights)/(n[het] + 2n[hom]}
#' Where:
#' n[het] = number of observed heterozygous alleles
#' n[hom] = number of observed homozygous alleles
#' 
#' @param data data.frame in 'slim' format with at least columns
#'  'Sample.Name', 'Heterozygous', and 'Height'.
#' @param na replaces NA values.
#' @param add logical default is TRUE which will add/overwrite column 'H' for
#' average peak height, and 'Peaks' for the total number of peaks in the profile.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with with at least columns 'Sample.Name', 'H', and 'Peaks'.
#' 
#' @references
#' Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
#'  Journal of the Royal Statistical Society: Series C (Applied Statistics),
#'  Volume 59, Issue 5, 2010,
#'  Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
#' \url{http://dx.doi.org/10.1111/j.1467-9876.2010.00722.x}

calculateH <- function(data, na=NULL, add=TRUE, debug=FALSE){

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!any(grepl("Sample.Name", names(data)))){
    stop("'data' must contain a column 'Sample.Name'.",
         call. = TRUE)
  }

  if(!any(grepl("Heterozygous", names(data), fixed = TRUE))){
    stop("'data' must contain a column 'Heterozygous.'",
         call. = TRUE)
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
  
  # Check na.
  if(!is.logical(add)){
    stop("'add' must be TRUE or FALSE.",
         call. = TRUE)
  }
  
  # PREPARE -----------------------------------------------------------------
  
  if(add){

    # Check if columns exist.
    if("H" %in% names(data)){
      message("Column 'H' will be overwritten.")
    }
    if("Peaks" %in% names(data)){
      message("Column 'Peaks' will be overwritten.")
    }
    
    # Add columns for result.
    data$H <- NA
    data$Peaks <- NA
    
  } else {

    resSample <- NULL
    resH <- NULL
    resPeaks <- NULL
    
#     # Create empty result data frame with NAs.
#     res <- data.frame(t(rep(NA,3)))
#     # Add column names.
#     names(res) <- c("Sample.Name","H","Peaks")
#     # Remove all NAs
#     res <- res[-1,]
    
  }

  # Create a vector 'Z', where 1 is heterozygous and 2 is homozygous.
  Z <- data$Heterozygous
  Z <- as.numeric(Z)
  Z[Z == 0] <- 2

	# Get the sample names.
	sampleNames <- unique(data$Sample.Name)
  
	# Loop through all samples.
	for (s in seq(along = sampleNames)) {
	
		# Get sample name.
		cSampleName <- sampleNames[s]

		# Subset sample data.
		cSampleRows <- data$Sample.Name == cSampleName
		cSampleData <- data[cSampleRows, ]
    cZ <- Z[cSampleRows]
		
		# Sum all peak heights.
		totalPeakHeight <- sum(cSampleData$Height, na.rm=TRUE)
		
		# Sum number of peaks.
		totalObservedPeaks <- sum(!is.na(cSampleData$Height))

    # Number of peaks adjusted for 'invisible' homozygotes.
		totalAdjustedPeaks <- sum(cZ[!is.na(cSampleData$Height)])

    # Calculate the average peak height.
		avgPeakHeight <- totalPeakHeight / totalAdjustedPeaks 

    if(add){
      
      # Add result to dataframe.
      data[cSampleRows, ]$H <- avgPeakHeight
      data[cSampleRows, ]$Peaks <- totalObservedPeaks
      
    } else {
      
#       # Save result in temporary data frame.
#       tmp <- data.frame(Sample.Name = cSampleName,
#                         H = avgPeakHeight,
#                         Peaks = totalObservedPeaks)
#       
#       # Add result to data frame.
#       res <- rbind(res, tmp)
      
      # Add to result.
      resSample <- c(resSample, cSampleName)
      resH <- c(resH, avgPeakHeight)
      resPeaks <- c(resPeaks, totalObservedPeaks)
      
    }
    
	}

  # Copy to return variable or create dataframe.
  if(add){
    res <- data
  } else {
    res <- data.frame(Sample.Name=resSample, H=resH, Peaks=resPeaks)
  }
  
  # Replace NA:s
  if(!is.null(na)){
    n <- sum(is.na(res$H))
    res[is.na(res$H), ]$H <- na
    message(paste(n, " NA's replaced with '", na, "'.", sep=""))
  }

	# Return result.
	return(res)

}
