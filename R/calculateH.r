################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 13.04.2013: Rewrote the function to work with 'slim' data.
# 11.04.2013: Changed 'Z' to 'Heterozygous' (het/hom now indicated by 1/0,
#             but changed to 2 in function)
# <11.04.2013: Roxygenized and changed name from 'averagePeakHeight' to 'calculateH'.
# <11.04.2013: Added if condition for 'slim' and option 'debugInfo'.
# <11.04.2013: First version

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
#' @param data a data frame in 'slim' format with at least columns
#'  'Sample.Name', 'Heterozygous', and 'Height'.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with with columns: 'Sample.Name', 'H', and 'Peaks'.
#' 


calculateH <- function(data, debug=FALSE){

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!any(grepl("Sample.Name", names(data)))){
    stop("'data' must contain a column 'Sample.Name'",
         call. = TRUE)
  }

  if(!any(grepl("Heterozygous", names(data), fixed = TRUE))){
    stop("'data' must contain a column 'Heterozygous'",
         call. = TRUE)
  }
  
  if(!any(grepl("Height", names(data)))){
    stop("'data' must contain a column 'Height'",
         call. = TRUE)
  }
  
  # Check if slim format.
  if(sum(grepl("Height", names(data))>1)){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
  
  # Check if character data.
  if(!is.numeric(data$Height)){
    warning("'Height' must be numeric. 'data' converted")
    data$Height <- as.numeric(data$Height)
  }
  
  # PREPARE -----------------------------------------------------------------
  
  # Create empty result data frame with NAs.
	res <- data.frame(t(rep(NA,3)))
	# Add column names.
	names(res) <- c("Sample.Name","H","Peaks")
	# Remove all NAs
	res <- res[-1,]
  
  # Add a column Z, where 1 is heterozygous and 2 is homozygous.
  data$Z <- data$Heterozygous
  data$Z[data$Z == 0] <- 2
  data$Z <- as.numeric(data$Z)

	# Get the sample names.
	sampleNames <- unique(data$Sample.Name)
  
		# Loop through all samples.
		for (s in seq(along = sampleNames)) {
		
			# Get sample name.
			cSampleName <- sampleNames[s]

			# Subset sample data.
			cSampleRows <- data$Sample.Name == cSampleName
			cSampleData <- data[cSampleRows, ]
			
			# Sum all peak heights.
			totalPeakHeight <- sum(cSampleData$Height, na.rm=TRUE)
			
			# Sum number of peaks.
			totalObservedPeaks <- sum(!is.na(cSampleData$Height))

      # Number of peaks adjusted for 'invisible' homozygotes.
			totalAdjustedPeaks <- sum(cSampleData$Z[!is.na(cSampleData$Height)])

      # Calculate the average peak height.
			avgPeakHeight <- totalPeakHeight / totalAdjustedPeaks 

			# Save result in temporary data frame.
			tmp <- data.frame(Sample.Name = cSampleName,
                        H = avgPeakHeight,
                        Peaks = totalObservedPeaks)

			# Add result to data frame.
			res <- rbind(res, tmp)

		}

	# Return result.
	return(res)

}
