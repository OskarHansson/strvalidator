################################################################################
# TODO LIST
# TODO: Option to ignore "OL"
# TODO: rfu threshold.

################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 02: New option 'col', new variable names (no dot)
# 01: First version.

#' @title Calculate summary
#'
#' @description
#' \code{countPeaks} Calculates the number of observed peaks per marker.
#'
#' @details
#' Takes (GM-formatted) data for samples as input.
#' Counts the number of peaks per marker (values in 'col' columns).
#'  
#' @param data a data frame containing at least columns 'Sample.Name', 'Marker', and \code{col}.
#' @param col string specifying the column(s) to count values in.
#' @param unique logical indicating if counting occurences or unique values per marker.
#' 
#' @return data.frame with columns 'Sample.Name', 'Marker', and 'Peaks'.
#' 

countPeaks <- function(data, col="Height", unique=TRUE){

	# Create empty result data frame with NAs.
	res <- data.frame(t(rep(NA,4)))
	# Add column names.
	names(res) <- c("Sample.Name","Marker","Allele","Peaks")
	# Remove all NAs
	res <- res[-1,]

	# Get the sample names.
	sampleNames <- unique(data$Sample.Name)

		# Loop through all samples.
		for (s in seq(along = sampleNames)) {
		
			# Get sample name.
			currentSampleName <- sampleNames[s]

			# Subset sample data.
			currentSampleRows <- data$Sample.Name == currentSampleName
			currentData <- data[currentSampleRows,]

			# Subset height data.
			valueColumns <- grepl(col, names(currentData))
			valueData <- currentData[ , valueColumns]

			# Check option.
			if(unique){
	
				# Sum number of unique values per marker.
				observedPeaks <- apply(valueData, 1,
					function(x) sum(!is.na(unique(as.character(x))),na.rm=T))

			} else {

				# Sum number of values per marker.
				observedPeaks <- apply(valueData, 1,
					function(x) sum(!is.na(x),na.rm=T))

			}

			# Get markers.
			markers <- currentData$Marker

			# Save result in temporary data frame.
			tmp <- data.frame(Sample.Name = currentSampleName,
					Marker = markers,
					Peaks = observedPeaks)

			# Add result to data frame.
			res <- rbind(res, tmp)

		}

	# Return result.
	return(res)

}
