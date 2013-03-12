################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 07: Roxygenized.
# 06: filter profile using data in slim format (faster).

#' @title Filter out profiles from DNA results
#'
#' @description
#' \code{filterProfile} Filters out the result matching a specified
#' known profiles from typing data containing 'noise' such as stutters.
#'
#' @details
#' Returns data where allele names match 'ref' allele names.
#' Required columns are: 'Sample.Name', 'Marker', and 'Allele'.
#' 
#' @param data data frame with genotype data in 'slim' format.
#' @param ref data frame with reference profile in 'slim' format.
#' 
#' @return data.frame with extracted result.
#' 

filterProfile <- function(data, ref){

	# Get sample and reference names.
	refSampleNames<- unique(ref$Sample.Name)
	
	# Initiate boolean match vector to FALSE.
	matchingData <- is.na(data$Sample.Name)

	# Loop through all reference samples.
	for(s in seq(along=refSampleNames)){

		# Select current subset.
		selectedSamples <- grepl(refSampleNames[s],data$Sample.Name)

		# Get current marker.
		refMarkers <- unique(ref$Marker)
	
		# Loop through all markers.
		for(m in seq(along=refMarkers)){

			# Get reference alleles.
			refAlleles <- ref$Allele[ref$Marker==refMarkers[m]]

			# Loop through all alleles.
			for(a in seq(along=refAlleles)){

				# Get matching alleles in data.
				currentMatch <- selectedSamples & 
					data$Marker==refMarkers[m] & 
					data$Allele==refAlleles[a]

				# 'Concatenate' booleans
				matchingData <- matchingData | currentMatch

			}
		}
	}

	return(data[matchingData, ])
}