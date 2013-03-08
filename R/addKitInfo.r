################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 01: First version

#' @title Adds all kit information
#'
#' @description
#' \code{addKitInfo} Adds all kit information as new columns to 'data' for each
#'  'Marker' according to the specified 'kit'.
#'
#' @details
#' 'data' contains short tandem repeat (STR) marker/locus names in the required
#'   column. Based on the provided kit name other information is attached to
#'   'data' in new columns.
#'   
#' @param data Data frame containing at least a column named 'Marker'.
#' @param kit String or integer specifying the STR typing kit.
#' 
#' @return data.frame the original data frame containing additional columns.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' # Get marker names for Promega PowerPlex ESX 17.
#' x <- data.frame(Marker = getKit("ESX17")$locus)
#' # Get other kit information using string name.
#' y <- addKitInfo(data=x, kit="ESX17")
#' print(x)
#' print(y)

addKitInfo <- function(data, kit){
# Adds all kit information to 'data' by 'Marker' based on the information in 'kit'.

	# Get kit information.
	kitData <- getKit(kit)

	# Get marker names.
	markers <- unique(data$Marker)

	# Loop over all markers.
	for(m in seq(along=markers)){

		# Add information.
		data$Dye[data$Marker==markers[m]] <- 
			kitData$dye[kitData$locus==markers[m]]
		data$RangeMin[data$Marker==markers[m]] <- 
			kitData$rangeMin[kitData$locus==markers[m]]
		data$RangeMax[data$Marker==markers[m]] <- 
			kitData$rangeMax[kitData$locus==markers[m]]
		data$RepeatUnit[data$Marker==markers[m]] <- 
			kitData$repeatUnit[kitData$locus==markers[m]]
		data$Offset[data$Marker==markers[m]] <- 
			kitData$offset[kitData$locus==markers[m]]
		data$ProbPCR[data$Marker==markers[m]] <- 
			kitData$probPCR[kitData$locus==markers[m]]
	}
	return(data)
}