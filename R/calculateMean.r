################################################################################
# TODO LIST
# TODO: Also calculate sd.
# TODO: Automatically convert to numeric if factors. If factors convert to numeric.
#data$Amount.1<-as.numeric(levels(data$Amount.1))[data$Amount.1]
#data$Amount.2<-as.numeric(levels(data$Amount.2))[data$Amount.2]
#data$Concentration.2<-as.numeric(levels(data$Concentration.2))[data$Concentration.2]


################################################################################
# CHANGE LOG
# 04: Roxygenized.
# 03: new option option 'calculateNA'

#' @title Calculate mean
#'
#' @description
#' \code{calculateMean} calculates the mean of given one or more replicate measurments.
#'
#' @details
#' Calculates the average given (one or) more replicate measurements.
#' 
#' @param data a data frame containing at least 'Sample.Name' and 'colBaseName'.
#' @param ignoreNA logical indicating how to handle NA.
#' @param calculateNA value to replace NAs with before calculation.
#' @param replaceNA value to replace NAs with in the final data frame.
#' 
#' @return data.frame with additional columns 'N' and 'Mean'.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' data(set1)
#' # Remove all except positive control samples.
#' set1 <- trim(data=set1, samples="PC")
#' 
#' calculateMean(data=set1, colBaseName="Height")
#'        

calculateMean <- function(data, colBaseName, ignoreNA=TRUE, calculateNA=NULL, replaceNA=NULL){

	# Grab columns.
	cols <- grep(colBaseName,names(data))

	# Check and convert.
	for(c in seq(along=cols)){

		if(!is.numeric(data[, cols[c]])){

			# Convert if factors.	
			if(!is.null(levels(data[, cols[c]]))){

				data[, cols[c]] <- as.numeric(levels(data[, cols[c]]))[data[, cols[c]]]

			}

			# Convert if string.	
			if(is.character(data[, cols[c]])){

				data[, cols[c]] <- as.numeric(data[, cols[c]])

			}

		}
	}

	# Calculate with NA by replacing it a value.
	if(!is.null(calculateNA)){
		data[ , cols][is.na(data[ , cols])] <- calculateNA
	} 

	# Calculate Means and N.
	data$Mean <- rowMeans(data[,cols], na.rm = ignoreNA)
	data$N <- rowSums(!is.na(data[,cols]))

	# Replace NAs.
	if(!is.null(replaceNA)){
		data$Mean[is.na(data$Mean)] <- replaceNA
	}

	# Return data frame.
	return(data)
}