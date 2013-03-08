################################################################################
# TODO LIST
# TODO: NB! Only works with slimmed data???.
#     test if 'addZygosity' works with non-slimmed data.

################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 01: First version

#' @title Add zygosity
#'
#' @description
#' \code{addZygosity} Adds the zygosity for each marker.
#'
#' @details Compares a data set to reference profiles.
#' Adds zygosity (i.e. the number of expected peaks) to a new column in 'data'.
#'   
#' @param data Data frame containing at least a column named 'Alleles'.
#' @param byName  if TRUE matching of samples and ref by name, 
#' if FALSE by position.
#' @param matchSource if 'ref' sample names from 'ref' is used as source for
#' matching. Allowes for partial matching of sample names
#' (e.g. 'AB' matches '01_ABc'). NB! names in 'ref' must be unique to sample
#' names refering to a single DNA source. If 'data' sample names from 
#' 'data' is used as source for matching.  Only identical sample names match.
#' @param ignoreCase If TRUE name matching is case insensitive.
#' 
#' @return data.frame the original data frame containing additional columns.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' # Make some test data
#' x <- data.frame(Marker=c("D3","D18","FGA"), Allele.1=c(10,8,20), Allele.2=c(12,11,22))
#' addZygosity(x)

addZygosity <- function(data, ref, 
			byName=TRUE, ignoreCase=TRUE, matchSource="ref"){

	res <- data
	res$Zygosity <- NA

	# If 'Zygosity' column does not exist. Call function and calculate column.
	if(!'Zygosity' %in% colnames(ref)){
		ref <- calculateZygosity(ref)
	}

	# Get the sample names.
	if(matchSource=="data"){
		names <- unique(data$Sample.Name)
	}
	if(matchSource=="ref"){
		names <- unique(ref$Sample.Name)
	}

	# Loop through all samples.
	for (s in seq(along = names)) {
	
		# Get sample name.
		currentName <- names[s]

		# Subset sample data.
		if(matchSource=="data"){
			if(ignoreCase){
				cSampleRows <- toupper(data$Sample.Name) == toupper(currentName)
			} else {
				cSampleRows <- data$Sample.Name == currentName
			}
			cSampleData <- data[cSampleRows,]
		}
		if(matchSource=="ref"){
			if(ignoreCase){
				cSampleRows <- grepl(toupper(currentName), toupper(data$Sample.Name))
			} else {
				cSampleRows <- grepl(currentName, data$Sample.Name)
			}
			cSampleData <- data[cSampleRows,]
		}

		# Subset reference data.
		if(byName==TRUE) {
			if(matchSource=="data"){
				if(ignoreCase){
					cReferenceRows <- toupper(ref$Sample.Name) == toupper(currentName)
				} else {
					cReferenceRows <- ref$Sample.Name == currentName
				}
				cReferenceData <- ref[cReferenceRows,]
			}
			if(matchSource=="ref"){
				if(ignoreCase){
					cReferenceRows <- grepl(toupper(currentName), toupper(ref$Sample.Name))
				} else {
					cReferenceRows <- grepl(currentName, ref$Sample.Name)
				}
				cReferenceData <- ref[cReferenceRows,]
			}
		} else if (byName==FALSE){
			cReferenceData <- ref[cSampleRows,]
		}
			
		# Get the marker names.
		markerNames <- unique(cSampleData$Marker)

		# Loop through all markers.
		for (m in seq(along = markerNames)) {

			# Get current marker name.
			cMarker <- markerNames[m]

			# Get correct zygosity value.
			res[cSampleRows & res$Marker == cMarker,]$Zygosity <- 
				cReferenceData[cReferenceData$Marker == cMarker,]$Zygosity 
		}
		# Get markers.
		markers <- markerNames

	}

	# Return result.
	return(res)

}
