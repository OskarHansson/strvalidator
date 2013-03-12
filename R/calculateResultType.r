################################################################################
# TODO LIST
# TODO: use string constants instead of hard coding.

################################################################################
# CHANGE LOG
# 05: Roxygenized.
# 04: Added factors.
# 03: Grouping of mixed results.
# 02: Grouping of partial results.
# 01: First version.

#' @title Calculate result type
#'
#' @description
#' \code{calculateResultType} calculate the result type for samples.
#'
#' @details
#' Calculates result types for samples in 'data'.
#' Defined types are: 'No result', 'Mixture', 'Partial', and 'Complete'.
#' Subtypes can be defined by parameters.
#' An integer passed to 'dropoutT' defines a subtype of 'Complete' "Complete profile all peaks >dropoutT".
#' An integer or vector passed to 'mixtureLimits' define subtypes of 'Mixture' "> [mixtureLimits] markers".
#' An integer or vector passed to 'partialLimits' define subtypes of 'Partial' "> [partialLimits] peaks".
#' A string with marker names separated by pipe (|) passed to 'markerSubset' and
#'  a string 'subsetName' defines a subtype of 'Partial' "Complete [subsetName]".
#'  
#' @param data a data frame containing at least the column 'Sample.Name'.
#' @param dropoutT integer indicating the dropout threshold.
#' @param mixtureLimits integer or vector indicating subtypes of 'Mixture'.
#' @param partialLimits integer or vector indicating subtypes of 'Partial'.
#' @param subsetName string naming the subset of 'Complete'.
#' @param markerSubset string with marker names defining the subset of 'Complete'.
#' 
#' @return data.frame with columns 'Sample.Name','Type','Sub.Type'
#' 

calculateResultType <- function(data, dropoutT=NULL, mixtureLimits=NULL, 
				partialLimits=NULL, subsetName=NA, markerSubset=NULL){

	# Get sample names.
	sampleNames <- unique(data$Sample.Name)

	# Create result data frame.
	res <- data.frame(matrix(NA, length(sampleNames), 3))
	names(res) <- paste(c("Sample.Name","Type","Sub.Type"))  # Add new column names.

	# Loop over all samples.
	for(s in seq(along=sampleNames)){

		# Get current sample.
		sampleData <- data[data$Sample==sampleNames[s],]

		# Check result type.
		if(all(is.na(sampleData$Allele))){
			# No result.

			res[s, ] <- c(sampleNames[s], "No result", "No result")
		
		} else if(max(table(sampleData$Marker))>2){
			# Mixture.

			markers <- length(unique(sampleData$Marker[!is.na(sampleData$Allele)]))
			if(!is.null(mixtureLimits)){
				for(t in rev(seq(along=mixtureLimits))){
					if(markers<=mixtureLimits[t]){
						subtype <- paste("<=", mixtureLimits[t], "markers")
					} else if(markers > mixtureLimits[length(mixtureLimits)]){
						subtype <- paste(">", mixtureLimits[length(mixtureLimits)], "markers")
					}
				}
			} else {
					subtype <- paste(markers, "markers")
			}
			res[s, ] <- c(sampleNames[s], "Mixture", subtype)
			
		} else if(any(is.na(sampleData$Allele))){
			# Partial profile.

			alleles <- sum(!is.na(sampleData$Allele))
			if(!is.null(partialLimits)){
				for(t in rev(seq(along=partialLimits))){
					if(alleles<=partialLimits[t]){
						subtype <- paste("<=", partialLimits[t], "peaks")
					} else if(alleles > partialLimits[length(partialLimits)]){
						subtype <- paste(">", partialLimits[length(partialLimits)], "peaks")
					}
				}
			} else {
					subtype <- paste(alleles, "peaks")
			}
			res[s, ] <- c(sampleNames[s], "Partial", subtype)
		

			# Check for subset.
			if(!is.null(markerSubset)){
				# Subset data.
				selectedMarkers <- grepl(markerSubset, sampleData$Marker)		
				if(all(!is.na(sampleData$Allele[selectedMarkers]))){
					# Full subset profile.
					res[s, ] <- c(sampleNames[s], "Partial" , paste("Complete", subsetName))
				}
			}
			
		} else if(!any(is.na(sampleData$Allele))){
			# Complete profile.
			res[s, ] <- c(sampleNames[s], "Complete profile", "Complete profile")

			# Check against threshold.
			if(!is.null(dropoutT) && all(sampleData$Height>dropoutT)){
				# Complete profile, all peaks > T.
				res[s, ] <- c(sampleNames[s], "Complete profile", paste("all peaks >", dropoutT))
			}

		}
	}

	# Construct factor levels.
	factorLabels <- NULL
	blankLabels <- NULL
	mixtureLabels <- NULL
	partialLabels <- NULL
	completeLabels <- NULL

	factorLabelsSub <- NULL
	blankLabelsSub <- NULL
	mixtureLabelsSub <- NULL
	partialLabelsSub <- NULL
	completeLabelsSub <- NULL
	
	# Partial Labels.

	if(!is.null(markerSubset)){
		partialLabelsSub <- c(partialLabelsSub, subsetName)
	}
	if(!is.null(partialLimits)){
		for(t in rev(seq(along=partialLimits))){
			if(t == length(partialLimits)){
				partialLabelsSub <- c(partialLabelsSub, paste(">", partialLimits[t], "peaks"))
			}
			partialLabelsSub <- c(partialLabelsSub, paste("<=", partialLimits[t], "peaks"))
		}
	}
	partialLabels <- "Partial"
	partialLabelsSub <- c("Partial", partialLabelsSub)

	# Mixture Labels.
	if(!is.null(mixtureLimits)){
		for(t in rev(seq(along=mixtureLimits))){
			if(t == length(mixtureLimits)){
				mixtureLabelsSub <- c(mixtureLabelsSub, paste(">", mixtureLimits[t], "markers"))
			}
			mixtureLabelsSub <- c(mixtureLabelsSub, paste("<=", mixtureLimits[t], "markers"))
		}
	}
	mixtureLabels <- "Mixture"
	mixtureLabelsSub <- c("Mixture", mixtureLabelsSub)

	# Complete Labels.
	if(!is.null(dropoutT)){
		completeLabelsSub <- c(completeLabelsSub, paste("all peaks >", dropoutT))
	}
	completeLabels <- "Complete profile"
	completeLabelsSub <- c(completeLabelsSub, "Complete profile")

	# Blank Labels.
	blankLabels <- "No result"
	blankLabelsSub <- "No result"

	# All factor labels.
	factorLabels <- c(mixtureLabels, completeLabels, partialLabels, blankLabels)
	factorLabelsSub <- c(mixtureLabelsSub, completeLabelsSub, partialLabelsSub, blankLabelsSub)

	# Assign factors.
	res$Type <- factor(res$Type, levels = factorLabels)
	res$Sub.Type <- factor(res$Sub.Type, levels = factorLabelsSub)

	return(res)
}

