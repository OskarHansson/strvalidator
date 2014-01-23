################################################################################
# TODO LIST
# TODO: use string constants instead of hard coding.
# TODO: Add missing markers to samples IMPORTANT!.

################################################################################
# CHANGE LOG
# 22.01.2014: Fixed bug by adding check that 'Height' is numeric and convert.
# 15.01.2014: Fixed NA's when 'mixtureLimits' and 'partialLimits' is NULL.
# 15.01.2014: Added message to show progress.
# 03.11.2013: Added debug parameter and data check.
# <03.11.2013: Roxygenized.
# <03.11.2013: Added factors.
# <03.11.2013: Grouping of mixed results.
# <03.11.2013: Grouping of partial results.
# <03.11.2013: First version.

#' @title Calculate result type
#'
#' @description
#' \code{calculateResultType} calculate the result type for samples.
#'
#' @details
#' Calculates result types for samples in 'data'.
#' Defined types are: 'No result', 'Mixture', 'Partial', and 'Complete'.
#' Subtypes can be defined by parameters.
#' An integer passed to 'threshold' defines a subtype of 'Complete' "Complete profile all peaks >threshold".
#' An integer or vector passed to 'mixtureLimits' define subtypes of 'Mixture' "> [mixtureLimits] markers".
#' An integer or vector passed to 'partialLimits' define subtypes of 'Partial' "> [partialLimits] peaks".
#' A string with marker names separated by pipe (|) passed to 'markerSubset' and
#'  a string 'subsetName' defines a subtype of 'Partial' "Complete [subsetName]".
#'  
#' @param data a data frame containing at least the column 'Sample.Name'.
#' @param kit character string or integer defining the kit.
#' @param addMissingMarker logical, defualt is TRUE which adds missing markers.
#' @param threshold integer indicating the dropout threshold.
#' @param mixtureLimits integer or vector indicating subtypes of 'Mixture'.
#' @param partialLimits integer or vector indicating subtypes of 'Partial'.
#' @param subsetName string naming the subset of 'Complete'.
#' @param markerSubset string with marker names defining the subset of 'Complete'.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with columns 'Sample.Name','Type','Subtype'
#' 

calculateResultType <- function(data, kit=NULL, addMissingMarker=TRUE,
                                threshold=NULL, mixtureLimits=NULL,
                                partialLimits=NULL, subsetName=NA,
                                markerSubset=NULL, debug=FALSE){

  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("head(data)")
    print(head(data))
    print("threshold")
    print(threshold)
    print("mixtureLimits")
    print(mixtureLimits)
    print("partialLimits")
    print(partialLimits)
    print("subsetName")
    print(subsetName)
    print("markerSubset")
    print(markerSubset)
  }
  
  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!any(grepl("Sample.Name", names(data)))){
    stop("'data' must contain a column 'Sample.Name'",
         call. = TRUE)
  }
  
  if(!any(grepl("Marker", names(data)))){
    stop("'data' must contain a column 'Marker'",
         call. = TRUE)
  }
  if(!any(grepl("Allele", names(data)))){
    stop("'data' must contain a column 'Allele'",
         call. = TRUE)
  }
  
  # Check if slim format.  
  if(sum(grepl("Allele", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
  
  if(addMissingMarker){
    if(is.null(kit)){
      stop("'kit' must be provided if 'addMissingMarker' is TRUE",
           call. = TRUE)
    } else {
      if(is.na(getKit(kit=kit, what="Short.Name"))){
        stop(paste("'kit' does not exist", "\nAvailable kits:",
                   paste(getKit(), collapse=", ")), call. = TRUE)
      }
    }
  }

  # PREPARE -------------------------------------------------------------------
  
  if(addMissingMarker){
    # Add missing markers to samples.
    markers <- getKit(kit=kit, what="Marker")
    data <- addMarker(data=data, marker=markers, ignoreCase=TRUE, debug=debug)
  }
  
  if(!is.numeric(data$Height)){
    message("'Height' not numeric. Converting to numeric.")
    data$Height <- as.numeric(data$Height)
  }
  
  # CALCULATE -----------------------------------------------------------------
  # NB! Strings used for classification must be identical to the ones used to
  # create factors.
  
  # Get sample names.
	sampleNames <- unique(data$Sample.Name)

	# Create result data frame.
	res <- data.frame(matrix(NA, length(sampleNames), 3))
	# Add new column names.
	names(res) <- paste(c("Sample.Name","Type","Subtype"))  

	# Loop over all samples.
	for(s in seq(along=sampleNames)){

	  # Show progress.
	  message(paste("Calculate result type for sample (",
                  s, " of ", length(sampleNames), "): ", sampleNames[s], sep=""))
	  
		# Get current sample.
		sampleData <- data[data$Sample==sampleNames[s],]
    
    if(debug){
		  print("Current sample data:")
		  print(sampleData)
		}
		
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
					subtype <- paste("Mixture")
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
					subtype <- paste("Partial")
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
			if(!is.null(threshold) && all(sampleData$Height>threshold)){
				# Complete profile, all peaks > T.
				res[s, ] <- c(sampleNames[s], "Complete profile", paste("all peaks >", threshold))
			}

		}
	}

  # FACTORS -------------------------------------------------------------------
  
	# Construct factor levels in correct order.
  # NB! Strings must be identical to the ones used in classification.
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
		partialLabelsSub <- c(partialLabelsSub, paste("Complete", subsetName))
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
	if(!is.null(threshold)){
		completeLabelsSub <- c(completeLabelsSub, paste("all peaks >", threshold))
	}
	completeLabels <- "Complete profile"
	completeLabelsSub <- c(completeLabelsSub, "Complete profile")

	# Blank Labels.
	blankLabels <- "No result"
	blankLabelsSub <- "No result"

	# All factor labels.
	factorLabels <- c(mixtureLabels, completeLabels, partialLabels, blankLabels)
	factorLabelsSub <- c(mixtureLabelsSub, completeLabelsSub, partialLabelsSub, blankLabelsSub)

  if(debug){
    print("factorLabels")
    print(factorLabels)
    print("factorLabelsSub")
    print(factorLabelsSub)
  }
  
	# Assign factors.
	res$Type <- factor(res$Type, levels = factorLabels)
	res$Subtype <- factor(res$Subtype, levels = factorLabelsSub)

  if(debug){
    print("head(res):")
    print(head(res))
    print(paste("EXIT:", match.call()[[1]]))
  }
  
	return(res)
}

