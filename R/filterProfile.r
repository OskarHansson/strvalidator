################################################################################
# TODO LIST
# TODO: Test if 'fat' warning is working.

################################################################################
# CHANGE LOG
# 28.04.2013: Fixed "NA" bug (NA worked but not "NA").
# 15.04.2013: Option 'ignoreCase'.
# 12.04.2013: Options 'keepNA' and 'addMissingLoci' implemented as 'slow' method. 
# 11.04.2013: Fixed bug when more than one reference sample.
# 11.04.2013: Added debug, datachecks and remove NA alleles.
# <11.04.2013: Roxygenized.
# <11.04.2013: filter profile using data in slim format (faster).

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
#' @param keepNA logical. FALSE discards NA alleles.
#' @param addMissingLoci logical.
#' @param ignoreCase logical TRUE ignore case.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with extracted result.
#' 

filterProfile <- function(data, ref, addMissingLoci=FALSE,
                          keepNA=FALSE, ignoreCase=TRUE, debug=FALSE){

  if(debug){
    print(paste("IN:", match.call()[[1]]))
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

  if(!any(grepl("Height", names(data)))){
    stop("'data' must contain a column 'Height'",
         call. = TRUE)
  }

  # Check reference dataset.
  if(!any(grepl("Sample.Name", names(ref)))){
    stop("'ref' must contain a column 'Sample.Name'",
         call. = TRUE)
  }
  if(!any(grepl("Marker", names(ref)))){
    stop("'ref' must contain a column 'Marker'",
         call. = TRUE)
  }
  if(!any(grepl("Allele", names(ref)))){
    stop("'ref' must contain a column 'Allele'",
         call. = TRUE)
  }
  
  # Check if slim format.
  if(sum(grepl("Allele", names(ref))>1)){
    stop("'ref' must be in 'slim' format",
         call. = TRUE)
  }
  if(sum(grepl("Allele", names(data))>1)){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }

  # Check if character data.
  if(!is.character(ref$Allele)){
    warning("'Allele' must be character. 'ref' converted")
    data$Allele <- as.character(data$Allele)
  }
  if(!is.character(data$Allele)){
    warning("'Allele' must be character. 'data' converted")
    data$Allele <- as.character(data$Allele)
  }

  
  # SELECT METHOD -------------------------------------------------------------
  
  if(!addMissingLoci & !keepNA){
  
    # 'FAST' METHOD -----------------------------------------------------------
    # NB! Discards all NA alleles/loci.
    
    if(debug){
      print("'fast' method")
    }

    # Get reference names.
    refSampleNames<- unique(ref$Sample.Name)

    # Clean NA (both 'false' and true NA).
    naAllele <- length(data$Allele[data$Allele=="NA"])
    if(naAllele > 0){
      data$Allele[data$Allele=="NA"] <- NA
      message(paste(naAllele, "\"NA\" in 'Allele' converted to NA"))
    }
    naAllele <- sum(is.na(data$Allele))
    if(naAllele > 0){
      data <- data[!is.na(data$Allele),]
      message(paste("Removed", naAllele, "rows where Allele=<NA>"))
    }
    
    if(debug){
      print("ref samples:")
      print(refSampleNames)
      print("data samples:")
      print(unique(data$Sample.Name))
      flush.console()
    }
    
  	# Initiate boolean match vector to FALSE.
  	matchingData <- is.na(data$Sample.Name)
  
  	# Loop through all reference samples.
  	for(s in seq(along=refSampleNames)){
  
  	  # Get current reference subset.
  	  currentRef <- ref[ref$Sample.Name==refSampleNames[s],]
  	  
      # Select current subset.
  		if(ignoreCase){
  		  selectedSamples <- grepl(toupper(refSampleNames[s]),
                                 toupper(data$Sample.Name))
  		} else {
  		  selectedSamples <- grepl(refSampleNames[s], data$Sample.Name)
  		}
  	  
  		# Get current marker.
  		refMarkers <- unique(currentRef$Marker)
  	
  		# Loop through all markers.
  		for(m in seq(along=refMarkers)){
  
  			# Get reference alleles.
  			refAlleles <- currentRef$Allele[currentRef$Marker==refMarkers[m]]
  
  			# Loop through all alleles.
  			for(a in seq(along=refAlleles)){
  
          
  			  # Get matching alleles in data.
  				mM <- data$Marker==refMarkers[m]
  				#mM[is.na(mM)]<-FALSE
  				mA <- data$Allele==refAlleles[a]
  				#mA[is.na(mA)]<-FALSE
  				currentMatch <- selectedSamples & mM & mA
  				
          # 'Concatenate' booleans
  				matchingData <- matchingData | currentMatch
          
  			}
  		}
  	}

    # Create return data frame.
    resDf <- data[matchingData, ]
    
  } else {
    
    # 'SLOW' METHOD -----------------------------------------------------------
    # NB! Possible to keep NA alleles and add missing loci.
    
    if(debug){
      print("'slow' method")
    }

    # Get reference names.
    refSampleNames<- unique(ref$Sample.Name)
    
    # Create result vectors.
    resSample <- character(0)
    resMarker <- character(0)
    resAllele <- character(0)
    resHeight <- numeric(0)
    
    # Loop through all reference samples.
    for(r in seq(along=refSampleNames)){
      
      # Get current reference subset.
      currentRef <- ref[ref$Sample.Name==refSampleNames[r],]

      # Get current data subset.
      # Select current subset.
      if(ignoreCase){
        selectedSamples <- grepl(toupper(refSampleNames[r]),
                                 toupper(data$Sample.Name))
      } else {
        selectedSamples <- grepl(refSampleNames[r], data$Sample.Name)
      }
      currentDataSubset <- data[selectedSamples, ]

      # Get sample names.
      dataSampleNames<- unique(currentDataSubset$Sample.Name)
      
      # Get current marker.
      refMarkers <- unique(currentRef$Marker)
      
      for(s in seq(along=dataSampleNames)){

        # Get current sample
        currentData <- currentDataSubset[currentDataSubset$Sample.Name == dataSampleNames[s], ]

        # Loop through all markers.
        for(m in seq(along=refMarkers)){
          
          # Get reference alleles.
          refAlleles <- currentRef$Allele[currentRef$Marker==refMarkers[m]]

          # Get 
          dataAlleles <- currentData$Allele[currentData$Marker==refMarkers[m]]
          dataHeight <- currentData$Height[currentData$Marker==refMarkers[m]]
          
          # dataAlleles is of length 0 if no matching marker.
          if(length(dataAlleles) == 0 & addMissingLoci){
            
            # Add missing marker and set NA in allele.      
            resSample <- c(resSample, dataSampleNames[s])
            resMarker <- c(resMarker, refMarkers[m])
            resAllele <- c(resAllele, NA)
            resHeight <- c(resHeight, NA)
            if(debug){
              print(paste("missing marker added:", refMarkers[m]))
            }
          }
          
          # dataAlleles is of length >0 if there are alleles.
          if(length(dataAlleles) > 0){
          
            # Filter allales.
            selected <- dataAlleles %in% refAlleles
            matching <- dataAlleles[selected]
            height <- dataHeight[selected]
            
            # matching is of length 0 if no matching allele.
            if(length(matching) == 0 & keepNA){
              # Setting matching=NA causes the below loop to be executed once.
              matching <- NA
              
              if(debug){
                print(paste("NA kept for marker", refMarkers[m]))
              }

            }
            
            # Loop through all alleles.
            for(a in seq(along=matching)){
              
              # Add to result.
              resSample <- c(resSample, dataSampleNames[s])
              resMarker <- c(resMarker, refMarkers[m])
              resAllele <- c(resAllele, matching[a])
              resHeight <- c(resHeight, height[a])
              
            }
          }
        }
      }
    }

    # Create return data frame.
    resDf <- data.frame(Sample.Name=resSample,
                        Marker=resMarker,
                        Allele=resAllele,
                        Height=as.numeric(resHeight),
                        stringsAsFactors=FALSE)
    
  }
  
  # RETURN --------------------------------------------------------------------
  
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
	return(resDf)
  
}
