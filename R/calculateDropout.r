################################################################################
# TODO LIST
# TODO: ...


################################################################################
# CHANGE LOG
# 18.07.2013: Fixed "OL" bug.
# 18.07.2013: Added 'debug' parameter, example, and text in details.
# 15.05.2013: Added check that 'Height' is numeric, convert if not.
# 13.04.2013: Rewritten function for 'slim' data only.
# <11.04.2013: Roxygenized and changed name from 'dropStat' to 'calculateDropout'.
# <11.04.2013: fixed bug in dropcount/heterozygote not correct.

#' @title Calculate drop-out events
#'
#' @description
#' \code{calculateDropout} calculate drop-out events (allele and locus) and records the surviving peak height.
#'
#' @details
#' Calculates drop-out events. In case of allele dropout the peak height of the surviving allele is given.
#' Homozygous alleles in the reference set can be either single or double notation (X or X X).
#' Markers present in the reference set but not in the data set will be added to the result.
#' NB! "Sample Names" in 'ref' must be unique 'core' name of replicate sample names in 'data'.
#' Use \code{checkSubset} to make sure subsetting works as intended.
#' 
#' @param data data frame in GeneMapper format containing at least a column 'Allele'.
#' @param ref data frame in GeneMapper format.
#' @param ignoreCase logical, default TRUE for case insensitive.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with columns 'Sample.Name', 'Marker', 'Allele', 'Height', 'Dropout',
#' 'Rfu', and 'Heterozygous'.
#' Dropout: 0 indicate no dropout, 1 indicate allele dropout, and 2 indicate locus dropout.
#' Rfu: height of surviving allele.
#' Heterozygous: 1 for heterozygous and 0 for homozygous.
#' 
#' @export true
#' @examples
#' data(set4)
#' data(ref4)
#' drop <- calculateDropout(data=set4, ref=ref4, ignoreCase=TRUE)


calculateDropout <- function(data, ref, ignoreCase=TRUE, debug=FALSE){
  
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
  # Check if numeric data.
  if(!is.numeric(data$Height)){
    warning("'Height' must be numeric. 'data' converted")
    data$Height <- as.numeric(data$Height)
  }
  
  # ANALYZE -------------------------------------------------------------------

  # Create vectors for temporary.
  samplesVec <- character()
  markersVec <- character()
  allelesVec <- character()
  heightsVec <- numeric()
  dropoutVec <- numeric()
  rfuVec <- numeric()
  hetVec <- numeric()
  samplesTmp <- NULL
  markersTmp <- NULL
  allelesTmp <- NULL
  heightsTmp <- NULL
  dropoutTmp <- NULL
  rfuTmp <- NULL
  hetTmp <- NULL
  
  # Get sample names.
  sampleNamesRef <- unique(ref$Sample.Name)
  
  # Loop through all reference samples.
  for(r in seq(along=sampleNamesRef)){

    # Select current subsets.
    if(ignoreCase){
      selectedSamples <- grepl(toupper(sampleNamesRef[r]),
                               toupper(data$Sample.Name))
      selectedRefs <- grepl(toupper(sampleNamesRef[r]),
                            toupper(ref$Sample.Name))
    } else {
      selectedSamples <- grepl(sampleNamesRef[r],
                               data$Sample.Name)
      selectedRefs <- grepl(sampleNamesRef[r],
                            ref$Sample.Name)
    }
    
    # Extract the results from the current reference sample.
    dataSubset <- data[selectedSamples, ]
    refSubset <- ref[selectedRefs, ]
    
    # Get markers for current ref sample.
    # NB! Needed for handling mixed typing kits.
    markers <- unique(refSubset$Marker)
    
    # Get sample names in subset.
    sampleNames <- unique(dataSubset$Sample.Name)
    
    # Loop through all individual samples.
    # NB! Needed for detection of locus dropout.
    for(s in seq(along=sampleNames)){
      
      # Extract the result for the current sample.
      selectedReplicate <- data$Sample.Name == sampleNames[s]
      dataSample <- data[selectedReplicate, ]
      
      # Loop through all markers.
      for(m in seq(along=markers)){    
        
        # Get reference alleles and calculate expected number of alleles.
        refAlleles <- unique(refSubset$Allele[refSubset$Marker == markers[m]])
        expected <- length(refAlleles)
        
        # Get sample alleles and peak heights.
        selectMarker <- dataSample$Marker == markers[m]
        dataAlleles <- dataSample$Allele[selectMarker]
        dataHeight <- dataSample$Height[selectMarker]

        # Get data mathcing ref alleles and heights.
        matching <- dataAlleles %in% refAlleles
        matchedAlleles <- dataAlleles[matching]
        observed <- length(matchedAlleles)
        peakHeight <- dataHeight[matching]

        if(debug){
          if(length(dataHeight) != length(dataAlleles)){
            print("dataAlleles")
            print(dataAlleles)
            print("dataHeight")
            print(dataHeight)
          }
        }
        
        # Count observed reference alleles.
        dropCount <- expected - observed
        
        # Handle locus dropout.
        if(observed == 0){
          
          allelesTmp <- NA
          heightsTmp <- NA
          records <- 1
          
        } else {
          
          allelesTmp <- matchedAlleles
          heightsTmp <- peakHeight
          records <- length(matchedAlleles)
          
        }
        # Uppdate vector.
        allelesVec <- c(allelesVec, allelesTmp)
        heightsVec <- c(heightsVec, heightsTmp)
        
        # Samples and markers.
        samplesTmp <- rep(sampleNames[s], records)
        markersTmp <- rep(markers[m], records)
        # Update vectors.
        samplesVec <- c(samplesVec, samplesTmp)
        markersVec <- c(markersVec, markersTmp)

        # Indicate zygosity (1-Heterozygote, 0-Homozygote).
        if(expected == 1){
          
          hetTmp <- rep(0, records)
          het=FALSE
          
        } else if(expected == 2){
          
          hetTmp <- rep(1, records)
          het=TRUE
          
        } else {
          
          warning(paste("Unhandled number of expected alleles (expected =",
                        expected,"in sample",  sampleNames[s]),
                  call. = TRUE, immediate. = FALSE, domain = NULL)
          
        }
        # Update vector.
        hetVec <- c(hetVec, hetTmp)
        
        # Indicate dropout:
        # 0 - for no dropout, 1 - for allele dropout, 2 - for locus dropout
        if(dropCount == 0){
          
          dropoutTmp <- rep(0, records)
          
        } else if(dropCount == 1 & het){
          
          dropoutTmp <- rep(1, records)
          
        } else if(dropCount == 1 & !het){
          
          dropoutTmp <- rep(2, records)
          
        } else if(dropCount == 2 & het){
          
          dropoutTmp <- rep(2, records)
          
        } else {
          
          warning(paste("Unhandled combination (dropCount =",
                        dropCount,", het =", het),
                  call. = TRUE, immediate. = FALSE, domain = NULL)
          
        }
        # Update vector.
        dropoutVec <- c(dropoutVec, dropoutTmp)

        # Store peak height of surviving allele, or NA.
        if(dropCount == 1 & observed > 0){
          
          rfuTmp <- peakHeight
          
        } else {
          
          rfuTmp <- rep(NA, records)
          
        }
        # Update vector.
        rfuVec <- c(rfuVec, rfuTmp)
                
        if(debug){
          nb <- vector()
          nb[1] <- length(samplesTmp)
          nb[2] <- length(markersTmp)
          nb[3] <- length(allelesTmp)
          nb[4] <- length(heightsTmp)
          nb[5] <- length(dropoutTmp)
          nb[6] <- length(rfuTmp)
          nb[7] <- length(hetTmp)
          
          if(!all(nb[1] == nb)){
            print("records")
            print(records)
            print("samplesTmp")
            print(samplesTmp)
            print("markersTmp")
            print(markersTmp)
            print("allelesTmp")
            print(allelesTmp)
            print("heightsTmp")
            print(heightsTmp)
            print("dropoutTmp")
            print(dropoutTmp)
            print("rfuTmp")
            print(rfuTmp)
            print("hetTmp")
            print(hetTmp)
          }
        }
        
      }

    }
    
  }
  
  if(debug){
    print("samplesVec")
    print(str(samplesVec))
    print("markersVec")
    print(str(markersVec))
    print("allelesVec")
    print(str(allelesVec))
    print("heightsVec")
    print(str(heightsVec))
    print("dropooutVec")
    print(str(dropoutVec))
    print("rfuVec")
    print(str(rfuVec))
    print("hetVec")
    print(str(hetVec))
  }

  # Create return dataframe.
  dataDrop <- data.frame(Sample.Name=samplesVec,
                         Marker=markersVec,
                         Allele=allelesVec,
                         Height=heightsVec,
                         Dropout=dropoutVec,
                         Rfu=rfuVec,
                         Heterozygous=hetVec, 
                         stringsAsFactors=FALSE)

  return(dataDrop)
  
}
