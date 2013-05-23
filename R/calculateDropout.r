################################################################################
# TODO LIST
# TODO: change to ggplot2 perform lm and use add CI all-in-one?


################################################################################
# CHANGE LOG
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
#' NB! "Sample Names" in 'ref' must be unique 'core' name of replicate sample names in 'data'.
#' NB! Homozygous alleles must be doubled (X -> X/X).
#' 
#' @param data data frame in GeneMapper format containing at least a column 'Allele'.
#' @param ref data frame in GeneMapper format.
#' @param ignoreCase logical, default TRUE for case insensitive.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with columns 'Dropout', indicating no dropout (0), 
#' allele (1) and locus dropout (2), and 'Rfu', 'Heterozygous'.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' #newData<-calculateDropout(data=data, ref=ref)


calculateDropout <- function(data, ref, ignoreCase=TRUE){
  
  debug <- FALSE
  
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
        dataAlleles <- unique(dataSample$Allele[selectMarker])
        dataHeight <- dataSample$Height[selectMarker]

        # Get data mathcing ref alleles and heights.
        matching <- dataAlleles %in% refAlleles
        matchedAlleles <- dataAlleles[matching]
        observed <- length(matchedAlleles)
        peakHeight <- dataHeight[matching]

        # Count observed reference alleles.
        dropCount <- expected - observed
        
        # Handle locus dropout.
        if(observed == 0){
          
          allelesVec <- c(allelesVec, NA)
          heightsVec <- c(heightsVec, NA)
          records <- 1
          
        } else {
          
          allelesVec <- c(allelesVec, matchedAlleles)
          heightsVec <- c(heightsVec, peakHeight)
          records <- length(matchedAlleles)
          
        }
        
        # Add to result vectors.
        samplesVec <- c(samplesVec, rep(sampleNames[s], records))
        markersVec <- c(markersVec, rep(markers[m], records))
        
        # Indicate zygosity (1-Heterozygote, 0-Homozygote).
        if(expected == 1){
          
          hetVec<- c(hetVec, rep(0, records))
          het=FALSE
          
        } else if(expected == 2){
          
          hetVec <- c(hetVec, rep(1, records))
          het=TRUE
          
        } else {
          
          warning(paste("Unhandled number of expected alleles (expected =",
                        expected,"in sample",  sampleNames[s]),
                  call. = TRUE, immediate. = FALSE, domain = NULL)
          
        }
        
        # Indicate dropout:
        # 0 - for no dropout, 1 - for allele dropout, 2 - for locus dropout
        if(dropCount == 0){
          
          dropoutVec <- c(dropoutVec, rep(0, records))
          
        } else if(dropCount == 1 & het){
          
          dropoutVec<- c(dropoutVec, rep(1, records))
          
        } else if(dropCount == 1 & !het){
          
          dropoutVec <- c(dropoutVec, rep(2, records))
          
        } else if(dropCount == 2 & het){
          
          dropoutVec <- c(dropoutVec, rep(2, records))
          
        } else {
          
          warning(paste("Unhandled combination (dropCount =",
                        dropCount,", het =", het),
                  call. = TRUE, immediate. = FALSE, domain = NULL)
          
        }
        
        # Store peak height of surviving allele, or NA.
        if(dropCount == 1 & observed > 0){
          
          rfuVec <- c(rfuVec, peakHeight)
          
        } else {
          
          rfuVec <- c(rfuVec, rep(NA, records))
          
        }
        
      }

    }
    
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
