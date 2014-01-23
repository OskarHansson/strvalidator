################################################################################
# TODO LIST
# TODO: optional percentile.
# TODO: calculate the distributions...
# TODO: Regression Analysis and construction of Hb bins...

################################################################################
# CHANGE LOG
# 08.01.2014: Filter data and only consider peaks matching reference.
# 08.01.2014: Fixed bug when two highest peaks are equal.
# 27.11.2013: Added stop for NA's in 'Dye'.
# 20.10.2013: Added calculations and column for size difference 'Delta', 'Hb.Min', 'Lb.Min'.
# 09.09.2013: Added parameter 'hb' to specify the definition of Hb.
# 26.07.2013: Removed parameters 'minHeight', 'maxHeight', 'matchSource' and related code.
# 04.06.2013: Added warning/stop for missing markers.
# 20.04.2013: Lb can be calculated per dye channel with no missing markers.
# 20.04.2013: Changes max/min to max1/max2 so can handle unfiltered data.
# 20.04.2013: If ref=NULL use guess 'ref' from 'data' and issue a warning.
# 20.04.2013: Fixed bug for homozygous: min/2 to max/2.
# 14.04.2013: Reworked the code:
#             Removed dependency of column 'Zygosity' by adding parameter 'ref'.

#' @title Calculate balance
#'
#' @description
#' \code{calculateBalance} calculates the inter and intra locus balance.
#'
#' @details
#' Calculates the inter and intra locus balance for a dataset.
#' Only peaks corresponding to reference alleles will be included in analysis
#' (does not require filtered data).
#' Also calculates the allele size difference between heterozygous alleles.
#' Takes 'slimmed' data for samples and references as input.
#' NB! Requires at least one row for each marker per sample, even if no data.
#' NB! 'X' and 'Y' will be handled as '1' and '2' respectively.
#' 
#' @param data a data frame containing at least
#'  'Sample.Name', 'Marker', 'Height', 'Allele', and Dye'.
#' @param perSample logical, default TRUE calculates balance for each sample, 
#'  FALSE calculates the average across all samples.
#' @param lb string. 'prop' is defualt and locus balance is calculated proportionally
#' 'norm' locus balance is normalised in relation to the locus with the highest total peakheight.
#' @param perDye logical, default is TRUE and locus balance is calculated within each dye.
#'  FALSE locus balance is calculated globally across all dyes.
#' @param hb numerical, definition of heterozygous balance. hb=1; HMW/LMW, hb=2; Max1(Ph)/Max2(Ph).
#' @param ignoreCase logical indicating if sample matching should ignore case.
#' 
#' @return data.frame with with columns 'Sample.Name', 'Marker', 'Delta', 'Hb', 'Lb', 'MPH'. 
#' Or 'Sample.Name', 'Marker', 'Delta.Mean',
#' 'Hb.n', 'Hb.Min', 'Hb.Mean', 'Hb.Sd', 'Hb.95',
#' 'Lb.n', 'Lb.Min', 'Lb.Mean', 'Lb.Sd'.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples 
#' data(ref2)
#' data(set2)
#' # Calculate average balances.
#' calculateBalance(data=set2, ref=ref2, perSample=FALSE)

calculateBalance <- function(data, ref, perSample=TRUE, lb="prop", perDye=TRUE,
                             hb=1, ignoreCase=TRUE, debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("data")
    print(str(data))
    print("ref")
    print(str(ref))
    print("perSample")
    print(perSample)
    print("lb")
    print(lb)
    print("perDye")
    print(perDye)
    print("hb")
    print(hb)
    print("ignoreCase")
    print(ignoreCase)
  }
  
  # Check data ----------------------------------------------------------------
  
  if(perDye){
    if(is.null(data$Dye)){
      stop("'Dye' does not exist!")
    }
    if(any(is.na(data$Dye))){
      stop("'Dye' contain NA!")
    }
  }
  
  if(is.null(data$Sample.Name)){
    stop("'Sample.Name' does not exist!")
  }

  if(is.null(data$Marker)){
    stop("'Marker' does not exist!")
  }
  
  if(!any(grepl("Allele", names(data)))){
    stop("'Allele' does not exist!")
  }
  
  if(!any(grepl("Height", names(data)))){
    stop("'Height' does not exist!")
  }
  
  # Check if slim format.  
  if(sum(grepl("Allele", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
  
  if(sum(grepl("Height", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
    
  if(is.null(ref$Sample.Name)){
      stop("'Sample.Name' does not exist in ref!")
  }
  
  if(is.null(ref$Marker)){
    stop("'Marker' does not exist in ref!")
  }
  
  if(!any(grepl("Allele", names(ref)))){
    stop("'Allele' does not exist in ref!")
  }

  # Check if slim format.  
  if(sum(grepl("Allele", names(ref))) > 1){
    stop("'ref' must be in 'slim' format",
         call. = TRUE)
  }
  
  # Check if all markers for all samples.
  testMarkers <- unique(data$Marker)
  testSamples <- unique(data$Sample.Name)
  for(s in seq(along=testSamples)){
    comp <- testMarkers %in% unique(data$Marker[data$Sample.Name == testSamples[s]])
    if(!all(comp)){
      stop(paste("Marker", testMarkers[!comp],
                 "is missing for sample", testSamples[s],"!"),
           call. = TRUE)
    }
  }

  # Prepare -------------------------------------------------------------------

  # Check data type of Height.
  if(typeof(data$Height)!="integer" & typeof(data$Height)!="double" ){
    message("'Height' not numeric. Converting to numeric.")
    # Convert to numeric.
    data$Height <- suppressWarnings(as.numeric(data$Height))
  }
  
  # Create empty result data frame with NAs.
  res <- data.frame(t(rep(NA,5)))
  # Add column names.
  names(res) <- c("Sample.Name","Marker","Hb","Lb","MPH")
  # Remove all NAs
  res <- res[-1,]
  
  # Get the sample names.
  sampleNames <- unique(ref$Sample.Name)

  # Analyse -------------------------------------------------------------------
  
  # Loop through all samples.
  for (r in seq(along = sampleNames)) {

    # get current sample name.
    subsetBy <- sampleNames[r]
    
    # Subset sample data.
    cSampleRows <- grepl(subsetBy, data$Sample.Name, ignore.case=ignoreCase)
    cSubsetData <- data[cSampleRows,]
    
    # Subset reference data.
    cReferenceRows <- grepl(subsetBy, ref$Sample.Name, ignore.case=ignoreCase)
    cSubsetRef <- ref[cReferenceRows,]
      
    # Get data for current subset.
    cRef <- cSubsetRef[cSubsetRef$Sample.Name == subsetBy, ]
    markerNames <- unique(cSubsetRef$Marker)
    cSampleNames <- unique(cSubsetData$Sample.Name)

    # Loop through all samples in subset.
    for(s in seq(along=cSampleNames)){

      # Initialise variables.
      markerPeakHeightSum <- vector()
      markerDye <- vector()
      delta <- vector()
      hetBalance <- vector()
      mph <- vector()
      locusBalance <- vector()
      
      # Current sample name.
      cSample <- cSampleNames[s]

      # Get data for current sample.
      cData <- cSubsetData[cSubsetData$Sample.Name == cSample, ]
      
      # Loop through all markers.
      for (m in seq(along = markerNames)) {
        
        # Get rows for current marker.
        markerRows <- cData$Marker == markerNames[m]
        markerRowsRef <- cRef$Marker == markerNames[m]
        
        if(perDye){
          # Keep track of dyes.
          markerDye[m] <- unique(cData$Dye[markerRows])
        }
        
        # Get reference alleles.
        expPeaks <- sum(!is.na(unique(cRef$Allele[markerRowsRef])))
        expAlleles <- unique(cRef$Allele[markerRowsRef])

        if(expPeaks != 1 && expPeaks != 2){
        
          msg <- paste("Expected peaks is not 1 or 2 in reference",
                        subsetBy,
                        "marker",
                        markerNames[m],
                        ". \nThis case is not handled and will result in 'NA'.",
                       collapse = " ")
          warning(msg)
          
          if(debug){
            print(msg)
          }
          
        }

        # Get heights.
        cHeights <- cData$Height[markerRows]

        # Get alleles.
        cAlleles <- cData$Allele[markerRows]

        # Filter against reference alleles.
        matchIndex <- cAlleles %in% expAlleles
        cAlleles <- cAlleles[matchIndex]
        cHeights <- cHeights[matchIndex]

        if(length(cAlleles) > 2){
          stop("More than two alleles after matching with reference sample!")
        }
        if(length(cHeights) > 2){
          stop("More than two heigths after matching with reference sample!")
        }
        
        # Sum peaks in current marker (NB! remember to filter first).
        markerPeakHeightSum[m] <- sum(cHeights)
        
        # Handle amelogenin.
        cAlleles <- gsub("X", "1", cAlleles, fixed=TRUE)
        cAlleles <- gsub("Y", "2", cAlleles, fixed=TRUE)

        # Get min and max peak height.
        if(!all(is.na(cHeights))){
          
          # Get highest peak height (first match in case equal height).
          indexMax1 <- which(cHeights == max(cHeights, na.rm=TRUE))[1]
          max1 <- cHeights[indexMax1]
          
          # Check if additional peak.
          if(length(cHeights[-indexMax1]) > 0){
            
            # Get second heighest peak height.
            max2 <- cHeights[-indexMax1]
            
          } else {
            
            max2 <- NA
            
          }
          
          # Get allele for highest peak.
          allele1 <- cAlleles[indexMax1]
          
          if(!is.na(max2)){
            
            # Get allele for second highest peak.
            allele2 <- cAlleles[-indexMax1]
            
          } else {
            
            allele2 <- NA
            
          }

        } else {
          
          max2 <- NA
          max1 <- NA
          allele1 <- NA
          allele2 <- NA
          
        }
        
        # Calculate Hb for two peak loci.
        nominator <- NA
        denominator <- NA
        if(expPeaks == 2){
          
          if(hb == 1){
          # High molecular weight over low molecular weigt.
            
            if(!is.na(allele1) && !is.na(allele2)){

              if(as.numeric(allele1) > as.numeric(allele2)){
                nominator <- max1
                denominator <- max2
              } else {
                nominator <- max2
                denominator <- max1
              }
              
            }
            
          } else if(hb == 2){
          # Highest peak over second highest peak.
            
            nominator <- max2
            denominator <- max1
            
          } else {
          # Not supported.
            
            nominator <- NA
            denominator <- NA
            
          }
          
          # Heterozygote balance.
          hetBalance[m] <- nominator / denominator

          # Mean peak height.
          mph[m] <-  sum(nominator, denominator) / 2
          
          # Difference in size between the alleles.
          delta[m] <- abs(as.numeric(allele1) - as.numeric(allele2))
          
        } else if(expPeaks == 1){
          
          # Heterozygote balance.
          hetBalance[m] <- NA
          
          # Mean peak height.
          mph[m] <-  max1 / 2
          
          # Difference in size between the alleles.
          delta[m] <- NA
            
        } else {
          hetBalance[m] <- NA
          mph[m] <-  NA
          delta[m] <- NA
        }

      }

      # Check ok dye channels.
      dyeOk <- logical(0)
      if(perDye){
        dyes <- unique(markerDye)
        for(d in seq(along=dyes)){
          # Channel is marked as ok if peaks in all markers in that channel.
          dyeOk[d] <- sum(markerPeakHeightSum[markerDye==dyes[d]] == 0) == 0
        }
      }
      
      # Check if missing markers.
      allMarkersOk <- TRUE
      if(sum(markerPeakHeightSum == 0) > 0) {
        allMarkersOk <- FALSE
      }

      if(debug){
        print("dyes")
        print(dyes)
        print("markerPeakHeightSum")
        print(markerPeakHeightSum)
        print("dyeOk")
        print(dyeOk)
      }
      
      
      # Calculate inter locus balance.
      if(lb=="norm"){
        if(perDye & any(dyeOk)){
          for(d in seq(along=dyes)){
            # Calculate per dye.
            if(dyeOk[d]){
              lbTmp <- markerPeakHeightSum[markerDye==dyes[d]] / max(markerPeakHeightSum[markerDye==dyes[d]])
            } else {
              lbTmp <- rep(NA, length(markerPeakHeightSum[markerDye==dyes[d]]))
            }
            # Here we must concatenate per dye.
            locusBalance <- c(locusBalance, lbTmp)
          }
        } else if (allMarkersOk) {
          # Calculate all at once.
          locusBalance <- markerPeakHeightSum / max(markerPeakHeightSum)
        } else {
          locusBalance <- rep(NA, length(markerPeakHeightSum))
        }
      } else if(lb=="prop"){
        if(perDye & any(dyeOk)){
          for(d in seq(along=dyes)){
            # Calculate per dye.
            if(dyeOk[d]){
              lbTmp <- markerPeakHeightSum[markerDye==dyes[d]] / sum(markerPeakHeightSum[markerDye==dyes[d]])
            } else {
              lbTmp <- rep(NA, length(markerPeakHeightSum[markerDye==dyes[d]]))
            }
            # Here we must concatenate per dye.
            locusBalance <- c(locusBalance, lbTmp)
          }
        } else if (allMarkersOk) {
          # Calculate all at once.
          locusBalance <- markerPeakHeightSum / sum(markerPeakHeightSum)
        } else {
          locusBalance <- rep(NA, length(markerPeakHeightSum))
        }
      } else {
        warning(paste("Invalid 'lb' (", lb, "). Lb will be NA."))
        locusBalance <- rep(NA, length(markerPeakHeightSum))
      }

      if(debug){
        print("cSample")
        print(cSample)
        print("markerNames")
        print(markerNames)
        print("hetBalance")
        print(hetBalance)
        print("locusBalance")
        print(locusBalance)
        print("mph")
        print(mph)
      }
      
      # Save result in temporary data frame.
      tmp <- data.frame(Sample.Name = cSample,
                        Marker = markerNames,
                        Delta = delta,
                        Hb = hetBalance,
                        Lb = locusBalance,
                        MPH = mph)
      
      # Add result to data frame.
      res <- rbind(res, tmp)

    }
    
  }
  
  if(perSample==FALSE) {
    
    lbN <- vector()
    lbMin <- vector()
    lbMean <- vector()
    lbSd <- vector()
    hbN <- vector()
    deltaMean <- vector()
    hbMin <- vector()
    hbMean <- vector()
    hbSd <- vector()
    hb95 <- vector()
    
    # Get the marker names.
    markerNames <- unique(res$Marker)
    
    # Loop through all markers.
    for (m in seq(along = markerNames)) {
      
      # Get marker name.
      cMarkerName <- markerNames[m]
      
      # Calculate means, n, and standard deviations.
      lbMin[m] <- suppressWarnings(min(res$Lb[res$Marker==cMarkerName], na.rm=TRUE))
      lbMean[m] <- mean(res$Lb[res$Marker==cMarkerName], na.rm=TRUE)
      lbSd[m] <- sd(res$Lb[res$Marker==cMarkerName], na.rm=TRUE)
      lbN[m] <- sum(!is.na(res$Lb[res$Marker==cMarkerName]))
      
      deltaMean[m] <- mean(res$Delta[res$Marker==cMarkerName], na.rm=TRUE)
      
      hbMin[m] <- suppressWarnings(min(res$Hb[res$Marker==cMarkerName], na.rm=TRUE))
      hbMean[m] <- mean(res$Hb[res$Marker==cMarkerName], na.rm=TRUE)
      hbSd[m] <- sd(res$Hb[res$Marker==cMarkerName], na.rm=TRUE)
      hbN[m] <- sum(!is.na(res$Hb[res$Marker==cMarkerName]))
      hb95[m] <- quantile(res$Hb[res$Marker==cMarkerName], 0.05, na.rm = TRUE)
      
    }
    
    # Create empty result data frame with NAs.
    resPerSample <- data.frame(t(rep(NA,13)))
    # Add column names.
    names(resPerSample) <- c("Sample.Name","Marker",
                             "Hb.n", "Delta.Min", "Hb.Min", "Hb.Mean", "Hb.Sd", "Hb.95",
                             "Lb.n", "Lb.Min", "Lb.Mean", "Lb.Sd")
    # Remove all NAs
    res <- res[-1,]
    
    # Save result in data frame.
    resPerSample <- data.frame(Sample.Name = "Total",
                               Marker = markerNames,
                               Hb.n = hbN,
                               Delta.Mean = deltaMean,
                               Hb.Min = hbMin,
                               Hb.Mean = hbMean,
                               Hb.Sd = hbSd,
                               Hb.95 = hb95,
                               Lb.n = lbN,
                               Lb.Min = lbMin,
                               Lb.Mean = lbMean,
                               Lb.Sd = lbSd,
                               stringsAsFactors = FALSE)
    
    if(debug){
      print(paste("EXIT:", match.call()[[1]]))
    }

    # Return result.
    return(resPerSample )
  }

  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  # Return result.
  return(res)
  
}
