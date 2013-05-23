################################################################################
# TODO LIST
# TODO: Autodetect if data has not been filtered and warn. 
# TODO: check how to calc total Lb when using min/max. Should give NA is a 
#       peak has been removed in a color/sample.
# TODO: calculate the distributions...

################################################################################
# CHANGE LOG
# 20.04.2013: Lb can be calculated per dye channel with no missing markers.
# 20.04.2013: Changes max/min to max1/max2 so can handle unfiltered data.
# 20.04.2013: If ref=NULL use guess 'ref' from 'data' and issue a warning.
# 20.04.2013: Fixed bug for homozygous: min/2 to max/2.
# 14.04.2013: Reworked the code:
#             Removed dependency of column 'Zygosity' by adding parameter 'ref'.
# <14.04.2013: Roxygenized. Name changed from 'balanceSlim' to 'calculateBalance'.
# <14.04.2013: new option option 'calculateNA'
# <14.04.2013: Changed min/max to include given values.
# <14.04.2013: Calculates MpH for homozygotes ('balanceSlim').
# <14.04.2013: Implemented parameter 'perDye' in 'balanceSlim'. 'balance' deleted.
# <14.04.2013: 'balanceSlim' is working.

#' @title Calculate balance
#'
#' @description
#' \code{calculateBalance} calculates the inter and intra locus balance.
#'
#' @details
#' Calculates the inter and intra locus balance for a filtered dataset.
#' Takes 'slimmed' data for samples and references as input.At the moment
#' it is better to discard data prior to analysis than to use min/maxHeight.
#' 
#' @param data a data frame containing at least
#'  'Sample.Name', 'Marker', 'Height', 'Allele', and Dye'.
#' @param perSample logical, default TRUE calculates balance for each sample, 
#'  FALSE calculates the average across all samples.
#' @param lb string. 'prop' is defualt and locus balance is calculated proportionally
#'  in relation to the whole sample. 'norm' locus balance is normalised in relation to 
#'  the locus with the highest total peakheight.  
#' @param perDye logical, default is TRUE and locus balance is calculated within each dye.
#'  FALSE locus balance is calculated globally.
#' @param minHeight integer giving the lower bound.
#' @param maxHeight integer giving the upper bound.
#' @param ignoreCase logical indicating if sample matching should ignore case.
#' @param matchSource string. Use 'ref' for wildcard matching of unique sample
#'  names in 'ref' with sample names in 'data' (e.g. 'F' match 'F1' and 'AFG').
#'  Use 'data' for exact matching of unique sample names in 'data' with sample
#'  names in 'ref' (e.g. 'F' match 'F' but not 'F1' or 'AFG').
#' 
#' @return data.frame with with columns 'Sample.Name', 'Marker', 'Hb', 'Lb', 'MpH'. 
#' Or 'Sample.Name','Marker','Hb.n', 'Hb.Mean', 'Hb.Sd', 'Hb.95','Lb.n', 'Lb.Mean', 'Lb.Sd'.
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
                             minHeight=NULL, maxHeight=NULL,
                             ignoreCase=TRUE, matchSource="ref", debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Check data ----------------------------------------------------------------
  
  if(perDye){
    # Keep track of dyes.
      if(is.null(data$Dye)){
      stop("'Dye' does not exist!")
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
  
  # Prepare -------------------------------------------------------------------

  # Check data type of Height.
  if(typeof(data$Height)!="integer" & typeof(data$Height)!="double" ){
    warning("'Height' not numeric. Converting to numeric.")
    # Convert to numeric.
    data$Height <- as.numeric(data$Height)
  }
  
  # Create empty result data frame with NAs.
  res <- data.frame(t(rep(NA,5)))
  # Add column names.
  names(res) <- c("Sample.Name","Marker","Hb","Lb","MpH")
  # Remove all NAs
  res <- res[-1,]
  
  # Get the sample names.
  if(matchSource=="data"){
    sampleNames <- unique(data$Sample.Name)
  }
  if(matchSource=="ref"){
    sampleNames <- unique(ref$Sample.Name)
  }

  # Analyse -------------------------------------------------------------------
  
  # Loop through all samples.
  for (r in seq(along = sampleNames)) {

    # Get sample name.
    subsetBy <- sampleNames[r]
    
    # Subset sample data.
    if(matchSource=="data"){
      if(ignoreCase){
        cSampleRows <- toupper(data$Sample.Name) == toupper(subsetBy)
      } else {
        cSampleRows <- data$Sample.Name == subsetBy
      }
      cSubsetData <- data[cSampleRows,]
    }
    if(matchSource=="ref"){
      if(ignoreCase){
        cSampleRows <- grepl(toupper(subsetBy), toupper(data$Sample.Name))
      } else {
        cSampleRows <- grepl(subsetBy, data$Sample.Name)
      }
      cSubsetData <- data[cSampleRows,]
    }
    
    # Subset reference data.
    if(matchSource=="data"){
      if(ignoreCase){
        cReferenceRows <- toupper(ref$Sample.Name) == toupper(subsetBy)
      } else {
        cReferenceRows <- ref$Sample.Name == subsetBy
      }
      cSubsetRef <- ref[cReferenceRows,]
    }
    if(matchSource=="ref"){
      if(ignoreCase){
        cReferenceRows <- grepl(toupper(subsetBy), toupper(ref$Sample.Name))
      } else {
        cReferenceRows <- grepl(subsetBy, ref$Sample.Name)
      }
      cSubsetRef <- ref[cReferenceRows,]
    }
      
    # Get data for current subset.
    cRef <- cSubsetRef[cSubsetRef$Sample.Name == subsetBy, ]
    markerNames <- unique(cSubsetRef$Marker)
    cSampleNames <- unique(cSubsetData$Sample.Name)

    # Loop through all samples in subset.
    for(s in seq(along=cSampleNames)){

      # Initialise variables.
      markerPeakHeightSum <- vector()
      markerDye <- vector()
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
        
        # Filter high/low peaks.
        minOk <- TRUE
        if(!is.null(minHeight)){
          minOk <- cData$Height[markerRows] >= minHeight
        }
        maxOk <- TRUE
        if(!is.null(maxHeight)){
          maxOk <- cData$Height[markerRows] <= maxHeight
        }
        ok <- minOk & maxOk
        
        markerPeakHeightSum[m] <- sum(cData$Height[markerRows][ok], na.rm=TRUE)
        if(perDye){
          # Keep track of dyes.
          markerDye[m] <- unique(cData$Dye[markerRows])
        }
        
        # Count number of height values.
        nbOfPeaks <- sum(!is.na(cData$Height[markerRows][ok]))
        expPeaks <- sum(!is.na(unique(cRef$Allele[markerRowsRef])))
        
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
        cHeights <- cData$Height[markerRows][ok]

        # Get min and max peak height.
        if(!all(is.na(cHeights))){
          max1 <- max(cHeights, na.rm=TRUE)
          remainingPeaks <- cHeights[cHeights!=max1]
          if(!all(is.na(remainingPeaks))){
            max2 <- max(cHeights[cHeights!=max1], na.rm=TRUE)
          } else {
            max2 <- NA
          }
        } else {
          max2 <- NA
          max1 <- NA
        }

        # Calculate Hb for two peak loci.
        if(expPeaks == 2){
            
          # Heterozygote balance.
          hetBalance[m] <- max2 / max1
          
          # Mean peak height.
          mph[m] <-  sum(max2,max1) / 2
          
        } else if(expPeaks == 1){
          
          # Heterozygote balance.
          hetBalance[m] <- NA
          
          # Mean peak height.
          mph[m] <-  max1 / 2
          
        } else {
          hetBalance[m] <- NA
          mph[m] <-  NA
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
                        Hb = hetBalance,
                        Lb = locusBalance,
                        MpH = mph)
      
      # Add result to data frame.
      res <- rbind(res, tmp)

    }
    
  }
  
  if(perSample==FALSE) {
    
    lbN <- vector()
    lbMean <- vector()
    lbSd <- vector()
    hbN <- vector()
    hbMean <- vector()
    hbSd <- vector()
    hb95 <- vector()
    
    # Get the marker names.
    markerNames <- unique(res$Marker)
    
    # Loop through all markers.
    for (m in seq(along = markerNames)) {
      
      # Get marker name.
      cMarkerName <- markerNames[m]
      
      # Sum Lb per marker over all samples.
      lbMean[m] <- mean(res$Lb[res$Marker==cMarkerName], na.rm=TRUE)
      lbSd[m] <- sd(res$Lb[res$Marker==cMarkerName], na.rm=TRUE)
      lbN[m] <- sum(!is.na(res$Lb[res$Marker==cMarkerName]))
      
      hbMean[m] <- mean(res$Hb[res$Marker==cMarkerName], na.rm=TRUE)
      hbSd[m] <- sd(res$Hb[res$Marker==cMarkerName], na.rm=TRUE)
      hbN[m] <- sum(!is.na(res$Hb[res$Marker==cMarkerName]))
      hb95[m] <- quantile(res$Hb[res$Marker==cMarkerName], 0.05, na.rm = TRUE)
      
    }
    
    # Create empty result data frame with NAs.
    resPerSample <- data.frame(t(rep(NA,10)))
    # Add column names.
    names(resPerSample) <- c("Sample.Name","Marker",
                             "Hb.n", "Hb.Mean", "Hb.Sd", "Hb.95",
                             "Lb.n", "Lb.Mean", "Lb.Sd")
    # Remove all NAs
    res <- res[-1,]
    
    # Save result in data frame.
    resPerSample <- data.frame(Sample.Name = "Total",
                               Marker = markerNames,
                               Hb.n = hbN,
                               Hb.Mean = hbMean,
                               Hb.Sd = hbSd,
                               Hb.95 = hb95,
                               Lb.n = lbN,
                               Lb.Mean = lbMean,
                               Lb.Sd = lbSd)
    
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
