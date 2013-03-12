################################################################################
# TODO LIST
# TODO: noMissing must also check that all expected peaks are in all loci.
# TODO: also, check perDye so that Lb can be calculated even if some loci is missing.
# TODO: check how to calc total Lb.
# TODO: calculate the distributions...

################################################################################
# CHANGE LOG
# 12: Roxygenized. Name changed from 'balanceSlim' to 'calculateBalance'.
# 11: new option option 'calculateNA'
# 11: Changed min/max to include given values.
# 11: Calculates MpH for homozygotes ('balanceSlim').
# 10: Implemented parameter 'perDye' in 'balanceSlim'. 'balance' deleted.
# 09: 'balanceSlim' is working.
# NB! To correctly calculate Hb one needs to know if it is heterozygotic or homozygotic and if a peak is missing.
# Especially if one use cut-off minHeight and maxHeight (ex. max=1000, and peaks 1200/900 --> false homozygote)

#' @title Calculate balance
#'
#' @description
#' \code{calculateBalance} calculates the inter and intra locus balance.
#'
#' @details
#' Takes (GM-formatted) data for samples as input (Must be filtered to contain the clean profile??)
#' Seem to give the same result filtered/not filtered). NB! Remove ladder
#' 
#' @param data a data frame containing at least 'Sample.Name', 'Marker', 'Height', Dye', and 'Zygosity'.
#' @param perSample logical, default TRUE calculates balance for each sample, 
#'  FALSE calculates the avrage across all samples.
#' @param lb string. 'prop' is defualt and locus balance is calculated proportionally
#'  in relation to the whole sample. 'norm' locus balance is normalised in relation to 
#'  the locus with the highest total peakheight.  
#' @param perDye logical, default is TRUE and locus balance is calculated within each dye.
#'  FALSE locus balance is calculated globally.
#' @param minHeight integer giving the lower bound.
#' @param maxHeight integer giving the upper bound.
#' 
#' @return data.frame with with columns 'Sample.Name', 'Marker', 'Hb', 'Lb', 'MpH'. 
#' Or 'Sample.Name','Marker','Hb.n', 'Hb.Mean', 'Hb.Sd', 'Hb.95','Lb.n', 'Lb.Mean', 'Lb.Sd'.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples 
#' data(ref1)
#' data(set1)
#' # Format data frame.
#' ref1 <- slim(data=ref1, fix=c("Sample.Name","Marker"), stack=c("Allele"))
#' 
#' # Indicate heterozygous / homozygous alleles.
#' ref1 <- calculateZygosity(ref1)
#'        
#' # Remove all except positive control samples.
#' set1 <- trim(data=set1, samples="PC")
#'        
#' # Format data frame.
#' set1 <- slim(data=set1, fix=c("Sample.Name","Marker"), stack=c("Allele","Height"))
#' 
#' # Filter out data matching alleles in reference.
#' set1 <- filterProfile(data=set1, ref=ref1)
#' 
#' # Add zygosity.
#' set1$Zygosity <- rep(ref1$Zygosity,8)
#' 
#' # Add dye. 
#' set1 <- addDye(data=set1, kit="ESX17")
#' 
#' # Calculate average balances.
#' calculateBalance(data=set1, perSample=FALSE)

calculateBalance <- function(data, perSample=TRUE, lb="prop", perDye=TRUE, minHeight=NULL, maxHeight=NULL){
  
  ## TODO: Fix error handling!
  if(perDye){
    # Keep track of dyes.
    if(is.null(data$Dye)){
      stop("'Dye' does not exist!")
    }
  }
  if(is.null(data$Zygosity)){
    stop("'Zygosity' does not exist!")
  }
  if(is.null(data$Height)){
    stop("'Height' does not exist!")
  }
  if(is.null(data$Sample.Name)){
    stop("'Sample.Name' does not exist!")
  }
  if(is.null(data$Marker)){
    stop("'Marker' does not exist!")
  }
  
  # Get the sample names.
  sampleNames <- unique(data$Sample.Name)
  
  # Get the marker names.
  markerNames <- unique(data$Marker)
  
  # Get hight columns.
  heightColumns <- grepl("Height",names(data))
  
  # Check data type of Height.
  if(typeof(data$Height)!="integer" & typeof(data$Height)!="double" ){
    # Convert to numeric.
    data$Height <- as.numeric(data$Height)
    
  }
  
  # Create empty result data frame with NAs.
  res <- data.frame(t(rep(NA,5)))
  # Add column names.
  names(res) <- c("Sample.Name","Marker","Hb","Lb","MpH")
  # Remove all NAs
  res <- res[-1,]
  
  # Loop through all samples.
  for (s in seq(along = sampleNames)) {
    
    # Initialise variables.
    markerSum <- vector()
    markerDye <- vector()
    hetBalance <- vector()
    mph <- vector()
    locusBalance <- vector()
    
    # Get sample name.
    cSampleName <- sampleNames[s]
    
    # Subset sample data.
    cSampleRows <- data$Sample.Name == cSampleName
    currentData <- data[cSampleRows,]
    
    # Loop through all markers.
    for (m in seq(along = markerNames)) {
      
      # Get marker name.
      cMarkerName <- markerNames[m]
      
      # Get rows for current marker.
      markerRows <- currentData$Marker==markerNames[m]
      
      # Filter high/low peaks.
      minOk <- TRUE
      if(!is.null(minHeight)){
        minOk <- currentData[markerRows,heightColumns] >= minHeight
      }
      maxOk <- TRUE
      if(!is.null(maxHeight)){
        maxOk <- currentData[markerRows,heightColumns] <= maxHeight
      }
      ok <- minOk & maxOk
      
      markerSum[m] <- sum(currentData[markerRows,heightColumns][ok], na.rm=TRUE)
      if(perDye){
        # Keep track of dyes.
        markerDye[m] <- currentData[markerRows, ]$Dye
      }
      
      # Count number of height values.
      nbOfPeaks <- sum(!is.na(currentData[markerRows,heightColumns][ok]))
      expPeaks <- currentData[markerRows,]$Zygosity
      
      # TODO: This is just a control until all is tested.			
      if(length(expPeaks)!=0 & length(unique(expPeaks))!=1){
        warning(paste("Zygosity not consistent for sample",cSampleName,"marker",cMarkerName))
      } else {
        expPeaks <- unique(expPeaks)
      }
      
      # Get min and max peak height.
      min <- min(currentData[markerRows,heightColumns][ok], na.rm=TRUE)
      max <- max(currentData[markerRows,heightColumns][ok], na.rm=TRUE)
      
      # Calculate Hb for two peak loci.
      if(expPeaks==2 && nbOfPeaks==2){
        
        # Heterozygote balance.
        hetBalance[m] <- min / max
        
        # Mean peak height.
        mph[m] <-  sum(min,max) / 2
        
      } else if(expPeaks==1 && nbOfPeaks==1){
        
        # Heterozygote balance.
        hetBalance[m] <- NA
        
        # Mean peak height.
        mph[m] <-  min / 2
        
        # TODO: This is just a control until all is tested.			
        if(min!=max){
          warning(paste("min!=max",cSampleName,"marker",cMarkerName))
        }
        
      } else {
        hetBalance[m] <- NA
        mph[m] <-  NA
      }
    }
    # TODO: noMissing must also check that all expected peaks are in all loci.
    # TODO: also, check perDye so that Lb can be calculated even if some loci is missing.
    
    # Check if missing markers.
    noMissing <- TRUE
    if(sum(markerSum==0) > 0) {  #### TODO OR: length(markerSum)<numberOfExpectedMarkers
      noMissing <- FALSE
    }
    if(perDye){
      # Get dye names.
      dyes <- unique(markerDye)
    }
    
    # Calculate inter locus balance.
    if(noMissing & lb=="norm"){
      if(perDye){
        for(d in seq(along=dyes)){
          lbTmp <- markerSum[markerDye==dyes[d]] / max(markerSum[markerDye==dyes[d]])
          locusBalance <- c(locusBalance, lbTmp)
        }
      } else {
        locusBalance <- markerSum / max(markerSum)
      }
    } else if(noMissing & lb=="prop"){
      if(perDye){
        for(d in seq(along=dyes)){
          lbTmp <- markerSum[markerDye==dyes[d]] / sum(markerSum[markerDye==dyes[d]])
          locusBalance <- c(locusBalance, lbTmp)
        }
      } else {
        locusBalance <- markerSum / sum(markerSum)
      }
    } else {
      locusBalance <- NA
    }
    
    # Save result in temporary data frame.
    tmp <- data.frame(Sample.Name = cSampleName,
                      Marker = markerNames,
                      Hb = hetBalance,
                      Lb = locusBalance,
                      MpH = mph)
    
    # Add result to data frame.
    res <- rbind(res, tmp)
    
  }
  
  if(perSample==FALSE) {
    
    lbN <- vector()
    lbMean <- vector()
    lbSd <- vector()
    hbN <- vector()
    hbMean <- vector()
    hbSd <- vector()
    hb95 <- vector()
    
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
    
    # Return result.
    return(resPerSample )
  }
  
  # Return result.
  return(res)
  
}