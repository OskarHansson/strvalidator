################################################################################
# TODO LIST
# TODO: ...

# NOTE: Column names used for calculations with data.table is declared
# in globals.R to avoid NOTES in R CMD CHECK.

################################################################################
# CHANGE LOG (last 20 changes)
# 24.08.2018: Removed unused variables.
# 06.08.2017: Added audit trail.
# 23.05.2016: Changed name on some result columns.
# 22.05.2016: Added masked data to result for manual investigation.
# 20.05.2016: 'Blocked' changed to 'masked' throughout.
# 20.05.2016: Implemented method 7 (assumes log-normal distribution).
# 28.02.2016: Added check for ILS dye.
# 09.01.2016: Added more attributes to result.
# 21.10.2015: Added attributes.
# 06.10.2015: Added importFrom for data.table
# 28.08.2015: Added importFrom
# 26.06.2015: Added global AT per dye.
# 26.06.2015: Fixed hard-coded kit/dye set.
# 03.05.2015: First version.

#' @title Calculate Analytical Threshold
#'
#' @description
#' Calculate analytical thresholds estimates.
#'
#' @details
#' Calculate the analytical threshold (AT) according to method 1, 2, and 4 as
#' recommended in the reference by analyzing the background signal (noise).
#' In addition method 7, a log-normal version of method 1 has been implemented.
#' Method 1: The average signal + 'k' * the standard deviation.
#' Method 2: The percentile rank method. The percentage of noise peaks below 'rank.t'.
#' Method 4: Utilize the mean and standard deviation and the critical value obtained 
#' from the t-distribution for confidence interval 'alpha' (one-sided) and observed
#' peaks analyzed (i.e. not masked) minus one as degrees of freedom, and the number
#' of samples.
#' Method 7: The average natural logarithm of the signal + k * the standard deviation.
#' 
#' If samples containing DNA are used, a range around the allelic peaks can be masked
#' from the analysis to discard peaks higher than the noise. Masking can be within
#' each dye or across all dye channels.
#' Similarly a range around the peaks of the internal lane standard (ILS) can be 
#' masked across all dye channels. Which can bleed-through in week samples
#' (i.e. negative controls)
#' The mean, standard deviation, and number of peaks are calculated per dye per sample,
#' per sample, globally across all samples, and globally across all samples per dye,
#' for each method to estimate AT. Also the complete percentile rank list is calculated.
#' 
#' @param data a data frame containing at least 'Dye.Sample.Peak',
#'  'Sample.File.Name', 'Marker', 'Allele', 'Height', and 'Data.Point'.
#' @param ref a data frame containing at least
#'  'Sample.Name', 'Marker', 'Allele'.
#' @param mask.height logical to indicate if high peaks should be masked.
#' @param height integer for global lower peak height threshold for peaks
#' to be excluded from the analysis. Active if 'mask.peak=TRUE.
#' @param mask.sample logical to indicate if sample allelic peaks should be masked.
#' @param per.dye logical TRUE if sample peaks should be masked per dye channel.
#' FALSE if sample peaks should be masked globally across dye channels.
#' @param range.sample integer to specify the masking range in (+/-) data points.
#' Active if mask.sample=TRUE.
#' @param mask.ils logical to indicate if internal lane standard peaks should be masked.
#' @param range.ils integer to specify the masking range in (+/-) data points.
#' Active if mask.ils=TRUE.
#' @param k numeric factor for the desired confidence level (method AT1).
#' @param alpha numeric one-sided confidence interval to obtain the
#' critical value from the t-distribution (method AT4).
#' @param rank.t numeric percentile rank threshold (method AT2).
#' @param ignore.case logical to indicate if sample matching should ignore case.
#' @param word logical to indicate if word boundaries should be added before sample matching.
#' @param debug logical to indicate if debug information should be printed.
#' 
#' @return list of three data frames. The first with result per dye per sample,
#'  per sample, globally across all samples, and globally across all samples
#'  per dye, for each method. The second is the complete percentile rank list.
#'  The third is the masked raw data used for calculation to enable manual
#'  check of the result.
#' 
#' @export
#' 
#' @importFrom stats sd qt
#' @importFrom utils str head tail
#' @importFrom data.table data.table setnames
#' 
#' @seealso \code{\link{maskAT}}, \code{\link{checkSubset}}
#' 
#' @references
#'  J. Bregu et.al.,
#'   Analytical thresholds and sensitivity: establishing RFU thresholds for
#'   forensic DNA analysis, J. Forensic Sci. 58 (1) (2013) 120-129,
#'   ISSN 1556-4029, DOI: 10.1111/1556-4029.12008.
#' \url{http://onlinelibrary.wiley.com/doi/10.1111/1556-4029.12008/abstract}
#' 
#' 

calculateAT <- function(data, ref=NULL, mask.height=TRUE, height=500,
                        mask.sample=TRUE, per.dye = TRUE, range.sample=20,
                        mask.ils=TRUE, range.ils=10,
                        k=3, rank.t=0.99, alpha=0.01,
                        ignore.case=TRUE, word=FALSE, debug=FALSE){
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("Parameters:")
    print("data")
    print(str(data))
    print("ref")
    print(str(ref))
    print("mask.height")
    print(mask.height)
    print("height")
    print(height)
    print("mask.sample")
    print(mask.sample)
    print("per.dye")
    print(per.dye)
    print("range.sample")
    print(range.sample)
    print("mask.ils")
    print(mask.ils)
    print("range.ils")
    print(range.ils)
    print("k")
    print(k)
    print("rank.t")
    print(rank.t)
    print("alpha")
    print(alpha)
    print("ignore.case")
    print(ignore.case)
    print("word")
    print(word)
  }
  
  # Check data ----------------------------------------------------------------
  
  if(is.null(data$Dye.Sample.Peak)){
    stop("'data' must contain a column 'Dye.Sample.Peak'")
  }
  
  if(is.null(data$Sample.File.Name)){
    stop("'data' must contain a column 'Sample.File.Name'")
  }
  
  if(is.null(data$Marker)){
    stop("'data' must contain a column 'Marker'")
  }
  
  if(is.null(data$Allele)){
    stop("'data' must contain a column 'Allele'")
  }
  
  if(is.null(data$Height)){
    stop("'data' must contain a column 'Height'")
  }
  
  if(is.null(data$Data.Point)){
    stop("'data' must contain a column 'Data.Point'")
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
  
  if(sum(grepl("Data.Point", names(data))) > 1){
    stop("'data' must be in 'slim' format",
         call. = TRUE)
  }
  
  if(!is.null(ref)){
    
    if(is.null(ref$Sample.Name)){
      stop("'ref' must contain a column 'Sample.Name'")
    }
    
    if(is.null(ref$Marker)){
      stop("'ref' must contain a column 'Marker'")
    }
    
    if(is.null(ref$Allele)){
      stop("'ref' must contain a column 'Allele'")
    }
    
    # Check if slim format.  
    if(sum(grepl("Allele", names(ref))) > 1){
      stop("'ref' must be in 'slim' format",
           call. = TRUE)
    }
    
  }
  
  # Check parameters.  
  if(!is.logical(mask.height)){
    stop("'mask.height' must be logical",
         call. = TRUE)
  }
  
  if(!is.numeric(height)){
    stop("'height' must be numeric",
         call. = TRUE)
  }
  
  if(!is.logical(mask.sample)){
    stop("'mask.sample' must be logical",
         call. = TRUE)
  }
  
  if(!is.logical(per.dye)){
    stop("'per.dye' must be logical",
         call. = TRUE)
  }
  
  if(!is.numeric(range.sample)){
    stop("'range.sample' must be numeric",
         call. = TRUE)
  }
  
  if(!is.logical(mask.ils)){
    stop("'mask.ils' must be logical",
         call. = TRUE)
  }
  
  if(!is.numeric(range.ils)){
    stop("'range.ils' must be numeric",
         call. = TRUE)
  }
  
  if(!is.numeric(k)){
    stop("'k' must be numeric",
         call. = TRUE)
  }
  
  if(!is.numeric(rank.t)){
    stop("'rank.t' must be numeric",
         call. = TRUE)
  }
  
  if(!is.numeric(alpha)){
    stop("'alpha' must be numeric",
         call. = TRUE)
  }
  
  if(!is.logical(ignore.case)){
    stop("'ignore.case' must be logical",
         call. = TRUE)
  }
  
  if(!is.logical(word)){
    stop("'word' must be logical",
         call. = TRUE)
  }
  
  if(!is.logical(debug)){
    stop("'debug' must be logical",
         call. = TRUE)
  }
  
  # Prepare -------------------------------------------------------------------
  
  if(!all(c("Masked", "Dye") %in% names(data))){
    # Mask data for AT calculation
    # (need to be separate function to enable control plots in GUI).
    data <- maskAT(data=data, ref=ref,mask.height=mask.height, height=height,
                    mask.sample=mask.sample, per.dye=per.dye, range.sample=range.sample,
                    mask.ils=mask.ils, range.ils=range.ils,
                    ignore.case=ignore.case, word=word, debug=debug)
    
  }
  
  # Get all dyes.
  dyes <- as.character(unique(data$Dye))
  dyeILS <- unique(data$Dye[data$ILS])
  dyesKit <- setdiff(dyes, dyeILS)

  # Get number of samples.
  nSamples <- length(unique(data$Sample.File.Name))
  
  # Internal functions --------------------------------------------------------
  
  # Function to calculate the percentile rank .
  percentileRank <- function(x) trunc(rank(x))/length(x)
  
  # Function to get height above a percentile.
  rankThreshold <- function(x, t) min(x[percentileRank(x) > t])
  
  # Convert -------------------------------------------------------------------

  # Check ILS dye.  
  if(length(dyeILS) == 0){
    
    message("No ILS dye found!")
    message("Identified dyes: ", paste(dyes, collapse = ", "))
    
    # Strip masked data.
    dt <- data[data$Masked==FALSE,]
    
  } else {
    
    # Strip masked data, and ILS channel.
    dt <- data[data$Masked==FALSE & data$Dye!=dyeILS,]
    
  }

  # Convert to data.table for performance.
  dt <- data.table::data.table(dt)
  
  # Analyse1 ------------------------------------------------------------------
  
  # Calculate for sample per dye.
  at.sample.dye <- dt[, list(Dye.Mean=mean(Height, na.rm=TRUE),
                             Dye.Sd=sd(Height, na.rm=TRUE),
                             Dye.Mean.ln=mean(log(Height), na.rm=TRUE),
                             Dye.Sd.ln=sd(log(Height), na.rm=TRUE),
                             Dye.Peaks=sum(Masked==FALSE),
                             Dye.AT2=rankThreshold(Height, rank.t)),
                      by=list(Sample.File.Name, Dye)]
  
  # Extract AT2 and remove from dataset to get final row order correct.
  at.sample.dye.AT2 <- at.sample.dye$Dye.AT2
  at.sample.dye$Dye.AT2 <- NULL
  
  # Calculate globally for each dye.
  at.dye <- dt[, list(Mean=mean(Height, na.rm=TRUE),
                      Sd=sd(Height, na.rm=TRUE),
                      Mean.ln=mean(log(Height), na.rm=TRUE),
                      Sd.ln=sd(log(Height), na.rm=TRUE),
                      Peaks=sum(Masked==FALSE),
                      AT2=rankThreshold(Height, rank.t)),
               by=list(Dye)]
  
  # Calculate for sample.
  at.sample <- dt[, list(Mean=mean(Height, na.rm=TRUE),
                         Sd=sd(Height, na.rm=TRUE),
                         Mean.ln=mean(log(Height), na.rm=TRUE),
                         Sd.ln=sd(log(Height), na.rm=TRUE),
                         Peaks=sum(Masked==FALSE),
                         AT2=rankThreshold(Height, rank.t)),
                  by=list(Sample.File.Name)]
  
  # Extract AT2 and remove from dataset to get final row order correct.
  at.sample.AT2 <- at.sample$AT2
  at.sample$AT2 <- NULL
  
  # Join the result.
  at.sample.dye$Sample.Mean <- rep(at.sample$Mean, each=length(dyesKit))
  at.sample.dye$Sample.Sd <- rep(at.sample$Sd, each=length(dyesKit))
  at.sample.dye$Sample.Mean.ln <- rep(at.sample$Mean.ln, each=length(dyesKit))
  at.sample.dye$Sample.Sd.ln <- rep(at.sample$Sd.ln, each=length(dyesKit))
  at.sample.dye$Sample.Peaks <- rep(at.sample$Peaks, each=length(dyesKit))
  
  # Calculate globally for all data.
  at.global <- dt[, list(Mean=mean(Height, na.rm=TRUE),
                         Sd=sd(Height, na.rm=TRUE),
                         Peaks=sum(Masked==FALSE),
                         Mean.ln=mean(log(Height), na.rm=TRUE),
                         Sd.ln=sd(log(Height), na.rm=TRUE),
                         AT2=rankThreshold(Height, rank.t))]
  
  # Join the result.
  at.sample.dye$Global.Mean <- rep(at.global$Mean, nrow(at.sample.dye))
  at.sample.dye$Global.Sd <- rep(at.global$Sd, nrow(at.sample.dye))
  at.sample.dye$Global.Mean.ln <- rep(at.global$Mean.ln, nrow(at.sample.dye))
  at.sample.dye$Global.Sd.ln <- rep(at.global$Sd.ln, nrow(at.sample.dye))
  at.sample.dye$Global.Peaks <- rep(at.global$Peaks, nrow(at.sample.dye))
  
  # Calculate AT1.
  at.sample.dye$Dye.AT1 <- at.sample.dye$Dye.Mean + k * at.sample.dye$Dye.Sd
  at.sample.dye$Sample.AT1 <- at.sample.dye$Sample.Mean + k * at.sample.dye$Sample.Sd
  at.sample.dye$Global.AT1 <- at.sample.dye$Global.Mean + k * at.sample.dye$Global.Sd
  
  # Calculate AT1 per dye.
  at.dye$AT1 <- at.dye$Mean + k * at.dye$Sd
  for(d in seq(along=dyesKit)){
    colName <- paste("Global", dyesKit[d],"AT1", sep=".")
    colVal <- rep(at.dye$AT1[d], nrow(at.sample.dye))
    colCnt <- length(colVal)
    dtNew <- data.table::data.table(col = colVal)
    data.table::setnames(dtNew, colName)
    at.sample.dye <- data.table::data.table(at.sample.dye, dtNew)
  }
  
  # Add AT2 results.
  at.sample.dye$Dye.AT2 <- at.sample.dye.AT2
  at.sample.dye$Sample.AT2 <- rep(at.sample.AT2, each=length(dyesKit))
  at.sample.dye$Global.AT2 <- rep(at.global$AT2, nrow(at.sample.dye))
  
  # Add AT2 per dye.
  for(d in seq(along=dyesKit)){
    colName <- paste("Global", dyesKit[d],"AT2", sep=".")
    colVal <- rep(at.dye$AT2[d], nrow(at.sample.dye))
    colCnt <- length(colVal)
    dtNew <- data.table::data.table(col = colVal)
    data.table::setnames(dtNew, colName)
    at.sample.dye <- data.table::data.table(at.sample.dye, dtNew)
  }
  
  # Calculate AT4.
  #Note: Actually no point using t-distribution since degrees of freedom
  # (number of observations - 1) are large (>100).
  at.sample.dye$Dye.AT4 <- at.sample.dye$Dye.Mean + abs(qt(alpha, at.sample.dye$Dye.Peaks - 1)) * (1 + 1 / 1)^0.5 * at.sample.dye$Dye.Sd
  at.sample.dye$Sample.AT4 <- at.sample.dye$Sample.Mean + abs(qt(alpha, at.sample.dye$Sample.Peaks - 1)) * (1 + 1 / 1)^0.5 * at.sample.dye$Sample.Sd
  at.sample.dye$Global.AT4 <- at.sample.dye$Global.Mean + abs(qt(alpha, at.sample.dye$Global.Peaks - 1)) * (1 + 1 / nSamples)^0.5 * at.sample.dye$Global.Sd
  
  # Calculate AT4 per dye.
  at.dye$AT4 <- at.dye$Mean + abs(qt(alpha, at.dye$Peaks - 1)) * (1 + 1 / 1)^0.5 * at.dye$Sd
  for(d in seq(along=dyesKit)){
    colName <- paste("Global", dyesKit[d],"AT4", sep=".")
    colVal <- rep(at.dye$AT4[d], nrow(at.sample.dye))
    colCnt <- length(colVal)
    dtNew <- data.table::data.table(col = colVal)
    data.table::setnames(dtNew, colName)
    at.sample.dye <- data.table::data.table(at.sample.dye, dtNew)
  }
  
  # Calculate AT7.
  at.sample.dye$Dye.AT7 <- exp(at.sample.dye$Dye.Mean.ln + k * at.sample.dye$Dye.Sd.ln)
  at.sample.dye$Sample.AT7 <- exp(at.sample.dye$Sample.Mean.ln + k * at.sample.dye$Sample.Sd.ln)
  at.sample.dye$Global.AT7 <- exp(at.sample.dye$Global.Mean.ln + k * at.sample.dye$Global.Sd.ln)

  # Calculate AT7 per dye.
  at.dye$AT7 <- exp(at.dye$Mean.ln + k * at.dye$Sd.ln)
  for(d in seq(along=dyesKit)){
    colName <- paste("Global", dyesKit[d],"AT7", sep=".")
    colVal <- rep(at.dye$AT7[d], nrow(at.sample.dye))
    colCnt <- length(colVal)
    dtNew <- data.table::data.table(col = colVal)
    data.table::setnames(dtNew, colName)
    at.sample.dye <- data.table::data.table(at.sample.dye, dtNew)
  }
  
  # Finally -------------------------------------------------------------------  
  
  # Add number of samples.
  at.sample.dye$Total.Samples <-  nSamples
  
  # Update audit trail.
  at.sample.dye <- auditTrail(obj = at.sample.dye, f.call = match.call(),
                              package = "strvalidator")
  
  # Convert back to data.frame.
  res1 <- data.frame(at.sample.dye)
  
  
  # Analyse2 ------------------------------------------------------------------
  
  # Calculate complete percentile rank list.
  at.rank <- data.frame(Height=unique(sort(dt$Height)),
                        Rank=unique(percentileRank(sort(dt$Height))),
                        Observations=as.numeric(table(dt$Height)))
  
  # Update audit trail.
  at.rank <- auditTrail(obj = at.rank, f.call = match.call(), 
                        package = "strvalidator")
  
  # Convert back to data frame.
  res2 <- data.frame(at.rank)

  
  # Masked data ---------------------------------------------------------------

  # Update audit trail.
  data <- auditTrail(obj = data, f.call = match.call(), 
                     package = "strvalidator")
  
  # Convert back to data.frame.
  res3 <- data.frame(data)

    
  if(debug){
    print("str(res1)")
    print(str(res1))
    print("head(res1)")
    print(head(res1))
    print("tail(res1)")
    print(tail(res1))
    print("str(res2)")
    print(str(res2))
    print("head(res2)")
    print(head(res2))
    print("tail(res2)")
    print(tail(res2))
    print("str(res3)")
    print(str(res3))
    print("head(res3)")
    print(head(res3))
    print("tail(res3)")
    print(tail(res3))
  }
  
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  # Return list of the two dataframes.
  res <- list(res1, res2, res3)
  
  # Return result.
  return(res)
  
}
