################################################################################
# TODO LIST
# TODO: Implement word boundaries (filterProfile).


################################################################################
# CHANGE LOG (last 20 changes)
# 15.08.2016: Rewritten to use data.table for efficiency and new metrics.
# 29.06.2016: Added option to remove sex markers and quality sensor.
# 25.01.2016: Fixed save attribute saves dataset.
# 09.01.2016: Added more attributes to result.
# 06.01.2016: Added attributes to result.
# 12.10.2014: Fixed bug when NA in Allele column.
# 26.09.2014: Accept vector for 'exclude'.
# 12.09.2014: Included 'exclude' parameter.
# 10.09.2014: Included total peak height in result.
# 04.03.2014: Fixed bug when no NA and NA!=NULL.
# 25.02.2014: Option to add directly to dataset.
# 25.02.2014: Option to replace NAs.
# 13.04.2013: Rewrote the function to work with 'slim' data.
# 11.04.2013: Changed 'Z' to 'Heterozygous' (het/hom now indicated by 1/0,
#             but changed to 2 in function)

#' @title Calculate Peak Height.
#'
#' @description
#' Calculate peak height metrics for samples.
#'
#' @details
#' Calculates the total peak height (TPH), and number of observed peaks (Peaks),
#' for each sample by default. If a reference dataset is provided average peak
#' height (H), and profile proportion (Proportion) are calculated.
#' 
#' H is calculated according to the formula:
#' \eqn{H = sum(peak heights)/(n[het] + 2n[hom]}
#' Where:
#' n[het] = number of observed heterozygous alleles
#' n[hom] = number of observed homozygous alleles
#' 
#' Tip: If it is known that all expected peaks are observed and no unexpected
#' peaks are present, the dataset can be used as a reference for itself.
#' 
#' Note: If a reference dataset is provided the known alleles will be extracted
#' from the dataset.
#' 
#' @param data data.frame with at least columns 'Sample.Name' and 'Height'.
#' @param ref data.frame with at least columns 'Sample.Name' and 'Allele'.
#' @param na.replace replaces NA values in the final result.
#' @param exclude character vector (case sensitive) e.g. "OL" excludes rows with
#'  "OL" in the 'Allele' column.
#' @param add logical default is TRUE which will add or overwrite columns
#' 'TPH', 'Peaks', 'H', and 'Proportion' in the provided 'data'.
#' @param sex.rm logical, default FALSE to include sex markers in the analysis.
#' @param qs.rm logical, default TRUE to exclude quality sensors from the analysis.
#' @param kit character, required if sex.rm=TRUE or qs.rm=TRUE to define the kit.
#' @param ignore.case logical TRUE ignores case in sample name matching.
#' @param exact logical TRUE for exact sample name matching.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with with at least columns 'Sample.Name', 'TPH', and 'Peaks'.
#' 
#' @export
#' 
#' @references
#' Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
#'  Journal of the Royal Statistical Society: Series C (Applied Statistics),
#'  Volume 59, Issue 5, 2010,
#'  Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
#' \url{http://dx.doi.org/10.1111/j.1467-9876.2010.00722.x}
#' 
#' @importFrom utils str
#' @importFrom data.table data.table :=


calculateHeight <- function(data, ref=NULL, na.replace=NULL, add=TRUE, exclude=NULL,
                            sex.rm=FALSE, qs.rm=FALSE, kit=NULL, ignore.case=TRUE,
                            exact=FALSE, debug=FALSE){

  # Parameters that are changed by the function must be saved first.
  attr_data <- substitute(data)

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!any(grepl("Sample.Name", names(data)))){
    stop("'data' must contain a column 'Sample.Name'.", call. = TRUE)
  }

  if(!any(grepl("Height", names(data)))){
    stop("'data' must contain a column 'Height'.", call. = TRUE)
  }
  
  # Check if slim format.
  if(sum(grepl("Height", names(data))>1)){
    stop("'data' must be in 'slim' format.", call. = TRUE)
  }
  
  # Check ref.  
  if(!is.null(ref)){
    
    # Check dataset.
    if(!any(grepl("Sample.Name", names(ref)))){
      stop("'ref' must contain a column 'Sample.Name'.", call. = TRUE)
    }
    
    if(!any(grepl("Allele", names(ref)))){
      stop("'ref' must contain a column 'Allele'.", call. = TRUE)
    }
    
    # Check if slim format.
    if(sum(grepl("Allele", names(ref))>1)){
      stop("'ref' must be in 'slim' format.", call. = TRUE)
    }

  }

  # Check na.
  if(length(na.replace) > 1){
    stop("'na.replace' must be of length 1.", call. = TRUE)
  }

  # Check logical arguments.
  if(!is.logical(add)){
    stop("'add' must be logical.", call. = TRUE)
  }
  
  if(!is.logical(sex.rm)){
    stop("'sex.rm' must logical.", call. = TRUE)
  }
  
  if(!is.logical(qs.rm)){
    stop("'qs.rm' must be logical.", call. = TRUE)
  }

  # Check dependencies.  
  if(sex.rm){
    if(is.null(kit)){
      stop("'kit' can't be NULL if sex.rm=TRUE.")
    }
  }
  if(qs.rm){
    if(is.null(kit)){
      stop("'kit' can't be NULL if qs.rm=TRUE.")
    }
  }
  
  # PREPARE -----------------------------------------------------------------
  
  # Check if numeric data.
  if(!is.numeric(data$Height)){
    
    # Convert to numeric.    
    data$Height <- as.numeric(data$Height)
    
    message("The column 'Height' was converted to numeric.")
    
  }
    
  # Remove sex markers. 
  if(sex.rm){

    # Check if kit is provided.    
    if(is.null(kit)){
      
      message("No kit defined. Attempt to auto detect:")
      
      kit <-detectKit(data)[1]
      
      message("Using kit=", kit)
      
      # Check kit.  
      if(is.na(kit)){
        stop("No matching kit was found in the kit definition file.")
      }
      
    }
    
    message("Removing sex markers defined in kit: ", kit, ".")
    
    # Get sex markers.    
    sexMarkers <- getKit(kit = kit, what = "Sex.Marker")
    
    if(debug){
      print("Sex markers:")
      print(sexMarkers)
    }
    
    message("Removing sex markers from dataset:")
    # Loop through and remove all sex markers.
    for(i in seq(along = sexMarkers)){
      
      tmp1 <- nrow(data)
      
      data <- data[data$Marker != sexMarkers[i],]
      
      tmp2 <- nrow(data)
      
      message("Removed ", tmp1 - tmp2,
              " rows with Marker=", sexMarkers[i], ".")
      
    }
    
    if(!is.null(ref)){
      
      # Loop through and remove all quality sensors.
      message("Removing sex markers from reference dataset:")
      # Loop through and remove all sex markers.
      for(i in seq(along = sexMarkers)){
        
        tmp1 <- nrow(ref)
        
        ref <- ref[ref$Marker != sexMarkers[i],]
        
        tmp2 <- nrow(ref)
        
        message("Removed ", tmp1 - tmp2,
                " rows with Marker=", sexMarkers[i], ".")
        
      }
      
    }
    
  }
    
  # Remove quality sensors. 
  if(qs.rm){

    # Check if kit is provided.    
    if(is.null(kit)){
      
      message("No kit defined. Attempt to auto detect:")
      
      kit <-detectKit(data)[1]
      
      message("Using kit=", kit)
      
      # Check kit.  
      if(is.na(kit)){
        stop("No matching kit was found in the kit definition file.")
      }
      
    }
    
    message("Removing quality sensors defined in kit: ", kit, ".")
    
    # Get quality sensors.
    qsMarkers <- getKit(kit = kit, what = "Quality.Sensor")
    
    if(debug){
      print("Quality sensors:")
      print(qsMarkers)
    }
    
    # Loop through and remove all quality sensors.
    message("Removing quality sensors from dataset:")
    for(i in seq(along = qsMarkers)){
      
      tmp1 <- nrow(data)
      
      data <- data[data$Marker != qsMarkers[i],]
      
      tmp2 <- nrow(data)
      
      message("Removed ", tmp1 - tmp2,
              " rows with Marker=", qsMarkers[i], ".")
      
    }
    
    if(!is.null(ref)){
      
      # Loop through and remove all quality sensors.
      message("Removing quality sensors from reference dataset:")
      for(i in seq(along = qsMarkers)){
        
        tmp1 <- nrow(ref)
        
        ref <- ref[ref$Marker != qsMarkers[i],]
        
        tmp2 <- nrow(ref)
        
        message("Removed ", tmp1 - tmp2,
                " rows with Marker=", qsMarkers[i], ".")
        
      }
      
    }
    
  }
    
    
  if(!is.null(exclude)){
    message("Removing excluded alleles from dataset:")
    
    for(e in seq(along=exclude)){
      
      # Remove excluded alleles, accept NA values (or will result in all NA for that row).
      tmp1 <- nrow(data)
      data <- data[data$Allele != exclude[e] | is.na(data$Allele), ]
      tmp2 <- nrow(data)
      message("Removed ", tmp1 - tmp2, " rows with Allele=", exclude[e], ".")
      
    }
    
  }
    
  # Check if reference dataset was provided.
  if(!is.null(ref)){
    
    # Check if missing alleles (Y markers in female profiles.)
    if(any(is.na(ref$Allele))){
      # Remove any row with Allele=NA.
      
      tmp1 <- nrow(ref)
      ref <- ref[!is.na(ref$Allele), ]
      tmp2 <- nrow(ref)
      message("Removed ", tmp1 - tmp2, " rows with Allele=NA in reference dataset.")
      
    }
    
    # Extract known alleles.
    data <- filterProfile(data = data, ref = ref, keep.na = FALSE, add.missing.loci=FALSE,
                          ignore.case = ignore.case, exact = exact)
    message("Known alleles extracted from dataset.")
    
    if(!"Copies" %in% names(ref)){
      # Add 
      ref <- calculateCopies(data = ref)
      message("Number of allele copies added to reference dataset.")
      
    }
    
    # Convert to data.table and calculate number of allele copies and expected peaks.
    DTref <- data.table(ref)
    DTref[, N.Alleles:=sum(Copies), by=list(Sample.Name)]
    DTref[, Expected:=.N, by=list(Sample.Name)]
    
    # Add to dataset.
    data <- addData(data = data, new.data = DTref,
                    by.col = "Sample.Name", what = c("N.Alleles","Expected"),
                    exact = exact, debug = debug)
    message("Expected number of alleles added to dataset.")
    
  }
    
  # CALCULATE -----------------------------------------------------------------
  
  # Convert to data.table and calculate metrics.
  DT <- data.table(data)
  
  if(add & nrow(DT) > 0){
    # Calculate and add to dataset (repeat over all rows in sample).
    
    # Calculate total peak height for each sample.
    DT[, TPH:=sum(Height, na.rm = TRUE), by=list(Sample.Name)]
    
    # Calculate number of observed peaks for each sample.
    DT[, Peaks:=sum(!is.na(Height)), by=list(Sample.Name)]
    
    if("N.Alleles" %in% names(DT)){
      
      # Calculate average peak height for each sample.
      DT[, H:=TPH/N.Alleles, by=list(Sample.Name)]
      
    } else {
      
      message("A column 'N.Alleles' was not found in 'data'.")
      message("Average peak height will not be calculated.")
      message("Provide a reference dataset to enable calculation of 'H'.")
      
    }
    
    if("Expected" %in% names(DT)){
      
      # Calculate proportion observed profile for each sample.
      DT[, Proportion:=Peaks/Expected, by=list(Sample.Name)]
      
    } else {
      
      message("A column 'Expected' was not found in 'data'.")
      message("Profile proportion will not be calculated.")
      message("Provide a reference dataset to enable calculation of profile proportion.")
      
    }
    
    # Assign to result.
    res <- DT
    
  } else if (!add & nrow(DT) > 0){
    # Calculate per sample in a new dataset.

    if(!is.null(ref)){
      
      # Calculate total peak height for each sample.
      DT[, TPH:=sum(Height, na.rm = TRUE), by=list(Sample.Name)]
      
      # Calculate number of observed peaks for each sample.
      DT[, Peaks:=sum(!is.na(Height)), by=list(Sample.Name)]
      
      
      # Calculate total and average peak height, number of peaks,
      # and profile proportion for each sample.    
      res <- DT[, list(TPH=unique(TPH),
                    H=unique(TPH)/unique(N.Alleles),
                    Peaks=unique(Peaks),
                    Expected=unique(Expected),
                    Proportion=unique(Peaks)/unique(Expected)),
                by=list(Sample.Name)]
      
    } else {
      
      # Calculate total peak height and number of peaks for each sample.    
      res <- DT[, list(TPH=sum(Height, na.rm = TRUE),
                    Peaks=sum(!is.na(Height))),
                by=list(Sample.Name)]
      
      message("Average peak height and profile proportion will not be calculated.")
      message("Provide a reference dataset to enable calculation.")
      
    }
    
  } else if(!nrow(DT) > 0) {
    
    message("Dataset is empty. Returning NULL")
    res <- NULL
    
  } else {
    
    # This should not happen.
    message("There was an unexpected error")
    
  }
    
  # Convert to data.frame to avoid unexpected results in other functions.
  res <- as.data.frame(res)

  # FINALIZE ------------------------------------------------------------------
  
  # Replace NA:s
  if(!is.null(na.replace)){
    # Check if NA:s and change to 'na.replace'.

    if(any(is.na(res$TPH))){
      n <- sum(is.na(res$TPH))
      res[is.na(res$TPH), ]$TPH <- na.replace
      message("Replaced ", n, " NA's in 'TPH' with '", na.replace, "'.")
    }
    
    if(any(is.na(res$Peaks))){
      n <- sum(is.na(res$Peaks))
      res[is.na(res$Peaks), ]$Peaks <- na.replace
      message("Replaced ", n, " NA's in 'Peaks' with '", na.replace, "'.")
    }
    
    if(any(is.na(res$H))){
      n <- sum(is.na(res$H))
      res[is.na(res$H), ]$H <- na.replace
      message("Replaced ", n, " NA's in 'H' with '", na.replace, "'.")
    }
    
    if(any(is.na(res$Proportion))){
      n <- sum(is.na(res$Proportion))
      res[is.na(res$Proportion), ]$Proportion <- na.replace
      message("Replaced ", n, " NA's in 'Proportion' with '", na.replace, "'.")
    }
    
  }
	
	# Add attributes to result.
	attr(res, which="kit") <- kit
	attr(res, which="calculateHeight, strvalidator") <- as.character(utils::packageVersion("strvalidator"))
	attr(res, which="calculateHeight, call") <- match.call()
	attr(res, which="calculateHeight, date") <- date()
	attr(res, which="calculateHeight, data") <- attr_data
	attr(res, which="calculateHeight, na.replace") <- na.replace
	attr(res, which="calculateHeight, add") <- add
	attr(res, which="calculateHeight, exclude") <- exclude
	attr(res, which="calculateHeight, sex.rm") <- sex.rm
	attr(res, which="calculateHeight, qs.rm") <- qs.rm
	attr(res, which="calculateHeight, ignore.case") <- ignore.case
	attr(res, which="calculateHeight, exact") <- exact
	
	# Return result.
	return(res)

}
