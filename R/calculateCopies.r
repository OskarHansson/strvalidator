################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 06.08.2017: Added audit trail.
# 05.08.2016: Fixed wrong variable 'res' -> 'data'.
# 04.08.2016: Added attributes.
# 20.07.2016: First version


#' @title Calculate Allele Copies
#'
#' @description
#' Calculates the number of alleles in each marker.
#'
#' @details Calculates the number of unique values in the 'Allele*' columns
#' for each marker, the number of allele copies, or indicate heterozygous loci.
#' Observed - number of unique alleles.
#' Copies - number of allele copies, '1' for heterozygotes and '2' for homozygotes.
#' Heterozygous - '1' for heterozygous and '0' for homozygous loci.
#' NB! The 'copies' and 'heterozygous' option are intended for known complete
#' profiles, while 'observed' can be used for any samples to count the number
#' of peaks. Sample names must be unique. The result is per marker but repeated
#' for each row of that marker. Data in 'fat' format is auto slimmed.
#'   
#' @param data Data frame containing at least columns 'Sample.Name', 'Marker,
#' and 'Allele*'.
#' @param observed logical indicating if a column 'Observed' should be
#' used to count the number of unique alleles.
#' @param heterozygous logical indicating if a column 'Heterozygous' should be
#' used to indicate heterozygotes with 1 and homozygotes with 0.
#' @param copies logical indicating if a column 'Copies' should be used to
#' indicate the number of allele copies with 1 for heterozygotes and 2 for
#' homozygotes.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame the original data frame with optional columns 'Observed',
#' 'Copies', and 'Heterozygous'.
#' 
#' @export
#' 
#' @importFrom data.table data.table :=
#' 

calculateCopies <- function(data, observed=FALSE, copies=TRUE,
                            heterozygous=FALSE, debug=FALSE){

  # Parameters that are changed by the function must be saved first.
  attr_data <- substitute(data)
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # CHECK DATA ----------------------------------------------------------------
  
  # Check dataset.
  if(!"Sample.Name" %in% names(data)){
    stop("'data' must contain a column 'Sample.Name'", call. = TRUE)
  }
  
  if(!"Marker" %in% names(data)){
    stop("'data' must contain a column 'Marker'", call. = TRUE)
  }
  if(!"Allele" %in% names(data)){
    stop("'data' must contain a column 'Allele'", call. = TRUE)
  }
  
  # PREPARE -------------------------------------------------------------------

  # Check if slim format.  
  if(sum(grepl("Allele", names(data))) > 1){
    message("'data' must be in 'slim' format. Attempt to auto slim:")
    
    # Autodetect column names to keep fixed.
    fixCol <- colNames(data=data, slim=TRUE, numbered=TRUE, concatenate="|")
    message("Fix columns:", fixCol)
    
    # Autodetect column names to stack.
    stackCol <- colNames(data=data, slim=FALSE, numbered=TRUE, concatenate="|")
    message("Stack columns:", stackCol)
    
    # Slim require a vector of strings.
    fixCol <- unlist(strsplit(fixCol, "|", fixed = TRUE))
    stackCol <- unlist(strsplit(stackCol, "|", fixed = TRUE))
    
    # Slim data.      
    data <- slim(data = data, fix = fixCol, stack = stackCol,
                 keep.na = TRUE, debug = debug)
    message("Auto slim completed.")
    
  }
  
  # Check 'false' NA.
  naAllele <- length(data$Allele[data$Allele=="NA"])
  if(naAllele > 0){
    data$Allele[data$Allele=="NA"] <- NA
    message("Converted ", naAllele, " string 'NA' in column 'Allele' to NA.")
  }
  
  # CALCULATE -----------------------------------------------------------------

  # Convert to data.table for performance.
  DT <- data.table(data)

  # Calculate number of unique alleles.  
  DT[ , Observed:=length(unique(Allele)), by=list(Sample.Name, Marker)]
  
  if(heterozygous){
    # Indicate heterozygotes as 1 and homozygotes as 0.

    if("Heterozygous" %in% names(data)){
      message("Column 'Heterozygous' already exist and will be overwritten.")
    }
    
    # Add or overwrite column.
    data$Heterozygous <- as.numeric(NA)

    # Add indicator.
    data$Heterozygous[DT$Observed==1] <- 0 # Change 1 -> 0
    data$Heterozygous[DT$Observed==2] <- 1 # Change 2 -> 1
    
  }
  
  if(copies) {
    # Indicate number of allele copies
    # (homozygotes as 2 and heterozygotes as 1).
    
    if("Copies" %in% names(data)){
      message("Column 'Copies' already exist and will be overwritten.")
    }
    
    # Add or overwrite column.
    data$Copies <- as.numeric(NA)
    
    # Add indicator.
    data$Copies[DT$Observed==1] <- 2 # Change 1 -> 2
    data$Copies[DT$Observed==2] <- 1 # Change 2 -> 1
    
  }
  
  if(observed){
    
    if("Observed" %in% names(data)){
      message("Column 'Observed' already exist and will be overwritten.")
    }

    # Add number of unique alleles.
    data$Observed <- DT$Observed

  }

  # Update audit trail.
  data <- auditTrail(obj = data, f.call = match.call(), package = "strvalidator")
  
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  # Return data frame.
  return(data)
  
}