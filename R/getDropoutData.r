################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 08: Roxygenized and changed name from 'getDropData' to 'getDropoutData'.

#' @title Get drop-out events
#'
#' @description
#' \code{getDropoutData} extracts the drop-out information from a data set
#' where drop-outs have been calculated.
#'
#' @details
#' Extracts loci, heterozygote peak heights, and dropout status from dataset.
#' Required columns: 'Marker' with locus names, 'Height.1' and 'Height.2', 
#' 'Dropout' with dropout status as described for \code{calculateDropout}, 
#' 'Heterozygous' with 1 for heterozygous loci and 0 for homozygous loci.
#' Gets all observations (e.g. 90/0, 105/95 -> 90-1, 105-0, 95-0)
#' 
#' @param data data frame with genotype data in "GeneMapper" format.
#' Requires at least columns
#' 'Marker' (locus names), 'Height' (Height.1 and Height.2), 
#' 'Dropout' (no dropout '0', allele '1', and locus dropout '2'), 
#' 'Heterozygous' (heterozygous '1' or homozygous '0' loci).
#' 
#' @return data.frame a data frame with the following columns 
#' 'Marker' (locus name), 'Height' (peak height in rfu of the surviving allele),
#' 'Dropout' (indicates if the sister allele has dropped out '1' or not '0').
#' 

getDropoutData <- function(data){
  
  # Grep columns.
  col.m <- grepl("Marker", names(data))
  col.a <- grepl("Allele", names(data))
  col.h <- grepl("Height", names(data))
  col.d <- grepl("Dropout", names(data))
  
  # Discard homozygotes.
  data <- data[data$Heterozygous==1,]
  
  # Initiate vectors
  marker.v<-vector()
  allele.v<-vector()
  height.v<-vector()
  dropout.v<-vector()
  
  # Initiate index.
  i<-1
  
  # Loop through all rows.
  for(r in 1:nrow(data)){
    
    # Get current row indexes.
    row.index <- row.names(data)
    
    # Get data for current row.
    marker <- as.character(data[r,col.m])
    alleles <- as.character(data[r,col.a])
    heights <- as.numeric(data[r,col.h])
    dropout <- as.numeric(data[r,col.d])
    
    # Remove NA
    alleles <- alleles[!is.na(alleles)]
    heights <- heights[!is.na(heights)]
    
    # Loop over all alleles.
    for(a in seq(along=alleles)){
      marker.v[i] <- marker
      allele.v[i] <- alleles[a]
      height.v[i] <- heights[a]
      dropout.v[i] <- dropout
      i<-i+1
    }
    
  }
  
  # Create data frame.
  data.drop<-data.frame(Marker=marker.v,
                        Allele=allele.v,
                        Height=height.v,
                        Dropout=dropout.v,
                        stringsAsFactors = FALSE)
  
  return(data.drop)
}
