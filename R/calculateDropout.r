################################################################################
# TODO LIST
# TODO: ...
# TODO: change to ggplot2 perform lm and use add CI all-in-one?

################################################################################
# CHANGE LOG
# 08: Roxygenized and changed name from 'dropStat' to 'calculateDropout'.
# 07: fixed bug in dropcount/heterozygote not correct.

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
#' 
#' @return data.frame with columns 'Dropout', indicating no dropout (0), 
#' allele (1) and locus dropout (2), and 'Rfu', 'Heterozygous'.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' #newData<-dropStat(data=data, ref=ref)


calculateDropout <- function(data, ref, ignoreCase=TRUE){

  # Copy data frame
  data.drop <- data
  if(any(grepl("Dropout|Rfu|Heterozygous", names(data)))) {
    data.drop$Dropout <- NULL
    data.drop$Rfu <- NULL
    data.drop$Heterozygote <- NULL
  }
  data.drop$Dropout <- NA
  data.drop$Rfu <- NA
  data.drop$Heterozygous <- NA
  
  # Get column numbers.
  ref.col.a <- grepl("Allele", names(ref))
  col.m <- grepl("Marker", names(data))
  col.a <- grepl("Allele", names(data))
  col.h <- grepl("Height", names(data))
  
  # Get sample names.
  sample.names <- unique(data$Sample.Name)
  ref.sample.names <- unique(ref$Sample.Name)
  
  # Loop through all reference samples.
  for(s in seq(along=ref.sample.names)){
    
    # Select current subsets.
    if(ignoreCase){
      selected.samples <- grepl(toupper(ref.sample.names[s]),toupper(data$Sample.Name))
      selected.refs <- grepl(toupper(ref.sample.names[s]),toupper(ref$Sample.Name))
    } else {
      selected.samples <- grepl(ref.sample.names[s],data$Sample.Name)
      selected.refs <- grepl(ref.sample.names[s],ref$Sample.Name)
    }
    data.subset <- data[selected.samples,]
    ref.subset <- ref[selected.refs,]
    
    # Loop through all rows.
    for(r in 1:nrow(data.subset)){
      
      # Get current row indexes.
      row.index <- row.names(data.subset)
      
      # Get current marker.
      marker <- data.subset[r,col.m]
      
      # Get reference alleles.
      ref.allele <- unique(as.character(ref.subset[ref.subset$Marker==marker,ref.col.a]))
      
      # Get data for current row.
      allele.v <- as.matrix(data.subset[r,col.a])
      height.v <- as.matrix(data.subset[r,col.h])
      
      # Find matching alleles.
      observed <- ref.allele %in% allele.v
      tmp<-match(ref.allele,allele.v)
      heights <- tmp[!is.na(tmp)]
      
      # Count observed reference alleles.
      dropCount <- sum(!observed)
      
      # Indicate zygosity (1-Heterozygote, 0-Homozygote).
      if(length(ref.allele)==1){
        data.drop[row.index[r],]$Heterozygous <- 0
        het=FALSE
      } 
      if(length(ref.allele)==2){
        data.drop[row.index[r],]$Heterozygous <- 1
        het=TRUE
      } 
      
      # Write '0' for no dropout, '1' for allele dropout and '2' for locus dropout.
      if(dropCount==0){
        data.drop[row.index[r],]$Dropout <- 0
      } else if(dropCount==1 && het){
        data.drop[row.index[r],]$Dropout <- 1
      } else if(dropCount==1 && !het){
        data.drop[row.index[r],]$Dropout <- 2
      } else if(dropCount==2 && het){
        data.drop[row.index[r],]$Dropout <- 2
      } else {
        warning(paste("Unhandled combination (dropCount =",dropCount,", het =", het),
                call. = TRUE, immediate. = FALSE, domain = NULL)
      }
      
      # Store peak height of survived allele or NA if homozygote.
      if(dropCount==1){
        
        if(length(height.v[,heights])>0){
          data.drop[row.index[r],]$Rfu <- height.v[,heights]
        } else {
          data.drop[row.index[r],]$Rfu <- NA
        }
      }
      
    }
  }
  
  return(data.drop)
}
