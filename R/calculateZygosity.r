################################################################################
# TODO LIST
# TODO: test if 'addZygosity' works with non-slimmed data.

################################################################################
# CHANGE LOG
# 03: Support slimmed data.
# 02: Roxygenized.
# 01: First version


#' @title Calculate zygosity
#'
#' @description
#' \code{calculateZygosity} Calculates the number of alleles in each marker.
#'
#' @details Adds a column 'Zygosity' to 'data'. Calculates the number of
#' unique values in 'Allele*' columns. Indicates zygosity as '1' for 
#' homozygous and '2' for a heterozygous loci. Sample names must be unique.
#'   
#' @param data Data frame containing at least a column named 'Alleles'.
#' 
#' @return data.frame the original data frame containing additional columns.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' print("Example will come later")

calculateZygosity <- function(data){

  # Add the new column.
  data$Zygosity<-NA
  
  # Get allele columns.
  col.a <- grepl("Allele", names(data))

  if(sum(col.a) > 1){
    print("GeneMapper format")
    
    # Loop over all rows.
    for(r in 1:nrow(data)){
      
      # Sum number of unique allele values.
      data$Zygosity[r] <- sum(!is.na(unique(as.character(data[r,col.a]))))
      
    }
  } else if(sum(col.a) == 1){
    print("Slim format")
    
    samples <- unique(data$Sample.Name)

    # Loop over each sample.
    for(s in seq(along=samples)){

      # Get selection for current sample.
      sampleSel <- data$Sample.Name==samples[s]
      # Get unique markers for current sample.
      markers <- unique(data$Marker[sampleSel])

      # Loop over each marker in current sample.
      for(m in seq(along=markers)){

        # Get selection for current marker.
        markerSel <- data$Marker == markers[m]

        # Narrow down selection.
        currentSel <- sampleSel & markerSel

        # Get unique alleles for current selection.
        alleles <- unique(data$Allele[currentSel])
        alleles <- alleles[!is.na(alleles)]

        # Add zygosity to data frame.
        data$Zygosity[currentSel] <- length(alleles)
        
      }
    }
    
  }
  
  # Return data frame.
  return(data)
}