################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 09: Roxygenized and changed name from stutterTable to tableStutter
# 08 n.alleles
# 07 95% percentile and Max

#' @title Table stutter
#'
#' @description
#' \code{tableStutter} summarizes a stutter analysis in table format.
#'
#' @details
#' Summarize the stutter analysis in table format with different scope.
#' 
#' @param data data frame from a stutter analysis by \code{calculateStutter}.
#' @param per string, summarize the analysis 'global', by 'locus', or by 'stutter'.
#' 
#' @return data.frame with summarized result.
#' 


tableStutter <- function(data, per="stutter"){
  
  # Create empty result data frame with NAs.
  s.table <- data.frame(t(rep(NA,8)))
  # Add column names.
  names(s.table ) <- c("Marker", "Type", "n.alleles", "n.stutters", 
                       "Mean", "Stdv", "Perc.95", "Max")
  # Remove all NAs
  s.table  <- s.table [-1,]
  
  if(per=="global") {
    # Calculate a global average across all data.
    sum.allele<-length(unique(data$Allele))
    sum.obs<-length(data$Ratio)
    xbar<-mean(data$Ratio)
    stdv<-sd(data$Ratio)
    quant <- as.numeric(quantile(data$Ratio, 0.95)) 
    xmax <- max(data$Ratio)
    tmp<-data.frame("Marker"=NA, "Type"=NA,
                    "n.alleles"=sum.allele, "n.stutters"=sum.obs, 
                    "Mean"=xbar, "Stdv"=stdv, "Perc.95"=quant, "Max"=xmax)
    s.table<-rbind(s.table,tmp)
    
  } else {
    
    # Get all markers.
    marker <- unique(data$Marker)
    
    # Loop over all markers.
    for(m in seq(along=marker)){
      
      # Subset marker.			
      data.subset <- data[data$Marker==marker[m],]
      
      # Get types.
      type <- unique(data.subset$Type)
      
      if(per=="stutter") {
        # Calculate an average per stutter type.
        for(t in seq(along=type)){
          
          sum.allele <- length(unique(data.subset$Allele[data.subset$Type==type[t]]))
          sum.obs <- length(data.subset$Ratio[data.subset$Type==type[t]])
          if(is.null(sum.obs)){sum.obs=NA}
          xbar <- mean(data.subset$Ratio[data.subset$Type==type[t]])
          stdv <- sd(data.subset$Ratio[data.subset$Type==type[t]])
          quant <- as.numeric(quantile(data.subset$Ratio[data.subset$Type==type[t]], 0.95))
          xmax <- max(data.subset$Ratio[data.subset$Type==type[t]])
          tmp <- data.frame("Marker"=marker[m], "Type"=type[t],
                            "n.alleles"=sum.allele, "n.stutters"=sum.obs, 
                            "Mean"=xbar, "Stdv"=stdv, "Perc.95"=quant, "Max"=xmax)
          s.table<-rbind(s.table,tmp)
        }
      }
      if(per=="locus") {
        # Calculate an average per locus.
        sum.allele <- length(unique(data.subset$Allele))
        sum.obs <- length(data.subset$Ratio)
        if(is.null(sum.obs)){sum.obs=NA}
        xbar <- mean(data.subset$Ratio)
        stdv <- sd(data.subset$Ratio)
        quant <- as.numeric(quantile(data.subset$Ratio, 0.95))
        xmax <- max(data.subset$Ratio)
        tmp<-data.frame("Marker"=marker[m], "Type"=NA,
                        "n.alleles"=sum.allele, "n.stutters"=sum.obs, 
                        "Mean"=xbar, "Stdv"=stdv, "Perc.95"=quant, "Max"=xmax)
        s.table <- rbind(s.table,tmp)
        
      }
    }
  }
  #s.table <- s.table[order(s.table$Marker, s.table$Stutter),]
  
  return(s.table)
  
}
