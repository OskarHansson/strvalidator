################################################################################
# TODO LIST
# TODO: This function is based on the old version in PCRsim and could 
# probably benefit of a complete re-write...
# TODO: Handle negative samples.
#
# NOTE:
# NB! To avoid 'object not found' for data.table see solution at:
# http://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package#comment43941455_10529888

################################################################################
# CHANGE LOG
# 11.11.2015: Added importFrom ggplot2.
# 29.08.2015: Added importFrom.
# 31.05.2015: Added 'numbered=TRUE' to 'slim' function.
# 10.02.2015: Changed error message.
# 09.12.2014: Function moved from PCRsim package.

#' @title Generate EPG
#'
#' @description
#' Visualises an EPG from DNA profiling data.
#'
#' @details
#' Generates a electropherogram like plot from 'data' and 'kit'.
#' If 'Size' is not present it is estimated from kit information and allele values.
#' If 'Height' is not present a default of 1000 RFU is used.
#' Off-ladder alleles can be plotted if 'Size' is provided.
#' There are various options to customise the plot scale and labels.
#' It is also possible to plot 'distributions' of peak heights.
#' 
#' @param data data frame containing at least columns 'Sample.Name', 'Allele', and 'Marker'.
#' @param kit string or integer representing the STR typing kit.
#' @param title string providing the title for the EPG.
#' @param peaks logical TRUE to plot peaks for distributions using mean peak height.
#' @param type character plot type "distr" and "profile".
#' @param collapse logical TRUE to add the peak heights of identical alleles peaks within each marker.
#' NB! Removes off-ladder alleles.
#' @param silent logical FALSE to show plot.
#' @param ignore.case logical FALSE for case sensitive marker names.
#' @param at numeric analytical threshold (Height <= at will not be plotted).
#' @param scale character "free" free x and y scale, alternatively "free_y" or "free_x".
#' @param limit.x logical TRUE to fix x-axis to size range.
#' To get a common x scale set scale="free_y" and limit.x=TRUE.
#' @param label.size numeric for allele label text size.
#' @param label.angle numeric for allele label print angle.
#' @param label.vjust numeric for vertical justification of allele labels.
#' @param label.hjust numeric for horizontal justification of allele labels.
#' @param expand numeric for plot are expansion (to avoid clipping of labels).
#' @param debug logical for printing debug information to the console.
#' 
#' @return ggplot object.
#' 
#' @export
#' 
#' @importFrom utils str head tail flush.console
#' @importFrom stats as.formula
#' @importFrom ggplot2 geom_polygon ggplot aes_string scale_fill_manual
#' geom_boxplot scale_colour_manual geom_rect geom_text scale_y_continuous
#' facet_grid facet_wrap coord_cartesian theme element_blank labs xlab ylab
#' 

generateEPG <- function(data, kit, title=NULL, peaks=TRUE, type="profile",
                        collapse=TRUE, silent=FALSE, ignore.case=TRUE,
                        at=0, scale="free", limit.x=TRUE, label.size=3,
                        label.angle=0, label.vjust=1, label.hjust=0.5,
                        expand=0.1, debug=FALSE){
  
  # Debug info.
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("data:")
    print(str(data))
    print(head(data))
    print(tail(data))
    print("kit:")
    print(kit)
    print("title:")
    print(title)
    flush.console()
  }
  
  if(!"Sample.Name" %in% names(data)){
    stop("'data' must contain a column 'Sample.Name'",
         call. = TRUE)
  }
  
  if(!"Marker" %in% names(data)){
    stop("'data' must contain a column 'Marker'",
         call. = TRUE)
  }
  
  if(length(grep("Allele", names(data))) == 0){
    
    stop("'data' must contain a column 'Allele'.",
         call. = TRUE)
  }
  
  if(!is.logical(peaks)){
    stop("'peaks' must be logical.",
         call. = TRUE)
  }
  
  if(!is.logical(collapse)){
    stop("'collapse' must be logical.",
         call. = TRUE)
  }
  
  if(!is.logical(silent)){
    stop("'silent' must be logical.",
         call. = TRUE)
  }
  
  if(!is.logical(ignore.case)){
    stop("'ignore.case' must be logical.",
         call. = TRUE)
  }
  
  if(!is.numeric(at)){
    stop("'at' must be numeric.",
         call. = TRUE)
  }

  if(!is.numeric(label.size)){
    stop("'label.size' must be numeric.",
         call. = TRUE)
  }
  
  if(!is.numeric(label.angle)){
    stop("'label.angle' must be numeric.",
         call. = TRUE)
  }

  if(!is.numeric(label.vjust)){
    stop("'label.vjust' must be numeric.",
         call. = TRUE)
  }

  if(!is.numeric(label.hjust)){
    stop("'label.hjust' must be numeric.",
         call. = TRUE)
  }

  if(!is.numeric(expand)){
    stop("'expand' must be numeric.",
         call. = TRUE)
  }
  
  if("Size" %in% names(data)){
    if(!is.numeric(data$Size)){
      data$Size <- as.numeric(data$Size)
      message("'Size' must be numeric. Data converted!")
    }
  }
  
  
  # Prepare -------------------------------------------------------------------
  
  # Variables.
  distr <- FALSE # Flag to plot distribution as boxplot.
  mSpace <- 1.1  # Factor to make room for marker regions.
  # To prevent R CMD CECK NOTE:
  # no visible binding for global variable 'Height' / 'Id'
  # when used in data.table
  Height <- NULL
  Id <- NULL

  if(!"Height" %in% names(data)){
    # Add Height if not present. 
    data$Height <- 1000
    message("'Height' is missing. Using default!")
  }
  
  # Get kit information.
  kitInfo <- getKit(kit=kit, what="Range")
  
  # Get kit markers and colors.
  markerInfo <- getKit(kit=kit, what="Color")
  markerInfo <- addColor(markerInfo, have="Color", need="Dye")
  
  # Get unique Dyes.
  kitDye <- unique(markerInfo$Dye)
  # Get unique Colors.
  kitColors <- unique(markerInfo$Color)
  # Convert R colors.
  manualPlotColors <- addColor(kitColors, have="Color", need="R.Color")
  
  # Check if 'fat' format.
  if(length(grep("Allele", names(data))) > 1) {
    message("Alleles is 'fat' format, converting to 'slim'.")
    
    # Debug info.
    if(debug){
      print("alleles is 'fat' format:")
      print(str(data))
      print(head(data))
      print(tail(data))
    } else {}
    
    # Slim data frame.
    fixCol <- colNames(data=data, slim = TRUE, numbered=TRUE, concatenate=NULL, debug=debug)
    stackCol <- colNames(data=data, slim = FALSE, numbered=TRUE, concatenate=NULL, debug=debug)
    data <- slim(data=data, fix=fixCol, stack=stackCol, debug=debug)
    
    # Debug info.
    if(debug){
      print("convert to 'slim' format:")
      print(str(data))
      print(head(data))
      print(tail(data))
    }
    
  }
  
  # Check if numeric.
  if(!is.numeric(data$Height)){
    data$Height <- as.numeric(data$Height)
    message("'Height' is character, data converted to numeric.")
  }
  
  # Convert NA to empty string to prevent ggplot2 error (and prevent allele labels).
  if(any(is.na(data))){
    data[is.na(data)] <- ""
    message("NA's were replaced by empty string.")
    
    # Debug info.
    if(debug){
      print("'data' after convert NA to empty string:")
      print(str(data))
      print(head(data))
      print(tail(data))
    }
    
  }
  
  # Apply analytical threshold (AT).
  if(any(data$Height < at)){
    data <- data[!(data$Height < at), ]
    message(paste("Removed peaks below at =", at, "RFU"))
  }
  
  # Check type.
  if(type=="distr") {
    distr <- TRUE
  } else if(type=="profile") {
    distr <- FALSE
  } else {
    message(paste("Plot type '", type,
                  "' not supported, using default 'profile'", sep=""))
  }
  
  # Create EPG -----------------------------------------------------------------
  
  # Add unique 'Id' column (combine 'Allele' and 'Marker') for grouping.
  data$Id <- paste(data$Allele, data$Marker, sep="") 
  
  # collapse data.
  if(collapse){
    
    # Check if necessary to collapse.
    if(anyDuplicated(data$Id)){
      
      if(distr){
        data <- compact(data=data, per.sample=TRUE, debug=debug)
      } else {
        data <- compact(data=data, per.sample=FALSE, debug=debug)
      }
      
      # 'Id' lost in 'compact'
      # Add unique 'Id' column (combine 'Allele' and 'Marker') for grouping.
      data$Id <- paste(data$Allele, data$Marker, sep="") 
      
      # Debug info.
      if(debug){
        print("profile collapseed:")
        print(str(data))
        print(head(data))
        print(tail(data))
      }
      
    }
    
  }
  
  # Debug info.
  if(debug){
    print("Id added to 'data':")
    print(head(data))
    print(tail(data))
  }
  
  # Check if size is provided.
  if(!"Size" %in% names(data)){
    # Calculate size of alleles.
    data <- addSize(data=data, kit=getKit(kit=kit, what="Offset"),
                    bins=FALSE, ignore.case=ignore.case, debug=debug)
    
    # Remove NA rows.
    if(any(is.na(data$Size))){
      data <- data[!is.na(data$Size),]
      message("Rows with Size=NA removed.")
    }
    
  }
  
  # Check if distributions.
  if(!distr){
    
    # Calculate coordinates for plotting peaks.
    data <- heightToPeak(data=data, debug=debug)
    
    # NB! Height must be numeric or there will be problems mixing
    # continous and categorical data on the same axis... 
    if(!is.numeric(data$Height)){
      data$Height <- as.numeric(data$Height)
      if(debug){
        print("'data$Height' converted to numeric!")
      }
    }
    
  } else {
    
    # Creat a data table copy of the data frame.
    tmpDT <- data.table::data.table(data , keep.rownames=TRUE)

    # Calculate the mean height for each allele.
    tmpDFmph <- tmpDT[, mean(Height), by=Id]
    names(tmpDFmph)<- c("Id","Height")
    
    tmpX<-unique(data[ , c("Marker","Allele")])
    tmpDF <- cbind(tmpX, tmpDFmph)
    
    # Calculate size of alleles.
    dataMean <- addSize(data=tmpDF, kit=getKit(kit=kit, what="Offset"),
                        bins=FALSE, ignore.case=ignore.case, debug=debug)
    
    # Calculate coordinates for plotting peaks.
    dataMean <- heightToPeak(data=dataMean)
    
  }
  
  
  # Copy unique 'Marker'-'Allele' combinations in 'data'
  # to a new data frame for handling allele names.
  #alleleInfo <- data[!duplicated(data[c("Marker","Allele")]),]
  #alleleInfo <- alleleInfo[alleleInfo$Height!=0,]
  alleleInfo <- data[data$Height!=0,]

  # Add dye information.
  alleleInfo <- addColor(data=alleleInfo, kit=kit,
                         need="Dye", ignore.case=ignore.case)
  
  # Replace NA with the smallest size in kit (plot can't handle all NAs).
  alleleInfo$Size[is.na(alleleInfo$Size)] <- min(kitInfo$Marker.Min)
  
  # Add dye information.
  data <- addColor(data=data, kit=kit)
  if(distr){
    dataMean <- addColor(data=dataMean, kit=kit, ignore.case=ignore.case)
    dataMean <- addColor(data=dataMean, have="Dye", need="R.Color")
  }
  
  # Sort 'Marker' and 'Dye' factors according 'kit'.
  data <- sortMarker(data=data, kit=kit)
  
  if(debug){
    print("'data' after sort markers")
    print(head(data))
    print(str(data))
    print(head(data$Marker))
  }
  
  if(distr){
    dataMean <- sortMarker(data=dataMean, kit=kit)
  }
  
  # Get information for annotation of markers.
  mDye <- markerInfo$Dye
  mXmin <- kitInfo$Marker.Min
  mXmax <- kitInfo$Marker.Max
  mText <- kitInfo$Marker
  mYmax <- vector()
  
  # Loop over all dye channels.
  for(c in seq(along=kitDye)){
    # Find the maximum value and repeat for the whole current dye channel.
    tmpHeight <- data$Height[data$Dye==kitDye[c]]
    tmpHeight[is.na(tmpHeight)] <- 1 # Make sure we not end up with NA.
    if(length(tmpHeight)==0){
      # Make height at least 1.
      tmpHeight <- 1
    } 
    tmpYmax <- max(tmpHeight)
    if(tmpYmax==0){
      # Make Y max at least 1.
      tmpYmax <- 1
    }
    mYmax <- c(mYmax, 
               rep(tmpYmax, 
                   length(markerInfo$Marker[markerInfo$Dye==kitDye[c]])))
  }
  
  # Make sure numeric.
  mYmax <- as.numeric(mYmax)
  
  if(scale=="free_x"){
    # Fixed y axis.
    mYmax <- max(mYmax)
  }
  
  # Debug info.
  if(debug){
    print("data:")
    print(head(data))
    print(str(data))
    print("mDye:")
    print(mDye)
    print("mXmin:")
    print(mXmin)
    print("mXmax:")
    print(mXmax)
    print("mYmax:")
    print(mYmax)
    print("mText:")
    print(mText)
    print("mSpace:")
    print(mSpace)
  }  
  
  # Create annotation data frame for loci.
  markerRanges <- data.frame(Dye=factor(mDye),        # Facet.
                             Color=mDye,    	        # Dye.
                             Xmin=mXmin,			        # Marker lower range.	
                             Xmax=mXmax,			        # Marker upper range.
                             Size=(mXmin+mXmax)/2,		# Midpoint of marker range.
                             Height=mYmax,		        # Lower edge of marker range.
                             Height2=mYmax * mSpace,  # Upper edge of marker range.
                             Text=mText)			        # Marker names.
  
  # Debug info.
  if(debug){
    print("markerRanges:")
    print(str(markerRanges))
    print(head(markerRanges))
    print("alleleInfo:")
    print(str(alleleInfo))
    print(head(alleleInfo))
  }
  
  # Create plot.
  gp <- ggplot(data=data, aes_string(x="Size", y="Height"))

  # Plot data.
  if(!distr){
    # Plot peak height as peaks.
    gp <- gp + geom_polygon(aes_string(group="Id", fill="Dye"), data=data)
    gp <- gp + scale_fill_manual(values=manualPlotColors)
  } else {
    # Plot boxplots for distributions.
    gp <- gp + geom_boxplot(aes_string(group="Id", color="Dye"),
                            outlier.size=1, data=data)
    gp <- gp + scale_colour_manual(values=manualPlotColors)
    
    if(peaks){
      # Plot mean peak height as peaks.
      gp <- gp + geom_polygon(aes_string(group="Id", fill="Dye"),
                              data=dataMean)
      gp <- gp + scale_fill_manual(values=manualPlotColors)
    }
  }

  # Add marker regions.
  gp <- gp + geom_rect(aes_string(xmin="Xmin", xmax="Xmax",
                                  ymin="Height", ymax="Height2"),
                       alpha = .2, data=markerRanges, fill="blue", color="red")
  
  # Add marker names.
  gp <- gp + geom_text(aes_string(label="Text", y="Height2"), 
                       data=markerRanges, size=3, vjust = 1)
  
  # Add allele names.
  gp <- gp + geom_text(aes_string(label="Allele", x="Size", y=0),
                       data=alleleInfo, size=label.size, angle=label.angle,
                       vjust=label.vjust, hjust=label.hjust)

  # Expand plot area (to avoid clipping).
  gp <- gp + scale_y_continuous(expand=c(expand, 0))


  # Facet according to dye channel.
  gp <- gp + facet_grid("Dye ~ Marker")
  # NB! 'facet_wrap' does not seem to support strings.
  #     Use 'as.formula(paste("string1", "string2"))' as a workaround.
  gp <- gp + facet_wrap(as.formula(paste("~", "Dye")), ncol=1, drop=FALSE, scales=scale)
  
  # Set limits.
  if(limit.x){
    gp <- gp + coord_cartesian(xlim = c(min(mXmin), max(mXmax))) 
  }
  
  # Strip facet labels.
  gp <- gp + theme(strip.text = element_blank())
  
  # Strip facet background.
  gp <- gp + theme(strip.background = element_blank())
  
  # Add title and axis labels.
  gp <- gp + labs(title=title)
  gp <- gp + xlab("Size (bp)")
  gp <- gp + ylab("Peak height (RFU)")
  
  # Turn off clipping.
  #gt <- ggplot_gtable(ggplot_build(gp))
  #gt$layout$clip[gt$layout$name=="panel"] <- "off"
  #gp <- gridExtra::arrangeGrob(gt)
  
  # Show plot.
  if(!silent){
    print(gp)
  }
  
  # Debug info.
  if(debug){
    print(paste("EXIT:", match.call()[[1]]))
  }
  
  # Return plot object.
  return(gp)
  
}