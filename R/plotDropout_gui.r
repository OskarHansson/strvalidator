################################################################################
# TODO LIST
# TODO: ...NOT FINISHED!!!

################################################################################
# CHANGE LOG
# 17.05.2013: save plot moved to external function.
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: First version.

#' @title Plot Dropout GUI
#'
#' @description
#' \code{plotDropout_gui} is a GUI simplifying the creation of plots from dropout data.
#'
#' @details Plot dropout data.
#' @param env environment in wich to search for data frames and save result.
#' @param debug logical indicating printing debug information.

#' 



plotDropout_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require(ggplot2)
  library(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- NULL
  gData_org <- NULL
  gData_name <- NULL
  gDataColumns <- NULL
  dropout_plot <- NULL
  separator <- .Platform$file.sep # Platform dependent path separator.
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    #print(head(data))
  }
  
  
  w <- gwindow(title="Plot dropout data", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Dataset and kit",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Select dataset:", container=f0)

  dataset_drp <- gdroplist(items=c("<Select dataset>",
                                   listObjects(env=env,
                                               objClass="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = f0) 
  
  glabel(text=" and the kit used:", container=f0)

  kit_drp <- gdroplist(items=getKit(), 
                           selected = 1,
                           editable = FALSE,
                           container = f0) 

  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    gData <<- get(val_obj, envir=env)

    # Check if suitable for plot dropout...
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height",
                     "Dropout", "Rfu", "Heterozygous")
    
    if(!all(requiredCol %in% colnames(gData))){

      gData <<- NULL
      gDataColumns <<- NULL
      missingCol <- requiredCol[!requiredCol %in% colnames(gData)]
      
      message <- paste("The following columns are required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {

      gDataColumns <<- names(gData)
      gData_name <<- val_obj
      gData_org <<- gData
      
      # Detect kit.
      kitIndex <- detectKit(gData)
      # Select in dropdown.
      svalue(kit_drp, index=TRUE) <- kitIndex
      
    }
    
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Plot settings",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 

  grid1 <- glayout(container = f1, spacing = 1)

  grid1[1,1] <- glabel(text="Plot title:", container=grid1)
  grid1[1,2] <- title_txt <- gedit(text="Allele and locus dropout",
                                   width=40,
                                   container=grid1)
  
  grid1[2,1] <- glabel(text="X title:", container=grid1)
  grid1[2,2] <- x_title_txt <- gedit(text="",
                                     container=grid1)

  grid1[3,1] <- glabel(text="Y title:", container=grid1)
  grid1[3,2] <- y_title_txt <- gedit(text="Marker",
                                     container=grid1)

  # FRAME 7 ###################################################################
  
  f7 <- gframe(text = "Plot heatmap by",
               horizontal=FALSE,
               container = gv) 
  
  grid7 <- glayout(container = f7)
  
  grid7[1,1] <- plot_h_btn <- gbutton(text="Average peak height",
                                           border=TRUE,
                                           container=grid7) 

  grid7[1,2] <- plot_amount_btn <- gbutton(text="Amount",
                                           border=TRUE,
                                           container=grid7) 

  grid7[1,3] <- plot_conc_btn <- gbutton(text="Concentration",
                                           border=TRUE,
                                           container=grid7) 

  addHandlerChanged(plot_h_btn, handler = function(h, ...) {

    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "H")
    
    if(!all(requiredCol %in% colnames(gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(gData)]

      message <- paste("Additional columns are required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {

      enabled(plot_h_btn) <- FALSE
      .plotStutter(what="heat_h")
      enabled(plot_h_btn) <- TRUE
      
    }
    
  } )

  addHandlerChanged(plot_amount_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "Amount")
    
    if(!all(requiredCol %in% colnames(gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(gData)]

      message <- paste("Additional columns are required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_amount_btn) <- FALSE
      .plotStutter(what="heat_amount")
      enabled(plot_amount_btn) <- TRUE
      
    }
    
  } )
  
  addHandlerChanged(plot_conc_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "Concentration")
    
    if(!all(requiredCol %in% colnames(gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(gData)]

      message <- paste("Additional columns are required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_conc_btn) <- FALSE
      .plotStutter(what="heat_conc")
      enabled(plot_conc_btn) <- TRUE
      
    }
    
  } )
  
  # FRAME 5 ###################################################################
  
  f5 <- gframe(text = "Save plot as image",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  grid5 <- glayout(container = f5)
  
  grid5[1,1] <- g5_save_btn <- gbutton(text = "Save",
                                       border=TRUE,
                                       container = grid5) 
  
  
  addHandlerChanged(g5_save_btn, handler = function(h, ...) {
    
    savePlot_gui(object=dropout_plot)
    
  } )
  
  # ADVANCED OPTIONS ##########################################################
  
  # FRAME 4 ###################################################################
  
  e4 <- gexpandgroup(text="X labels",
                     horizontal=FALSE,
                     container = f1)
  
  grid4 <- glayout(container = e4)
  
  grid4[1,1] <- glabel(text="Text size (pts):", container=grid4)
  grid4[1,2] <- size_txt <- gedit(text="5", width=4, container=grid4)

  grid4[1,3] <- glabel(text="Angle:", container=grid4)
  grid4[1,4] <- angle_spb <- gspinbutton (from=0, to=360, by=1,
                                         value=270,
                                         container=grid4) 

  grid4[2,1] <- glabel(text="Justification (v/h):", container=grid4)
  grid4[2,2] <- vjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                          value=0.3,
                                          container=grid4)

  grid4[2,3] <- hjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                          value=1,
                                          container=grid4)

  # FUNCTIONS #################################################################
  
  
  .plotStutter <- function(what){
    
    # Get values.
    val_title <- svalue(title_txt)
    val_xtitle <- svalue(x_title_txt)
    val_ytitle <- svalue(y_title_txt)
    val_angle <- as.numeric(svalue(angle_spb))
    val_vjust <- as.numeric(svalue(vjust_spb))
    val_hjust <- as.numeric(svalue(hjust_spb))
    val_size <- as.numeric(svalue(size_txt))
    val_kit <- svalue(kit_drp)
    
    if(debug){
      print("val_title")
      print(val_title)
      print("val_xtitle")
      print(val_xtitle)
      print("val_ytitle")
      print(val_ytitle)
      print("val_angle")
      print(val_angle)
      print("val_vjust")
      print(val_vjust)
      print("val_hjust")
      print(val_hjust)
      print("val_size")
      print(val_size)
      print("str(gData)")
      print(str(gData))
      print("levels(gData$Allele)")
      print(levels(gData$Allele))
      print("levels(gData$Stutter)")
      print(levels(gData$Stutter))
    }
    
    
    if (!is.na(gData) && !is.null(gData)){
      
      
      # Call functions.
      
      # Color information.
      if(is.null(gData$Dye)){
        gData <- addDye(data=gData, kit=val_kit)
      }

      # Sort by marker in kit
      gData <- sortMarkers(data=gData,
                          kit=val_kit,
                          addMissingLevels = TRUE)
      
      
      if(debug){
        print("Before plot: str(gData)")
        print(str(gData))
      }
      
      # Plotting...
      if(what == "heat_h"){
        
        # Sort according to average peak height 'H'
        gData <- gData[order(gData$H),]
        gData$H<-as.integer(gData$H)
        gData$H<-factor(gData$H)
        gData$Dropout<-factor(gData$Dropout)
        col<-c(rgb(0,0.737,0), rgb(1,0.526,1), rgb(0.526,0,0.526))
        gp <- ggplot(gData, aes_string(x = "H", y = "Marker", fill = "Dropout"))
        gp <- gp + geom_tile(colour = "white")
        gp <- gp + scale_fill_manual(values=col,
                                     name="Dropout",
                                     breaks=c("0", "1", "2"),
                                     labels=c("none", "allele", "locus"))
        gp <- gp + guides(fill = guide_legend(reverse=TRUE))
        gp <- gp + theme(axis.text.x=element_text(angle=val_angle,
                                                  vjust = val_vjust,
                                                  size = val_size))
        gp <- gp + labs(title=val_title)
        gp <- gp + xlab(val_xtitle)
        gp <- gp + ylab(val_ytitle)

        # Reverse y-axis.
        gp <- gp + scale_y_discrete(limits = rev(levels(gData$Marker)) )

      } else if (what == "heat_amount") {
        
        # Sort according to average amount of DNA
        gData <- gData[order(gData$Amount),]
        
        # Add levels
        # NB! JUST ONE TIME BEFORE PLOTTING
        # If string save as numeric.
        #gData$Amount<-as.numeric(gData$Amount)
        # If factors convert to numeric.
        #gData$Amount<-as.numeric(levels(gData$Amount))[gData$Amount]
        #gData$Amount<-ifelse(is.na(gData$Amount),0,gData$Amount)
        
        gData$Sample.Name<-paste(gData$Amount, " (", gData$Sample.Name, ")", sep="")
        
        gData <- gData [order(gData$Amount),]
        
        gData$Dropout<-factor(gData$Dropout)
        
        #gData<-sortMarkers(data=gData,kit=val_kit,addMissingLevels = TRUE)
        
        xlabels <- gData[!duplicated(gData[, c("Sample.Name", "Amount")]), ]$Amount
        xlabels <- round(as.double(xlabels), digits=2)
        
        col<-c(rgb(0,0.737,0), rgb(1,0.526,1), rgb(0.526,0,0.526))

        gp <- ggplot(gData, aes_string(x = "Sample.Name", y = "Marker", fill = "Dropout"))
        gp <- gp + geom_tile(colour = "white") #OK
        gp <- gp + scale_fill_manual(values=col, name="Dropout", breaks=c("0", "1", "2"),
                                               labels=c("none", "allele", "locus"))
        gp <- gp + guides(fill = guide_legend(reverse=TRUE)) # OK
        gp <- gp + theme(axis.text.x=element_text(angle=val_angle,
                                                  hjust = val_hjust,
                                                  vjust = val_vjust,
                                                  size = val_size))
        gp <- gp + labs(title=val_title)
        gp <- gp + ylab(val_ytitle)
        gp <- gp + xlab(val_xtitle)

        # Reverse y-axis and relabel x-ticks.
        gp <- gp + scale_y_discrete(limits = rev(levels(gData$Marker))) + 
          scale_x_discrete(labels=formatC(xlabels, 2, format = "f")) +
          theme(axis.text.x=element_text(family="sans", face="bold", size=6))
        
        
      } else if (what == "heat_concentration") {
        
      } else if (what == "heat_mx") {

        # Mx Data: Use this to add quant/proportion data if only key samples has been quantified.
        # newdata<-addData(newdata, quant, byCol="Sample.Name")
        # newdata<-addData(newdata, prop, byCol="Sample.Name")
        
        # Mx Data:
        # newdata<-newdata[,grepl("Sample.Name|Marker|Dropout|Ratio",names(newdata))]
        # newdata<-newdata[,grepl("Sample.Name|Marker|Dropout|Proportion",names(newdata))]
        
        # Mx Data:
        # gData <- newdata[order(newdata$Ratio),]
        # gData <- newdata[order(newdata$Proportion),]
      
        # Mx data:
        # gData$Sample.Name<-paste(gData$Ratio, " (", gData$Sample.Name, ")", sep="")
        # gData$Sample.Name<-paste(gData$Proportion, " (", gData$Sample.Name, ")", sep="")
      
        # Mx data:
        # gData <- gData [order(gData$Ratio),]
        # gData <- gData [order(gData$Proportion),]
      
        # Mx data SGM Plus.
        # gData<-addDye(gData,"SGM Plus")
        # gData<-sortMarkers(gData,"SGM Plus")
      
        # Mx Data:
        # xlabels<-gData[!duplicated(gData[, c("Sample.Name", "Ratio")]), ]$Ratio
        # xlabels<-gData[!duplicated(gData[, c("Sample.Name", "Proportion")]), ]$Proportion
      
        # Mx data:
        # hm.title <- "Heatmap: allele and locus dropout for 'F' SGM Plus (3500)"
        # hm.xlab <- "Proportion"
        # Mx data:
      
        #gp <- gp + scale_y_discrete(limits = rev(levels(gData$Marker))) + 
        #  scale_x_discrete(labels=formatC(xlabels, 4, format = "f")) +
        #  theme(axis.text.x=element_text(angle=-90, hjust = 0, vjust = 0.4, size = 10))

      }

      
      print(gp)
      
      # Store in global variable.
      dropout_plot <<- gp
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  }
  
  # Show GUI.
  visible(w) <- TRUE
  
}
