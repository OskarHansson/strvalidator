################################################################################
# TODO LIST
# TODO: ...NOT FINISHED!!!

################################################################################
# CHANGE LOG
# 18.07.2013: Check before overwrite object.
# 15.07.2013: Save as ggplot object to workspace instead of image.
# 15.07.2013: Added save GUI settings.
# 04.06.2013: Fixed bug in 'missingCol'.
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
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.

#' 



plotDropout_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Load dependencies.  
  require(ggplot2)
  library(gWidgets)
  options(guiToolkit="RGtk2")

  # Global variables.
  .gData <- NULL
  .gDataColumns <- NULL
  .gPlot <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    #print(head(data))
  }
  
  # Main window.  
  w <- gwindow(title="Plot dropout data", visible=FALSE)
  
  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })

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
    .gData <<- get(val_obj, envir=env)

    # Check if suitable for plot dropout...
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height",
                     "Dropout", "Rfu", "Heterozygous")
    
    if(!all(requiredCol %in% colnames(.gData))){

      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
      
      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
      # Reset components.
      .gData <<- NULL
      .gDataColumns <<- NULL
      svalue(f5_save_edt) <- ""
      
    } else {

      # Load or change components.
      .gDataColumns <<- names(.gData)
      
      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep="")
      # Detect kit.
      kitIndex <- detectKit(.gData)
      # Select in dropdown.
      svalue(kit_drp, index=TRUE) <- kitIndex
      
    }
    
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 

  grid1 <- glayout(container = f1, spacing = 1)

  grid1[1,1] <- glabel(text="Plot title:", container=grid1)
  grid1[1,2] <- title_edt <- gedit(text="Allele and locus dropout",
                                   width=40,
                                   container=grid1)
  
  grid1[2,1] <- glabel(text="X title:", container=grid1)
  grid1[2,2] <- x_title_edt <- gedit(text="",
                                     container=grid1)

  grid1[3,1] <- glabel(text="Y title:", container=grid1)
  grid1[3,2] <- y_title_edt <- gedit(text="Marker",
                                     container=grid1)

  f1_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f1)
  
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
    
    if(!all(requiredCol %in% colnames(.gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]

      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {

      enabled(plot_h_btn) <- FALSE
      .plotStutter(what="heat_h")
      enabled(plot_h_btn) <- TRUE
      
    }
    
    # Change save button.
    svalue(f5_save_btn) <- "Save"
    enabled(f5_save_btn) <- TRUE
    
  } )

  addHandlerChanged(plot_amount_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "Amount")
    
    if(!all(requiredCol %in% colnames(.gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]

      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_amount_btn) <- FALSE
      .plotStutter(what="heat_amount")
      enabled(plot_amount_btn) <- TRUE
      
    }
    
    # Change save button.
    svalue(f5_save_btn) <- "Save"
    enabled(f5_save_btn) <- TRUE
    
  } )
  
  addHandlerChanged(plot_conc_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "Concentration")
    
    if(!all(requiredCol %in% colnames(.gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]

      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_conc_btn) <- FALSE
      .plotStutter(what="heat_conc")
      enabled(plot_conc_btn) <- TRUE
      
    }
    
    # Change save button.
    svalue(f5_save_btn) <- "Save"
    enabled(f5_save_btn) <- TRUE
    
  } )
  
  # FRAME 5 ###################################################################
  
  f5 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f5)
  
  f5_save_edt <- gedit(text="", container=f5)
  
  f5_save_btn <- gbutton(text = "Save",
                         border=TRUE,
                         container = f5) 
  
  addHandlerChanged(f5_save_btn, handler = function(h, ...) {
    
    val_name <- svalue(f5_save_edt)

    # Change button.
    svalue(f5_save_btn) <- "Processing..."
    enabled(f5_save_btn) <- FALSE
    
    # Save data.
    saveObject(name=val_name, object=.gPlot, parent=w, env=env)
    
    # Change button.
    svalue(f5_save_btn) <- "Saved"
    
  } )
  
  # ADVANCED OPTIONS ##########################################################
  
  # FRAME 4 ###################################################################
  
  e4 <- gexpandgroup(text="X labels",
                     horizontal=FALSE,
                     container = f1)
  
  grid4 <- glayout(container = e4)
  
  grid4[1,1] <- glabel(text="Text size (pts):", container=grid4)
  grid4[1,2] <- size_txt <- gedit(text="10", width=4, container=grid4)

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
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
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
      print("str(.gData)")
      print(str(.gData))
      print("levels(.gData$Allele)")
      print(levels(.gData$Allele))
      print("levels(.gData$Stutter)")
      print(levels(.gData$Stutter))
    }
    
    
    if (!is.na(.gData) && !is.null(.gData)){
      
      
      # Call functions.
      
      # Color information.
      if(is.null(.gData$Dye)){
        .gData <- addDye(data=.gData, kit=val_kit)
      }

      # Sort by marker in kit
      .gData <- sortMarkers(data=.gData,
                          kit=val_kit,
                          addMissingLevels = TRUE)
      
      
      if(debug){
        print("Before plot: str(.gData)")
        print(str(.gData))
      }
      
      # Plotting...
      if(what == "heat_h"){
        
        # Sort according to average peak height 'H'
        .gData <- .gData[order(.gData$H),]
        .gData$H<-as.integer(.gData$H)
        .gData$H<-factor(.gData$H)
        .gData$Dropout<-factor(.gData$Dropout)
        col<-c(rgb(0,0.737,0), rgb(1,0.526,1), rgb(0.526,0,0.526))
        gp <- ggplot(.gData, aes_string(x = "H", y = "Marker", fill = "Dropout"))
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
        gp <- gp + scale_y_discrete(limits = rev(levels(.gData$Marker)) )

      } else if (what == "heat_amount") {
        
        # Sort according to average amount of DNA
        .gData <- .gData[order(.gData$Amount),]
        
        # Add levels
        # NB! JUST ONE TIME BEFORE PLOTTING
        # If string save as numeric.
        #.gData$Amount<-as.numeric(.gData$Amount)
        # If factors convert to numeric.
        #.gData$Amount<-as.numeric(levels(.gData$Amount))[.gData$Amount]
        #.gData$Amount<-ifelse(is.na(.gData$Amount),0,.gData$Amount)
        
        .gData$Sample.Name<-paste(.gData$Amount, " (", .gData$Sample.Name, ")", sep="")
        
        .gData <- .gData [order(.gData$Amount),]
        
        .gData$Dropout<-factor(.gData$Dropout)
        
        #.gData<-sortMarkers(data=.gData,kit=val_kit,addMissingLevels = TRUE)
        
        xlabels <- .gData[!duplicated(.gData[, c("Sample.Name", "Amount")]), ]$Amount
        xlabels <- round(as.double(xlabels), digits=2)
        
        col<-c(rgb(0,0.737,0), rgb(1,0.526,1), rgb(0.526,0,0.526))

        gp <- ggplot(.gData, aes_string(x = "Sample.Name", y = "Marker", fill = "Dropout"))
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
        gp <- gp + scale_y_discrete(limits = rev(levels(.gData$Marker))) + 
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
        # .gData <- newdata[order(newdata$Ratio),]
        # .gData <- newdata[order(newdata$Proportion),]
      
        # Mx data:
        # .gData$Sample.Name<-paste(.gData$Ratio, " (", .gData$Sample.Name, ")", sep="")
        # .gData$Sample.Name<-paste(.gData$Proportion, " (", .gData$Sample.Name, ")", sep="")
      
        # Mx data:
        # .gData <- .gData [order(.gData$Ratio),]
        # .gData <- .gData [order(.gData$Proportion),]
      
        # Mx data SGM Plus.
        # .gData<-addDye(.gData,"SGM Plus")
        # .gData<-sortMarkers(.gData,"SGM Plus")
      
        # Mx Data:
        # xlabels<-.gData[!duplicated(.gData[, c("Sample.Name", "Ratio")]), ]$Ratio
        # xlabels<-.gData[!duplicated(.gData[, c("Sample.Name", "Proportion")]), ]$Proportion
      
        # Mx data:
        # hm.title <- "Heatmap: allele and locus dropout for 'F' SGM Plus (3500)"
        # hm.xlab <- "Proportion"
        # Mx data:
      
        #gp <- gp + scale_y_discrete(limits = rev(levels(.gData$Marker))) + 
        #  scale_x_discrete(labels=formatC(xlabels, 4, format = "f")) +
        #  theme(axis.text.x=element_text(angle=-90, hjust = 0, vjust = 0.4, size = 10))

      }

      
      print(gp)
      
      # Store in global variable.
      .gPlot <<- gp
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  }

  # INTERNAL FUNCTIONS ########################################################
  
  .loadSavedSettings <- function(){
    
    # First check status of save flag.
    if(!is.null(savegui)){
      svalue(f1_savegui_chk) <- savegui
      enabled(f1_savegui_chk) <- FALSE
      if(debug){
        print("Save GUI status set!")
      }  
    } else {
      # Load save flag.
      if(exists(".plotDropout_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".plotDropout_gui_savegui", envir=env)
      }
      if(debug){
        print("Save GUI status loaded!")
      }  
    }
    if(debug){
      print(svalue(f1_savegui_chk))
    }  
    
    # Then load settings if true.
    if(svalue(f1_savegui_chk)){
      if(exists(".plotDropout_gui_title", envir=env, inherits = FALSE)){
        svalue(title_edt) <- get(".plotDropout_gui_title", envir=env)
      }
      if(exists(".plotDropout_gui_x_title", envir=env, inherits = FALSE)){
        svalue(x_title_edt) <- get(".plotDropout_gui_x_title", envir=env)
      }
      if(exists(".plotDropout_gui_y_title", envir=env, inherits = FALSE)){
        svalue(y_title_edt) <- get(".plotDropout_gui_y_title", envir=env)
      }
#       if(exists(".plotDropout_gui_points_shape", envir=env, inherits = FALSE)){
#         svalue(shape_txt) <- get(".plotDropout_gui_points_shape", envir=env)
#       }
#       if(exists(".plotDropout_gui_points_alpha", envir=env, inherits = FALSE)){
#         svalue(alpha_txt) <- get(".plotDropout_gui_points_alpha", envir=env)
#       }
#       if(exists(".plotDropout_gui_points_jitterh", envir=env, inherits = FALSE)){
#         svalue(jitterh_txt) <- get(".plotDropout_gui_points_jitterh", envir=env)
#       }
#       if(exists(".plotDropout_gui_points_jitterv", envir=env, inherits = FALSE)){
#         svalue(jitterv_txt) <- get(".plotDropout_gui_points_jitterv", envir=env)
#       }
#       if(exists(".plotDropout_gui_axes_y_min", envir=env, inherits = FALSE)){
#         svalue(y_min_txt) <- get(".plotDropout_gui_axes_y_min", envir=env)
#       }
#       if(exists(".plotDropout_gui_axes_y_max", envir=env, inherits = FALSE)){
#         svalue(y_max_txt) <- get(".plotDropout_gui_axes_y_max", envir=env)
#       }
#       if(exists(".plotDropout_gui_axes_x_min", envir=env, inherits = FALSE)){
#         svalue(x_min_txt) <- get(".plotDropout_gui_axes_x_min", envir=env)
#       }
#       if(exists(".plotDropout_gui_axes_x_max", envir=env, inherits = FALSE)){
#         svalue(x_max_txt) <- get(".plotDropout_gui_axes_x_max", envir=env)
#       }
      if(exists(".plotDropout_gui_xlabel_size", envir=env, inherits = FALSE)){
        svalue(size_txt) <- get(".plotDropout_gui_xlabel_size", envir=env)
      }
      if(exists(".plotDropout_gui_xlabel_angle", envir=env, inherits = FALSE)){
        svalue(angle_spb) <- get(".plotDropout_gui_xlabel_angle", envir=env)
      }
      if(exists(".plotDropout_gui_xlabel_justh", envir=env, inherits = FALSE)){
        svalue(hjust_spb) <- get(".plotDropout_gui_xlabel_justh", envir=env)
      }
      if(exists(".plotDropout_gui_xlabel_justv", envir=env, inherits = FALSE)){
        svalue(vjust_spb) <- get(".plotDropout_gui_xlabel_justv", envir=env)
      }
      
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".plotDropout_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".plotDropout_gui_title", value=svalue(title_edt), envir=env)
      assign(x=".plotDropout_gui_x_title", value=svalue(x_title_edt), envir=env)
      assign(x=".plotDropout_gui_y_title", value=svalue(y_title_edt), envir=env)
#       assign(x=".plotDropout_gui_points_plot", value=svalue(e2_plotpoints_chk), envir=env)
#       assign(x=".plotDropout_gui_points_shape", value=svalue(shape_txt), envir=env)
#       assign(x=".plotDropout_gui_points_alpha", value=svalue(alpha_txt), envir=env)
#       assign(x=".plotDropout_gui_points_jitterh", value=svalue(jitterh_txt), envir=env)
#       assign(x=".plotDropout_gui_points_jitterv", value=svalue(jitterv_txt), envir=env)
#       assign(x=".plotDropout_gui_axes_y_min", value=svalue(y_min_txt), envir=env)
#       assign(x=".plotDropout_gui_axes_y_max", value=svalue(y_max_txt), envir=env)
#       assign(x=".plotDropout_gui_axes_x_min", value=svalue(x_min_txt), envir=env)
#       assign(x=".plotDropout_gui_axes_x_max", value=svalue(x_max_txt), envir=env)
      assign(x=".plotDropout_gui_xlabel_size", value=svalue(size_txt), envir=env)
      assign(x=".plotDropout_gui_xlabel_angle", value=svalue(angle_spb), envir=env)
      assign(x=".plotDropout_gui_xlabel_justh", value=svalue(hjust_spb), envir=env)
      assign(x=".plotDropout_gui_xlabel_justv", value=svalue(vjust_spb), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".plotDropout_gui_savegui", envir=env, inherits = FALSE)){
        remove(".plotDropout_gui_savegui", envir = env)
      }
      if(exists(".plotDropout_gui_title", envir=env, inherits = FALSE)){
        remove(".plotDropout_gui_title", envir = env)
      }
      if(exists(".plotDropout_gui_x_title", envir=env, inherits = FALSE)){
        remove(".plotDropout_gui_x_title", envir = env)
      }
      if(exists(".plotDropout_gui_y_title", envir=env, inherits = FALSE)){
        remove(".plotDropout_gui_y_title", envir = env)
      }
#       if(exists(".plotDropout_gui_points_plot", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_points_plot", envir = env)
#       }
#       if(exists(".plotDropout_gui_points_shape", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_points_shape", envir = env)
#       }
#       if(exists(".plotDropout_gui_points_alpha", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_points_alpha", envir = env)
#       }
#       if(exists(".plotDropout_gui_points_jitterh", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_points_jitterh", envir = env)
#       }
#       if(exists(".plotDropout_gui_points_jitterv", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_points_jitterv", envir = env)
#       }
#       if(exists(".plotDropout_gui_axes_y_min", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_axes_y_min", envir = env)
#       }
#       if(exists(".plotDropout_gui_axes_y_max", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_axes_y_max", envir = env)
#       }
#       if(exists(".plotDropout_gui_axes_x_min", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_axes_x_min", envir = env)
#       }
#       if(exists(".plotDropout_gui_axes_x_max", envir=env, inherits = FALSE)){
#         remove(".plotDropout_gui_axes_x_max", envir = env)
#       }
      if(exists(".plotDropout_gui_xlabel_size", envir=env, inherits = FALSE)){
        remove(".plotDropout_gui_xlabel_size", envir = env)
      }
      if(exists(".plotDropout_gui_xlabel_angle", envir=env, inherits = FALSE)){
        remove(".plotDropout_gui_xlabel_angle", envir = env)
      }
      if(exists(".plotDropout_gui_xlabel_justh", envir=env, inherits = FALSE)){
        remove(".plotDropout_gui_xlabel_justh", envir = env)
      }
      if(exists(".plotDropout_gui_xlabel_justv", envir=env, inherits = FALSE)){
        remove(".plotDropout_gui_xlabel_justv", envir = env)
      }
      
      
      if(debug){
        print("Settings cleared!")
      }
    }
    
    if(debug){
      print("Settings saved!")
    }
    
  }
  
  # END GUI ###################################################################
  
  # Load GUI settings.
  .loadSavedSettings()
  
  # Show GUI.
  visible(w) <- TRUE
  
}
