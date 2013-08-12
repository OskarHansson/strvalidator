################################################################################
# TODO LIST
# TODO: http://stackoverflow.com/questions/17314058/ggplot2-control-number-of-panels-per-row-when-using-facet
# TODO: Option to drop markers? Autofind gender marker (fixed?).

################################################################################
# CHANGE LOG
# 19.07.2013: Added plot balance vs. 'H'.
# 19.07.2013: Changed edit widget to spinbutton for 'shape' and 'alpha'.
# 18.07.2013: Check before overwrite object.
# 15.07.2013: Removed section 'discard data'.
# 15.07.2013: Save as ggplot object to workspace instead of image.
# 15.07.2013: Added save GUI settings.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.
# 17.05.2013: save plot moved to external function.
# 17.05.2013: listDataFrames() -> listObjects()
# 29.04.2013: Various UI fixes. Remove NA rows before trim.
# 29.04.2013: New parameter debug (no longer defined in function).
# 21.04.2013: First version.

#' @title Plot Balance GUI
#'
#' @description
#' \code{plotBalance_gui} is a GUI simplifying the creation of plots from balance data.
#'
#' @details Plot balance data.
#' @param env environment in wich to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.

plotBalance_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gPlot <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.
  w <- gwindow(title="Plot balance", visible=FALSE)
  
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
  
  f0_samples_lbl <- glabel(text=" (0 samples)", container=f0)

  glabel(text=" and the kit used:", container=f0)

  kit_drp <- gdroplist(items=getKit(), 
                           selected = 1,
                           editable = FALSE,
                           container = f0) 

  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gData <<- get(val_obj, envir=env)
      # Check if suitable for plot balance...
  
      requiredCol <- c("Sample.Name", "Marker", "Hb", "Lb", "MpH")
      
      if(!all(requiredCol %in% colnames(.gData))){
  
        missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
        
        message <- paste("Additional columns required:\n",
                         paste(missingCol, collapse="\n"), sep="")
        
        gmessage(message, title="message",
                 icon = "error",
                 parent = w) 
        
        # Reset components.
        .gData <<- NULL
        svalue(f5_save_edt) <- ""
        svalue(dataset_drp, index=TRUE) <- 1
        svalue(f0_samples_lbl) <- " (0 samples)"
        
      } else {

        # Load or change components.
        .gDataName <<- val_obj

        # Suggest name.
        svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep="")
        
        svalue(f0_samples_lbl) <- paste(" (",
                                        length(unique(.gData$Sample.Name)),
                                        " samples)", sep="")

        # Detect kit.
        kitIndex <- detectKit(.gData)
        # Select in dropdown.
        svalue(kit_drp, index=TRUE) <- kitIndex
        
      }

    } else {
      
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(f0_samples_lbl) <- " (0 samples)"
      
    }    
    
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 

  grid1 <- glayout(container = f1, spacing = 1)

  grid1[1,1] <- glabel(text="Plot title:", container=grid1)
  grid1[1,2] <- title_edt <- gedit(text="Balance",
                                   width=40,
                                   container=grid1)
  
  grid1[2,1] <- glabel(text="X title:", container=grid1)
  grid1[2,2] <- x_title_edt <- gedit(text="Mean peak height",
                                     container=grid1)

  grid1[3,1] <- glabel(text="Y title:", container=grid1)
  grid1[3,2] <- y_title_edt <- gedit(text="Ratio",
                                     container=grid1)

  f1_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                                container=f1)
  
  # FRAME 7 ###################################################################
  
  f7 <- gframe(text = "Plot Balance data",
               horizontal=FALSE,
               container = gv) 
  
  grid7 <- glayout(container = f7)
  
  grid7[1,1] <- plot_hb_btn <- gbutton(text="Heterozygous balance vs. Height",
                                           border=TRUE,
                                           container=grid7) 
  
  grid7[1,2] <- plot_hb_h_btn <- gbutton(text="Heterozygous balance vs. 'H'",
                                       border=TRUE,
                                       container=grid7) 
  
  grid7[1,3] <- plot_lb_btn <- gbutton(text="Locus balance vs. Height",
                                       border=TRUE,
                                       container=grid7) 

  grid7[1,4] <- plot_lb_h_btn <- gbutton(text="Locus balance vs. 'H'",
                                       border=TRUE,
                                       container=grid7) 
  
  addHandlerChanged(plot_hb_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Hb", "Lb", "MpH")
    
    if(!all(requiredCol %in% colnames(.gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
      
      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_hb_btn) <- FALSE
      .plotBalance(what="Hb")
      enabled(plot_hb_btn) <- TRUE
      
    }
      
    # Change save button.
    svalue(f5_save_btn) <- "Save"
    enabled(f5_save_btn) <- TRUE
    
  } )
  
  addHandlerChanged(plot_hb_h_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Hb", "Lb", "H")
    
    if(!all(requiredCol %in% colnames(.gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
      
      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_hb_h_btn) <- FALSE
      .plotBalance(what="Hb_H")
      enabled(plot_hb_h_btn) <- TRUE
      
    }
    
    # Change save button.
    svalue(f5_save_btn) <- "Save"
    enabled(f5_save_btn) <- TRUE
    
  } )

  addHandlerChanged(plot_lb_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Hb", "Lb", "MpH")
    
    if(!all(requiredCol %in% colnames(.gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
      
      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_lb_btn) <- FALSE
      .plotBalance(what="Lb")
      enabled(plot_lb_btn) <- TRUE
      
    }
    
    # Change save button.
    svalue(f5_save_btn) <- "Save"
    enabled(f5_save_btn) <- TRUE
  
  } )

  addHandlerChanged(plot_lb_h_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Hb", "Lb", "H")
    
    if(!all(requiredCol %in% colnames(.gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
      
      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_lb_h_btn) <- FALSE
      .plotBalance(what="Lb_H")
      enabled(plot_lb_h_btn) <- TRUE
      
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
  
  e2 <- gexpandgroup(text="Data points",
               horizontal=FALSE,
               container = f1)
  
  grid2 <- glayout(container = e2)
  
  grid2[1,1] <- glabel(text="Shape:", container=grid2)
  grid2[1,2] <- shape_spb <- gspinbutton(from=0, to=25,
                                         by=1, value=18,
                                         container=grid2)

  grid2[1,3] <- glabel(text="Alpha:", container=grid2)
  grid2[1,4] <- alpha_spb <- gspinbutton(from=0, to=1,
                                         by=0.01, value=0.60,
                                         container=grid2)

  grid2[1,5] <- glabel(text="Jitter:", container=grid2)
  grid2[1,6] <- jitter_txt <- gedit(text="0.1", width=4, container=grid2)

  # FRAME 3 ###################################################################

  e3 <- gexpandgroup(text="Axes",
                     horizontal=FALSE,
                     container = f1)
  
  grid3 <- glayout(container = e3, spacing = 1)
  
  grid3[1,1:2] <- glabel(text="Limit Y axis (min-max)", container=grid3)
  grid3[2,1] <- y_min_txt <- gedit(text="", width=5, container=grid3)
  grid3[2,2] <- y_max_txt <- gedit(text="", width=5, container=grid3)

  grid3[3,1:2] <- glabel(text="Limit X axis (min-max)", container=grid3)
  grid3[4,1] <- x_min_txt <- gedit(text="", width=5, container=grid3)
  grid3[4,2] <- x_max_txt <- gedit(text="", width=5, container=grid3)

  grid3[1,3] <- glabel(text="    ", container=grid3) # Add some space.
  
  grid3[1,4] <- glabel(text="Scales:", container=grid3)
  grid3[2:4,4] <- scales_opt <- gradio(items=c("fixed","free_x","free_y","free"),
                                      selected = 2,
                                      horizontal = FALSE,
                                      container = grid3)
  
  # FRAME 4 ###################################################################
  
  e4 <- gexpandgroup(text="X labels",
                     horizontal=FALSE,
                     container = f1)
  
  grid4 <- glayout(container = e4)
  
  grid4[1,1] <- glabel(text="Text size (pts):", container=grid4)
  grid4[1,2] <- size_txt <- gedit(text="8", width=4, container=grid4)

  grid4[1,3] <- glabel(text="Angle:", container=grid4)
  grid4[1,4] <- angle_spb <- gspinbutton (from=0, to=360, by=1,
                                         value=270,
                                         container=grid4) 

  grid4[2,1] <- glabel(text="Justification (v/h):", container=grid4)
  grid4[2,2] <- vjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                          value=0.5,
                                          container=grid4)

  grid4[2,3] <- hjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                          value=0,
                                          container=grid4)

  
  
  # FUNCTIONS #################################################################
  
  
  .plotBalance <- function(what){
    
    # Get values.
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_shape <- as.numeric(svalue(shape_spb))
    val_alpha <- as.numeric(svalue(alpha_spb))
    val_jitter <- as.numeric(svalue(jitter_txt))
    val_ymin <- as.numeric(svalue(y_min_txt))
    val_ymax <- as.numeric(svalue(y_max_txt))
    val_xmin <- as.numeric(svalue(x_min_txt))
    val_xmax <- as.numeric(svalue(x_max_txt))
    val_angle <- as.numeric(svalue(angle_spb))
    val_vjust <- as.numeric(svalue(vjust_spb))
    val_hjust <- as.numeric(svalue(hjust_spb))
    val_size <- as.numeric(svalue(size_txt))
    val_scales <- svalue(scales_opt)
    val_kit <- svalue(kit_drp)
    
    if(debug){
      print("val_title")
      print(val_title)
      print("val_xtitle")
      print(val_xtitle)
      print("val_ytitle")
      print(val_ytitle)
      print("val_shape")
      print(val_shape)
      print("val_alpha")
      print(val_alpha)
      print("val_jitter")
      print(val_jitter)
      print("val_ymin")
      print(val_ymin)
      print("val_ymax")
      print(val_ymax)
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
    }
    
    
    if (!is.na(.gData) && !is.null(.gData)){
      
      
      # Call functions.
      # Add color information.
      if(is.null(.gData$Dye)){
        .gData <- addDye(data=.gData, kit=val_kit)
        warning("'Dye' is missing. Adding dye information.")

      }
      # Sort by marker in kit
      .gData <- sortMarkers(data=.gData,
                          kit=val_kit,
                          addMissingLevels = TRUE)
    
      # TODO: Option to drop markers? Autofind gender marker.
      # Drop Amelogenin:
      .gData <- .gData[.gData$Marker != "AMEL" & .gData$Marker != "Amelogenin", ]
      .gData$Marker <- factor(.gData$Marker, levels=levels(.gData$Marker)[levels(.gData$Marker)!="AMEL" & .gData$Marker != "Amelogenin"])
      
      # Create factors and round IMPORTANT!
      #.gData$Type <- factor(round(.gData$Type,2))
      
      # Sort Balance/allele factors. IMPORTANT!
      #.gData$Balance <- factor(.gData$Balance, levels=sort(unique(as.numeric(as.character(.gData$Balance)))))
      #.gData$Allele <- factor(.gData$Allele, levels=sort(unique(as.numeric(as.character(.gData$Allele)))))
      
      # Height must be numeric (not string).
      if(!is.numeric(.gData$MpH)){
        .gData$MpH <- as.numeric(as.character(.gData$MpH))
        warning("'MpH' not numeric, converting to numeric.")
        
      }
      
      # Calculate number of columns.
      val_ncol <- ceiling(length(unique(.gData$Marker)) / length(unique(.gData$Dye)))
      
      # Calculate number of columns.
      val_palette <- unique(dyeToColor(.gData)$Color)

      if(debug){
        print("Before plot: str(.gData)")
        print(str(.gData))
        print("Number of columns")
        print(val_ncol)
        print("levels(.gData$MpH)")
        print(levels(.gData$MpH))
        print("levels(.gData$Hb)")
        print(levels(.gData$Hb))
      }
      
      # Plotting alleles for observed Balances per marker.
      if(what == "Hb"){

        gp <- ggplot(.gData, aes_string(x="MpH", y="Hb", colour="Dye"))
        
      } else if (what == "Hb_H") {
        
        gp <- ggplot(.gData, aes_string(x="H", y="Hb", colour="Dye"))

      } else if (what == "Lb") {
        
        gp <- ggplot(.gData, aes_string(x="MpH", y="Lb", colour="Dye"))
        
      } else if (what == "Lb_H") {
        
        gp <- ggplot(.gData, aes_string(x="H", y="Lb", colour="Dye"))
        
      }
      
      gp <- gp + geom_point(shape=val_shape, alpha=val_alpha, position=position_jitter(width=val_jitter)) 
      gp <- gp + facet_grid(Dye ~ Marker) + facet_wrap(~ Marker, ncol=val_ncol, drop=FALSE, scales=val_scales)
      gp <- gp + scale_colour_manual(guide=FALSE,values=val_palette)
      
      if(!is.na(val_ymin) && !is.na(val_ymax)){
        gp <- gp + ylim(val_ymin,val_ymax)
      }
      
      if(!is.na(val_xmin) && !is.na(val_xmax)){
        gp <- gp + xlim(val_xmin,val_xmax)
      }

      gp <- gp + guides(fill = guide_legend(reverse=TRUE))
      gp <- gp + theme(axis.text.x=element_text(angle=val_angle, hjust=val_hjust, vjust=val_vjust, size=val_size))
      gp <- gp + labs(title=val_title)
      gp <- gp + xlab(val_xtitle)
      gp <- gp + ylab(val_ytitle)
      
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
      if(exists(".plotBalance_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".plotBalance_gui_savegui", envir=env)
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
      if(exists(".plotBalance_gui_title", envir=env, inherits = FALSE)){
        svalue(title_edt) <- get(".plotBalance_gui_title", envir=env)
      }
      if(exists(".plotBalance_gui_x_title", envir=env, inherits = FALSE)){
        svalue(x_title_edt) <- get(".plotBalance_gui_x_title", envir=env)
      }
      if(exists(".plotBalance_gui_y_title", envir=env, inherits = FALSE)){
        svalue(y_title_edt) <- get(".plotBalance_gui_y_title", envir=env)
      }
      if(exists(".plotBalance_gui_points_shape", envir=env, inherits = FALSE)){
        svalue(shape_spb) <- get(".plotBalance_gui_points_shape", envir=env)
      }
      if(exists(".plotBalance_gui_points_alpha", envir=env, inherits = FALSE)){
        svalue(alpha_spb) <- get(".plotBalance_gui_points_alpha", envir=env)
      }
      if(exists(".plotBalance_gui_points_jitter", envir=env, inherits = FALSE)){
        svalue(jitter_txt) <- get(".plotBalance_gui_points_jitter", envir=env)
      }
      if(exists(".plotBalance_gui_axes_y_min", envir=env, inherits = FALSE)){
        svalue(y_min_txt) <- get(".plotBalance_gui_axes_y_min", envir=env)
      }
      if(exists(".plotBalance_gui_axes_y_max", envir=env, inherits = FALSE)){
        svalue(y_max_txt) <- get(".plotBalance_gui_axes_y_max", envir=env)
      }
      if(exists(".plotBalance_gui_axes_x_min", envir=env, inherits = FALSE)){
        svalue(x_min_txt) <- get(".plotBalance_gui_axes_x_min", envir=env)
      }
      if(exists(".plotBalance_gui_axes_x_max", envir=env, inherits = FALSE)){
        svalue(x_max_txt) <- get(".plotBalance_gui_axes_x_max", envir=env)
      }
      if(exists(".plotBalance_gui_axes_scales", envir=env, inherits = FALSE)){
        svalue(scales_opt) <- get(".plotBalance_gui_axes_scales", envir=env)
      }
      if(exists(".plotBalance_gui_xlabel_size", envir=env, inherits = FALSE)){
        svalue(size_txt) <- get(".plotBalance_gui_xlabel_size", envir=env)
      }
      if(exists(".plotBalance_gui_xlabel_angle", envir=env, inherits = FALSE)){
        svalue(angle_spb) <- get(".plotBalance_gui_xlabel_angle", envir=env)
      }
      if(exists(".plotBalance_gui_xlabel_justh", envir=env, inherits = FALSE)){
        svalue(hjust_spb) <- get(".plotBalance_gui_xlabel_justh", envir=env)
      }
      if(exists(".plotBalance_gui_xlabel_justv", envir=env, inherits = FALSE)){
        svalue(vjust_spb) <- get(".plotBalance_gui_xlabel_justv", envir=env)
      }
      
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".plotBalance_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".plotBalance_gui_title", value=svalue(title_edt), envir=env)
      assign(x=".plotBalance_gui_x_title", value=svalue(x_title_edt), envir=env)
      assign(x=".plotBalance_gui_y_title", value=svalue(y_title_edt), envir=env)
      assign(x=".plotBalance_gui_points_shape", value=svalue(shape_spb), envir=env)
      assign(x=".plotBalance_gui_points_alpha", value=svalue(alpha_spb), envir=env)
      assign(x=".plotBalance_gui_points_jitter", value=svalue(jitter_txt), envir=env)
      assign(x=".plotBalance_gui_axes_y_min", value=svalue(y_min_txt), envir=env)
      assign(x=".plotBalance_gui_axes_y_max", value=svalue(y_max_txt), envir=env)
      assign(x=".plotBalance_gui_axes_x_min", value=svalue(x_min_txt), envir=env)
      assign(x=".plotBalance_gui_axes_x_max", value=svalue(x_max_txt), envir=env)
      assign(x=".plotBalance_gui_axes_scales", value=svalue(scales_opt), envir=env)
      assign(x=".plotBalance_gui_xlabel_size", value=svalue(size_txt), envir=env)
      assign(x=".plotBalance_gui_xlabel_angle", value=svalue(angle_spb), envir=env)
      assign(x=".plotBalance_gui_xlabel_justh", value=svalue(hjust_spb), envir=env)
      assign(x=".plotBalance_gui_xlabel_justv", value=svalue(vjust_spb), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".plotBalance_gui_savegui", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_savegui", envir = env)
      }
      if(exists(".plotBalance_gui_title", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_title", envir = env)
      }
      if(exists(".plotBalance_gui_x_title", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_x_title", envir = env)
      }
      if(exists(".plotBalance_gui_y_title", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_y_title", envir = env)
      }
      if(exists(".plotBalance_gui_points_shape", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_points_shape", envir = env)
      }
      if(exists(".plotBalance_gui_points_alpha", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_points_alpha", envir = env)
      }
      if(exists(".plotBalance_gui_points_jitter", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_points_jitter", envir = env)
      }
      if(exists(".plotBalance_gui_axes_y_min", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_axes_y_min", envir = env)
      }
      if(exists(".plotBalance_gui_axes_y_max", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_axes_y_max", envir = env)
      }
      if(exists(".plotBalance_gui_axes_x_min", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_axes_x_min", envir = env)
      }
      if(exists(".plotBalance_gui_axes_x_max", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_axes_x_max", envir = env)
      }
      if(exists(".plotBalance_gui_axes_scales", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_axes_scales", envir = env)
      }
      if(exists(".plotBalance_gui_xlabel_size", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_xlabel_size", envir = env)
      }
      if(exists(".plotBalance_gui_xlabel_angle", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_xlabel_angle", envir = env)
      }
      if(exists(".plotBalance_gui_xlabel_justh", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_xlabel_justh", envir = env)
      }
      if(exists(".plotBalance_gui_xlabel_justv", envir=env, inherits = FALSE)){
        remove(".plotBalance_gui_xlabel_justv", envir = env)
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
