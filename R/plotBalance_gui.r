################################################################################
# TODO LIST
# TODO: Option to drop markers? Autofind gender marker.

################################################################################
# CHANGE LOG
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
#' @param debug logical indicating printing debug information.

#' 


plotBalance_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.
  require(pcrsim)
  require(ggplot2)
  library(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- NULL
  gDataName <- NULL
  balance_plot <- NULL
  separator <- .Platform$file.sep # Platform dependent path separator.
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Plot balance", visible=FALSE)
  
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
    
    if(exists(val_obj, envir=env)){
      
      gData <<- get(val_obj, envir=env)
      # Check if suitable for plot balance...
  
      requiredCol <- c("Sample.Name", "Marker", "Hb", "Lb", "MpH")
      
      if(!all(requiredCol %in% colnames(gData))){
  
        gData <<- NULL

        svalue(dataset_drp, index=TRUE) <- 1

        svalue(f0_samples_lbl) <- " (0 samples)"
        
        svalue(low_txt) <- ""
        svalue(high_txt) <- ""

        message <- paste("The dataset is not a balance table\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="message",
                 icon = "info",
                 parent = w) 
        
      } else {
  
        gDataName <<- val_obj
  
        svalue(f0_samples_lbl) <- paste(" (",
                                        length(unique(gData$Sample.Name)),
                                        " samples)", sep="")
        # Get low/high.        
        svalue(low_txt) <- min(gData$MpH, na.rm=TRUE)
        svalue(high_txt) <- max(gData$MpH, na.rm=TRUE)
        
        # Detect kit.
        kitIndex <- detectKit(gData)
        # Select in dropdown.
        svalue(kit_drp, index=TRUE) <- kitIndex
        
      }

    } else {
      
      gData <<- NULL

      svalue(f0_samples_lbl) <- " (0 samples)"

      svalue(low_txt) <- ""
      svalue(high_txt) <- ""
      
    }    
    
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Plot settings",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 

  grid1 <- glayout(container = f1, spacing = 1)

  grid1[1,1] <- glabel(text="Plot title:", container=grid1)
  grid1[1,2] <- title_txt <- gedit(text="Balance",
                                   width=40,
                                   container=grid1)
  
  grid1[2,1] <- glabel(text="X title:", container=grid1)
  grid1[2,2] <- x_title_txt <- gedit(text="Mean peak height",
                                     container=grid1)

  grid1[3,1] <- glabel(text="Y title:", container=grid1)
  grid1[3,2] <- y_title_txt <- gedit(text="Ratio",
                                     container=grid1)

  # FRAME 7 ###################################################################
  
  f7 <- gframe(text = "Plot Balance data",
               horizontal=FALSE,
               container = gv) 
  
  grid7 <- glayout(container = f7)
  
  grid7[1,1] <- plot_hb_btn <- gbutton(text="Heterozygous balance vs. Height",
                                           border=TRUE,
                                           container=grid7) 
  
  grid7[1,2] <- plot_lb_btn <- gbutton(text="Locus balance vs. Height",
                                           border=TRUE,
                                           container=grid7) 
  
  addHandlerChanged(plot_hb_btn, handler = function(h, ...) {
    
    enabled(plot_hb_btn) <- FALSE
    .plotBalance(what="Hb")
    enabled(plot_hb_btn) <- TRUE
    
  } )
  
  addHandlerChanged(plot_lb_btn, handler = function(h, ...) {
    
    enabled(plot_lb_btn) <- FALSE
    .plotBalance(what="Lb")
    enabled(plot_lb_btn) <- TRUE
    
  } )

  # FRAME 5 ###################################################################

  f5 <- gframe(text = "Save plot as image",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  grid5 <- glayout(container = f5)
  
  grid5[1,1] <- g5_save_btn <- gbutton(text = "Save plot",
                                       border=TRUE,
                                       container = grid5) 
  
  
  addHandlerChanged(g5_save_btn, handler = function(h, ...) {
    
    savePlot_gui(object=balance_plot)
    
  } )
  
  # ADVANCED OPTIONS ##########################################################
  
  e2 <- gexpandgroup(text="Data points",
               horizontal=FALSE,
               container = f1)
  
  grid2 <- glayout(container = e2)
  
  grid2[1,1] <- glabel(text="Shape:", container=grid2)
  grid2[1,2] <- shape_txt <- gedit(text="18", width=4, container=grid2)

  grid2[1,3] <- glabel(text="Alpha:", container=grid2)
  grid2[1,4] <- alpha_txt <- gedit(text="0.6", width=4, container=grid2)

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
                                          value=1,
                                          container=grid4)

  
  # FRAME 6 ###################################################################
  
  f6 <- gframe(text = "Discard data based on mean peak height",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  grid6 <- glayout(container = f6, spacing = 1)
  
  grid6[1,1] <- trim_btn <- gbutton(text="Trim dataset",
                                    border=TRUE,
                                    container = grid6) 
  
  grid6[1,2] <- glabel(text="  Low", container=grid6)
  grid6[1,3] <- low_txt <- gedit(text="", width=6, container=grid6)
  grid6[1,4] <- glabel(text="  High", container=grid6)
  grid6[1,5] <- high_txt <- gedit(text="", width=6, container=grid6)
  
  grid6[2,1] <- g6_save_btn <- gbutton(text="Save As",
                                     border=TRUE,
                                     container = grid6) 
  
  grid6[2,2:5] <- g6_save_txt <- gedit(text=gDataName,
                                     container=grid6,
                                     anchor=c(-1 ,0))
  
  
  addHandlerChanged(g6_save_btn, handler = function(h, ...) {
    
    val_name <- svalue(g6_save_txt)
    val_data <- gData
    
    if (!is.na(val_data) && !is.null(val_data)){

      # Save data.
      assign(val_name, val_data, envir=env)
      
      # Refresh.
      selected <- svalue(dataset_drp)
      dataset_drp[] <- c("<Select dataset>", listObjects(env=env,
                                                         objClass="data.frame")) 
      svalue(dataset_drp) <- selected
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  } )
  
  
  addHandlerChanged(trim_btn, handler = function(h, ...) {
    
    val_low <- as.numeric(svalue(low_txt))
    val_high <- as.numeric(svalue(high_txt))
    val_name <- gDataName

    if(debug){
      print("low")
      print(val_low)
      print("high")
      print(val_high)
      print("head(gData):")
      print(head(gData))
      print(nrow(gData))
    }
    
    enabled(trim_btn) <- FALSE
    
    # Remove NA rows.
    n1 <- nrow(gData)
    gData <<- gData[!is.na(gData$MpH), ]
    n2 <- nrow(gData)
    message(paste(n1-n2, "rows with MpH=NA removed."))

    # Crop data.
    n1 <- nrow(gData)
    gData <<- gData[gData$MpH<=val_high, ]
    n2 <- nrow(gData)
    message(paste(n1-n2, "rows with MpH>high removed."))
    n1 <- nrow(gData)
    gData <<- gData[gData$MpH>=val_low, ]
    n2 <- nrow(gData)
    message(paste(n1-n2, "rows with MpH<low removed."))

    enabled(trim_btn) <- TRUE

    if(debug){
      print("head(gData):")
      print(head(gData))
      print(nrow(gData))
    }
    
    # Create new name.
    newName <- paste(val_name,"_",val_low,"-",val_high, sep="")
    svalue(g6_save_txt) <- newName
    
  } )

  
  # FUNCTIONS #################################################################
  
  
  .plotBalance <- function(what){
    
    # Get values.
    val_title <- svalue(title_txt)
    val_xtitle <- svalue(x_title_txt)
    val_ytitle <- svalue(y_title_txt)
    val_shape <- as.numeric(svalue(shape_txt))
    val_alpha <- as.numeric(svalue(alpha_txt))
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
      print("str(gData)")
      print(str(gData))
    }
    
    
    if (!is.na(gData) && !is.null(gData)){
      
      
      # Call functions.
      # Add color information.
      if(is.null(gData$Dye)){
        gData <- addDye(data=gData, kit=val_kit)
        warning("'Dye' is missing. Adding dye information.")

      }
      # Sort by marker in kit
      gData <- sortMarkers(data=gData,
                          kit=val_kit,
                          addMissingLevels = TRUE)
    
      # TODO: Option to drop markers? Autofind gender marker.
      # Drop Amelogenin:
      gData <- gData[gData$Marker != "AMEL" & gData$Marker != "Amelogenin", ]
      gData$Marker <- factor(gData$Marker, levels=levels(gData$Marker)[levels(gData$Marker)!="AMEL" & gData$Marker != "Amelogenin"])
      
      # Create factors and round IMPORTANT!
      #gData$Type <- factor(round(gData$Type,2))
      
      # Sort Balance/allele factors. IMPORTANT!
      #gData$Balance <- factor(gData$Balance, levels=sort(unique(as.numeric(as.character(gData$Balance)))))
      #gData$Allele <- factor(gData$Allele, levels=sort(unique(as.numeric(as.character(gData$Allele)))))
      
      # Height must be numeric (not string).
      if(!is.numeric(gData$MpH)){
        gData$MpH <- as.numeric(as.character(gData$MpH))
        warning("'MpH' not numeric, converting to numeric.")
        
      }
      
      # Calculate number of columns.
      val_ncol <- ceiling(length(unique(gData$Marker)) / length(unique(gData$Dye)))
      
      # Calculate number of columns.
      val_palette <- unique(dyeToColor(gData)$Color)

      if(debug){
        print("Before plot: str(gData)")
        print(str(gData))
        print("Number of columns")
        print(val_ncol)
        print("levels(gData$MpH)")
        print(levels(gData$MpH))
        print("levels(gData$Hb)")
        print(levels(gData$Hb))
      }
      
      # Plotting alleles for observed Balances per marker.
      if(what == "Hb"){

        gp <- ggplot(gData, aes_string(x="MpH", y="Hb", colour="Dye"))
        
      } else if (what == "Lb") {
        
        gp <- ggplot(gData, aes_string(x="MpH", y="Lb", colour="Dye"))

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
      balance_plot <<- gp
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  }
  
  # Show GUI.
  visible(w) <- TRUE
  
}
