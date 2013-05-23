################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 17.05.2013: save plot moved to external function.
# 17.05.2013: listDataFrames() -> listObjects()
# 28.04.2013: GUI layout changes.
# <28.04.2013: Added dropdown with data frames in passed environment.
# <28.04.2013: First version.

#' @title Plot Stutter GUI
#'
#' @description
#' \code{plotStutter_gui} is a GUI simplifying the creation of plots from stutter data.
#'
#' @details Plot stutter data by parent allele or by peak height.
#' @param env environment in wich to search for data frames.
#' @param debug logical indicating printing debug information.
#' 



plotStutter_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require(ggplot2)
  library(gWidgets)
  options(guiToolkit="RGtk2")
  
  data <- NULL
  data_name <- NULL
  data_org <- NULL
  stutter_plot <- NULL
  separator <- .Platform$file.sep # Platform dependent path separator.
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Plot stutter proportions", visible=FALSE)
  
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
    data <<- get(val_obj, envir=env)
    # Check if suitable for plot stutter...

    requiredCol <- c("Marker", "Allele", "HeightA", "Stutter", "Type")
    
    if(!all(requiredCol %in% colnames(data))){

      data <<- NULL
      
      message <- paste("The dataset is not a stutter table\n\n",
                       "The following columns are required:\n",
                       paste(requiredCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "info",
               parent = w) 
      
    } else {

      data_name <<- val_obj
      data_org <<- data
      
      # Detect kit.
      kitIndex <- detectKit(data)
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
  grid1[1,2] <- title_txt <- gedit(text="Stutter ratios",
                                   width=40,
                                   container=grid1)
  
  grid1[2,1] <- glabel(text="X title:", container=grid1)
  grid1[2,2] <- x_title_txt <- gedit(text="True allele",
                                     container=grid1)

  grid1[3,1] <- glabel(text="Y title:", container=grid1)
  grid1[3,2] <- y_title_txt <- gedit(text="Ratio",
                                     container=grid1)

  # FRAME 7 ###################################################################
  
  f7 <- gframe(text = "Plot stutter data",
               horizontal=FALSE,
               container = gv) 
  
  grid7 <- glayout(container = f7)
  
  grid7[1,1] <- plot_allele_btn <- gbutton(text="Ratio vs. Allele",
                                           border=TRUE,
                                           container=grid7) 
  
  grid7[1,2] <- plot_height_btn <- gbutton(text="Ratio vs. Height",
                                           border=TRUE,
                                           container=grid7) 
  
  addHandlerChanged(plot_allele_btn, handler = function(h, ...) {
    
    enabled(plot_allele_btn) <- FALSE
    .plotStutter(what="allele")
    enabled(plot_allele_btn) <- TRUE
    
  } )
  
  addHandlerChanged(plot_height_btn, handler = function(h, ...) {
    
    enabled(plot_height_btn) <- FALSE
    .plotStutter(what="height")
    enabled(plot_height_btn) <- TRUE
    
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
    
    savePlot_gui(object=stutter_plot)
    
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
  
  f6 <- gframe(text = "Trim (discard data based on height of true allele)",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  grid6 <- glayout(container = f6, spacing = 1)
  
  grid6[1,1] <- check_btn <- gbutton(text="Get low/high",
                                     border=TRUE,
                                     container = grid6) 

  grid6[2,1] <- trim_btn <- gbutton(text="Trim dataset",
                                    border=TRUE,
                                    container = grid6) 
  
  grid6[2,2] <- glabel(text="  Low", container=grid6)
  grid6[2,3] <- low_txt <- gedit(text="", width=6, container=grid6)
  grid6[2,4] <- glabel(text="  High", container=grid6)
  grid6[2,5] <- high_txt <- gedit(text="", width=6, container=grid6)
  
  grid6[3,1] <- reset_btn <- gbutton(text="Reload original data",
                                     border=TRUE,
                                     container = grid6) 

  grid6[4,1] <- g6_save_btn <- gbutton(text="Save As",
                                     border=TRUE,
                                     container = grid6) 
  
  grid6[4,2:5] <- g6_save_txt <- gedit(text=data_name,
                                     container=grid6,
                                     anchor=c(-1 ,0))
  
  
  addHandlerChanged(g6_save_btn, handler = function(h, ...) {
    
    val_name <- svalue(g6_save_txt)
    val_data <- data
    
    if (!is.na(val_data) && !is.null(val_data)){

      # Save data.
      assign(val_name, val_data, envir=env)
      
      # Refresh dropdown, but keep selected item.
      selected <- svalue(dataset_drp)
      dataset_drp[] <<- c("<Select dataset>", listObjects(env=env,
                                                          objClass="data.frame"))
      svalue(dataset_drp) <- selected
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  } )
  
  
  addHandlerChanged(check_btn, handler = function(h, ...) {
    
    svalue(low_txt) <- min(data$HeightA)
    svalue(high_txt) <- max(data$HeightA)
    
  } )

  addHandlerChanged(trim_btn, handler = function(h, ...) {
    
    val_low <- as.numeric(svalue(low_txt))
    val_high <- as.numeric(svalue(high_txt))
    val_name <- data_name

    if(debug){
      print("low")
      print(val_low)
      print("high")
      print(val_high)
      print("head(data):")
      print(head(data))
      print(nrow(data))
    }
    
    enabled(trim_btn) <- FALSE
    data <<- data[data$HeightA<=val_high, ]
    data <<- data[data$HeightA>=val_low, ]
    enabled(trim_btn) <- TRUE

    if(debug){
      print("head(data):")
      print(head(data))
      print(nrow(data))
    }
    
    # Create new name.
    newName <- paste(val_name,"_",val_low,"-",val_high, sep="")
    svalue(g6_save_txt) <- newName
    
  } )

  addHandlerChanged(reset_btn, handler = function(h, ...) {
    
    data <<- data_org
    
  } )
  
  
  # FUNCTIONS #################################################################
  
  
  .plotStutter <- function(what){
    
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
      print("str(data)")
      print(str(data))
      print("levels(data$Allele)")
      print(levels(data$Allele))
      print("levels(data$Stutter)")
      print(levels(data$Stutter))
    }
    
    
    if (!is.na(data) && !is.null(data)){
      
      
      # Call functions.
      # Add color information.
      data <- addDye(data=data, kit=val_kit)
      # Sort by marker in kit
      data <- sortMarkers(data=data,
                          kit=val_kit,
                          addMissingLevels = TRUE)
      # Drop Amelogenin:
      data <- data[data$Marker != "AMEL", ]
      data$Marker <- factor(data$Marker, levels=levels(data$Marker)[levels(data$Marker)!="AMEL"])
      
      # Create factors and round IMPORTANT!
      data$Type <- factor(round(data$Type,2))
      
      # Sort stutter/allele factors. IMPORTANT!
      data$Stutter <- factor(data$Stutter, levels=sort(unique(as.numeric(as.character(data$Stutter)))))
      data$Allele <- factor(data$Allele, levels=sort(unique(as.numeric(as.character(data$Allele)))))
      
      # Height must be numeric (not string).
      data$HeightA <- as.numeric(as.character(data$HeightA))
      
      
      if(debug){
        print("Before plot: str(data)")
        print(str(data))
        print("levels(data$Allele)")
        print(levels(data$Allele))
        print("levels(data$Stutter)")
        print(levels(data$Stutter))
      }
      
      # Plotting alleles for observed stutters per marker.
      if(what == "allele"){

        gp <- ggplot(data, aes_string(x="Allele", y="Ratio", colour="Type"))
        
      } else if (what == "height") {
        
        gp <- ggplot(data, aes_string(x="HeightA", y="Ratio", colour="Type"))

      }
      gp <- gp + geom_point(shape=val_shape, alpha=val_alpha, position=position_jitter(width=val_jitter)) 
      gp <- gp + facet_grid(Dye ~ Marker) + facet_wrap(~ Marker, ncol=4, drop=FALSE, scales=val_scales)
      
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
      stutter_plot <<- gp
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  }
  
  # Show GUI.
  visible(w) <- TRUE
  
}
