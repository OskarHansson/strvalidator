################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed expanded 'gexpandgroup'.
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 29.04.2016: 'Save as' textbox expandable.
# 25.04.2016: First version.

#' @title Plot Profile Slope
#'
#' @description
#' GUI simplifying the creation of plots from slope data.
#'
#' @details Select a dataset to plot. Plot slope by sample.
#' Automatic plot titles can be replaced by custom titles.
#' A name for the result is automatiaclly suggested.
#' The resulting plot can be saved as either a plot object or as an image.
#' @param env environment in wich to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#' 
#' @return TRUE
#' 
#' @export
#' 
#' @importFrom utils help str
#' @importFrom stats as.formula
#' @importFrom grid unit textGrob grid.newpage grid.draw
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggplot2 ggplot aes_string geom_point theme element_text labs
#'  xlab ylab theme_gray theme_bw theme_linedraw theme_light theme_dark
#'  theme_minimal theme_classic theme_void geom_errorbar position_dodge
#' 
#' @seealso \url{http://docs.ggplot2.org/current/} for details on plot settings.

plotSlope_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE, parent=NULL){

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gPlot <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.
  w <- gwindow(title="Plot slope", visible=FALSE)
  
  # Runs when window is closed.
  addHandlerDestroy(w, handler = function (h, ...) {
    
    # Save GUI state.
    .saveSettings()
    
    # Focus on parent window.
    if(!is.null(parent)){
      focus(parent)
    }
    
  })
  
  # Vertical main group.
  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # Help button group.
  gh <- ggroup(container = gv, expand=FALSE, fill="both")
  
  savegui_chk <- gcheckbox(text="Save GUI settings", checked=FALSE, container=gh)
  
  addSpring(gh)
  
  help_btn <- gbutton(text="Help", container=gh)
  
  addHandlerChanged(help_btn, handler = function(h, ...) {
    
    # Open help page for function.
    print(help("plotSlope_gui", help_type="html"))
    
  })
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Dataset",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Select dataset:", container=f0)
  
  dataset_drp <- gcombobox(items=c("<Select dataset>",
                                   listObjects(env=env,
                                               obj.class="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = f0) 
  
  f0_samples_lbl <- glabel(text=" (0 samples)", container=f0)
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    # Check if suitable.
    requiredCol <- c("Sample.Name", "Group", "Slope", "Lower", "Upper")
    ok <- checkDataset(name=val_obj, reqcol=requiredCol,
                       env=env, parent=w, debug=debug)
    
    if(ok){
      # Load or change components.
      
      # Get data.
      .gData <<- get(val_obj, envir=env)
      .gDataName <<- val_obj
      
      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep="")
      
      svalue(f0_samples_lbl) <- paste(" (",
                                      length(unique(.gData$Sample.Name)),
                                      " samples)", sep="")
      
      # Enable buttons.
      enabled(plot_sample_btn) <- TRUE

    } else {
      
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index=TRUE) <- 1
      svalue(f0_samples_lbl) <- " (0 samples)"
      
    }
    
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  f1_titles_chk <- gcheckbox(text="Override automatic titles.",
                             checked=FALSE, container=f1)
  
  addHandlerChanged(f1_titles_chk, handler = function(h, ...) {
    val <- svalue(f1_titles_chk)
    if(val){
      enabled(f1g1) <- TRUE
    } else {
      enabled(f1g1) <- FALSE
    }
  } )
  
  f1g1 <- glayout(container = f1, spacing = 1)
  enabled(f1g1) <- svalue(f1_titles_chk)
  
  f1g1[1,1] <- glabel(text="Plot title:", container=f1g1)
  f1g1[1,2] <- title_edt <- gedit(text="",
                                  width=40,
                                  container=f1g1)
  
  f1g1[2,1] <- glabel(text="X title:", container=f1g1)
  f1g1[2,2] <- x_title_edt <- gedit(text="",
                                    container=f1g1)
  
  f1g1[3,1] <- glabel(text="Y title:", container=f1g1)
  f1g1[3,2] <- y_title_edt <- gedit(text="",
                                    container=f1g1)
  
  f1g2 <- glayout(container = f1)
  f1g2[1,1] <- glabel(text="Plot theme:", anchor=c(-1 ,0), container=f1g2)
  items_theme <- c("theme_grey()","theme_bw()","theme_linedraw()",
                   "theme_light()","theme_dark()","theme_minimal()",
                   "theme_classic()","theme_void()")
  f1g2[1,2] <- f1_theme_drp <- gcombobox(items = items_theme,
                                         selected = 1,
                                         container = f1g2)
  
  # FRAME 7 ###################################################################
  
  f7 <- gframe(text = "Plot slope data",
               horizontal=FALSE,
               container = gv) 
  
  grid7 <- glayout(container = f7)
  
  grid7[1,1] <- plot_sample_btn <- gbutton(text="Slope vs. Sample", container=grid7) 
  
  addHandlerChanged(plot_sample_btn, handler = function(h, ...) {
    
    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Group", "Slope", "Lower", "Upper")
    
    if(!all(requiredCol %in% colnames(.gData))){
      
      missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]
      
      message <- paste("Additional columns required:\n",
                       paste(missingCol, collapse="\n"), sep="")
      
      gmessage(message, title="message",
               icon = "error",
               parent = w) 
      
    } else {
      
      enabled(plot_sample_btn) <- FALSE
      .plotSlope(what="Sample")
      enabled(plot_sample_btn) <- TRUE
      
    }
    
  } )
  
  # FRAME 5 ###################################################################

  f5 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f5)
  
  f5_save_edt <- gedit(text="", container=f5, expand = TRUE)
  
  f5_save_btn <- gbutton(text = "Save as object", container = f5) 
  
  f5_ggsave_btn <- gbutton(text = "Save as image", container = f5) 
  
  addHandlerClicked(f5_save_btn, handler = function(h, ...) {
    
    val_name <- svalue(f5_save_edt)
    
    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- "Processing..."
    unblockHandlers(f5_save_btn)
    enabled(f5_save_btn) <- FALSE
    
    # Save data.
    saveObject(name=val_name, object=.gPlot,
               parent=w, env=env, debug=debug)
    
    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- "Object saved"
    unblockHandlers(f5_save_btn)
    
  } )
  
  addHandlerChanged(f5_ggsave_btn, handler = function(h, ...) {
    
    val_name <- svalue(f5_save_edt)
    
    # Save data.
    ggsave_gui(ggplot=.gPlot, name=val_name, 
               parent=w, env=env, savegui=savegui, debug=debug)
    
  } )
  
  # ADVANCED OPTIONS ##########################################################
  
  # FRAME 4 ###################################################################

  e4 <- gexpandgroup(text="X labels",
                     horizontal=FALSE,
                     container = f1)

  # Start collapsed.
  visible(e4) <- FALSE
  
  grid4 <- glayout(container = e4)

  grid4[1,1] <- glabel(text="Text size (pts):", container=grid4)
  grid4[1,2] <- e4_size_edt <- gedit(text="8", width=4, container=grid4)

  grid4[1,3] <- glabel(text="Angle:", container=grid4)
  grid4[1,4] <- e4_angle_spb <- gspinbutton (from=0, to=360, by=1,
                                             value=0,
                                             container=grid4)

  grid4[2,1] <- glabel(text="Justification (v/h):", container=grid4)
  grid4[2,2] <- e4_vjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                             value=0.5,
                                             container=grid4)

  grid4[2,3] <- e4_hjust_spb <- gspinbutton (from=0, to=1, by=0.1,
                                             value=0.5,
                                             container=grid4)

  # FUNCTIONS #################################################################
  
  .plotSlope <- function(what){
    
    # Get values.
    val_titles <- svalue(f1_titles_chk)
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_angle <- as.numeric(svalue(e4_angle_spb))
    val_vjust <- as.numeric(svalue(e4_vjust_spb))
    val_hjust <- as.numeric(svalue(e4_hjust_spb))
    val_size <- as.numeric(svalue(e4_size_edt))
    val_theme <- svalue(f1_theme_drp)
    
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
      print("val_theme")
      print(val_theme)
    }
    
    if (!is.na(.gData) && !is.null(.gData)){
      
      # Create default tit
      if(val_titles){
        maintitle <- val_title
        xtitle <- val_xtitle
        ytitle <- val_ytitle
      } else {
        maintitle <- "Profile slope"
        xtitle <- "Sample"
        ytitle <- "Slope"
      }

      # Create plot with groups.
      gp <- ggplot(data = .gData,
                   aes_string(colour="Group", y="Slope", x="Sample.Name"))
      
      # Apply theme.
      gp <- gp + eval(parse(text=val_theme))
      
      # Add point layer.  
      gp <- gp + geom_point(position=position_dodge(width=0.3))
      
      # Define the top and bottom of the errorbars
      limits <- aes_string(ymax = "Upper", ymin = "Lower")
      
      # Add error bars.
      gp <- gp + geom_errorbar(limits, width=0.2,
                               position=position_dodge(width=0.3))
      
      # Add titles etc.
      gp <- gp + theme(axis.text.x=element_text(angle=val_angle,
                                                hjust=val_hjust,
                                                vjust=val_vjust,
                                                size=val_size))
      gp <- gp + labs(title=maintitle)
      gp <- gp + xlab(xtitle)
      gp <- gp + ylab(ytitle)
      
      # plot.
      print(gp)
      
      # Change save button.
      svalue(f5_save_btn) <- "Save as object"
      enabled(f5_save_btn) <- TRUE
      
      # Store in global variable.
      .gPlot <<- gp
      
    } else {
      
      gmessage(msg="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    } 
    
  }
  
  # INTERNAL FUNCTIONS ########################################################
  
  .loadSavedSettings <- function(){
    
    # First check status of save flag.
    if(!is.null(savegui)){
      svalue(savegui_chk) <- savegui
      enabled(savegui_chk) <- FALSE
      if(debug){
        print("Save GUI status set!")
      }  
    } else {
      # Load save flag.
      if(exists(".strvalidator_plotSlope_gui_savegui", envir=env, inherits = FALSE)){
        svalue(savegui_chk) <- get(".strvalidator_plotSlope_gui_savegui", envir=env)
      }
      if(debug){
        print("Save GUI status loaded!")
      }  
    }
    if(debug){
      print(svalue(savegui_chk))
    }  
    
    # Then load settings if true.
    if(svalue(savegui_chk)){
      if(exists(".strvalidator_plotSlope_gui_title", envir=env, inherits = FALSE)){
        svalue(title_edt) <- get(".strvalidator_plotSlope_gui_title", envir=env)
      }
      if(exists(".strvalidator_plotSlope_gui_title_chk", envir=env, inherits = FALSE)){
        svalue(f1_titles_chk) <- get(".strvalidator_plotSlope_gui_title_chk", envir=env)
      }
      if(exists(".strvalidator_plotSlope_gui_x_title", envir=env, inherits = FALSE)){
        svalue(x_title_edt) <- get(".strvalidator_plotSlope_gui_x_title", envir=env)
      }
      if(exists(".strvalidator_plotSlope_gui_y_title", envir=env, inherits = FALSE)){
        svalue(y_title_edt) <- get(".strvalidator_plotSlope_gui_y_title", envir=env)
      }
      if(exists(".strvalidator_plotSlope_gui_xlabel_size", envir=env, inherits = FALSE)){
        svalue(e4_size_edt) <- get(".strvalidator_plotSlope_gui_xlabel_size", envir=env)
      }
      if(exists(".strvalidator_plotSlope_gui_xlabel_angle", envir=env, inherits = FALSE)){
        svalue(e4_angle_spb) <- get(".strvalidator_plotSlope_gui_xlabel_angle", envir=env)
      }
      if(exists(".strvalidator_plotSlope_gui_xlabel_justh", envir=env, inherits = FALSE)){
        svalue(e4_hjust_spb) <- get(".strvalidator_plotSlope_gui_xlabel_justh", envir=env)
      }
      if(exists(".strvalidator_plotSlope_gui_xlabel_justv", envir=env, inherits = FALSE)){
        svalue(e4_vjust_spb) <- get(".strvalidator_plotSlope_gui_xlabel_justv", envir=env)
      }
      if(exists(".strvalidator_plotSlope_gui_theme", envir=env, inherits = FALSE)){
        svalue(f1_theme_drp) <- get(".strvalidator_plotSlope_gui_theme", envir=env)
      }
      
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(savegui_chk)){
      
      assign(x=".strvalidator_plotSlope_gui_savegui", value=svalue(savegui_chk), envir=env)
      assign(x=".strvalidator_plotSlope_gui_title", value=svalue(title_edt), envir=env)
      assign(x=".strvalidator_plotSlope_gui_title_chk", value=svalue(f1_titles_chk), envir=env)
      assign(x=".strvalidator_plotSlope_gui_x_title", value=svalue(x_title_edt), envir=env)
      assign(x=".strvalidator_plotSlope_gui_y_title", value=svalue(y_title_edt), envir=env)
      assign(x=".strvalidator_plotSlope_gui_xlabel_size", value=svalue(e4_size_edt), envir=env)
      assign(x=".strvalidator_plotSlope_gui_xlabel_angle", value=svalue(e4_angle_spb), envir=env)
      assign(x=".strvalidator_plotSlope_gui_xlabel_justh", value=svalue(e4_hjust_spb), envir=env)
      assign(x=".strvalidator_plotSlope_gui_xlabel_justv", value=svalue(e4_vjust_spb), envir=env)
      assign(x=".strvalidator_plotSlope_gui_theme", value=svalue(f1_theme_drp), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_plotSlope_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_title", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_title", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_title_chk", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_title_chk", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_x_title", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_x_title", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_y_title", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_y_title", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_xlabel_size", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_xlabel_size", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_xlabel_angle", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_xlabel_angle", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_xlabel_justh", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_xlabel_justh", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_xlabel_justv", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_xlabel_justv", envir = env)
      }
      if(exists(".strvalidator_plotSlope_gui_theme", envir=env, inherits = FALSE)){
        remove(".strvalidator_plotSlope_gui_theme", envir = env)
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
  focus(w)
  
}