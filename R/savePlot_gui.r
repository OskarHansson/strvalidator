################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 17.05.2013: First version.

#' @title Save plot GUI
#'
#' @description
#' \code{savePlot_gui} is a GUI simplifying the saving of plots as images.
#'
#' @details Pass a plot object directly to the function or provide an environment
#' to select one from. Specify name, dimensions, and resolution. The plot will
#' be save in the selected folder.
#' @param object plot object.
#' @param env environment in wich to search for data frames.
#' @param debug logical indicating printing debug information.
#' 


savePlot_gui <- function(object=NULL, env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require(ggplot2)
  library(gWidgets)
  options(guiToolkit="RGtk2")
  
  # Assign object to environment so it is listed in dropdown.
  assign(deparse(substitute(object)), object, envir = env)
  
  xplot <- object
  
  separator <- .Platform$file.sep # Platform dependent path separator.
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  w <- gwindow(title="Save plot", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # FRAME 0 ###################################################################
  
  f1 <- gframe(text="Plot object",
               horizontal=FALSE,
               spacing = 10,
               container = gv) 
  
  f1g1 <- glayout(container = f1, spacing = 1)
  
  f1g1[1,1] <- glabel(text="Select plot object:", container=f1g1)
  
  f1g1_drp_items <- c("<Select plot object>",
                 listObjects(env=env, objClass="ggplot"))
  
  # If plot provided select the object from dropdown.
  if(is.null(object)){
    selectedPlot <- 1
  } else {
    selectedPlot <- which(f1g1_drp_items == deparse(substitute(object)))
  }

  f1g1[1,2] <- f1g1_plot_drp <- gdroplist(items = f1g1_drp_items,
                                         selected = selectedPlot,
                                         editable = FALSE,
                                         container = f1g1)
  
  addHandlerChanged(f1g1_plot_drp, handler = function (h, ...) {
    
    val_obj <- svalue(f1g1_plot_drp)
    
    if(exists(val_obj, envir=env)){
      
      xplot <<- get(val_obj, envir=env)
      
    } else {
      
      xplot <<- NULL

    }
  } )
  
  # FRAME 2 ###################################################################

  f2 <- gframe(text="Image settings",
               horizontal=FALSE, spacing = 10, container = gv)
  
  # GRID 1 --------------------------------------------------------------------

  f2g1 <- glayout(container = f2, spacing = 5)
  
  f2g1[1,1] <- glabel(text="File name:",
                         container=f2g1,
                         anchor=c(-1 ,0))
  
  f2g1[2,1] <- f2g2_name_edt <- gedit(text="",
                                    width=25,
                                    initial.msg=".png is added",
                                    container=f2g1)

  # GRID 2 --------------------------------------------------------------------

  f2g2 <- glayout(container = f2, spacing = 5)
  
  f2g2[1,1] <- glabel(text="Width:", container=f2g2, anchor=c(-1 ,0))
  
  f2g2[1,2] <- f2g2_width_edt <- gedit(text="3000",
                                     width=4,
                                     initial.msg="",
                                     container=f2g2)

  f2g2[1,3] <- glabel(text="Height:", container=f2g2, anchor=c(-1 ,0))
  
  f2g2[1,4] <- f2g2_height_edt <- gedit(text="2000",
                                      width=4,
                                      initial.msg="",
                                      container=f2g2)
  
  f2g2[1,5] <- glabel(text="Resolution:", container=f2g2, anchor=c(-1 ,0))
  
  f2g2[1,6] <- f2g2_res_edt <- gedit(text="250",
                                    width=4,
                                    initial.msg="",
                                    container=f2g2)
  
  # FRAME 3 ###################################################################
  
  f3 <- gframe(text="Save",
               horizontal=FALSE, spacing = 10, container = gv)
  
  # GRID 1 --------------------------------------------------------------------

  
  f3g1 <- glayout(container = f3, spacing = 5)
  
  f3g1[1,1:2] <- glabel(text="File path:", container=f3g1, anchor=c(-1 ,0))
  
  pathDefText <- "Select folder..."
  f3g1[2,1] <- f3g1_path_brw <- gfilebrowse(text=pathDefText,
                                             quote=FALSE,
                                             type="selectdir",
                                             container=f3g1)
  
  f3g1[2,2] <- f3g1_save_btn <- gbutton(text = "Save",
                                         border=TRUE,
                                         container = f3g1) 
  
  # SAVE ######################################################################

  addHandlerChanged(f3g1_save_btn, handler = function(h, ...) {
    
    # Disable button until save is finished.
    enabled(f3g1_save_btn) <- FALSE
    svalue(f3g1_save_btn) <- "Saving..."
    
    # Load all settings.
    val_name <- svalue(f2g2_name_edt)
    val_w <- as.numeric(svalue(f2g2_width_edt))
    val_h <- as.numeric(svalue(f2g2_height_edt))
    val_res <- as.numeric(svalue(f2g2_res_edt))
    val_path <- svalue(f3g1_path_brw)
    val_OK <- TRUE
    
    if(val_name == ""){
      val_OK <- FALSE
    }
    
    if(val_path == pathDefText || val_path == ""){
      val_OK <- FALSE
    }
    
    if(val_w <20 || is.na(val_w)){
      val_OK <- FALSE
    }
    
    if(val_h <20 || is.na(val_h)){
      val_OK <- FALSE
    }
    
    if(val_res <20 || is.na(val_res)){
      val_OK <- FALSE
    }
    
    if(debug){
      print("val_OK:")
      print(val_OK)
      print("file:")
      print(paste(val_path, separator, val_name, ".png", sep=""))
    }
    
    if(!is.null(xplot) && val_OK){
      
      # Save EPG.      
      png(filename=paste(val_path, separator, val_name, ".png", sep=""),
          width=val_w, height=val_h, res=val_res)
      plot(xplot)
      dev.off()
      
    }
    
    if(!val_OK){
      
      gmessage(message="File name, path, width (>20), height (>20), and resolution (>20) must be provided.",
               title="Error",
               icon = "error")      
      
    }
    # Enable button.
    enabled(f3g1_save_btn) <- TRUE
    svalue(f3g1_save_btn) <- "Save plot"
    
  } )
  
  # Show GUI.
  visible(w) <- TRUE
  
}
