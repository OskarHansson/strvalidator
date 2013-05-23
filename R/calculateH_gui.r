################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 20.05.2013: First version.


#' @title Calculate H GUI
#'
#' @description
#' \code{calculateH_gui} is a GUI wrapper for the \code{calculateH} function.
#'
#' @details
#' Simplifies the use of the \code{calculateH} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

calculateH_gui <- function(env=parent.frame(), debug=FALSE){
  
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- NULL
  gDataName <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Calculate average peak height", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Datasets",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  f0g0 <- glayout(container = f0, spacing = 1)
  
  # Datasets ------------------------------------------------------------------
  
  f0g0[1,1] <- glabel(text="Select dataset:", container=f0g0)
  
  f0g0[1,2] <- dataset_drp <- gdroplist(items=c("<Select dataset>",
                                              listObjects(env=env,
                                                          objClass="data.frame")), 
                                      selected = 1,
                                      editable = FALSE,
                                      container = f0g0)
  
  f0g0[1,3] <- f0g0_samples_lbl <- glabel(text=" 0 samples", container=f0g0)
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env)){
      
      gData <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Heterozygous", "Height")
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gData <<- NULL
        svalue(dataset_drp, index=TRUE) <- 1
        svalue(f0g0_samples_lbl) <- " 0 samples"
        svalue(f2_save_edt) <- ""
        
        message <- paste("The dataset is not typing data\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
        
        gDataName <<- val_obj
        
        samples <- length(unique(gData$Sample.Name))
        svalue(f0g0_samples_lbl) <- paste("", samples, "samples")
        svalue(f2_save_edt) <- paste(gDataName, "_H", sep="")
        
      }
      
    } else {
      
      gData <<- NULL
      svalue(dataset_drp, index=TRUE) <- 1
      svalue(f0g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )  
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", container=f2)
  
  # BUTTON ####################################################################
  
  
  calculate_btn <- gbutton(text="Calculate",
                        border=TRUE,
                        container=gv)
  
  addHandlerChanged(calculate_btn, handler = function(h, ...) {
    
    val_name <- svalue(f2_save_edt)
    
    if(!is.null(gData)){
      
      # Change button.
      svalue(calculate_btn) <- "Processing..."
      enabled(calculate_btn) <- FALSE
      
      datanew <- calculateH(data=gData)
      
      # Save data.
      assign(val_name, datanew, envir=env)
      
      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
      
    } else {
      
      message <- "A dataset and a reference dataset have to be selected."
      
      gmessage(message, title="Datasets not selected",
               icon = "error",
               parent = w) 
      
    }
    
  } )
  
  
  # Show GUI.
  visible(w) <- TRUE
  
}
