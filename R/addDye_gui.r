################################################################################
# TODO LIST
# TODO: Make a general function to add any (selected) kit information.

################################################################################
# CHANGE LOG
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: .result removed, added save as group.
# 27.04.2013: First version.


#' @title Adds dye information
#'
#' @description
#' \code{addDye_gui} is a GUI wrapper for the \code{addDye} function.
#'
#' @details
#' Simplifies the use of the \code{addDye} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

addDye_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require("gWidgets")
  options(guiToolkit="RGtk2")
  
  gData <- data.frame(No.Data=NA)
  gDataName <- NULL
  gKit <- 1

  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print(head(gData))
  }
  
  
  w <- gwindow(title="Add dye to dataset", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=15,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # DATASET ###################################################################
  
  group0 <- ggroup(horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  grid0 <- glayout(container = group0, spacing = 1)
  
  grid0[1,1] <- glabel(text="Select dataset:", container=grid0)
  
  grid0[1,2] <- dataset_drp <- gdroplist(items=c("<Select dataset>",
                                                 listObjects(env=env,
                                                             objClass="data.frame")),
                                         selected = 1,
                                         editable = FALSE,
                                         container = grid0)
  
  grid0[1,3] <- dataset_samples_lbl <- glabel(text=" 0 samples",
                                              container=grid0)
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env)){
      
      gData <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Marker")
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gData <<- data.frame(No.Data=NA)
        gDataName <<- NULL

        svalue(dataset_samples_lbl) <- " 0 samples"
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
        svalue(dataset_samples_lbl) <- paste(" ", samples, "samples")
        gKit <<- detectKit(gData, index=FALSE)
        svalue(kit_drp) <- gKit
        svalue(f2_save_edt) <- paste(gDataName, "_dye", sep="")
        
      }
      
    } else {
      
      gData <<- data.frame(No.Data=NA)
      gDataName <<- NULL
      svalue(dataset_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )
  
  # KIT -----------------------------------------------------------------------
  
  grid0[2,1] <- glabel(text="Kit:", container=grid0)
  
  kit_drp <- gdroplist(items=getKit(),
                           selected = 1,
                           editable = FALSE,
                           container = grid0)
  
  grid0[2,2] <- kit_drp
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", container=f2)

  # BUTTON ####################################################################

  if(debug){
    print("BUTTON")
  }  
  
  add_btn <- gbutton(text="Add dye",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(add_btn, handler = function(h, ...) {
    
    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- gData
    val_name <- svalue(f2_save_edt)
    
    if(debug){
      print("gData")
      print(names(gData))
      print("val_kit")
      print(val_kit)
    }

    # Change button.
    svalue(add_btn) <- "Processing..."
    enabled(add_btn) <- FALSE
    
    datanew <- addDye(data=gData, kit=val_kit)
    
    # Save data.
    assign(val_name, datanew, envir=env)
    
    # Close GUI.
    dispose(w)
    
  } )
  
  # Show GUI.
  visible(w) <- TRUE
  
} # End of GUI
