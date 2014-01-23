################################################################################
# TODO LIST
# TODO: Make a general function to add any (selected) kit information?

################################################################################
# CHANGE LOG
# 27.11.2013: Added parameter 'overwrite=TRUE'.
# 18.09.2013: Updated to use 'addColor' insted of removed 'addDye'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.
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
  
  # Global variables.
  .gData <- data.frame(No.Data=NA)
  .gDataName <- NULL
  .gKit <- 1

  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print(head(.gData))
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
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gData <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Marker")
      
      if(!all(requiredCol %in% colnames(.gData))){
        
        missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]

        message <- paste("Additional columns required:\n",
                         paste(missingCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
      
        # Reset components.
        .gData <<- data.frame(No.Data=NA)
        .gDataName <<- NULL
        svalue(dataset_samples_lbl) <- " 0 samples"
        svalue(f2_save_edt) <- ""
        
      } else {

        # Load or change components.
        .gDataName <<- val_obj
        samples <- length(unique(.gData$Sample.Name))
        svalue(dataset_samples_lbl) <- paste(" ", samples, "samples")
        .gKit <<- detectKit(.gData, index=TRUE)
        svalue(kit_drp, index=TRUE) <- .gKit
        svalue(f2_save_edt) <- paste(.gDataName, "_dye", sep="")
        
        if(debug){
          print("Detected kit index")
          print(.gKit)
        }
        
      }
      
    } else {
      
      # Reset components.
      .gData <<- data.frame(No.Data=NA)
      .gDataName <<- NULL
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
    val_data <- .gData
    val_name <- svalue(f2_save_edt)
    
    if(debug){
      print(".gData")
      print(names(.gData))
      print("val_kit")
      print(val_kit)
    }

    # Change button.
    svalue(add_btn) <- "Processing..."
    enabled(add_btn) <- FALSE
    
    datanew <- addColor(data=.gData, kit=val_kit, need="Dye", overwrite=TRUE)
    
    # Save data.
    saveObject(name=val_name, object=datanew, parent=w, env=env)
    
    # Close GUI.
    dispose(w)
    
  } )
  
  # Show GUI.
  visible(w) <- TRUE
  
} # End of GUI
