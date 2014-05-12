################################################################################
# TODO LIST
# TODO: Make a general function to add any (selected) kit information?

################################################################################
# CHANGE LOG
# 11.05.2014: Implemented new option 'Ignore case' and save user settings functions.
# 06.05.2014: Implemented 'checkDataset'.
# 23.02.2014: Removed requirement for 'Sample.Name'.
# 11.02.2014: Pass debug to 'addColor'.
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
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

addDye_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Global variables.
  .gData <- data.frame(No.Data=NA)
  .gDataName <- NULL
  .gKit <- 1

  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print(head(.gData))
  }
  
  
  w <- gwindow(title="Add dye to dataset", visible=FALSE)
  
  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })
  
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
    
    # Check if suitable.
    requiredCol <- c("Marker")
    ok <- checkDataset(name=val_obj, reqcol=requiredCol,
                       env=env, parent=w, debug=debug)
    
    if(ok){
      # Load or change components.
      
      .gData <<- get(val_obj, envir=env)
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
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 

  f1_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f1)
  
  f1_ignore_chk <- gcheckbox(text="Ignore case in marker name.",
                             checked=FALSE, container=f1)
  
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
    val_ignore <- svalue(f1_ignore_chk)
    
    if(debug){
      print(".gData")
      print(names(.gData))
      print("val_kit")
      print(val_kit)
      print("val_ignore")
      print(val_ignore)
    }

    # Change button.
    svalue(add_btn) <- "Processing..."
    enabled(add_btn) <- FALSE
    
    datanew <- addColor(data=.gData, kit=val_kit, need="Dye",
                        overwrite=TRUE, ignore.case=val_ignore, debug=debug)
    
    # Save data.
    saveObject(name=val_name, object=datanew, parent=w, env=env)
    
    # Close GUI.
    dispose(w)
    
  } )

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
      if(exists(".strvalidator_addDye_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".strvalidator_addDye_gui_savegui", envir=env)
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
      if(exists(".strvalidator_addDye_gui_ignore", envir=env, inherits = FALSE)){
        svalue(f1_ignore_chk) <- get(".strvalidator_addDye_gui_ignore", envir=env)
      }
      
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".strvalidator_addDye_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".strvalidator_addDye_gui_ignore", value=svalue(f1_ignore_chk), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_addDye_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_addDye_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_addDye_gui_ignore", envir=env, inherits = FALSE)){
        remove(".strvalidator_addDye_gui_ignore", envir = env)
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
  
} # End of GUI
