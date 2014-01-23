################################################################################
# TODO LIST
# TODO: Make a general function to add any (selected) kit information?
# TODO: Option to add all uppercase/lower case marker names?

################################################################################
# CHANGE LOG
# 27.11.2013: First version.


#' @title Adds dye information
#'
#' @description
#' \code{addMarker_gui} is a GUI wrapper for the \code{addMarker} function.
#'
#' @details
#' Simplifies the use of the \code{addMarker} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

addMarker_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Global variables.
  .gData <- data.frame(No.Data=NA)
  .gDataName <- NULL
  .gKit <- 1

  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print(head(.gData))
  }
  
  # Main window.  
  w <- gwindow(title="Add missing markers to dataset", visible=FALSE)

  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })

  # Vertical main group.
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
        svalue(f2_save_edt) <- paste(.gDataName, "_marker", sep="")
        
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
  
  # FRAME 1 ###################################################################

  # OPTIONS -----------------------------------------------------------------------
  
  f1 <- gframe(text = "Options",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  f1_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f1)
  
  f1_ignore_chk <- gcheckbox(text="Ignore case (marker name)",
                             checked=FALSE,
                             container=f1)

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
  
  add_btn <- gbutton(text="Add marker",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(add_btn, handler = function(h, ...) {
    
    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_ignore <- svalue(f1_ignore_chk)
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
    
    datanew <- addMarker(data=.gData,
                         marker=getKit(val_kit, what="Marker"),
                         ignoreCase=val_ignore, debug=debug)
    
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
      if(exists(".strvalidator_addMarker_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".strvalidator_addMarker_gui_savegui", envir=env)
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
      if(exists(".strvalidator_addMarker_gui_ignore", envir=env, inherits = FALSE)){
        svalue(f1_ignore_chk) <- get(".strvalidator_addMarker_gui_ignore", envir=env)
      }
      
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".strvalidator_addMarker_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".strvalidator_addMarker_gui_ignore", value=svalue(f1_ignore_chk), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_addMarker_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_addMarker_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_addMarker_gui_ignore", envir=env, inherits = FALSE)){
        remove(".strvalidator_addMarker_gui_ignore", envir = env)
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
