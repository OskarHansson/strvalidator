################################################################################
# TODO LIST
# TODO: Make a general function to add any (selected) kit information?

################################################################################
# CHANGE LOG
# 06.05.2014: Implemented 'checkDataset'.
# 02.03.2014: Added 'saveGUI' and 'bins' option.
# 11.02.2014: First version.


#' @title Adds size information
#'
#' @description
#' \code{addSize_gui} is a GUI wrapper for the \code{addSize} function.
#'
#' @details
#' Simplifies the use of the \code{addSize} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

addSize_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gKit <- 1

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  # Main window.
  w <- gwindow(title="Add size to dataset", visible=FALSE)

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

    # Check if suitable.
    requiredCol <- c("Marker", "Allele")
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
      svalue(f2_save_edt) <- paste(.gDataName, "_size", sep="")
      
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
  
  # OPTIONS -----------------------------------------------------------------------
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  f1_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f1)
  
  f1_items <- c("Get size as defined in bins file (NA if not defined)",
                "Calculate an estimate from locus offset and number of repeats")
  
  f1_size_opt <- gradio(items=f1_items, selected=2, container=f1)
  
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
  
  add_btn <- gbutton(text="Add size",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(add_btn, handler = function(h, ...) {
    
    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_name <- svalue(f2_save_edt)
    val_data <- .gData
    val_bins <- svalue(f1_size_opt, index=TRUE) == 1 # TRUE / FALSE
    
    if(debug){
      print("val_data:")
      print(str(val_data))
      print(head(val_data))
      print("val_kit")
      print(val_kit)
    }
    
    if(val_bins){

      # Get kit with size information.
      val_kitinfo <- getKit(kit=val_kit, what="Size")
      
    } else {

      # Get kit with offset information.
      offset <- getKit(kit=val_kit, what="Offset")
      
      # Get kit with repeat information.
      repeatUnit <- getKit(kit=val_kit, what="Repeat")
      
      # Merge information.
      val_kitinfo <- merge(offset, repeatUnit, by="Marker", sort=FALSE)
      
    }
    
    # Change button.
    svalue(add_btn) <- "Processing..."
    enabled(add_btn) <- FALSE
    
    datanew <- addSize(data=val_data, kit=val_kitinfo,
                       bins=val_bins, debug=debug)
    
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
      if(exists(".strvalidator_addSize_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".strvalidator_addSize_gui_savegui", envir=env)
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
      if(exists(".strvalidator_addSize_gui_bins", envir=env, inherits = FALSE)){
        svalue(f1_size_opt) <- get(".strvalidator_addSize_gui_bins", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".strvalidator_addSize_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".strvalidator_addSize_gui_bins", value=svalue(f1_size_opt), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_addSize_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_addSize_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_addSize_gui_bins", envir=env, inherits = FALSE)){
        remove(".strvalidator_addSize_gui_bins", envir = env)
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
