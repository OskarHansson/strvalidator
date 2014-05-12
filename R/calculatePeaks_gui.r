################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 08.05.2014: Implemented 'checkDataset'.
# 11.01.2014: First version.

#' @title Calculate peaks GUI
#'
#' @description
#' \code{calculatePeaks_gui} is a GUI simplifying counting of peaks.
#'
#' @details Counts the number of peaks in samples and markers with option to
#'  discard off-ladder peaks and to label groups according to maximum number
#'  of peaks.
#' @param env environment in wich to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' 

calculatePeaks_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  .gData <- NULL
  .gDataName <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  w <- gwindow(title="Calculate peaks", visible=FALSE)
  
  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })

  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Dataset",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  g0 <- glayout(container = f0, spacing = 1)
  
  # Datasets ------------------------------------------------------------------
  
  g0[1,1] <- glabel(text="Select dataset:", container=g0)

  g0[1,2] <- g0_dataset_drp <- gdroplist(items=c("<Select dataset>",
                                   listObjects(env=env,
                                               objClass="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = g0)
  
  g0[1,3] <- g0_samples_lbl <- glabel(text=" 0 samples", container=g0)
  
  addHandlerChanged(g0_dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(g0_dataset_drp)
    
    # Check if suitable.
    requiredCol <- c("Sample.Name", "Height")
    ok <- checkDataset(name=val_obj, reqcol=requiredCol,
                       slim=TRUE, slimcol="Height",
                       env=env, parent=w, debug=debug)
    
    if(ok){
      
      # Load or change components.
      .gData <<- get(val_obj, envir=env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, "samples")
      svalue(f2_save_edt) <- paste(.gDataName, "_peaks", sep="")
        
    } else {
      
      # Reset components.
      .gData <<- NULL
      svalue(g0_dataset_drp, index=TRUE) <- 1
      svalue(g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""

    }
    
  } )

  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 

  f1_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f1)
  
  f1_no_ol_chk <- gcheckbox(text="Exclude off-ladder peaks (OL alleles).",
                           checked = FALSE,
                           container = f1)
  
  f1_per_marker_chk <- gcheckbox(text="Count peaks per marker.",
                            checked = FALSE,
                            container = f1)
  
  glabel(text="Define group labels (separate by comma):",
         container=f1, anchor=c(-1 ,0))
  f1_labels_edt <- gedit(text = "No contamination,Drop-in contamination,Gross contamination",
                         width = 60,
                         container = f1)
  
  glabel(text="Define maximum number of peaks for each group (separate by comma):",
         container=f1, anchor=c(-1 ,0))
  f1_bins_edt <- gedit(text = "0,2,3",
                       width = 60,
                       container = f1)

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
    
    # Get values.
    val_data <- .gData
    val_no_ol <- svalue(f1_no_ol_chk)
    val_per_marker <- svalue(f1_per_marker_chk)
    val_labels <- svalue(f1_labels_edt)
    val_bins <- svalue(f1_bins_edt)
    val_name <- svalue(f2_save_edt)
    
    # countPeaks require a vectors.
    val_labels <- unlist(strsplit(val_labels, ",", fixed = TRUE))
    val_bins <- as.numeric(unlist(strsplit(val_bins, ",", fixed = TRUE)))
    
    if(!is.null(val_data)){
      
      # Change button.
      svalue(calculate_btn) <- "Processing..."
      enabled(calculate_btn) <- FALSE
  
      datanew <- calculatePeaks(data=val_data,
                                bins=val_bins,
                                labels=val_labels,
                                nool=val_no_ol,
                                permarker=val_per_marker,
                                debug=debug)
      
      # Save data.
      saveObject(name=val_name, object=datanew, parent=w, env=env)
      
      if(debug){
        print(str(datanew))
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
    
    } else {
      
      message <- "A dataset has to be selected."
      
      gmessage(message, title="Datasets not selected",
               icon = "error",
               parent = w) 
      
    }
    
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
      if(exists(".strvalidator_calculatePeaks_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".strvalidator_calculatePeaks_gui_savegui", envir=env)
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
      if(exists(".strvalidator_calculatePeaks_gui_nool", envir=env, inherits = FALSE)){
        svalue(f1_no_ol_chk) <- get(".strvalidator_calculatePeaks_gui_nool", envir=env)
      }
      if(exists(".strvalidator_calculatePeaks_gui_labels", envir=env, inherits = FALSE)){
        svalue(f1_labels_edt) <- get(".strvalidator_calculatePeaks_gui_labels", envir=env)
      }
      if(exists(".strvalidator_calculatePeaks_gui_bins", envir=env, inherits = FALSE)){
        svalue(f1_bins_edt) <- get(".strvalidator_calculatePeaks_gui_bins", envir=env)
      }
      if(exists(".strvalidator_calculatePeaks_gui_permarker", envir=env, inherits = FALSE)){
        svalue(f1_per_marker_chk) <- get(".strvalidator_calculatePeaks_gui_permarker", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".strvalidator_calculatePeaks_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".strvalidator_calculatePeaks_gui_nool", value=svalue(f1_no_ol_chk), envir=env)
      assign(x=".strvalidator_calculatePeaks_gui_labels", value=svalue(f1_labels_edt), envir=env)
      assign(x=".strvalidator_calculatePeaks_gui_bins", value=svalue(f1_bins_edt), envir=env)
      assign(x=".strvalidator_calculatePeaks_gui_permarker", value=svalue(f1_per_marker_chk), envir=env)
            
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_calculatePeaks_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculatePeaks_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_calculatePeaks_gui_nool", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculatePeaks_gui_nool", envir = env)
      }
      if(exists(".strvalidator_calculatePeaks_gui_labels", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculatePeaks_gui_labels", envir = env)
      }
      if(exists(".strvalidator_calculatePeaks_gui_bins", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculatePeaks_gui_bins", envir = env)
      }
      if(exists(".strvalidator_calculatePeaks_gui_permarker", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculatePeaks_gui_permarker", envir = env)
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
