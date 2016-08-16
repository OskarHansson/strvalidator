################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 15.08.2016: Added save settings.
# 14.08.2016: Renamed to calculateCopies_gui and implementing new calculateCopies.
# 16.06.2016: 'Save as' textbox expandable.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.
# 20.05.2013: First version.

#' @title Calculate Allele Copies
#'
#' @description
#' GUI wrapper for the \code{link{calculateCopies}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateCopies}} function by
#' providing a graphical user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#' 
#' @return TRUE
#' 
#' @export
#' 
#' @importFrom utils help
#' 
#' @seealso \code{\link{calculateCopies}}

calculateCopies_gui <- function(env=parent.frame(), savegui=NULL,
                                debug=FALSE, parent=NULL){
  
  
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.
  w <- gwindow(title="Calculate allele copies", visible=FALSE)
  
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
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")
  
  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE,
                           container = gh)
  enabled(savegui_chk) <- FALSE
  
  addSpring(gh)
  
  help_btn <- gbutton(text="Help", container=gh)
  
  addHandlerChanged(help_btn, handler = function(h, ...) {
    
    # Open help page for function.
    print(help("calculateCopies_gui", help_type="html"))
    
  })
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Datasets", horizontal = FALSE, spacing = 5,
               container = gv)
  
  f0g0 <- glayout(container = f0, spacing = 1)
  
  # Datasets ------------------------------------------------------------------
  
  f0g0[1,1] <- glabel(text = "Select dataset:", container = f0g0)
  
  dataset_drp <- gdroplist(items = c("<Select dataset>",
                                     listObjects(env = env,
                                                 obj.class = "data.frame")),
                           selected = 1, editable = FALSE, container = f0g0)
  f0g0[1,2] <- dataset_drp
  
  f0g0_samples_lbl <- glabel(text = " 0 samples", container = f0g0)
  f0g0[1,3] <- f0g0_samples_lbl
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(name = val_obj, reqcol = requiredCol,
                       env = env, parent = w, debug = debug)
    
    if(ok){

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(f0g0_samples_lbl) <- paste("", samples, "samples")
      svalue(f2_save_edt) <- paste(val_obj, "_cop", sep = "")
        
      
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
    
  } )  

  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options", horizontal=FALSE,
               spacing = 10, container = gv) 
  
  f1_note <- paste("Note that the 'copies' and 'heterozygous' option are",
                   "intended for known complete profiles, while 'observed'",
                   "can be used for any samples to count the number of peaks.")

  gtext(text = f1_note, container = f1)
  
  f1_observed_chk <- gcheckbox(text = "Add number of unique alleles",
                               checked = FALSE, container = f1)
  
  f1_copies_chk <- gcheckbox(text = "Add number of allele copies",
                             checked = TRUE, container = f1)
  tooltip(f1_copies_chk) <- "Indicated by '1' for heterozygotes and '2' for homozygotes."
  
  f1_het_chk <- gcheckbox(text = "Add heterozygote indicator",
                          checked = FALSE, container=f1)
  tooltip(f1_het_chk) <- "Indicated by '1' for heterozygous and '0' for homozygous loci."

  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as", horizontal = TRUE, spacing = 5,
               container = gv)
  
  glabel(text = "Name for result:", container = f2)
  
  f2_save_edt <- gedit(text = "", expand = TRUE, container = f2)
  
  # BUTTON ####################################################################
  
  calculate_btn <- gbutton(text = "Calculate", border = TRUE, container = gv)
  
  addHandlerChanged(calculate_btn, handler = function(h, ...) {
    
    val_name <- svalue(f2_save_edt)
    val_data <- .gData
    val_data_name <- .gDataName
    val_obs <- svalue(f1_observed_chk)
    val_cop <- svalue(f1_copies_chk)
    val_het <- svalue(f1_het_chk)
    
    if(!is.null(val_data)){
      
      # Change button.
      svalue(calculate_btn) <- "Processing..."
      enabled(calculate_btn) <- FALSE
      
      datanew <- calculateCopies(data = val_data, observed = val_obs,
                                 copies = val_cop, heterozygous = val_het,
                                 debug = debug)

      # Add attributes.
      attr(datanew, which = "calculateCopies_gui, data") <- val_data_name
      attr(datanew, which = "calculateCopies_gui, observed") <- val_obs
      attr(datanew, which = "calculateCopies_gui, copies") <- val_cop
      attr(datanew, which = "calculateCopies_gui, heterozygous") <- val_het

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)
      
      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
      
    } else {
      
      message <- "A dataset has to be selected."
      
      gmessage(message = message, title = "Dataset not selected",
               icon = "error", parent = w)
      
    }
    
  } )
  
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
      if(exists(".strvalidator_calculateCopies_gui_savegui", envir=env, inherits = FALSE)){
        svalue(savegui_chk) <- get(".strvalidator_calculateCopies_gui_savegui", envir=env)
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
      if(exists(".strvalidator_calculateCopies_gui_observed", envir=env, inherits = FALSE)){
        svalue(f1_observed_chk) <- get(".strvalidator_calculateCopies_gui_observed", envir=env)
      }
      if(exists(".strvalidator_calculateCopies_gui_copies", envir=env, inherits = FALSE)){
        svalue(f1_copies_chk) <- get(".strvalidator_calculateCopies_gui_copies", envir=env)
      }
      if(exists(".strvalidator_calculateCopies_gui_het", envir=env, inherits = FALSE)){
        svalue(f1_het_chk) <- get(".strvalidator_calculateCopies_gui_het", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(savegui_chk)){
      
      assign(x=".strvalidator_calculateCopies_gui_savegui", value=svalue(savegui_chk), envir=env)
      assign(x=".strvalidator_calculateCopies_gui_observed", value=svalue(f1_observed_chk), envir=env)
      assign(x=".strvalidator_calculateCopies_gui_copies", value=svalue(f1_copies_chk), envir=env)
      assign(x=".strvalidator_calculateCopies_gui_het", value=svalue(f1_het_chk), envir=env)

    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_calculateCopies_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateCopies_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_calculateCopies_gui_observed", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateCopies_gui_observed", envir = env)
      }
      if(exists(".strvalidator_calculateCopies_gui_copies", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateCopies_gui_copies", envir = env)
      }
      if(exists(".strvalidator_calculateCopies_gui_het", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateCopies_gui_het", envir = env)
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
