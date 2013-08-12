################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 25.07.2013: Parameter 'fixed' changed to 'word'.
# 15.07.2013: Added save GUI settings.
# 15.07.2013: Added 'options' group.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.
# 17.05.2013: listDataFrames() -> listObjects()
# 27.04.2013: First version.


#' @title Check subsetting of a dataset
#'
#' @description
#' \code{checkSubset_gui} is a GUI wrapper for the \code{checkSubset} function.
#'
#' @details
#' Simplifies the use of the \code{checkSubset} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' 

checkSubset_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  # Global variables.
  .gData <- NULL
  .gRef <- NULL

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.
  w <- gwindow(title="Check subsetting", visible=FALSE)
  
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
               spacing = 10,
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
        svalue(dataset_samples_lbl) <- "0 samples"
        
      } else {

        # Load or change components.
        samples <- length(unique(.gData$Sample.Name))
        svalue(dataset_samples_lbl) <- paste("", samples, "samples")
        
      }
      
    } else {
      
      # Reset components.
      .gData <<- data.frame(No.Data=NA)
      svalue(dataset_samples_lbl) <- " 0 samples"
      
    }
  } )

  grid0[2,1] <- glabel(text="Select reference set:", container=grid0)
  
  grid0[2,2] <- dataset_ref_drp <- gdroplist(items=c("<Select dataset>",
                                                 listObjects(env=env,
                                                             objClass="data.frame")),
                                         selected = 1,
                                         editable = FALSE,
                                         container = grid0)
  
  grid0[2,3] <- dataset_ref_lbl <- glabel(text=" 0 reference samples",
                                              container=grid0)
  
  addHandlerChanged(dataset_ref_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_ref_drp)
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gRef <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Marker")
      
      if(!all(requiredCol %in% colnames(.gRef))){
        
        missingCol <- requiredCol[!requiredCol %in% colnames(.gRef)]

        message <- paste("Additional columns required:\n",
                         paste(missingCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 

        # Reset components.
        .gRef <<- data.frame(No.Data=NA)
        svalue(dataset_ref_lbl) <- "0 reference samples"
        
      } else {
        
        refs <- length(unique(.gRef$Sample.Name))
        svalue(dataset_ref_lbl) <- paste("", refs, "reference samples")
        
      }
      
    } else {
      
      .gRef <<- data.frame(No.Data=NA)
      svalue(dataset_ref_lbl) <- " 0 samples"
      
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
  
  f1_ignore_case_chk <- gcheckbox(text="Ignore case ('A' will match 'A', 'B-a.2', and 'A2')",
                                  checked = TRUE,
                                  container = f1)

  f1_word_chk <- gcheckbox(text="Add word boundaries ('A' will match 'A', 'B-A.2', and 'A 2' but not 'A2')",
                                  checked = FALSE,
                                  container = f1)

  # BUTTON --------------------------------------------------------------------

  if(debug){
    print("BUTTON")
  }  
  
  check_btn <- gbutton(text="Subset",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(check_btn, handler = function(h, ...) {
    
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_case_chk)
    val_word <- svalue(f1_word_chk)
    
    
    if (!is.null(.gData) || !is.null(.gRef)){
      
      chksubset_w <- gwindow(title = "Check subsetting",
                             visible = FALSE, name=title,
                             width = NULL, height= NULL, parent=w,
                             handler = NULL, action = NULL)
      
      chksubset_txt <- checkSubset(data=val_data,
                                   ref=val_ref,
                                   ignoreCase=val_ignore,
                                   word=val_word,
                                   console=FALSE)
      
      gtext (text = chksubset_txt, width = NULL, height = 300, font.attr = NULL, 
             wrap = FALSE, container = chksubset_w)
      
      visible(chksubset_w) <- TRUE
      
    } else {
      
      gmessage(message="Data frame is NULL!\n\n
               Make sure to select a dataset and a reference set",
               title="Error",
               icon = "error")      
      
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
      if(exists(".checkSubset_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".checkSubset_gui_savegui", envir=env)
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
      if(exists(".checkSubset_gui_ignore", envir=env, inherits = FALSE)){
        svalue(f1_ignore_case_chk) <- get(".checkSubset_gui_ignore", envir=env)
      }
      if(exists(".checkSubset_gui_fixed", envir=env, inherits = FALSE)){
        svalue(f1_word_chk) <- get(".checkSubset_gui_fixed", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".checkSubset_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".checkSubset_gui_ignore", value=svalue(f1_ignore_case_chk), envir=env)
      assign(x=".checkSubset_gui_fixed", value=svalue(f1_word_chk), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".checkSubset_gui_savegui", envir=env, inherits = FALSE)){
        remove(".checkSubset_gui_savegui", envir = env)
      }
      if(exists(".checkSubset_gui_ignore", envir=env, inherits = FALSE)){
        remove(".checkSubset_gui_ignore", envir = env)
      }
      if(exists(".checkSubset_gui_fixed", envir=env, inherits = FALSE)){
        remove(".checkSubset_gui_fixed", envir = env)
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
