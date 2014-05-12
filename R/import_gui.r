################################################################################
# TODO LIST
# TODO: Check folder DOES NOT WORK BECAUSE \ IS ESCAPE CHARACTER.

################################################################################
# CHANGE LOG
# 20.01.2014: Remove redundant "overwrite?" message dialog.
# 13.01.2014: Handle empty dataframe by stay in gui and show message.
# 10.12.2013: Updated with new parameter names in function 'import'.
# 12.11.2013: Pass debug to function.
# 18.07.2013: Check before overwrite object.
# 15.07.2013: Added save GUI settings.
# 11.06.2013: Fixed 'exists' added 'inherits=FALSE'. Added parameter 'debug'.
# 16.04.2013: Added object name check.

#' @title Import from text file
#'
#' @description
#' \code{import_gui} is a GUI wrapper for the \code{import} function.
#'
#' @details
#' Simplifies the use of the \code{import} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment into which the object will be saved.
#' Default is the current environment.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.


import_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Global variables.
  # separator <- .Platform$file.sep # Platform dependent path separator.
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Define variables.
  defaultDir <- "Select a directory..."
  defaultFile <- "Select a file..."

  
# Add new parameter , settings=FALSE  
#  # Load settings.
#   if(settings){
#     if(exists(".strvalidator_import_gui_file")){
#       defaultFile <- .strvalidator_import_gui_file
#     }
#     if(exists(".strvalidator_import_gui_dir")){
#       defaultDir <- .strvalidator_import_gui_dir
#     }
#     
#   }
  
  
  w <- gwindow(title="Import data from exported GeneMapper result files", 
               visible=FALSE)

  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })

  g <- ggroup(horizontal=FALSE,
               spacing=5,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # GUI #######################################################################

  savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=g)
  
  options <- c("Import multiple files from a directory into one dataset", 
               "Import a single file")

  import_opt <- gradio(items=options,
                       selected=2,
                       horizontal=FALSE,
                       container=g)

  addHandlerChanged(import_opt, handler = function (h, ...) {
    
    if(debug){
      print("OPTION")
    }

    # Get values.
    folder_opt_val <- if(svalue(import_opt, index=TRUE)==1){TRUE}else{FALSE}
    
    if(folder_opt_val){
      enabled(opt_frm) <- TRUE
      enabled(import_folder) <- TRUE
      enabled(import_file) <- FALSE
    } else {
      enabled(opt_frm) <- FALSE
      enabled(import_folder) <- FALSE
      enabled(import_file) <- TRUE
    }
    
  })

  import_file <- gfilebrowse(text=defaultFile,
                             initial.filename = defaultFile, # Not implemented in current version?
                             type="open",
                             quote = FALSE,
                             container=g)
  
  
  import_folder <- gfilebrowse(text=defaultDir, 
                               initial.dir = defaultDir, # Not implemented in current version?
                               type="selectdir",
                               quote = FALSE,
                               container=g)
  
  enabled(import_folder) <- FALSE
  
  
  # OPTIONS -------------------------------------------------------------------
  
  opt_frm <- gframe(text="Options",
                       pos=0,
                       horizontal=FALSE,
                       container=g)

  enabled(opt_frm) <- FALSE
  
  opt_pre_lbl <- glabel(text="Prefix:",
                       container=opt_frm,
                       anchor=c(-1 ,0))
  
  opt_pre_txt <- gedit(initial.msg="",
                      width = 25,
                      container=opt_frm,
                      expand=TRUE)
  
  opt_suf_lbl <- glabel(text="Suffix:",
                        container=opt_frm,
                        anchor=c(-1 ,0))
  
  opt_suf_txt <- gedit(initial.msg="",
                       width = 25,
                       container=opt_frm,
                       expand=TRUE)
  
  opt_ext_lbl <- glabel(text="Extension:",
                        container=opt_frm,
                        anchor=c(-1 ,0))
  
  opt_ext_txt <- gedit(text="txt",
                       width = 25,
                       container=opt_frm,
                       expand=TRUE)

  import_lbl <- glabel(text="Name:",
                       container=g,
                       anchor=c(-1 ,0))
  
  import_txt <- gedit(initial.msg="Name for new dataset",
                      width = 25,
                      container=g,
                      expand=TRUE)
  
  # IMPORT --------------------------------------------------------------------

  import_btn <- gbutton(text="Import",
                        border=TRUE,
                        container=g)

  
  addHandlerChanged(import_btn, handler = function(h, ...) {

    # Get values.
    file_val <- svalue(import_file)
    folder_val <- svalue(import_folder)
    prefix_val <- svalue(opt_pre_txt)
    suffix_val <- svalue(opt_suf_txt)
    extension_val <- svalue(opt_ext_txt)
    folder_opt_val <- if(svalue(import_opt, index=TRUE)==1){TRUE}else{FALSE}
    val_name <- svalue(import_txt)

    ok <- TRUE
    
    # Check that a name has been provided for the new data object.
    if(nchar(val_name) == 0){
      
      gmessage("A name for the dataset must be provided.",
               title="Error", icon="error", parent=w)
      
      ok <- FALSE
      
    }

#     # TODO: DOES NOT WORK BECAUSE \ IS ESCAPE CHARACTER.
#     # Check that folder exist.
#     if(!file.exists(folder_val)){
# 
#       ok <- FALSE
#       
#       gmessage("The provided folder does not exist or is not accessible.",
#                title="Error", icon="error", parent=w)
#       
#     }
    
    
    # Check if ok to import data to 'env'.
    if(ok){

      # Set arguments.
      if(folder_opt_val){
        file_val <- NA
      } else {
        folder_val <- NA
      }
      
      if(!nchar(prefix_val)>0){
        prefix_val <- NA
      }
      
      if(!nchar(suffix_val)>0){
        suffix_val <- NA
      }
      
      if(debug){
        print("val_name")
        print(val_name)
        print("prefix_val")
        print(prefix_val)
        print("suffix_val")
        print(suffix_val)
        print("folder_opt_val")
        print(folder_opt_val)
        print("file_val")
        print(file_val)
        print("folder_val")
        print(folder_val)
      }
      
      # Change button.
      svalue(import_btn) <- "Processing..."
      enabled(import_btn) <- FALSE
      
      # Call function.
      datanew <- import(folder=folder_opt_val,
                        extension=extension_val,
                        suffix=suffix_val,
                        prefix=prefix_val,
                        fileName=file_val,
                        folderName=folder_val,
                        debug=debug)
      
      if(length(datanew) == 0){
        
        # Show warning.
        gmessage(message="Dataset empty!\nCheck your file filter.",
                 title="Error",
                 icon = "error",
                 parent = w)
        
        # Change button.
        svalue(import_btn) <- "Import"
        enabled(import_btn) <- TRUE
        
      } else {

        # Save data.
        saveObject(name=val_name, object=datanew, parent=w, env=env)
        
        # Close GUI.
        dispose(w)
        
      }
      
    }
    
  } )

  # INTERNAL FUNCTIONS ########################################################
  
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
      if(exists(".strvalidator_import_gui_savegui", envir=env, inherits = FALSE)){
        svalue(savegui_chk) <- get(".strvalidator_import_gui_savegui", envir=env)
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
      if(exists(".strvalidator_import_gui_import_opt", envir=env, inherits = FALSE)){
        svalue(import_opt) <- get(".strvalidator_import_gui_import_opt", envir=env)
      }
      if(exists(".strvalidator_import_gui_prefix", envir=env, inherits = FALSE)){
        svalue(opt_pre_txt) <- get(".strvalidator_import_gui_prefix", envir=env)
      }
      if(exists(".strvalidator_import_gui_suffix", envir=env, inherits = FALSE)){
        svalue(opt_suf_txt) <- get(".strvalidator_import_gui_suffix", envir=env)
      }
      if(exists(".strvalidator_import_gui_extension", envir=env, inherits = FALSE)){
        svalue(opt_ext_txt) <- get(".strvalidator_import_gui_extension", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(savegui_chk)){
      
      assign(x=".strvalidator_import_gui_savegui", value=svalue(savegui_chk), envir=env)
      assign(x=".strvalidator_import_gui_import_opt", value=svalue(import_opt), envir=env)
      assign(x=".strvalidator_import_gui_prefix", value=svalue(opt_pre_txt), envir=env)
      assign(x=".strvalidator_import_gui_suffix", value=svalue(opt_suf_txt), envir=env)
      assign(x=".strvalidator_import_gui_extension", value=svalue(opt_ext_txt), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_import_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_import_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_import_gui_import_opt", envir=env, inherits = FALSE)){
        remove(".strvalidator_import_gui_import_opt", envir = env)
      }
      if(exists(".strvalidator_import_gui_prefix", envir=env, inherits = FALSE)){
        remove(".strvalidator_import_gui_prefix", envir = env)
      }
      if(exists(".strvalidator_import_gui_suffix", envir=env, inherits = FALSE)){
        remove(".strvalidator_import_gui_suffix", envir = env)
      }
      if(exists(".strvalidator_import_gui_extension", envir=env, inherits = FALSE)){
        remove(".strvalidator_import_gui_extension", envir = env)
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
