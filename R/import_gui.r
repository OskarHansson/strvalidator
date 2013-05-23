################################################################################
# TODO LIST
# TODO: Check folder DOES NOT WORK BECAUSE \ IS ESCAPE CHARACTER.


################################################################################
# CHANGE LOG
# 16.04.2013: Added object name check.

#' @title Import from GMIDX
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


import_gui <- function(env=parent.frame()){
  
  # Load dependencies.  
  library(gWidgets)
  options(guiToolkit="RGtk2")
  
  # Global variables.
  debug=FALSE
#  separator <- .Platform$file.sep # Platform dependent path separator.
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  w <- gwindow(title="Import data from exported GeneMapper result files", 
               visible=FALSE)

  g <- ggroup(horizontal=FALSE,
               spacing=5,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # GUI #######################################################################
  
  import_opt <- gradio(items=c("Import multiple files from a directory into one dataset", "Import a single file"),
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

  import_file <- gfilebrowse(text="Select a file...", 
                             type="open",
                             quote = FALSE,
                             container=g)
  
  
  import_folder <- gfilebrowse(text="Select a directory...", 
                             type="selectdir",
                             quote = FALSE,
                             container=g)
  
  enabled(import_folder) <- FALSE
  
  
  # OPTIONS -------------------------------------------------------------------
  
  opt_frm <- gframe(text="Import options",
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
    name_val <- svalue(import_txt)

    ok <- TRUE
    
    # Check that a name has been provided for the new data object.
    if(nchar(name_val) > 0){
      
      # Check for existing object and ask for user input.
      if(exists(name_val, where=env)){

        dialog <- gbasicdialog(title="Warning!", parent=w, do.buttons=TRUE)
        
        msg <- glabel(text=paste("An object named '",name_val,"' already exist.\n\n",
                                 "Do you want to overwrite?", sep=""),
                      container=dialog)
        
        ok <- visible(dialog, set=TRUE)
        
      }
      
    } else {
      
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
        print("name_val")
        print(name_val)
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
      
      # Call function.
      data <- import(folder=folder_opt_val,
                     extension=extension_val,
                     suffix=suffix_val,
                     prefix=prefix_val,
                     resultFiles=file_val, 
                     resultFolder=folder_val)
      
      # Save data.
      assign(x=name_val, value=data, envir=env)
      
      # Close GUI.
      dispose(w)
      
    }
    
  } )

  # Show GUI.
  visible(w) <- TRUE
  
}
