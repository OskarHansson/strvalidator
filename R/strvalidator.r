################################################################################
# TODO LIST
# TODO: Update saved object in dataset-env
# TODO: STUTTER TAB:  button, copy to clipboard.
# TODO: Multiple selection not working.

# NOTE:
# \u00B5 is the unicode for µ 

################################################################################
# CHANGE LOG
# 20.05.2013: New functions 'AddData', 'calculateH'.
# 17.05.2013: listDataFrames() -> listObjects()
# 13.05.2013: Save/Load workspace added.
# 09.05.2013: Removed tables and 'save as' from tabs, new function editData_gui()
# 27.04.2013: Trim updated with dataset selection.
# 25.04.2013: Slim updated with dataset selection.
# <25.04.2013: Added BALANCE tab.
# <25.04.2013: Test using an environment for passing data.
# <25.04.2013: First version.

#' @title GUI for STR Validator
#'
#' @description
#' \code{strvalidator} is validation toolbox GUI .
#'
#' @details
#' This graphical user interface make it very easy to perform an internal
#' validation of forensic STR kits. The GUI works as a toolbox for functions
#' required to analyse validation data.
#' @param debug logical indicating printing debug information.
#' @export
#' @examples
#' # To start the graphical user interface.
#' \dontrun{
#' strvalidator()
#' }



strvalidator <- function(debug=FALSE){
  
#   # Load packages.
#   if(!require("gWidgets")){
#     print("trying to install gWidgets...")
#     install.packages("gWidgets")
#     if(!require(gWidgets)){
#      stop("could not install gWidgets")
#     }
#   }
  #if (!require("gWidgetsRGtk2")) install.packages("gWidgetsRGtk2")
  #require(gWidgets)
  options(guiToolkit="RGtk2")
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  # Global variables.
  .strvalidator_dataset <- new.env()
  .separator <- .Platform$file.sep # Platform dependent path separator.
  .start_tab_name <- "Welcome"
  .file_tab_name <- "File"
  .drylab_tab_name <- "DryLab"
  .edit_tab_name <- "Edit"
  .stutter_tab_name <- "Stutter"
  .balance_tab_name <- "Balance"
  .drop_tab_name <- "Dropout"
  
  # MAIN WINDOW  ##############################################################
  
  # Main window.
  w <- gwindow(title="STR Validator - a forensic validation toolbox",
               visible = FALSE,
               name=title)
  
  # Main client area.
  nb <- gnotebook(closebuttons = FALSE,
                  dontCloseThese = NULL,
                  container = w)

  
  # NOTEBOOK ##################################################################
  
  # Define groups.
  start_tab <- ggroup(horizontal = FALSE,
                     spacing=10,
                     use.scrollwindow=FALSE,
                     container = nb,
                     label=.start_tab_name,
                     expand=FALSE)

  file_tab <- ggroup(horizontal = FALSE,
                       spacing=10,
                       use.scrollwindow=FALSE,
                       container = nb,
                       label=.file_tab_name,
                       expand=FALSE)
  
  drylab_tab <- ggroup(horizontal = FALSE,
                         spacing=10,
                         use.scrollwindow=FALSE,
                         container = nb,
                         label=.drylab_tab_name,
                         expand=FALSE)
  
  edit_tab <- ggroup(horizontal = FALSE,
                      spacing=5,
                      use.scrollwindow=FALSE,
                      container = nb,
                      label=.edit_tab_name,
                      expand=FALSE)
  
  stutter_tab <- ggroup(horizontal = FALSE,
                  spacing=5,
                  use.scrollwindow=FALSE,
                  container = nb,
                  label=.stutter_tab_name,
                  expand=FALSE)

  balance_tab <- ggroup(horizontal = FALSE,
                        spacing=5,
                        use.scrollwindow=FALSE,
                        container = nb,
                        label=.balance_tab_name,
                        expand=FALSE)
  
  drop_tab <- ggroup(horizontal = FALSE,
                        spacing=5,
                        use.scrollwindow=FALSE,
                        container = nb,
                        label=.drop_tab_name,
                        expand=FALSE)

  # START #####################################################################
  
  glabel("", container=start_tab) # Adds some space.
  
  # STR TYPING KIT ------------------------------------------------------------
  
  start_frame_1 <- gframe(text = "STR validator",
                          markup = FALSE,
                          pos = 0,
                          horizontal=TRUE,
                          container = start_tab,
                          expand=TRUE) 
  
  about_txt <- paste("STR validator is a package intended for validation of ",
                     "forensic short tandem repeat (STR)  DNA typing kit. ",
                     "This graphical user interface make it very easy to ",
                     "analyse data from internal validations. ",
                     "Keep in mind that this is an early version still under ",
                     "development (check the result carefully).\n\n",
                     "Please report bugs to:\n",
                     "https://github.com/OskarHansson/strvalidator/issues\n\n",
                     "The source is hosted at GitHub:\n",
                     "https://github.com/OskarHansson/strvalidator", sep="")
  
  gtext(text=about_txt, width = NULL, height = 300, font.attr = NULL, 
        wrap = TRUE, expand=TRUE, container = start_frame_1) 
  
  start_frame_2 <- gframe(text = "License",
                          markup = FALSE,
                          pos = 0,
                          horizontal=TRUE,
                          container = start_tab,
                          expand=TRUE) 
  
  license_txt <- paste("Copyright (C) 2013 Oskar Hansson\n\n",
                       "This program is free software; you can redistribute it and/or ",
                       "modify it under the terms of the GNU General Public License ",
                       "as published by the Free Software Foundation; either version 2 ",
                       "of the License, or (at your option) any later version.\n\n",
                       "This program is distributed in the hope that it will be useful, ",
                       "but WITHOUT ANY WARRANTY; without even the implied warranty of ",
                       "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the ",
                       "GNU General Public License for more details.\n\n",
                       "You should have received a copy of the GNU General Public License ",
                       "along with this program; if not, write to the Free Software ",
                       "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, ",
                       "MA  02110-1301, USA.", sep="")
  
  gtext(text=license_txt, width = NULL, height = 300, font.attr = NULL, 
        wrap = TRUE, expand=TRUE, container = start_frame_2) 
  
  
  # FILE ######################################################################
  
  if(debug){
    print("TAB: FILE")
  }
  
  
  # LOADED DATASETS -----------------------------------------------------------
  
  if(debug){
    print("LOADED DATASETS")
  }
  
  file_loaded_f <- gframe(text = "Loaded datasets",
                          markup = FALSE,
                          pos = 0,
                          horizontal=TRUE,
                          container = file_tab,
                          expand=TRUE)

  file_loaded_f1 <- ggroup(horizontal=FALSE,
                          container = file_loaded_f,
                          expand=FALSE)
  
  file_loaded_ws_btn <- gbutton(text="Load workspace",
                                    border=TRUE,
                                    container = file_loaded_f1)
  
  file_loaded_import_btn <- gbutton(text="Import from GMIDX",
                                     border=TRUE,
                                     container = file_loaded_f1)
  
  file_loaded_refresh_btn <- gbutton(text="Refresh list",
                                     border=TRUE,
                                     container = file_loaded_f1) 
  
  file_loaded_rm_btn <- gbutton(text="Remove selected",
                                 border=TRUE,
                                 container = file_loaded_f1) 

  file_loaded_ws_save_btn <- gbutton(text="Save workspace",
                                border=TRUE,
                                container = file_loaded_f1)
  
  file_loaded_rename_btn <- gbutton(text="Rename",
                                     border=TRUE,
                                     container = file_loaded_f1)
  
  file_loaded_tbl <- gtable(items=listObjects(env=.strvalidator_dataset,
                                              objClass="data.frame"), 
                            multiple = TRUE,
                            expand = TRUE,
                            container = file_loaded_f) 

  
  addHandlerChanged(file_loaded_rename_btn, handler = function (h, ...) {
    
    
    object <- svalue(file_loaded_tbl)
    
    if(length(object) == 1){
      
      newName_inp <- ginput(message="Enter new name",
                        title="Input",
                        icon = "info",
                        parent=w)
      
      newName_inp <- make.names(newName_inp)
      
      if(debug){
        print("newName_inp")
        print(newName_inp)
      }
      
      assign(newName_inp,
             get(object, envir = .strvalidator_dataset)
             , envir = .strvalidator_dataset)
      
      remove(list=object, envir=.strvalidator_dataset)
      
      .refreshLoaded()
      
      
    } else {
      gmessage(message="You can only rename one object at a time!",
               title="Error",
               icon = "error",
               parent = w) 
    }
    
  } )

  addHandlerChanged(file_loaded_ws_btn, handler = function (h, ...) {
    
    
    ws_path <- gfile(text="Select a saved workspace", type="open",
                     filter = list("R files" = list(patterns = c("*.R","*.Rdata"))),
                     multi=FALSE)
    
    if(!is.na(ws_path)){
      if(file.exists(ws_path)){
        
        load(file=ws_path, envir = .strvalidator_dataset)
        .refreshLoaded()
        
      } else {
        
        gmessage(message="The workspace file was not found",
                 title="File not found",
                 icon = "error",
                 parent = w) 
      }
    }    
    
  } )
  
  addHandlerChanged(file_loaded_import_btn, handler = function (h, ...) {
    
    import_gui(env=.strvalidator_dataset)
    .refreshLoaded()
    
  } )  

  addHandlerChanged(file_loaded_refresh_btn, handler = function (h, ...) {
    
    .refreshLoaded()
    
  })
  
  addHandlerChanged(file_loaded_rm_btn, handler = function(h, ...) {
    
    # Get selected dataset name(s).
    val_obj <- svalue(file_loaded_tbl)
    
    if(debug){
      print(paste("IN:", match.call()[[1]]))
      print("Changed, file_loaded_rm_btn")
      print(val_obj)
    }
    
    if (!is.na(val_obj) && !is.null(val_obj)){
      
      # Get active reference data frame.
      remove(list=val_obj, envir=.strvalidator_dataset)
      
      .refreshLoaded()
      
      
    } 
  } )

  addHandlerChanged(file_loaded_ws_save_btn, handler = function (h, ...) {
    
    
    ws_save_path <- gfile(text="Select a directory to save workspace in",
                          type="selectdir",
                          filter = list("R files" = list(patterns = c("*.R","*.Rdata"))),
                          multi=FALSE)
    
    
    ws_name <- ginput(message="Save current workspace as", text="",
           title="Input",
           icon ="info",
           parent=w)
    
    if(!is.na(ws_name) && !ws_name==""){
      
      ws_full_name <- paste(ws_save_path, .separator, ws_name, ".RData", sep="")
      
      if(debug){
        print(ws_full_name)
      }
      
      # TODO: check if file exists. ask for overwrite.
      #if(file.exists(ws_save_path)){
        
      if(file.exists(ws_save_path)){
        
        save(file=ws_full_name, 
             list=ls(envir = .strvalidator_dataset, all.names = TRUE),
             envir = .strvalidator_dataset)
        
      } else {
        
        gmessage(message="The workspace file was not found",
                 title="File not found",
                 icon = "error",
                 parent = w) 
      }
    
    } else {
      gmessage(message="A file name must be given",
               title="No file name",
               icon = "error",
               parent = w) 
    }
    
    
    
  } )
  
  
  # DATASETS ------------------------------------------------------------------  
  
  if(debug){
    print("DATASETS")
  }

  
  file_ws_f <- gframe(text = "Load dataframe from R workspace",
                      markup = FALSE,
                      pos = 0,
                      horizontal=TRUE,
                      container = file_tab,
                      expand=FALSE)

  file_ws_f1 <- ggroup(horizontal=FALSE,
                           container = file_ws_f,
                           expand=FALSE)
  
  glabel("", container=file_ws_f1) # Adds some space.
  
  file_ws_refresh_btn <- gbutton(text="Refresh dropdown",
                                 border=TRUE,
                                 container = file_ws_f1) 
  
  file_ws_load_btn <- gbutton(text="Load dataset",
                              border=TRUE,
                              container = file_ws_f1) 

  glabel("", container=file_ws_f1) # Adds some space.
  
  file_ws_drp <- gdroplist(items=c("<Select dataframe>", 
                                   listObjects(env=.strvalidator_dataset,
                                             objClass="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = file_ws_f) 
  
  addHandlerChanged(file_ws_refresh_btn, handler = function (h, ...) {
    
    .refreshWs()
  } )
  
  addHandlerChanged(file_ws_load_btn, handler = function(h, ...) {
    
    # Get selected dataset name.
    val_obj <- svalue(file_ws_drp)
    
    if (!is.na(val_obj) && !is.null(val_obj)){
      
      # Load dataset.
      assign(val_obj, get(val_obj), envir=.strvalidator_dataset)
      
      # Update list.
      .refreshLoaded()
      
    } 
  } )

  
  # STR TYPING KIT ------------------------------------------------------------
  
  if(debug){
    print("STR TYPING KIT")
  }
  
  #glabel("", container=file_tab) # Adds some space.
  
  
  # LOAD ----------------------------------------------------------------------  
  
  if(debug){
    print("LOAD")
  }
  
  file_load_f <- gframe(text = "Load .RData file",
                        markup = FALSE,
                        pos = 0,
                        horizontal=TRUE,
                        container = file_tab,
                        expand=FALSE)

  glabel("", container=file_load_f) # Adds some space.
  
  loadDefText <- "Select file..."
  file_load_brw <- gfilebrowse(text=loadDefText,
                               quote=FALSE,
                               type="open",
                               container=file_load_f)
  
  file_load_btn <- gbutton(text="Load",
                           border=TRUE,
                           expand=FALSE,
                           container=file_load_f)
  
  glabel("", container=file_load_f) # Adds some space.

  addHandlerChanged(file_load_btn, handler = function(h, ...) {
    
    # Get values.
    file_val <- svalue(file_load_brw)
    
    if(file.exists(file_val)){
      #if(file_val != "" && file_val != loadDefText){
      
      load(file_val, envir = .strvalidator_dataset)
      
      .refreshLoaded()
      
    } else {
      
      gmessage(message="File not found.",
               title="Error",
               icon = "error")      
    }       
  } )
  
  
  
  
  # EXPORT --------------------------------------------------------------------

  file_export_f <- gframe(text = "Export | Save",
                        markup = FALSE,
                        pos = 0,
                        horizontal=FALSE,
                        container = file_tab,
                        expand=FALSE)
  

  file_export_grid <- glayout(container = file_export_f)

  file_export_grid[1,1] <- file_export_chk <- gcheckbox(text="Use object names",
                                                        checked = TRUE,
                                                        container = file_export_grid)
  
  file_export_grid[2,1] <- glabel(text="File name (separated by | ):",
                                  container=file_export_grid,
                                  anchor=c(-1 ,0))
  
  
  file_export_grid[3,1] <- file_export_name_txt <- gedit(text="",
                                                     container=file_export_grid)
  
  enabled(file_export_name_txt) <- FALSE
  
  file_export_grid[2,2] <- glabel(text="File extension:",
                                  container=file_export_grid,
                                  anchor=c(-1 ,0))
  

  file_export_grid[3,2] <- file_export_ext_cbo <- gcombobox(items=c(".txt", ".RData"),
                                                            selected = 1,
                                                            editable = TRUE,
                                                            container = file_export_grid)
  
  file_export_grid[2,3] <- glabel(text="Delimeter:",
                                  container=file_export_grid,
                                  anchor=c(-1 ,0))
  
  file_export_grid[3,3] <- file_export_del_drp <- gdroplist(items=c("TAB","SPACE","COMMA"), 
                                                   selected = 1,
                                                   editable = FALSE,
                                                   container = file_export_grid) 
  

  file_export_grid[4,1] <- glabel(text="File path:",
                                    container=file_export_grid,
                                    anchor=c(-1 ,0))
  
  expDefText <- "Select folder..."
  file_export_grid[5,1:2] <- file_export_save_brw <- gfilebrowse(text=expDefText,
                                                        quote=FALSE,
                                                        type="selectdir",
                                                        container=file_export_grid)
  
  file_export_grid[5,3] <- file_export_save_btn <- gbutton(text="Save",
                                                    border=TRUE,
                                                    container=file_export_grid) 
  
  addHandlerChanged(file_export_chk, handler = function(h, ...) {
    
    # Get values.
    ext_val <- svalue(file_export_chk)
    
    if(ext_val){
      enabled(file_export_name_txt) <- FALSE
    } else {
      enabled(file_export_name_txt) <- TRUE
    }    
  } )
  
  addHandlerChanged(file_export_ext_cbo, handler = function(h, ...) {
    
    # Get values.
    ext_val <- svalue(file_export_ext_cbo)
    
    if(ext_val == ".RData"){
      enabled(file_export_del_drp) <- FALSE
    } else {
      enabled(file_export_del_drp) <- TRUE
    }    
  } )
  
  addHandlerChanged(file_export_save_btn, handler = function(h, ...) {
    
    # Get values.
    path_val <- svalue(file_export_save_brw)
    file_val <- svalue(file_export_name_txt)
    ext_val <- svalue(file_export_ext_cbo)
    del_val <- svalue(file_export_del_drp, index=TRUE)
    data_val <- svalue(file_loaded_tbl)
    chk_val <- svalue(file_export_chk)
    
    if(debug){
      print("path_val")
      print(path_val)
      print("file_val")
      print(file_val)
      print("ext_val")
      print(ext_val)
      print("del_val")
      print(del_val)
      print("data_val")
      print(data_val)
    }

    # Create file names.
    nbObj <- length(data_val)
    if(chk_val){
      #file_val <- deparse(substitute(data_val))
      file_val <- data_val
    } else {
      file_val <-  unlist(strsplit(file_val, "\\|"))
      if(length(file_val) == nbObj){
        file_val <- make.names(file_val, unique=TRUE)
      } else {
        file_val <- make.names(rep(file_val[1], nbObj), unique=TRUE)
      }
    }

    if(debug){
      print("file_val")
      print(file_val)
    }
    
    if(file_val != "" && path_val != expDefText){

      # Add trailing path separator if not present.
      if(substr(path_val, nchar(path_val), nchar(path_val)+1) != .separator){
        path_val <- paste(path_val, .separator, sep="")
      }

      if(debug){
        print("path_val")
        print(path_val)
      }
      
      # Use 'save' or 'write.table'.
      if(ext_val == ".RData"){

        for(i in seq(along=nbObj)){
          
          # Construct complete file name.
          complete_file_name <- paste(path_val, file_val[i], ".RData", sep="")
          
          save(list=data_val[i], file=complete_file_name, envir=.strvalidator_dataset)
        }
        
      } else {

        # Assign a delimeter character.
        if(del_val == 1){
          delimeter <- "\t"   
        } else if(del_val == 2){
          delimeter <- " "
        } else if(del_val == 3){
          delimeter <- ","
        } 

        if(debug){
          print("del_val")
          print(del_val)
        }

        for(i in seq(along=nbObj)){
          
          # Construct complete file name.
          complete_file_name <- paste(path_val, file_val[i], ext_val, sep="")
          
          write.table(x=get(data_val[i], envir=.strvalidator_dataset),
                      file = complete_file_name,
                      append = FALSE, quote = FALSE, sep = delimeter,
                      dec = ".", row.names = FALSE,
                      col.names = TRUE)
        }
      }
      
    } else {
      
      gmessage(message="File name and path must be provided.",
               title="Error",
               icon = "error")      
    }    
  } )
  
  

  # DRY LAB  ##################################################################
  
  if(debug){
    print("TAB: DRY LAB")
  }
  
  glabel("Here will come functions for:",
        container=drylab_tab,
         anchor=c(-1 ,0)) # Adds some space.
  glabel("Analysis of the panels and bin files.",
         container=drylab_tab,
         anchor=c(-1 ,0)) # Adds some space.
  glabel("Creation of marker ranges plot.",
        container=drylab_tab,
         anchor=c(-1 ,0)) # Adds some space.
  glabel("Creation of kit information objects.",
         container=drylab_tab,
         anchor=c(-1 ,0)) # Adds some space.
  glabel("Allelic ladder analysis.",
         container=drylab_tab,
         anchor=c(-1 ,0)) # Adds some space.
  
  
  # EDIT  #####################################################################
  
  if(debug){
    print("TAB: EDIT")
  }
  
  glabel("", container=edit_tab) # Adds some space.
  
  
  edit_grid <- glayout(container = edit_tab)
  
  # VIEW/EDIT -----------------------------------------------------------------
  
  edit_grid[1,1] <- edit_view_btn <- gbutton(text="View",
                                             border=TRUE,
                                             container = edit_grid) 
  
  edit_grid[1,2] <- glabel(text="View or edit a data frame.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_view_btn, handler = function(h, ...) {
    
    # Open GUI.
    editData_gui(env=.strvalidator_dataset)
    
  } )

  # TRIM ----------------------------------------------------------------------
  
  edit_grid[2,1] <- edit_trim_btn <- gbutton(text="Trim",
                                             border=TRUE,
                                             container = edit_grid) 
  
  edit_grid[2,2] <- glabel(text="Trim a dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_trim_btn, handler = function(h, ...) {
    
    # Open GUI.
    trim_gui(env=.strvalidator_dataset)
    
  } )

  # SLIM ----------------------------------------------------------------------
  
  edit_grid[3,1] <- edit_slim_btn <- gbutton(text="Slim",
                                        border=TRUE,
                                        container = edit_grid) 
  
  edit_grid[3,2] <- glabel(text="Slim a dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_slim_btn, handler = function(h, ...) {

      # Open GUI.
      slim_gui(env=.strvalidator_dataset)
      
  } )
  
  # FILTER --------------------------------------------------------------------
  
  edit_grid[4,1] <- edit_filter_btn <- gbutton(text="Filter",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[4,2] <- glabel(text="Filter a dataset using a reference set.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_filter_btn, handler = function(h, ...) {
    
    filterProfile_gui(env=.strvalidator_dataset)
    
  } )

  # GUESS ---------------------------------------------------------------------
  
  edit_grid[5,1] <- edit_guess_btn <- gbutton(text="Guess",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[5,2] <- glabel(text="Guess the profile from a dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_guess_btn, handler = function(h, ...) {
    
    guessProfile_gui(env=.strvalidator_dataset)
    
  } )

  # ADD DYE -------------------------------------------------------------------
  
  edit_grid[6,1] <- edit_addDye_btn <- gbutton(text="Add Dye",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[6,2] <- glabel(text="Add dye according to kit.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_addDye_btn, handler = function(h, ...) {
    
    # Open GUI.
    addDye_gui(env=.strvalidator_dataset)
    
  } )
  
  # ADD DATA -------------------------------------------------------------------
  
  edit_grid[7,1] <- edit_addData_btn <- gbutton(text="Add Data",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[7,2] <- glabel(text="Add new information to a dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_addData_btn, handler = function(h, ...) {
    
    # Open GUI.
    addData_gui(env=.strvalidator_dataset)
    
  } )

  # CHECK SUBSET --------------------------------------------------------------
  
  edit_grid[8,1] <- edit_check_btn <- gbutton(text="Check",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[8,2] <- glabel(text="Check the subsetting of a dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_check_btn, handler = function(h, ...) {
    
    # Open GUI.
    checkSubset_gui(env=.strvalidator_dataset)
    
  } )

  # CONCATENATE --------------------------------------------------------------
  
  edit_grid[9,1] <- edit_conc_btn <- gbutton(text="Concatenate",
                                              border=TRUE,
                                              container = edit_grid) 
  
  edit_grid[9,2] <- glabel(text="Concatenates two dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_conc_btn, handler = function(h, ...) {
    
    # Open GUI.
    concatenate_gui(env=.strvalidator_dataset)
    
  } )
  
  # CALCULATE HETEROZYGOUS ----------------------------------------------------
  
  edit_grid[10,1] <- edit_het_btn <- gbutton(text="Heterozygous",
                                           border=TRUE,
                                           container = edit_grid) 
  
  edit_grid[10,2] <- glabel(text="Indicate heterozygous loci.",
                            container=edit_grid,
                            anchor=c(-1 ,0))
  
  addHandlerChanged(edit_het_btn, handler = function(h, ...) {
    
    # Open GUI.
    calculateHeterozygous_gui(env=.strvalidator_dataset)
    
  } )

  # CALCULATE H ---------------------------------------------------------------
  
  edit_grid[11,1] <- edit_h_btn <- gbutton(text="Calculate H",
                                             border=TRUE,
                                             container = edit_grid) 
  
  edit_grid[11,2] <- glabel(text="Calculate the average peak height per sample.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_h_btn, handler = function(h, ...) {
    
    # Open GUI.
    calculateH_gui(env=.strvalidator_dataset)
    
  } )
  
  # STUTTER  ##################################################################
  
  
  glabel("", container=stutter_tab) # Adds some space.
  
  stutter_grid <- glayout(container = stutter_tab)
    
  
  # VIEW/EDIT -----------------------------------------------------------------
  
  stutter_grid[1,1] <- stutter_view_btn <- gbutton(text="View",
                                                   border=TRUE,
                                                   container = stutter_grid) 
  
  stutter_grid[1,2] <- glabel(text="View or edit a data frame.",
                              container=stutter_grid,
                              anchor=c(-1 ,0))
  
  addHandlerChanged(stutter_view_btn, handler = function(h, ...) {
    
    # Open GUI.
    editData_gui(env=.strvalidator_dataset)
    
  } )

  # CHECK SUBSET --------------------------------------------------------------

  stutter_grid[2,1] <- stutter_subset_btn <- gbutton(text="Check",
                                                   border=TRUE,
                                                   container = stutter_grid) 
  
  stutter_grid[2,2] <- glabel(text="Check subbsetting of the dataset.",
                              container=stutter_grid,
                              anchor=c(-1 ,0))
  
  addHandlerChanged(stutter_subset_btn, handler = function(h, ...) {
    
    # Open GUI.
    checkSubset_gui(env=.strvalidator_dataset)
    
  } )
 
  
  # CALCULATE -----------------------------------------------------------------
  
  stutter_grid[3,1] <- stutter_calculate_btn <- gbutton(text="Calculate",
                                        border=TRUE,
                                        container = stutter_grid) 
  
  stutter_grid[3,2] <- glabel(text="Calculate stutters for a dataset.",
                              container=stutter_grid,
                              anchor=c(-1 ,0))
  

  addHandlerChanged(stutter_calculate_btn, handler = function(h, ...) {

      # Open GUI.
      calculateStutter_gui(env=.strvalidator_dataset)
      
  } )

  # PLOT STUTTER --------------------------------------------------------------
  
  stutter_grid[4,1] <- stutter_plot_btn <- gbutton(text="Plot",
                                                    border=TRUE,
                                                    container = stutter_grid) 
  
  stutter_grid[4,2] <- glabel(text="Create plots for analysed data",
                               container=stutter_grid)
   
  addHandlerChanged(stutter_plot_btn, handler = function(h, ...) {
    
      # Call function.
      plotStutter_gui(env=.strvalidator_dataset)
      
  } )
  
  
  # SUMMARY TABLE -------------------------------------------------------------
  
  stutter_grid[5,1] <- stutter_table_btn <- gbutton(text="Summarize in table",
                                                        border=TRUE,
                                                        container = stutter_grid) 
  
  stutter_grid[5,2] <- stutter_table_opt <- gradio(items=c("global","locus","stutter"),
                                                    selected = 3,
                                                    horizontal = FALSE,
                                                    container = stutter_grid)
  
  # TODO: Implement
  enabled(stutter_table_btn) <- FALSE
  
  addHandlerChanged(stutter_table_btn, handler = function(h, ...) {
    
    val_opt <- svalue(stutter_table_opt)
    
    
  } )

  
  # BALANCE  ##################################################################

#   # TODO ## Regression Analysis and construction of Hb bins...
#   
#   #low<-seq(200,19999, by=200)
#   #high<-seq(299,20000, by=200)
#   #res <- vector()
#   #for(i in seq(along=low)){
#   #  res[i]<-quantile(plotData[plotData$MpH>=low[i] & plotData$MpH<high[i],]$Hb, 0.05, na.rm=TRUE)
#   #}
#   #xavg<-low+high/2
#   #plot(x=log(xavg), y=res)
#   #summary(lm(res ~ xavg))
#   #abline(lm(res ~ xavg))
#   

  glabel("", container=balance_tab) # Adds some space.
  
  balance_grid <- glayout(container = balance_tab)
  
  # VIEW/EDIT -----------------------------------------------------------------
  
  balance_grid[1,1] <- balance_view_btn <- gbutton(text="View",
                                                   border=TRUE,
                                                   container = balance_grid) 
  
  balance_grid[1,2] <- glabel(text="View or edit a data frame.",
                              container=balance_grid,
                              anchor=c(-1 ,0))
  
  addHandlerChanged(balance_view_btn, handler = function(h, ...) {
    
    # Open GUI.
    editData_gui(env=.strvalidator_dataset)
    
  } )

  # CALCULATE -----------------------------------------------------------------
  
  balance_grid[2,1] <- balance_calculate_btn <- gbutton(text="Calculate",
                                                        border=TRUE,
                                                        container = balance_grid) 
  
  balance_grid[2,2] <- glabel(text="Calculate intra/inter locus balance for a dataset.",
                              container=balance_grid,
                              anchor=c(-1 ,0))
  
  
  addHandlerChanged(balance_calculate_btn, handler = function(h, ...) {
    
      # Open GUI.
      calculateBalance_gui(env=.strvalidator_dataset)
      
  } )
  
  # PLOT BALANCE --------------------------------------------------------------
  
  balance_grid[3,1] <- balance_plot_btn <- gbutton(text="Plot",
                                                   border=TRUE,
                                                   container = balance_grid) 
  
  balance_grid[3,2] <- glabel(text="Create plots for analysed data",
                              container=balance_grid)
  
  addHandlerChanged(balance_plot_btn, handler = function(h, ...) {
    
    # Call function.
    plotBalance_gui(env=.strvalidator_dataset)
    
  } )
  
  
  # SUMMARY TABLE -------------------------------------------------------------

  
  # DROPOUT  ##################################################################
  
  glabel("", container=drop_tab) # Adds some space.
  
  drop_grid <- glayout(container = drop_tab)
  
  # VIEW/EDIT -----------------------------------------------------------------
  
  drop_grid[1,1] <- drop_view_btn <- gbutton(text="View",
                                             border=TRUE,
                                             container = drop_grid) 
  
  drop_grid[1,2] <- glabel(text="View or edit a data frame.",
                           container=drop_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(drop_view_btn, handler = function(h, ...) {
    
    # Open GUI.
    editData_gui(env=.strvalidator_dataset)
    
  } )

  # CALCULATE -----------------------------------------------------------------
  
  drop_grid[2,1] <- drop_calculate_btn <- gbutton(text="Calculate",
                                                        border=TRUE,
                                                        container = drop_grid) 
  
  drop_grid[2,2] <- glabel(text="Calculate dropouts for a dataset.",
                              container=drop_grid,
                              anchor=c(-1 ,0))
  
  
  addHandlerChanged(drop_calculate_btn, handler = function(h, ...) {
    
    # Open GUI.
    calculateDropout_gui(env=.strvalidator_dataset)
    
  } )
  
  # LOGISTIC REGRESSION -------------------------------------------------------
  
  drop_grid[3,1] <- drop_model_btn <- gbutton(text="Model",
                                             border=TRUE,
                                             container = drop_grid) 
  
  drop_grid[3,2] <- glabel(text="Model dropout risk",
                           container=drop_grid)
  
  addHandlerChanged(drop_model_btn, handler = function(h, ...) {
    
    # Call function.
    modelDropout_gui(env=.strvalidator_dataset)
    
  } )
  
  # PLOT DROPOUT --------------------------------------------------------------
  
  drop_grid[4,1] <- drop_plot_btn <- gbutton(text="Plot",
                                                   border=TRUE,
                                                   container = drop_grid) 
  
  drop_grid[4,2] <- glabel(text="Create plots for analysed data",
                              container=drop_grid)
  
  addHandlerChanged(drop_plot_btn, handler = function(h, ...) {
    
    # Call function.
    plotDropout_gui(env=.strvalidator_dataset)
    
  } )
  
  
  # SUMMARY TABLE -------------------------------------------------------------
  
  
  
# MAIN EVENT HANDLERS #########################################################
  addHandlerChanged(nb, handler = function (h, ...) {

    if(debug){
      print("NOTEBOOK CHANGED")
      print(if(is.null(h$pageno)) svalue(h$obj) else h$pageno)
    }

    # Refresh depending on active tab.
    #tab <- svalue(nb)
    tab <- if(is.null(h$pageno)) svalue(h$obj) else h$pageno
    tabName <- names(nb)[tab]

    if(tabName == .file_tab_name){
      
      .refreshLoaded()
      .refreshWs()
      
    }
    
  })
  
  addHandlerFocus(w, handler = function (h, ...) {

    if(debug){
      print(paste("IN:", match.call()[[1]]))
      print("FOCUS")
    }

    # Refresh depending on active tab.
    tab <- svalue(nb)
    tabName <- names(nb)[tab]
    
    if(tabName == .file_tab_name){
      
      .refreshLoaded()
      .refreshWs()
      
    }
      
  })

  .refreshWs <- function(){
      
    # Get data frames in global workspace.
    dfs <- listObjects(env=.GlobalEnv, objClass="data.frame")
    
    if(!is.null(dfs)){
      
      blockHandler(file_ws_drp)
      
      # Populate drop list.
      file_ws_drp[] <- c("<Select dataframe>", dfs)
      
      # Select first item.
      svalue(file_ws_drp, index=TRUE) <- 1 
      
      unblockHandler(file_ws_drp)
      
    }
  }
  
  .refreshLoaded <- function(){
  
    if(debug){
      print(paste("IN:", match.call()[[1]]))
    }
    
    # Get data frames.
    dfs <- listObjects(env=.strvalidator_dataset, objClass="data.frame")
    
    if(!is.null(dfs)){
      
      #blockHandler(file_loaded_tbl)
      
	  #delete(file_loaded_f, file_loaded_tbl)
	  #file_loaded_tbl <<- gtable(items=dfs, 
    #                        multiple = TRUE,
    #                        container = file_loaded_f) 
	  
      # Populate table.
      file_loaded_tbl[] <- dfs
      
      #unblockHandler(file_loaded_tbl)
      
    }
  }
  
  # Show GUI and first tab.
  svalue(nb)<-1
  visible(w)<-TRUE
  
} # END OF GUI
