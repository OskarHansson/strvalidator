################################################################################
# TODO LIST
# TODO: Update saved object in dataset-env
# TODO: Require gWidgetsRGtk2 ??
# TODO: Save .importPath in ws for last used path (only in coming gWidgets2 ??)
# TODO: STUTTER TAB:  button, copy to clipboard.
# TODO: Multiple selection not working.

# NOTE:
# \u00B5 is the unicode for µ 
# Access a file: system.file('doc', 'example', package = 'mypackage')
# NOTE: Can't import data frame named 'drop'
# NOTE: Buttons named 'Plot' will show up 'plot'.
# NOTE: Some button names will change due to locale.

################################################################################
# CHANGE LOG
# 11.07.2013: Removed section 'export', new function export_gui().
# 11.07.2013: Removed section 'load .RData', renamed button load ws -> load.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 10.06.2013: Added a 'save gui' option.
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
  .strvalidator_env <- new.env()
  .separator <- .Platform$file.sep # Platform dependent path separator.
  .save_gui <- TRUE
  .start_tab_name <- "Welcome"
  .file_tab_name <- "Workspace"
  .drylab_tab_name <- "DryLab"
  .edit_tab_name <- "Edit"
  .stutter_tab_name <- "Stutter"
  .balance_tab_name <- "Balance"
  .drop_tab_name <- "Dropout"
  .object_classes_view <- c("data.frame", "ggplot")
  .object_classes_import <- c("data.frame")
  
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
  
  about_txt <- paste("STR validator is a package intended for validation of ",
                     "forensic short tandem repeat (STR)  DNA typing kit. ",
                     "This graphical user interface make it very easy to ",
                     "analyse data from internal validations. ",
                     "The code has been extensively tested in order to assure correct results. ",
                     "However, some bugs might still persist, so check the result carefully.\n\n",
                     "Created by:\n",
                     "Oskar Hansson, Department of Forensic Biology (NIPH, Norway)\n\n",
                     "Please report bugs to:\n",
                     "https://github.com/OskarHansson/strvalidator/issues\n\n",
                     "The source is hosted at GitHub:\n",
                     "https://github.com/OskarHansson/strvalidator", sep="")
  
  gtext(text=about_txt, width = NULL, height = NULL, font.attr = NULL, 
        wrap = TRUE, expand=TRUE, container = start_tab) 
  
  start_license_btn <- gbutton(text = "License", container = start_tab, expand=FALSE) 

  addHandlerChanged(start_license_btn, handler = function (h, ...) {
    
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

    gmessage(message = license_txt,
             title="License",
             icon = "info",
             parent = w) 
    
    
  } )
  
  # FILE ######################################################################
  
  if(debug){
    print("TAB: FILE")
  }
  
  
  # LOADED DATASETS -----------------------------------------------------------
  
  if(debug){
    print("LOADED DATASETS")
  }
  
  file_loaded_f <- gframe(text = "Project",
                          markup = FALSE,
                          pos = 0,
                          horizontal=TRUE,
                          container = file_tab,
                          expand=TRUE)

  file_loaded_f1 <- ggroup(horizontal=FALSE,
                          container = file_loaded_f,
                          expand=FALSE)
  
  file_loaded_ws_btn <- gbutton(text="Open",
                                    border=TRUE,
                                    container = file_loaded_f1)
  
  file_loaded_ws_save_btn <- gbutton(text="Save",
                                     border=TRUE,
                                     container = file_loaded_f1)
  
  file_loaded_import_btn <- gbutton(text="Import",
                                     border=TRUE,
                                     container = file_loaded_f1)
  
  file_loaded_export_btn <- gbutton(text="Export",
                                    border=TRUE,
                                    container = file_loaded_f1)
  
  file_loaded_refresh_btn <- gbutton(text="Refresh",
                                     border=TRUE,
                                     container = file_loaded_f1) 
  
  file_loaded_rm_btn <- gbutton(text="Remove",
                                 border=TRUE,
                                 container = file_loaded_f1) 

  file_loaded_rename_btn <- gbutton(text="Rename",
                                     border=TRUE,
                                     container = file_loaded_f1)
  
  file_loaded_savegui_chk <- gcheckbox(text="Save GUI settings",
                                             checked=.save_gui,
                                             container=file_loaded_f1)
  
  
  file_loaded_tbl <- gtable(items=listObjects(env=.strvalidator_env,
                                              objClass=.object_classes_view), 
                            multiple = TRUE,
                            expand = TRUE,
                            container = file_loaded_f) 

  
  addHandlerChanged(file_loaded_savegui_chk, handler = function (h, ...) {
    
    .save_gui <<- svalue(file_loaded_savegui_chk)

    if (debug){
      print("In save GUI handler:")
      print(.save_gui)
    }
    
  } )

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
             get(object, envir = .strvalidator_env)
             , envir = .strvalidator_env)
      
      remove(list=object, envir=.strvalidator_env)
      
      .refreshLoaded()
      
      
    } else {
      gmessage(message="You can only rename one object at a time!",
               title="Error",
               icon = "error",
               parent = w) 
    }
    
  } )

  addHandlerChanged(file_loaded_ws_btn, handler = function (h, ...) {
    
    
    ws_path <- gfile(text="Select a saved workspace or dataset", type="open",
                     filter = list("R files" = list(patterns = c("*.R","*.Rdata"))),
                     multi=FALSE)
    
    if(!is.na(ws_path)){
      if(file.exists(ws_path)){
        
        load(file=ws_path, envir = .strvalidator_env)
        .refreshLoaded()
        .loadSavedSettings()
        
      } else {
        
        gmessage(message="The workspace file was not found",
                 title="File not found",
                 icon = "error",
                 parent = w) 
      }
    }    
    
  } )
  
  addHandlerChanged(file_loaded_import_btn, handler = function (h, ...) {
    
    import_gui(env=.strvalidator_env, savegui=.save_gui)
    .refreshLoaded()
    
  } )  

  addHandlerChanged(file_loaded_export_btn, handler = function (h, ...) {
    
    export_gui(env=.strvalidator_env, savegui=.save_gui)
    
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
      remove(list=val_obj, envir=.strvalidator_env)
      
      .refreshLoaded()
      
      
    } 
  } )

  addHandlerChanged(file_loaded_ws_save_btn, handler = function (h, ...) {
    
    
    ws_save_path <- gfile(text="Select a directory to save project in",
                          type="selectdir",
                          filter = list("R files" = list(patterns = c("*.R","*.Rdata"))),
                          multi=FALSE)
    
    ws_name <- ginput(message="Input project name",
                      text="",
                      title="Save as",
                      icon ="info",
                      parent=w)
    
    if(!is.na(ws_name) && !ws_name==""){
      
      ws_full_name <- paste(ws_save_path, .separator, ws_name, ".RData", sep="")
      
      if(debug){
        print(ws_full_name)
      }
      
      # TODO: check if file exists. ask for overwrite.
      #if(file.exists(ws_full_name)){
        
      if(file.exists(ws_save_path)){
        
        .saveSettings()
        
        save(file=ws_full_name, 
             list=ls(envir = .strvalidator_env, all.names = TRUE),
             envir = .strvalidator_env)
        
      } else {
        
        gmessage(message="The project directory was not found",
                 title="Directory not found",
                 icon = "error",
                 parent = w) 
      }
    
    } else {
      gmessage(message="A file name must be given",
               title="File name required",
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
                                   listObjects(env=.strvalidator_env,
                                             objClass=.object_classes_import)), 
                           selected = 1,
                           editable = FALSE,
                           container = file_ws_f) 
  
  addHandlerChanged(file_ws_refresh_btn, handler = function (h, ...) {
    
    .refreshWs()
  } )
  
  addHandlerChanged(file_ws_load_btn, handler = function(h, ...) {
    
    # Get selected dataset name.
    val_name <- svalue(file_ws_drp)
    
    if (!is.na(val_name) && !is.null(val_name)){
      
      # Load dataset.
      saveObject(name=val_name, object=get(val_name),
                 parent=w, env=.strvalidator_env)
      
      # Update list.
      .refreshLoaded()
      
    } 
  } )

  
  # STR TYPING KIT ------------------------------------------------------------
  
  if(debug){
    print("STR TYPING KIT")
  }
  
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
  
  edit_grid[1,2] <- glabel(text="View or edit a dataset",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_view_btn, handler = function(h, ...) {
    
    # Open GUI.
    editData_gui(env=.strvalidator_env)
    
  } )

  # TRIM ----------------------------------------------------------------------
  
  edit_grid[2,1] <- edit_trim_btn <- gbutton(text="Trim",
                                             border=TRUE,
                                             container = edit_grid) 
  
  edit_grid[2,2] <- glabel(text="Trim/discard samples or columns from a dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_trim_btn, handler = function(h, ...) {
    
    # Open GUI.
    trim_gui(env=.strvalidator_env, savegui=.save_gui)
    
  } )

  # SLIM ----------------------------------------------------------------------
  
  edit_grid[3,1] <- edit_slim_btn <- gbutton(text="Slim",
                                        border=TRUE,
                                        container = edit_grid) 
  
  edit_grid[3,2] <- glabel(text="Slim a dataset to 'long' format.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_slim_btn, handler = function(h, ...) {

      # Open GUI.
      slim_gui(env=.strvalidator_env, savegui=.save_gui)
      
  } )
  
  # FILTER --------------------------------------------------------------------
  
  edit_grid[4,1] <- edit_filter_btn <- gbutton(text="Filter",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[4,2] <- glabel(text="Filter a dataset using a reference set.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_filter_btn, handler = function(h, ...) {
    
    filterProfile_gui(env=.strvalidator_env, savegui=.save_gui)
    
  } )

  # CROP ----------------------------------------------------------------------
  
  edit_grid[5,1] <- edit_crop_btn <- gbutton(text="Crop",
                                             border=TRUE,
                                             container = edit_grid) 
  
  edit_grid[5,2] <- glabel(text="Crop/discard, or replace data.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_crop_btn, handler = function(h, ...) {
    
    # Open GUI.
    cropData_gui(env=.strvalidator_env, savegui=.save_gui)
    
  } )

  # GUESS ---------------------------------------------------------------------
  
  edit_grid[6,1] <- edit_guess_btn <- gbutton(text="Guess",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[6,2] <- glabel(text="Guess the profile from raw DNA result.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  
  addHandlerChanged(edit_guess_btn, handler = function(h, ...) {
    
    guessProfile_gui(env=.strvalidator_env, savegui=.save_gui)
    
  } )

  # ADD DYE -------------------------------------------------------------------
  
  edit_grid[7,1] <- edit_addDye_btn <- gbutton(text="Add Dye",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[7,2] <- glabel(text="Add dye information according to kit.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_addDye_btn, handler = function(h, ...) {
    
    # Open GUI.
    addDye_gui(env=.strvalidator_env)
    
  } )
  
  # ADD DATA -------------------------------------------------------------------
  
  edit_grid[8,1] <- edit_addData_btn <- gbutton(text="Add Data",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[8,2] <- glabel(text="Add new information to a dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_addData_btn, handler = function(h, ...) {
    
    # Open GUI.
    addData_gui(env=.strvalidator_env, savegui=.save_gui)
    
  } )

  # CHECK SUBSET --------------------------------------------------------------
  
  edit_grid[9,1] <- edit_check_btn <- gbutton(text="Check",
                                               border=TRUE,
                                               container = edit_grid) 
  
  edit_grid[9,2] <- glabel(text="Check the subsetting of a dataset.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_check_btn, handler = function(h, ...) {
    
    # Open GUI.
    checkSubset_gui(env=.strvalidator_env, savegui=.save_gui)
    
  } )

  # CONCATENATE --------------------------------------------------------------
  
  edit_grid[10,1] <- edit_conc_btn <- gbutton(text="Concatenate",
                                              border=TRUE,
                                              container = edit_grid) 
  
  edit_grid[10,2] <- glabel(text="Concatenate two datasets.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_conc_btn, handler = function(h, ...) {
    
    # Open GUI.
    concatenate_gui(env=.strvalidator_env)
    
  } )
  
  # CALCULATE HETEROZYGOUS ----------------------------------------------------
  
  edit_grid[11,1] <- edit_het_btn <- gbutton(text="Heterozygous",
                                           border=TRUE,
                                           container = edit_grid) 
  
  edit_grid[11,2] <- glabel(text="Indicate heterozygous loci.",
                            container=edit_grid,
                            anchor=c(-1 ,0))
  
  addHandlerChanged(edit_het_btn, handler = function(h, ...) {
    
    # Open GUI.
    calculateHeterozygous_gui(env=.strvalidator_env)
    
  } )

  # CALCULATE H ---------------------------------------------------------------
  
  edit_grid[12,1] <- edit_h_btn <- gbutton(text="Calculate H",
                                             border=TRUE,
                                             container = edit_grid) 
  
  edit_grid[12,2] <- glabel(text="Calculate the average peak height per sample.",
                           container=edit_grid,
                           anchor=c(-1 ,0))
  
  addHandlerChanged(edit_h_btn, handler = function(h, ...) {
    
    # Open GUI.
    calculateH_gui(env=.strvalidator_env)
    
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
    editData_gui(env=.strvalidator_env)
    
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
    checkSubset_gui(env=.strvalidator_env)
    
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
      calculateStutter_gui(env=.strvalidator_env, savegui=.save_gui)
      
  } )

  # PLOT STUTTER --------------------------------------------------------------
  
  stutter_grid[4,1] <- stutter_plot_btn <- gbutton(text="Plot",
                                                    border=TRUE,
                                                    container = stutter_grid) 
  
  stutter_grid[4,2] <- glabel(text="Create plots for analysed data.",
                               container=stutter_grid)
   
  addHandlerChanged(stutter_plot_btn, handler = function(h, ...) {
    
      # Call function.
      plotStutter_gui(env=.strvalidator_env, savegui=.save_gui)
      
  } )
  
  
  # SUMMARY TABLE -------------------------------------------------------------
  
  stutter_grid[5,1] <- stutter_table_btn <- gbutton(text="Summarize",
                                                        border=TRUE,
                                                        container = stutter_grid) 
  
  stutter_grid[5,2] <- glabel(text="Summarize stutterdata in a table.",
                              container=stutter_grid)

  addHandlerChanged(stutter_table_btn, handler = function(h, ...) {
    
    val_save <- svalue(file_loaded_savegui_chk)
    
    # Call function.
    tableStutter_gui(env=.strvalidator_env, savegui=.save_gui)
    
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
    editData_gui(env=.strvalidator_env)
    
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
      calculateBalance_gui(env=.strvalidator_env, savegui=.save_gui)
      
  } )
  
  # PLOT BALANCE --------------------------------------------------------------
  
  balance_grid[3,1] <- balance_plot_btn <- gbutton(text="Plot",
                                                   border=TRUE,
                                                   container = balance_grid) 
  
  balance_grid[3,2] <- glabel(text="Create plots for analysed data",
                              container=balance_grid)
  
  addHandlerChanged(balance_plot_btn, handler = function(h, ...) {
    
    # Call function.
    plotBalance_gui(env=.strvalidator_env, savegui=.save_gui)
    
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
    editData_gui(env=.strvalidator_env)
    
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
    calculateDropout_gui(env=.strvalidator_env, savegui=.save_gui)
    
  } )
  
  # LOGISTIC REGRESSION -------------------------------------------------------
  
  drop_grid[3,1] <- drop_model_btn <- gbutton(text="Model",
                                             border=TRUE,
                                             container = drop_grid) 
  
  drop_grid[3,2] <- glabel(text="Model dropout risk",
                           container=drop_grid)
  
  addHandlerChanged(drop_model_btn, handler = function(h, ...) {
    
    # Call function.
    modelDropout_gui(env=.strvalidator_env, savegui=.save_gui)
    
  } )
  
  # PLOT DROPOUT --------------------------------------------------------------
  
  drop_grid[4,1] <- drop_plot_btn <- gbutton(text="Plot",
                                                   border=TRUE,
                                                   container = drop_grid) 
  
  drop_grid[4,2] <- glabel(text="Create plots for analysed data",
                              container=drop_grid)
  
  addHandlerChanged(drop_plot_btn, handler = function(h, ...) {
    
    # Call function.
    plotDropout_gui(env=.strvalidator_env, savegui=.save_gui)
    
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

  # INTERNAL FUNCTIONS ########################################################
  
  .loadSavedSettings <- function(){
    
    # First load save flag.
    if(exists(".strvalidator_savegui", envir=.strvalidator_env, inherits = FALSE)){
      svalue(file_loaded_savegui_chk) <- get(".strvalidator_savegui", envir=.strvalidator_env)
      # .save_gui <<- svalue(file_loaded_savegui_chk) # Already saved in handler?
    }
    
    # Then load settings if true.
    if(svalue(file_loaded_savegui_chk)){
#       if(exists(".strvalidator...", envir=.strvalidator_env, inherits = FALSE)){
#         svalue(f1g1_ratio_spb) <- get(".strvalidator...", envir=.strvalidator_env)
#       }
    }
    
    if(debug){
      print("Saved settings loaded!")
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(file_loaded_savegui_chk)){
      
      assign(x=".strvalidator_savegui", value=svalue(file_loaded_savegui_chk), envir=.strvalidator_env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_savegui", envir=.strvalidator_env, inherits = FALSE)){
        remove(".strvalidator_savegui", envir = .strvalidator_env)
      }
      
      if(debug){
        print("Settings cleared!")
      }
    }
    
    if(debug){
      print("Settings saved!")
    }
    
  }
  
  .refreshWs <- function(){
      
    # Get data frames in global workspace.
    dfs <- listObjects(env=.GlobalEnv, objClass=.object_classes_import)
    
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
    dfs <- listObjects(env=.strvalidator_env, objClass=.object_classes_view)
    
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
