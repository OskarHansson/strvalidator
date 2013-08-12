################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 06.08.2013: Added exclude NA.
# 01.08.2013: GUI 'info' updates and bug fixes.
# 18.07.2013: Check before overwrite object.
# 17.07.2013: First version.

#' @title Crop or replace GUI
#'
#' @description
#' \code{cropData_gui} is a GUI simplifying cropping and replacing values in data frames.
#'
#' @details Select a data frame from the dropdown and crop/replace. Optionally
#' save as a new dataframe.
#' @param env environment in wich to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' 

cropData_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title="Crop or replace values in data frames", visible=FALSE)

  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })

  # Vertical main group.
  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # FRAME 0 ###################################################################
  
  f0 <- gframe(text = "Datasets",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  g0 <- glayout(container = f0, spacing = 1)
  
  g0[1,1] <- glabel(text="Select dataset:", container=g0)
  
  g0[1,2] <- dataset_drp <- gdroplist(items=c("<Select data frame>",
                                              listObjects(env=env,
                                                          objClass="data.frame")), 
                                      selected = 1,
                                      editable = FALSE,
                                      container = g0)
  
  g0[1,3] <- g0_samples_lbl <- glabel(text=" 0 samples,", container=g0)
  g0[1,4] <- g0_columns_lbl <- glabel(text=" 0 columns,", container=g0)
  g0[1,5] <- g0_rows_lbl <- glabel(text=" 0 rows", container=g0)
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gData <<- get(val_obj, envir=env)
      
      .gDataName <<- val_obj
      
      
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste(" ", samples, "samples,")
      svalue(g0_columns_lbl) <- paste(" ", ncol(.gData), "columns,")
      svalue(g0_rows_lbl) <- paste(" ", nrow(.gData), "rows")
      .refresh_column_drp()
      
      # Update 'Save as'.
      svalue(f3_save_edt) <- .gDataName
      samples <- length(unique(.gData$Sample.Name))
      svalue(f3_samples_lbl) <- paste(" ", samples, "samples,")
      svalue(f3_columns_lbl) <- paste(" ", ncol(.gData), "columns,")
      svalue(f3_rows_lbl) <- paste(" ", nrow(.gData), "rows")
      
      # Update info:
      svalue(f1_min_lbl) <- " Min:"
      svalue(f1_max_lbl) <- " Max:"
    
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(g0_samples_lbl) <- paste(" ", "<NA>", "samples,")
      svalue(g0_columns_lbl) <- paste(" ", "<NA>", "columns,")
      svalue(g0_rows_lbl) <- paste(" ", "<NA>", "rows")

      # Update info:
      svalue(f1_min_lbl) <- " Min:"
      svalue(f1_max_lbl) <- " Max:"
      
      # Update 'Save as'.
      svalue(f3_save_edt) <- ""
      svalue(f3_samples_lbl) <- paste(" ", "<NA>", "samples,")
      svalue(f3_columns_lbl) <- paste(" ", "<NA>", "columns,")
      svalue(f3_rows_lbl) <- paste(" ", "<NA>", "rows")
      
    }
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Select target",
               horizontal=TRUE,
               spacing = 15,
               container = gv) 
  
  glabel(text="Select column:", container=f1)
  
  f1_column_drp <- gdroplist(items="<Select column>", 
                                      selected = 1,
                                      editable = FALSE,
                                      container = f1)
  
  f1_na_chk <- gcheckbox(text="Exclude NA", checked=FALSE, container=f1)
  f1_min_lbl <- glabel(text=" Min:", container=f1)
  f1_max_lbl <- glabel(text=" Max:", container=f1)
  
  addHandlerChanged(f1_column_drp, handler = function (h, ...) {
    
    .refresh_info()
    .refresh_options()
    
  } )
  
  addHandlerChanged(f1_na_chk, handler = function (h, ...) {

    .refresh_info()
    
  } )  
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 15,
               container = gv) 
  
  f2_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f2)
  
  f2g1 <- glayout(container = f2, spacing = 10)
  
  f2g1[1,1] <- f2g1_task_opt <- gradio(items=c("Crop/Discard values", "Replace values"), 
                                            selected = 1,
                                            container = f2g1)

  f2_items <- c("above", "above or equal to",
                "below", "below or equal to",
                "equal to", "not equal to",
                "is NA", "is not NA")
  
  f2g1[1,3] <- f2g1_operation_drp <- gdroplist(items=f2_items, container=f2g1)
  
  f2g1[1,4] <- f2g1_target_edt <- gedit(text="", width=10, container=f2g1)

  f2g1[1,5] <- f2g1_new_lbl <- glabel(text="with",
                                      visible=FALSE,
                                      anchor=c(0 ,0),
                                      container=f2g1)

  f2g1[1,6] <- f2g1_new_edt <- gedit(text="", 
                                     visible=FALSE,
                                     width=10,
                                     container=f2g1)
  
  addHandlerChanged(f2g1_task_opt, handler = function (h, ...) {
    
    .refresh_options()
    
  } )  
  
  addHandlerChanged(f2g1_operation_drp, handler = function (h, ...) {
    
    .refresh_options()
    
  } )  
  
  # BUTTON ####################################################################
  
  apply_btn <- gbutton(text="Apply",
                       border=TRUE,
                       container = gv)
  
  
  addHandlerChanged(apply_btn, handler = function(h, ...) {
    
    val_column <- svalue(f1_column_drp)
    val_task <- svalue(f2g1_task_opt, index=TRUE)
    val_operation <- svalue(f2g1_operation_drp, index=TRUE)
    val_target <- svalue(f2g1_target_edt)
    val_new <- svalue(f2g1_new_edt)
    
    # If second round, get name from save box.
    if(svalue(dataset_drp, index=TRUE) == 1){
      .gDataName <<- svalue(f3_save_edt)      
    }
    
    # Convert to numerical values.
    if(is.numeric(.gData[ , val_column])){
      val_target <- as.numeric(val_target)
      val_new <- as.numeric(val_new)
    }

    if(debug){
      print("val_column")
      print(val_column)
      print("val_task")
      print(val_task)
      print("val_operation")
      print(val_operation)
      print("val_target")
      print(val_target)
      print("val_new")
      print(val_new)
    }
    
    # Change button.
    svalue(apply_btn) <- "Processing..."
    enabled(apply_btn) <- FALSE
    
    if(!is.null(.gData) && !is.null(.gData)){
      
      if(val_operation == 1){  # above
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] > val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] > val_target] <<- val_new
        }
        
      } else if (val_operation == 2){  # above or equal to

        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] >= val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] >= val_target] <<- val_new
        }
        
      } else if (val_operation == 3){  # below
      
        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] < val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] < val_target] <<- val_new
        }
      
      } else if (val_operation == 4){  # below or equal to
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] <= val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] <= val_target] <<- val_new
        }
        
      } else if (val_operation == 5){  # equal to
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] == val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] == val_target] <<- val_new
        }
        
      } else if (val_operation == 6){  # not equal to
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] != val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] != val_target] <<- val_new
        }
        
      } else if (val_operation == 7){  # is NA
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!is.na(.gData[val_column]), ]
        } else {  # replace
          .gData[val_column][is.na(.gData[val_column])] <<- val_new
        }
        
      } else if (val_operation == 8){  # is not NA
        
        if(val_task == 1){  # crop
          .gData <<- .gData[is.na(.gData[val_column]), ]
        } else {  # replace
          .gData[val_column][!is.na(.gData[val_column])] <<- val_new
        }
        
      }
      
    } else {
      
      gmessage(message="Data frame is NULL or NA!",
               title="Error",
               icon = "error")      
      
    }
    
    # Change button.
    svalue(apply_btn) <- "Apply"
    enabled(apply_btn) <- TRUE

    # Update 'Save as'.
    .refresh_info()
    currentName <- svalue(f3_save_edt)
    if(nchar(currentName) == 0){
      svalue(f3_save_edt) <- paste(.gDataName, val_target, sep="_")
    } else {
      svalue(f3_save_edt) <- paste(currentName, val_target, sep="_")
    }
    
  } )
  
  # FRAME 3 ###################################################################
  
  f3 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f3)
  
  f3_save_edt <- gedit(text="", container=f3)
  
  f3_save_btn <- gbutton(text = "Save",
                         border=TRUE,
                         container = f3) 
  
  f3_samples_lbl <- glabel(text=" 0 samples,", container=f3)
  f3_columns_lbl <- glabel(text=" 0 columns,", container=f3)
  f3_rows_lbl <- glabel(text=" 0 rows", container=f3)

  addHandlerChanged(f3_save_btn, handler = function(h, ...) {
    
    val_name <- svalue(f3_save_edt)
    datanew <- .gData
    
    if(debug){
      print("Save pressed!")
      print(head(.gData))
      print(val_name)
    }
    
    # Save data.
    saveObject(name=val_name, object=datanew, parent=w, env=env)
    
  } )
  
  # INTERNAL FUNCTIONS ########################################################
  
  .refresh_column_drp <- function(){
    
    # Get data frames in global workspace.
    dfs <- names(.gData)

    if(!is.null(dfs)){
      
      blockHandler(f1_column_drp)
      
      # Populate drop list.
      f1_column_drp[] <- c("<Select column>", dfs)
      
      unblockHandler(f1_column_drp)
      
    }
  }
  
  .refresh_info <- function(){
    
    val_col <- svalue(f1_column_drp)
    val_na <- svalue(f1_na_chk)

    if(length(val_col)!=0){
      if(val_col %in% names(.gData)){
        
        # Update info:
        svalue(f1_min_lbl) <- paste(" Min:", min(.gData[,val_col], na.rm=val_na))
        svalue(f1_max_lbl) <- paste(" Max:", max(.gData[,val_col], na.rm=val_na))
        
        # Update 'Save As'
        samples <- length(unique(.gData$Sample.Name))
        svalue(f3_samples_lbl) <- paste(" ", samples, "samples,")
        svalue(f3_columns_lbl) <- paste(" ", ncol(.gData), "columns,")
        svalue(f3_rows_lbl) <- paste(" ", nrow(.gData), "rows")
      
      } else {
        
        # Update info:
        svalue(f1_min_lbl) <- " Min:"
        svalue(f1_max_lbl) <- " Max:"
        
        svalue(f3_samples_lbl) <- paste(" ", "<NA>", "samples,")
        svalue(f3_columns_lbl) <- paste(" ", "<NA>", "columns,")
        svalue(f3_rows_lbl) <- paste(" ", "<NA>", "rows")
      }
    }

  }

  .refresh_options <- function(){
    
    val_col <- svalue(f2g1_task_opt, index=TRUE)
    val_drp <- svalue(f2g1_operation_drp, index=TRUE)
        
    # Crop.
    if(val_col == 1){
      
      enabled(f2g1_new_lbl) <- FALSE
      enabled(f2g1_new_edt) <- FALSE
      
      # NA
      if(val_drp == 7 || val_drp == 8){
        svalue(f2g1_target_edt) <- NA
        enabled(f2g1_target_edt) <- FALSE
        svalue(f2g1_new_edt) <- NA
      } else {
        enabled(f2g1_target_edt) <- TRUE
      }
      
    } else {
        
      enabled(f2g1_new_lbl) <- TRUE
      enabled(f2g1_new_edt) <- TRUE
        
      # NA
      if(val_drp == 7 || val_drp == 8){
        svalue(f2g1_target_edt) <- NA
        enabled(f2g1_target_edt) <- FALSE
        svalue(f2g1_new_edt) <- NA
      } else {
        enabled(f2g1_target_edt) <- TRUE
      }
    }
      
  }
  
  .loadSavedSettings <- function(){
    
    # First check status of save flag.
    if(!is.null(savegui)){
      svalue(f2_savegui_chk) <- savegui
      enabled(f2_savegui_chk) <- FALSE
      if(debug){
        print("Save GUI status set!")
      }  
    } else {
      # Load save flag.
      if(exists(".cropData_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f2_savegui_chk) <- get(".cropData_gui_savegui", envir=env)
      }
      if(debug){
        print("Save GUI status loaded!")
      }  
    }
    if(debug){
      print(svalue(f2_savegui_chk))
    }  
    
    # Then load settings if true.
    if(svalue(f2_savegui_chk)){
      if(exists(".cropData_gui_na", envir=env, inherits = FALSE)){
        svalue(f1_na_chk) <- get(".cropData_gui_na", envir=env)
      }
      if(exists(".cropData_gui_task", envir=env, inherits = FALSE)){
        svalue(f2g1_task_opt) <- get(".cropData_gui_task", envir=env)
      }
      if(exists(".cropData_gui_operation", envir=env, inherits = FALSE)){
        svalue(f2g1_operation_drp) <- get(".cropData_gui_operation", envir=env)
      }
      if(exists(".cropData_gui_target", envir=env, inherits = FALSE)){
        svalue(f2g1_target_edt) <- get(".cropData_gui_target", envir=env)
      }
      if(exists(".cropData_gui_new", envir=env, inherits = FALSE)){
        svalue(f2g1_new_edt) <- get(".cropData_gui_new", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f2_savegui_chk)){
      
      assign(x=".cropData_gui_savegui", value=svalue(f2_savegui_chk), envir=env)
      assign(x=".cropData_gui_na", value=svalue(f1_na_chk), envir=env)
      assign(x=".cropData_gui_task", value=svalue(f2g1_task_opt), envir=env)
      assign(x=".cropData_gui_operation", value=svalue(f2g1_operation_drp), envir=env)
      assign(x=".cropData_gui_target", value=svalue(f2g1_target_edt), envir=env)
      assign(x=".cropData_gui_new", value=svalue(f2g1_new_edt), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".cropData_gui_savegui", envir=env, inherits = FALSE)){
        remove(".cropData_gui_savegui", envir = env)
      }
      if(exists(".cropData_gui_na", envir=env, inherits = FALSE)){
        remove(".cropData_gui_na", envir = env)
      }
      if(exists(".cropData_gui_task", envir=env, inherits = FALSE)){
        remove(".cropData_gui_task", envir = env)
      }
      if(exists(".cropData_gui_operation", envir=env, inherits = FALSE)){
        remove(".cropData_gui_operation", envir = env)
      }
      if(exists(".cropData_gui_target", envir=env, inherits = FALSE)){
        remove(".cropData_gui_target", envir = env)
      }
      if(exists(".cropData_gui_new", envir=env, inherits = FALSE)){
        remove(".cropData_gui_new", envir = env)
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
