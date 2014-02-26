################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 07.02.2014: Removed redundant handler for 'f1_column_drp'.
# 30.11.2013: Fixed info when factors.
# 27.09.2013: Added option to specify data type and warning for dropout dataset.
# 26.09.2013: Fixed NA rows in resulting data frame.
# 16.09.2013: Changed 'edit' to 'combobox' populated with unique values.
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
      
      # Check for dropout dataset and warn.
      if("Dropout" %in% names(.gData)){
        message <- paste("Do not make subsets of a drop-out dataset before modelling!",
                         "1) Make a subset from the original data.",
                         "2) Run calculate dropout on that subset.",
                         "3) Model drop-out from the new drop-out dataset.", sep="\n  ")
        gmessage(message, title="Warning!",
                 icon = "warning", parent = w) 
      }
      
      
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

  f1 <- gframe(text = "Column",
               horizontal=TRUE,
               spacing = 15,
               container = gv) 
  
  glabel(text="Select target column:", container=f1)
  
  f1_column_drp <- gdroplist(items="<Select column>", 
                                      selected = 1,
                                      editable = FALSE,
                                      container = f1)
  
  glabel(text=" Info:", container=f1)
  f1_min_lbl <- glabel(text=" Min:", container=f1)
  f1_max_lbl <- glabel(text=" Max:", container=f1)
  f1_na_chk <- gcheckbox(text="Exclude NA", checked=FALSE, container=f1)
  
  addHandlerChanged(f1_na_chk, handler = function (h, ...) {

    .refresh_info()
    
  } )  

  addHandlerChanged(f1_column_drp, handler = function (h, ...) {
      
    val_column <- svalue(f1_column_drp)
    
    # Detect data type.
    if(!is.null(.gData) & !is.null(val_column)){
      # Check that a (existing) column is selected.
      if(val_column %in% names(.gData)){
        if(is.numeric(.gData[ , val_column])){
          svalue(f2_type_opt, index=TRUE) <- 1
        } else if(is.character(.gData[ , val_column])){
          svalue(f2_type_opt, index=TRUE) <- 2
        } else {
          if(debug){
            print("Selected column is not 'numeric' and not 'character'")
          }
        }
      }
    }
  
  } )  
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  f2_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f2)
  
  glabel(text="Action:", visible=FALSE, anchor=c(-1, -1), container=f2)
  
  f2g1 <- glayout(container = f2, spacing = 5)
  
  f2g1[1,1] <- f2g1_task_opt <- gradio(items=c("Crop/Discard values", "Replace values"), 
                                            selected = 1,
                                            container = f2g1)

  f2_items <- c("above", "above or equal to",
                "below", "below or equal to",
                "equal to", "not equal to",
                "is NA", "is not NA")
  
  f2g1[1,2] <- f2g1_operator_drp <- gdroplist(items=f2_items, container=f2g1)

  # Note: <Target> is only to create width of the widget.
  #       It will be replaced when a target column is selected.
  f2g1[1,3] <- f2g1_target_cbo <- gcombobox(items="<Target value>",
                                            selected = 1,
                                            editable = TRUE,
                                            container = f2g1)
  
  f2g1[1,4] <- f2g1_new_lbl <- glabel(text="with",
                                      visible=FALSE,
                                      anchor=c(0, -1),
                                      container=f2g1)

  f2g1[1,5] <- f2g1_new_edt <- gedit(text="", 
                                     visible=FALSE,
                                     width=15,
                                     container=f2g1)
  
  addHandlerChanged(f2g1_task_opt, handler = function (h, ...) {
    
    .refresh_options()
    
  } )  
  
  addHandlerChanged(f2g1_operator_drp, handler = function (h, ...) {
    
    .refresh_options()
    
  } )  
  
  glabel(text="Target column contain data of type:",
         visible=FALSE, anchor=c(-1 , -1), container=f2)
  
  f2_type_opt <- gradio(items=c("Numeric", "Character"),
                        horizontal = FALSE,
                        selected = 1,
                        container = f2)
  
  
  # BUTTON ####################################################################
  
  apply_btn <- gbutton(text="Apply",
                       border=TRUE,
                       container = gv)
  
  
  addHandlerChanged(apply_btn, handler = function(h, ...) {
    
    val_column <- svalue(f1_column_drp)
    val_task <- svalue(f2g1_task_opt, index=TRUE)
    val_operator <- svalue(f2g1_operator_drp, index=TRUE)
    val_target <- svalue(f2g1_target_cbo)
    val_new <- svalue(f2g1_new_edt)
    val_type <- svalue(f2_type_opt, index=TRUE)
    
    # If second round, get name from save box.
    if(svalue(dataset_drp, index=TRUE) == 1){
      .gDataName <<- svalue(f3_save_edt)      
    }

    # Check data type and convert.
    if(val_type == 1){  # Numeric.
      val_target <- as.numeric(val_target)
      val_new <- as.numeric(val_new)
      if(!is.numeric(.gData[ , val_column])){
        .gData[ , val_column] <<- as.numeric(.gData[ , val_column])
        warning("Target column not numeric. Data converted!")
      }
    } else if(val_type == 2){  # Character.
      val_target <- as.character(val_target) # Not needed, edit box always character.
      val_new <- as.character(val_new) # Not needed, edit box always character.
      if(!is.character(.gData[ , val_column])){
        .gData[ , val_column] <<- as.character(.gData[ , val_column])
        warning("Target column not character. Data converted!")
      }
    } else {
      warning(paste("Unsupported data type selected!", val_type))
    }

    if(debug){
      print("val_column")
      print(val_column)
      print("val_task")
      print(val_task)
      print("val_operator")
      print(val_operator)
      print("val_target")
      print(val_target)
      print("val_new")
      print(val_new)
      print("val_type")
      print(val_type)
    }
    
    # Change button.
    svalue(apply_btn) <- "Processing..."
    enabled(apply_btn) <- FALSE
    svalue(f3_save_btn) <- "Save"
    
    if(!is.null(.gData) && !is.null(.gData)){

      if(debug){
        print(".gData dim, str, head, tail:")
        print(dim(.gData))
        print(str(.gData))
        print(head(.gData))
        print(tail(.gData))
      }
      
      if(val_operator == 1){  # above
        
        # Remove all rows with NA in target column.
        .gData <<- .gData[!is.na(.gData[val_column]), ]
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] > val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] > val_target] <<- val_new
        }
        
      } else if (val_operator == 2){  # above or equal to

        # Remove all rows with NA in target column.
        .gData <<- .gData[!is.na(.gData[val_column]), ]

        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] >= val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] >= val_target] <<- val_new
        }
        
      } else if (val_operator == 3){  # below
      
        # Remove all rows with NA in target column.
        .gData <<- .gData[!is.na(.gData[val_column]), ]

        if(debug){
          print(".gData dim, str, head, tail:")
          print(dim(.gData))
          print(str(.gData))
          print(head(.gData))
          print(tail(.gData))
        }
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] < val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] < val_target] <<- val_new
        }
      
      } else if (val_operator == 4){  # below or equal to
        
        # Remove all rows with NA in target column.
        .gData <<- .gData[!is.na(.gData[val_column]), ]
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] <= val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] <= val_target] <<- val_new
        }
        
      } else if (val_operator == 5){  # equal to
        
        # Remove all rows with NA in target column.
        .gData <<- .gData[!is.na(.gData[val_column]), ]

        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] == val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] == val_target] <<- val_new
        }
        
      } else if (val_operator == 6){  # not equal to
        
        # Remove all rows with NA in target column.
        .gData <<- .gData[!is.na(.gData[val_column]), ]

        if(val_task == 1){  # crop
          .gData <<- .gData[!.gData[val_column] != val_target, ]
        } else {  # replace
          .gData[val_column][.gData[val_column] != val_target] <<- val_new
        }
        
      } else if (val_operator == 7){  # is NA
        
        if(val_task == 1){  # crop
          .gData <<- .gData[!is.na(.gData[val_column]), ]
        } else {  # replace
          .gData[val_column][is.na(.gData[val_column])] <<- val_new
        }
        
      } else if (val_operator == 8){  # is not NA
        
        if(val_task == 1){  # crop
          .gData <<- .gData[is.na(.gData[val_column]), ]
        } else {  # replace
          .gData[val_column][!is.na(.gData[val_column])] <<- val_new
        }
        
      }
      
      if(debug){
        print(".gData dim, str, head, tail:")
        print(dim(.gData))
        print(str(.gData))
        print(head(.gData))
        print(tail(.gData))
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
      print(val_name)
      print(".gData dim, str, head, tail:")
      print(dim(.gData))
      print(str(.gData))
      print(head(.gData))
      print(tail(.gData))
    }
    
    # Save data.
    saveObject(name=val_name, object=datanew, parent=w, env=env)
    svalue(f3_save_btn) <- "Saved!"
    
  } )
  
  # INTERNAL FUNCTIONS ########################################################
  
  .refresh_column_drp <- function(){
    
    if(debug){
      print("Refresh column dropdown")
    }
    
    # Get data frames in global workspace.
    dfs <- names(.gData)

    if(!is.null(dfs)){
      
      blockHandler(f1_column_drp)
      
      # Populate drop list.
      f1_column_drp[] <- c("<Select column>", dfs)
      
      unblockHandler(f1_column_drp)
      
    }
    
    if(debug){
      print("Column dropdown refreshed!")
    }
  }
  
  .refresh_info <- function(){

    if(debug){
      print("Refresh info")
    }
    
    val_col <- svalue(f1_column_drp)
    val_na <- svalue(f1_na_chk)
    
    if(length(val_col)!=0){
      if(val_col %in% names(.gData)){
        
        # Update info:
        if(is.factor(.gData[,val_col])){
          svalue(f1_min_lbl) <- paste(" Min:",
                                      min(as.character(.gData[,val_col]),
                                                   na.rm=val_na))
          svalue(f1_max_lbl) <- paste(" Max:",
                                      max(as.character(.gData[,val_col]),
                                                   na.rm=val_na))
        } else {
          svalue(f1_min_lbl) <- paste(" Min:",
                                      min(.gData[,val_col], na.rm=val_na))
          svalue(f1_max_lbl) <- paste(" Max:", 
                                      max(.gData[,val_col], na.rm=val_na))
        }
        
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

    if(debug){
      print("Info refreshed!")
    }
    
  }

  .refresh_options <- function(){
    
    if(debug){
      print("Refresh options")
    }
    
    val_col <- svalue(f2g1_task_opt, index=TRUE)
    val_drp <- svalue(f2g1_operator_drp, index=TRUE)
    
    # Update target combo with column content.
    if(!is.null(.gData)){
      selectedColumn <- svalue(f1_column_drp)
      if(length(selectedColumn)!=0){
        if(selectedColumn %in% names(.gData)){
          f2g1_target_cbo[,] <- unique(.gData[selectedColumn])
        }
      }
    }
        
    # Crop.
    if(val_col == 1){
      
      enabled(f2g1_new_lbl) <- FALSE
      enabled(f2g1_new_edt) <- FALSE
      
      # NA
      if(val_drp == 7 || val_drp == 8){
        svalue(f2g1_target_cbo) <- NA
        enabled(f2g1_target_cbo) <- FALSE
        svalue(f2g1_new_edt) <- NA
      } else {
        enabled(f2g1_target_cbo) <- TRUE
      }
      
    } else {
        
      enabled(f2g1_new_lbl) <- TRUE
      enabled(f2g1_new_edt) <- TRUE
        
      # NA
      if(val_drp == 7 || val_drp == 8){
        svalue(f2g1_target_cbo) <- NA
        enabled(f2g1_target_cbo) <- FALSE
        svalue(f2g1_new_edt) <- NA
      } else {
        enabled(f2g1_target_cbo) <- TRUE
      }
    }
      
    if(debug){
      print("Options refreshed!")
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
      if(exists(".strvalidator_cropData_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f2_savegui_chk) <- get(".strvalidator_cropData_gui_savegui", envir=env)
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
      if(exists(".strvalidator_cropData_gui_na", envir=env, inherits = FALSE)){
        svalue(f1_na_chk) <- get(".strvalidator_cropData_gui_na", envir=env)
      }
      if(exists(".strvalidator_cropData_gui_task", envir=env, inherits = FALSE)){
        svalue(f2g1_task_opt) <- get(".strvalidator_cropData_gui_task", envir=env)
      }
      if(exists(".strvalidator_cropData_gui_operator", envir=env, inherits = FALSE)){
        svalue(f2g1_operator_drp) <- get(".strvalidator_cropData_gui_operator", envir=env)
      }
      if(exists(".strvalidator_cropData_gui_new", envir=env, inherits = FALSE)){
        svalue(f2g1_new_edt) <- get(".strvalidator_cropData_gui_new", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f2_savegui_chk)){
      
      assign(x=".strvalidator_cropData_gui_savegui", value=svalue(f2_savegui_chk), envir=env)
      assign(x=".strvalidator_cropData_gui_na", value=svalue(f1_na_chk), envir=env)
      assign(x=".strvalidator_cropData_gui_task", value=svalue(f2g1_task_opt), envir=env)
      assign(x=".strvalidator_cropData_gui_operator", value=svalue(f2g1_operator_drp), envir=env)
      assign(x=".strvalidator_cropData_gui_new", value=svalue(f2g1_new_edt), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_cropData_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_cropData_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_cropData_gui_na", envir=env, inherits = FALSE)){
        remove(".strvalidator_cropData_gui_na", envir = env)
      }
      if(exists(".strvalidator_cropData_gui_task", envir=env, inherits = FALSE)){
        remove(".strvalidator_cropData_gui_task", envir = env)
      }
      if(exists(".strvalidator_cropData_gui_operator", envir=env, inherits = FALSE)){
        remove(".strvalidator_cropData_gui_operator", envir = env)
      }
      if(exists(".strvalidator_cropData_gui_new", envir=env, inherits = FALSE)){
        remove(".strvalidator_cropData_gui_new", envir = env)
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
