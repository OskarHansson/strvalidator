################################################################################
# TODO LIST
# TODO: .

################################################################################
# CHANGE LOG
# 06.08.2013: Added rows and columns to info.
# 18.07.2013: Check before overwrite object.
# 16.07.2013: Added save GUI settings.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 06.06.2013: Set initial table height to 200.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Suggestions for columns to fix/stack is provided.
# 24.05.2013: Improved error message for missing columns.
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: .result removed, added save as group.
# 25.04.2013: Add selection of dataset in gui. Removed parameter 'data'.
# 25.04.2013: New parameter 'debug'.
# <25.04.2013: First version.

#' @title Slim data frames
#'
#' @description
#' \code{slim_gui} is a GUI wrapper for the \code{slim} function.
#'
#' @details
#' Simplifies the use of the \code{slim} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

slim_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  # Global variables.
  .gData <- data.frame(No.Data=NA)
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.  
  w <- gwindow(title="Slim dataset", visible=FALSE)
  
  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })

  # Vertical main group.
  gv <- ggroup(horizontal=FALSE,
               spacing=5,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 

  # Vertical sub group.
  g0 <- ggroup(horizontal=FALSE,
              spacing=5,
              use.scrollwindow=FALSE,
              container = gv,
              expand=FALSE) 

  # Horizontal sub group.
  g1 <- ggroup(horizontal=TRUE,
              spacing=5,
              use.scrollwindow=FALSE,
              container = gv,
              expand=TRUE) 
  
  # Vertical sub group.
  g2 <- ggroup(horizontal=FALSE,
               spacing=5,
               use.scrollwindow=FALSE,
               container = gv,
               expand=FALSE) 
  
  
  
  # DATASET ###################################################################
  
  frame0 <- gframe(text = "Datasets",
               horizontal=FALSE,
               spacing = 5,
               container = g0) 
  
  g0 <- glayout(container = frame0, spacing = 1)
  
  g0[1,1] <- glabel(text="Select dataset:", container=g0)
  
  g0[1,2] <- dataset_drp <- gdroplist(items=c("<Select dataset>",
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
        svalue(fix_txt) <- ""
        svalue(stack_txt) <- ""
        .refresh_fix_tbl()
        .refresh_stack_tbl()
        svalue(g0_samples_lbl) <- " 0 samples,"
        svalue(g0_columns_lbl) <- " 0 columns,"
        svalue(g0_rows_lbl) <- " 0 rows"
        svalue(f2_save_edt) <- ""
        
      } else {

        # Load or change components.
        .refresh_fix_tbl()
        .refresh_stack_tbl()
        
        samples <- length(unique(.gData$Sample.Name))
        # Info.
        if("Sample.Name" %in% names(.gData)){
          samples <- length(unique(.gData$Sample.Name))
          svalue(g0_samples_lbl) <- paste(" ", samples, "samples,")
        } else {
          svalue(g0_samples_lbl) <- paste(" ", "<NA>", "samples,")
        }
        svalue(g0_columns_lbl) <- paste(" ", ncol(.gData), "columns,")
        svalue(g0_rows_lbl) <- paste(" ", nrow(.gData), "rows")
        # Result name.
        svalue(f2_save_edt) <- paste(val_obj, "_slim", sep="")

        # Guess column names to keep fixed.
        svalue(fix_txt) <- colNames(.gData, slim=TRUE, concatenate="|")
        
        # Guess column names to stack.
        svalue(stack_txt) <- colNames(.gData, slim=FALSE, concatenate="|")
      }
      
    } else {
      
      # Reset components.
      .gData <<- data.frame(No.Data=NA)
      svalue(fix_txt) <- ""
      svalue(stack_txt) <- ""
      .refresh_fix_tbl()
      .refresh_stack_tbl()
      svalue(g0_samples_lbl) <- paste(" ", "<NA>", "samples,")
      svalue(g0_columns_lbl) <- paste(" ", "<NA>", "columns,")
      svalue(g0_rows_lbl) <- paste(" ", "<NA>", "rows")
      
      svalue(f2_save_edt) <- ""
      
    }
  } )
  
  # SAMPLES ###################################################################

  if(debug){
    print("SAMPLES")
    print(unique(.gData$Sample.Name))
  }  
  
  fix_f <- gframe("Fix", horizontal=FALSE, container=g1, expand=TRUE)
  
  fix_lbl <- glabel(text="Columns to keep fixed:",
                    container=fix_f,
                    anchor=c(-1 ,0))
  
  fix_txt <- gedit(initial.msg="Doubleklick or drag column names to list",
                   width = 40,
                   container=fix_f)
  
  fix_tbl <- gtable(items=names(.gData), 
                    container=fix_f,
                    expand=TRUE)
  
  # Set initial size (only height is important here).
  size(fix_tbl) <- c(100,200)
  
  addDropTarget(fix_txt, handler=function(h,...) {
    
    if(debug){
      print("SAMPLES:DROPTARGET")
    } 

    # Get values.
    drp_val <- h$dropdata
    fix_val <- svalue(h$obj)
    
    # Add new value to selected.
    new <- ifelse(nchar(fix_val) > 0,
                  paste(fix_val, drp_val, sep="|"),
                  drp_val)
    
    # Update text box.
    svalue(h$obj) <- new
    
    # Update sample name table.
    tmp_tbl <- fix_tbl[,]  # Get all values.
    print(tmp_tbl)
    tmp_tbl <- tmp_tbl[tmp_tbl!=drp_val]  # Remove value added to selected.
    fix_tbl[,] <- tmp_tbl  # Update table.
    
  })
  
  # COLUMNS ###################################################################

  if(debug){
    print("STACK")
  }  
  
  stack_f <- gframe("Stack", horizontal=FALSE, container=g1, expand=TRUE)
  
  stack_lbl <- glabel(text="Columns to stack:",
                      container=stack_f,
                      anchor=c(-1 ,0))
  
  stack_txt <- gedit(initial.msg="Doubleklick or drag column names to list",
                     width = 40,
                     container=stack_f)
  
  stack_tbl <- gtable(items=names(.gData),
                      container=stack_f,
                      expand=TRUE)
  
  addDropTarget(stack_txt, handler=function(h,...) {
    # Get values.
    drp_val <- h$dropdata
    stack_val <- svalue(h$obj)
    
    # Add new value to selected.
    new <- ifelse(nchar(stack_val) > 0, 
                  paste(stack_val, drp_val, sep="|"),
                  drp_val)
    
    # Update text box.
    svalue(h$obj) <- new
    
    # Update column name table.
    tmp_tbl <- stack_tbl[,]  # Get all values.
    print(tmp_tbl)
    tmp_tbl <- tmp_tbl[tmp_tbl!=drp_val]  # Remove value added to selected.
    stack_tbl[,] <- tmp_tbl  # Update table.
    
  })
  
  # FRAME 1 ###################################################################

  if(debug){
    print("OPTIONS")
  }  
  
  f1 <- gframe("Options", horizontal=FALSE, container=g0)

  f1_savegui_chk <- gcheckbox(text="Save GUI settings",
                              checked=FALSE,
                              container=f1)
  
  f1_keep_chk <- gcheckbox(text="Keep rows in fixed columns even if no data in stacked columns",
                        checked=TRUE,
                        container=f1)
  
  glabel(text=paste("\nTip:",
                    "Manually edit the columns to fix and stack.\n",
                    "e.g. 'Allele' will stack 'Allele.1', 'Allele.2'..."),
         container=f1,
         anchor=c(-1 ,0))
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = g2) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", container=f2)

  # BUTTON ####################################################################

  if(debug){
    print("BUTTON")
  }  
  
  slim_btn <- gbutton(text="Slim dataset",
                      border=TRUE,
                      container=g2)
  
  addHandlerChanged(slim_btn, handler = function(h, ...) {
    
    # Get new dataset name.
    val_name <- svalue(f2_save_edt)
    
    if(nchar(val_name) > 0) {
      
      # Get values.
      fix_val <- svalue(fix_txt)
      stack_val <- svalue(stack_txt)
      keep_val <- svalue(f1_keep_chk)
      
      # Slim require a vector of strings.
      fix_val <- unlist(strsplit(fix_val, "|", fixed = TRUE))
      stack_val <- unlist(strsplit(stack_val, "|", fixed = TRUE))
      
      if(debug){
        print(".gData")
        print(names(.gData))
        print("fix_val")
        print(fix_val)
        print("stack_val")
        print(stack_val)
        print("keep_val")
        print(keep_val)
      }
  
      # Change button.
      svalue(slim_btn) <- "Processing..."
      enabled(slim_btn) <- FALSE
      
      datanew <- slim(data=.gData, fix=fix_val, stack=stack_val,
                      keepAllFixed=keep_val)
      
      # Save data.
      saveObject(name=val_name, object=datanew, parent=w, env=env)
      
      # Close GUI.
      dispose(w)
    
    } else {
      
      gmessage("A file name must be provided!", title="Error",
               icon = "error",
               parent = w) 
    }
    
  } )
  
  # Show GUI.
  visible(w) <- TRUE
  
  .refresh_fix_tbl <- function(){
    
    if(debug){
      print(paste("IN:", match.call()[[1]]))
    }
    
    # Refresh widget by removing it and...
    delete(fix_f, fix_tbl)
    
    # ...creating a new table.
    fix_tbl <<- gtable(items=names(.gData), 
                           container=fix_f,
                           expand=TRUE)
  
    addDropSource(fix_tbl, handler=function(h,...) svalue(h$obj))
  
    addHandlerDoubleclick(fix_tbl, handler = function(h, ...) {
      
      # Get values.
      tbl_val <- svalue (h$obj)
      fix_val <- svalue(fix_txt)
      
      # Add new value to selected.
      new <- ifelse(nchar(fix_val) > 0,
                    paste(fix_val, tbl_val, sep="|"),
                    tbl_val)
      
      # Update text box.
      svalue(fix_txt) <- new
      
      # Update sample name table.
      tmp_tbl <- fix_tbl[,]  # Get all values.
      tmp_tbl <- tmp_tbl[tmp_tbl!=tbl_val]  # Remove value added to selected.
      fix_tbl[,] <- tmp_tbl  # Update table.
      
    } )
  
  }

  .refresh_stack_tbl <- function(){
    
    if(debug){
      print(paste("IN:", match.call()[[1]]))
    }
    
    # Refresh widget by removing it and...
    delete(stack_f, stack_tbl)
    
    # ...creating a new table.
    stack_tbl <<- gtable(items=names(.gData), 
                       container=stack_f,
                       expand=TRUE)
    
    addDropSource(stack_tbl, handler=function(h,...) svalue(h$obj))
    
    addHandlerDoubleclick(stack_tbl, handler = function(h, ...) {
      
      # Get values.
      tbl_val <- svalue (h$obj)
      stack_val <- svalue(stack_txt)
      
      # Add new value to selected.
      new <- ifelse(nchar(stack_val) > 0,
                    paste(stack_val, tbl_val, sep="|"),
                    tbl_val)
      
      # Update text box.
      svalue(stack_txt) <- new
      
      # Update column name table.
      tmp_tbl <- stack_tbl[,]  # Get all values.
      tmp_tbl <- tmp_tbl[tmp_tbl!=tbl_val]  # Remove value added to selected.
      stack_tbl[,] <- tmp_tbl  # Update table.
      
    } )
    
  }
  
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
      if(exists(".slim_gui_savegui", envir=env, inherits = FALSE)){
        svalue(f1_savegui_chk) <- get(".slim_gui_savegui", envir=env)
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
      if(exists(".slim_gui_title", envir=env, inherits = FALSE)){
        svalue(f1_keep_chk) <- get(".slim_gui_title", envir=env)
      }
      
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(f1_savegui_chk)){
      
      assign(x=".slim_gui_savegui", value=svalue(f1_savegui_chk), envir=env)
      assign(x=".slim_gui_title", value=svalue(f1_keep_chk), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".slim_gui_savegui", envir=env, inherits = FALSE)){
        remove(".slim_gui_savegui", envir = env)
      }
      if(exists(".slim_gui_title", envir=env, inherits = FALSE)){
        remove(".slim_gui_title", envir = env)
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
  
} # End of GUI
