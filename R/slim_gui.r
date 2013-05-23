################################################################################
# TODO LIST
# TODO: .

################################################################################
# CHANGE LOG
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
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

slim_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- data.frame(No.Data=NA)
  gDataName <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Slim dataset", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=5,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  g0 <- ggroup(horizontal=FALSE,
              spacing=5,
              use.scrollwindow=FALSE,
              container = gv,
              expand=FALSE) 

  g1 <- ggroup(horizontal=TRUE,
              spacing=5,
              use.scrollwindow=FALSE,
              container = gv,
              expand=TRUE) 
  
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
  
  grid0 <- glayout(container = frame0, spacing = 1)
  
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
    
    if(exists(val_obj, envir=env)){
      
      gData <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Marker")
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gData <<- data.frame(No.Data=NA)
        svalue(fix_txt) <- ""
        svalue(stack_txt) <- ""
        .refresh_fix_tbl()
        .refresh_stack_tbl()

        svalue(dataset_samples_lbl) <- "0 samples"
        svalue(f2_save_edt) <- ""
        
        message <- paste("The dataset is not typing data\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
        
        .refresh_fix_tbl()
        .refresh_stack_tbl()
        
        gDataName <<- val_obj

        samples <- length(unique(gData$Sample.Name))
        svalue(dataset_samples_lbl) <- paste("", samples, "samples")
        svalue(f2_save_edt) <- paste(gDataName, "_slim", sep="")
        
      }
      
    } else {
      
      gData <<- data.frame(No.Data=NA)
      svalue(fix_txt) <- ""
      svalue(stack_txt) <- ""
      .refresh_fix_tbl()
      .refresh_stack_tbl()
      svalue(dataset_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )
  
  # SAMPLES ###################################################################

  if(debug){
    print("SAMPLES")
    print(unique(gData$Sample.Name))
  }  
  
  fix_f <- gframe("Fix", horizontal=FALSE, container=g1, expand=TRUE)
  
  fix_lbl <- glabel(text="Columns to keep fixed:",
                    container=fix_f,
                    anchor=c(-1 ,0))
  
  fix_txt <- gedit(initial.msg="Doubleklick or drag column names to list",
                   width = 40,
                   container=fix_f)
  
  fix_tbl <- gtable(items=names(gData), 
                    container=fix_f,
                    expand=TRUE)
  
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
  
  stack_tbl <- gtable(items=names(gData),
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
  
  # OPTIONS ###################################################################

  if(debug){
    print("OPTIONS")
  }  
  
  option_f <- gframe("Options", horizontal=FALSE, container=g0)
  
  keep_chk <- gcheckbox(text="Keep all rows in fixed columns",
                        checked=TRUE,
                        container=option_f)
  
  tip_lbl <- glabel(text=paste("\nTip:", 
                      "Manually edit the columns to fix and stack.\n",
                      "e.g. 'Allele' will stack 'Allele.1', 'Allele.2'..."),
                      container=option_f,
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
      keep_val <- svalue(keep_chk)
      
      # Slim require a vector of strings.
      fix_val <- unlist(strsplit(fix_val, "|", fixed = TRUE))
      stack_val <- unlist(strsplit(stack_val, "|", fixed = TRUE))
      
      if(debug){
        print("gData")
        print(names(gData))
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
      
      datanew <- slim(data=gData, fix=fix_val, stack=stack_val,
                      keepAllFixed=keep_val)
      
      
      # Save data.
      assign(val_name, datanew, envir=env)
      
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
    fix_tbl <<- gtable(items=names(gData), 
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
    stack_tbl <<- gtable(items=names(gData), 
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
  
} # End of GUI
