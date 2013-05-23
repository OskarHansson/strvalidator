################################################################################
# TODO LIST
# TODO: ...

# NB! Can't handle Sample.Names as factors?
################################################################################
# CHANGE LOG
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: .result removed, added save as group.
# 27.04.2013: Add selection of dataset in gui. Removed parameter 'data'.
# 27.04.2013: New parameter 'debug'.
# <27.04.2013: Changed data=NA to data=NULL
# <27.04.2013: First version.

#' @title Trim data GUI
#'
#' @description
#' \code{trim_gui} is a GUI wrapper for the \code{trim} function.
#'
#' @details
#' Simplifies the use of the \code{trim} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame with extracted result.


trim_gui <- function(env=parent.frame(), debug=FALSE){

  # Load dependencies.  
  library(gWidgets)
  options(guiToolkit="RGtk2")

  gData <- data.frame(Sample.Name="NA")
  gDataName <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  w <- gwindow(title="Trim dataset", visible=FALSE)
  
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
  
  if(debug){
    print("DATASET")
  }
  
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

      requiredCol <- c("Sample.Name")
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gData <<- data.frame(Sample.Name="NA")
        svalue(sample_txt) <- ""
        svalue(column_txt) <- ""
        .refresh_samples_tbl()
        .refresh_columns_tbl()
        
        svalue(dataset_samples_lbl) <- "0 samples"
        svalue(f2_save_edt) <- ""
        
        message <- paste("The dataset is not sample data\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
        
        gDataName <<- val_obj

        .refresh_samples_tbl()
        .refresh_columns_tbl()
        
        samples <- length(unique(gData$Sample.Name))
        svalue(dataset_samples_lbl) <- paste("", samples, "samples")
        svalue(f2_save_edt) <- paste(gDataName, "_trim", sep="")
        
      }
      
    } else {
      
      gData <<- data.frame(Sample.Name="NA")
      svalue(sample_txt) <- ""
      svalue(column_txt) <- ""
      .refresh_samples_tbl()
      .refresh_columns_tbl()
      svalue(dataset_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )
  
  # SAMPLES ###################################################################
  
  if(debug){
    print("SAMPLES")
  }
  
  sample_f <- gframe("Samples", horizontal=FALSE, container=g1, expand=TRUE)
  
  
  sample_opt <- gradio(items=c("Keep","Remove"),
                       selected=1,
                       horizontal=FALSE,
                       container=sample_f)

  sample_lbl <- glabel(text="Selected samples:",
                       container=sample_f,
                       anchor=c(-1 ,0))

  sample_txt <- gedit(initial.msg="Doubleklick or drag sample names to list",
                      width = 40,
                      container=sample_f)

  sample_tbl <- gtable(items=data.frame(Sample.Names=unique(gData$Sample.Name),
                                        stringsAsFactors=FALSE),
                       container=sample_f,
                       expand=TRUE)

  addDropTarget(sample_txt, handler=function(h,...) {
    # Get values.
    drp_val <- h$dropdata
    sample_val <- svalue(h$obj)
    
    # Add new value to selected.
    new <- ifelse(nchar(sample_val) > 0, paste(sample_val, drp_val, sep="|"), drp_val)
    
    # Update text box.
    svalue(h$obj) <- new
    
    # Update sample name table.
    tmp_tbl <- sample_tbl[,]  # Get all values.
    tmp_tbl <- tmp_tbl[tmp_tbl!=drp_val]  # Remove value added to selected.
    sample_tbl[,] <- tmp_tbl  # Update table.
    
  })

  
  # COLUMNS ###################################################################
  
  if(debug){
    print("COLUMNS")
  }
  
  column_f <- gframe("Columns", 
                     horizontal=FALSE, 
                     container=g1, 
                     expand=TRUE)
  
  column_opt <- gradio(items=c("Keep","Remove"),
                       selected=1,
                       horizontal=FALSE, 
                       container=column_f)
  
  column_lbl <- glabel(text="Selected columns:",
                       container=column_f,
                       anchor=c(-1 ,0))
  
  column_txt <- gedit(initial.msg="Doubleklick or drag column names to list", 
                      width = 40,
                      container=column_f)
  
  
  column_tbl <- gtable(items=names(gData), 
                       container=column_f,
                       expand=TRUE)

  addDropTarget(column_txt, handler=function(h,...) {
    # Get values.
    drp_val <- h$dropdata
    column_val <- svalue(h$obj)
    
    # Add new value to selected.
    new <- ifelse(nchar(column_val) > 0,
                  paste(column_val, drp_val, sep="|"),
                  drp_val)
    
    # Update text box.
    svalue(h$obj) <- new
    
    # Update column name table.
    tmp_tbl <- column_tbl[,]  # Get all values.
    tmp_tbl <- tmp_tbl[tmp_tbl!=drp_val]  # Remove value added to selected.
    column_tbl[,] <- tmp_tbl  # Update table.
    
  })
  
  # OPTIONS ###################################################################
  
  if(debug){
    print("OPTIONS")
  }  
  
  option_f <- gframe("Options",
                     horizontal=FALSE, 
                     container=g1, 
                     expand=TRUE)
  
  empty_chk <- gcheckbox(text="Remove empty columns",
                         checked=TRUE,
                         container=option_f)
  
  na_chk <- gcheckbox(text="Remove NA columns",
                      checked=TRUE,
                      container=option_f)
  
  word_chk <- gcheckbox(text="Add word boundaries",
                        checked=FALSE,
                        container=option_f)
  
  case_chk <- gcheckbox(text="Ignore case",
                        checked=TRUE, 
                        container=option_f)
  
  na_txt <- gedit(text="NA",
                  label="Replace missing values with:",
                  container=option_f)

  # FRAME 2 ###################################################################
  
  if(debug){
    print("SAVE")
  }  

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
  
  trim_btn <- gbutton(text="Trim dataset",
                      border=TRUE,
                      container=g2)
  
  addHandlerChanged(trim_btn, handler = function(h, ...) {
    
    # Get new dataset name.
    val_name <- svalue(f2_save_edt)

    if(nchar(val_name) > 0) {
      
      # Get values.
      sample_val <- svalue(sample_txt)
      column_val <- svalue(column_txt)
      word_val <- svalue(word_chk)
      case_val <- svalue(case_chk)
      sample_opt_val <- if(svalue(sample_opt, index=TRUE)==1){FALSE}else{TRUE}
      column_opt_val <- if(svalue(column_opt, index=TRUE)==1){FALSE}else{TRUE}
      na_val <- svalue(na_chk)
      empty_val <- svalue(empty_chk)
      na_txt_val <- svalue(na_txt)
      
      if(debug){
        print("gData")
        print(names(gData))
        print("column_val")
        print(column_val)
        print("word_val")
        print(word_val)
        print("case_val")
        print(case_val)
        print("sample_opt_val")
        print(sample_opt_val)
        print("column_opt_val")
        print(column_opt_val)
        print("na_val")
        print(na_val)
        print("empty_val")
        print(empty_val)
        print("na_txt_val")
        print(na_txt_val)
      }
  
      # Change button.
      svalue(trim_btn) <- "Processing..."
      enabled(trim_btn) <- FALSE
      
      datanew <- trim(data=gData, samples=sample_val, columns=column_val, 
                   word=word_val, ignoreCase=case_val, invertS=sample_opt_val, invertC=column_opt_val,
                   rmNaCol=na_val, rmEmptyCol=empty_val, missing=na_txt_val)
  
  
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
  
  .refresh_samples_tbl <- function(){
    
    if(debug){
      print(paste("IN:", match.call()[[1]]))
    }
    
    # Refresh widget by removing it and...
    delete(sample_f, sample_tbl)
    
    # ...creating a new table.
    sample_tbl <<- gtable(items=data.frame(Sample.Names=unique(gData$Sample.Name),
                                           stringsAsFactors=FALSE),
                       container=sample_f,
                       expand=TRUE)
    
    
    addDropSource(sample_tbl, handler=function(h,...) svalue(h$obj))

    addHandlerDoubleclick(sample_tbl, handler = function(h, ...) {
        
      # Get values.
      tbl_val <- svalue (h$obj)
      sample_val <- svalue(sample_txt)

      # Add new value to selected.
      new <- ifelse(nchar(sample_val) > 0,
                    paste(sample_val, tbl_val, sep="|"),
                    tbl_val)
        
      # Update text box.
      svalue(sample_txt) <- new
      
        
      # Update sample name table.
        tmp_tbl <- sample_tbl[,]  # Get all values.
        tmp_tbl <- tmp_tbl[tmp_tbl!=tbl_val]  # Remove value added to selected.
        sample_tbl[,] <- tmp_tbl  # Update table.
        
      } )
    
  }
  
  .refresh_columns_tbl <- function(){
    
    if(debug){
      print(paste("IN:", match.call()[[1]]))
    }
    
    # Refresh widget by removing it and...
    delete(column_f, column_tbl)
    
    # ...creating a new table.
    column_tbl <<- gtable(items=names(gData), 
                         container=column_f,
                         expand=TRUE)
    
    addDropSource(column_tbl, handler=function(h,...) svalue(h$obj))
    
    addHandlerDoubleclick(column_tbl, handler = function(h, ...) {
    
      # Get values.
      tbl_val <- svalue (h$obj)
      column_val <- svalue(column_txt)
      
      # Add new value to selected.
      new <- ifelse(nchar(column_val) > 0,
                    paste(column_val, tbl_val, sep="|"),
                    tbl_val)
      
      # Update text box.
      svalue(column_txt) <- new
      
      # Update column name table.
      tmp_tbl <- column_tbl[,]  # Get all values.
      tmp_tbl <- tmp_tbl[tmp_tbl!=tbl_val]  # Remove value added to selected.
      column_tbl[,] <- tmp_tbl  # Update table.
    
    } )
    
  }
    
} # End of GUI
