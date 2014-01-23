################################################################################
# TODO LIST
# TODO: Option to show only 'head' for large datasets. >X rows show head?
# TODO: Option to show data automatically when selected or just show info
#  and a button to show data?


################################################################################
# CHANGE LOG
# 02.12.2013: Added parameter 'name' for selection of 'data' in drop menu.
# 30.11.2013: Added info also when 'data' is passed.
# 20.11.2013: Specified package for function 'gtable' -> 'gWidgets::gtable'
# 16.11.2013: New parameter 'data' and 'edit' and implementation.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 21.05.2013: Added 'copy to clipboard'
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: First version.

#' @title Edit or view data frames GUI
#'
#' @description
#' \code{editData_gui} is a GUI simplifying editing and viewing of data frames.
#'
#' @details Select a data frame from the dropdown and view/edit. Optionally
#' save as a new dataframe.
#' @param env environment in wich to search for data frames.
#' @param data data.frame for instant viewing.
#' @param name character string with the name of the provided dataset.
#' @param edit logical TRUE for enable edit .
#' @param debug logical indicating printing debug information.
#' 

editData_gui <- function(env=parent.frame(), data=NULL, name=NULL, edit=TRUE, debug=FALSE){

  .gData <- data
  .gDataName <- name
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  if(edit){
    guiTitle="Edit or view data frame"
  } else{
    guiTitle="View data frame"
  }
  
  w <- gwindow(title=guiTitle, visible=FALSE)
  
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
  
  if(!is.null(.gDataName)){
    svalue(dataset_drp) <- .gDataName
  }
  
  g0[1,3] <- g0_samples_lbl <- glabel(text=" 0 samples,", container=g0)
  g0[1,4] <- g0_columns_lbl <- glabel(text=" 0 columns,", container=g0)
  g0[1,5] <- g0_rows_lbl <- glabel(text=" 0 rows", container=g0)
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gData <<- get(val_obj, envir=env)
      
      .gDataName <<- val_obj
      
      if("Sample.Name" %in% names(.gData)){
        samples <- length(unique(.gData$Sample.Name))
        svalue(g0_samples_lbl) <- paste(" ", samples, "samples,")
      } else {
        svalue(g0_samples_lbl) <- paste(" ", "<NA>", "samples,")
      }
      svalue(g0_columns_lbl) <- paste(" ", ncol(.gData), "columns,")
      svalue(g0_rows_lbl) <- paste(" ", nrow(.gData), "rows")
      
      .refresh_tbl()
      
    } else {
      svalue(g0_samples_lbl) <- paste(" ", "<NA>", "samples,")
      svalue(g0_columns_lbl) <- paste(" ", "<NA>", "columns,")
      svalue(g0_rows_lbl) <- paste(" ", "<NA>", "rows")
    }
  } )  
  
  # FRAME 1 ###################################################################

  f1 <- gframe(text = "Save | Copy",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 
  
  g1 <- glayout(container = f1, spacing = 1)
    
  g1[1,1] <- save_btn <- gbutton(text="Save as",
                              border=TRUE,
                              container = g1) 
  
  g1[1,2] <- save_txt <- gedit(text=.gDataName,
                            container=g1,
                            anchor=c(-1 ,0))

  g1[2,1] <- copy_btn <- gbutton(text="Copy to clipboard",
                                 border=TRUE,
                                 container = g1) 

  g1[2,2] <- glabel(text="NB! Dataset might get truncated if too large. Use 'File/Export' instead. ",
                                 border=TRUE,
                                 container = g1) 

  addHandlerChanged(copy_btn, handler = function(h, ...) {
    
    val_tbl <- data_tbl[]
    
    # Change button.
    svalue(copy_btn) <- "Copying..."
    enabled(copy_btn) <- FALSE
    
    # Copy data.
    write.table(val_tbl, "clipboard",
                sep="\t", row.names=FALSE)
    
    # Change button.
    svalue(copy_btn) <- "Copy to clipboard"
    enabled(copy_btn) <- TRUE

  } )

  addHandlerChanged(save_btn, handler = function(h, ...) {
    
    val_name <- svalue(save_txt)
    datanew <- data_tbl[]
    
    if (!is.na(val_name) && !is.null(val_name)){
      
      # Change button.
      svalue(save_btn) <- "Saving..."
      enabled(save_btn) <- FALSE

      # Save data.
      saveObject(name=val_name, object=datanew, parent=w, env=env)
      
      # Change button.
      svalue(save_btn) <- "Save as"
      enabled(save_btn) <- TRUE
    
    } else {
      
      gmessage(message="A name must be given!",
               title="Error",
               icon = "error")
      
    } 
    
  } )
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Data frame",
               horizontal=FALSE,
               spacing = 5,
               expand=TRUE,
               container = gv) 

  if(is.null(.gData)){
      data_tbl <- gWidgets::gtable(items=data.frame(Data="There is no data"),
                         container=f2, expand=TRUE)
  } else {
    
    # Update info.
    if("Sample.Name" %in% names(.gData)){
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste(" ", samples, "samples,")
    } else {
      svalue(g0_samples_lbl) <- paste(" ", "<NA>", "samples,")
    }
    svalue(g0_columns_lbl) <- paste(" ", ncol(.gData), "columns,")
    svalue(g0_rows_lbl) <- paste(" ", nrow(.gData), "rows")

    # Load data.
    if(edit){
      data_tbl <- gdf(items=.gData, container=f2, expand=TRUE)
    } else {
      data_tbl <- gWidgets::gtable(items=.gData, container=f2, expand=TRUE)
    }
  }
  
  # FUNCTIONS #################################################################

  .refresh_tbl <- function(){
    
    if(debug){
      print(paste("IN:", match.call()[[1]]))
    }
    
    # Update "save as" with current dataset name.
    svalue(save_txt) <- paste(.gDataName, "_edit", sep="")
    
    # Refresh widget by removing it and...
    delete(f2, data_tbl)
    
    visible(f2) <- FALSE
    # ...creating a new table.
    if(edit){
      data_tbl <<- gdf(items=.gData, container=f2, expand=TRUE)
    } else {
      data_tbl <<- gWidgets::gtable(items=.gData, container=f2, expand=TRUE)
    }
    
    visible(f2) <- TRUE
    
  }
  
  # Show GUI.
  visible(w) <- TRUE  
  
}
