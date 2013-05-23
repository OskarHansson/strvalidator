################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: .result removed, added save as group.
# 25.04.2013: First version.

#' @title Add data GUI
#'
#' @description
#' \code{addData_gui} is a GUI wrapper for \code{addData}.
#'
#' @details Add new information to a dataset from a second dataset by
#' a key column.
#' @param env environment in wich to search for data frames.
#' @param debug logical indicating printing debug information.
#' 

addData_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  dataDest <- NULL
  dataDestName <- NULL
  dataDestColumns <- NULL
  dataSource <- NULL
  dataSourceColumns <- NULL
  default_drp <- "<Select column>"
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Add data", visible=FALSE)
  
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
  
  # Datasets ------------------------------------------------------------------
  
  g0[1,1] <- glabel(text="Select destination dataset:", container=g0)

  g0[1,2] <- dataset_drp <- gdroplist(items=c("<Select dataset>",
                                   listObjects(env=env,
                                               objClass="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = g0)
  
  g0[1,3] <- g0_samples_lbl <- glabel(text=" 0 samples", container=g0)
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env)){

      dataDest <<- get(val_obj, envir=env)
      dataDestName <<- val_obj
      dataDestColumns <<- names(dataDest)
      
      samples <- length(unique(dataDest$Sample.Name))
      svalue(g0_samples_lbl) <- paste(" ", samples, "samples")
      svalue(f2_save_edt) <- paste(dataDestName, "_new", sep="")
      f1_key_drp[] <- c(default_drp,
                        intersect(dataDestColumns,dataSourceColumns))
      f1_key2_drp[] <- c(default_drp,
                        intersect(dataDestColumns,dataSourceColumns))
      
    } else {
      
      dataDest <<- NULL
      svalue(dataset_drp, index=TRUE) <- 1
      svalue(g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      f1_key_drp[] <- default_drp
      f1_key2_drp[] <- default_drp
    }
  } )  
  
  g0[2,1] <- glabel(text="Select source dataset:", container=g0)
  
  g0[2,2] <- refset_drp <- gdroplist(items=c("<Select dataset>",
                                   listObjects(env=env,
                                               objClass="data.frame")), 
                           selected = 1,
                           editable = FALSE,
                           container = g0) 
  
  g0[2,3] <- g0_ref_lbl <- glabel(text=" 0 samples", container=g0)
  
  addHandlerChanged(refset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(refset_drp)
    
    if(exists(val_obj, envir=env)){
      
      dataSource <<- get(val_obj, envir=env)
      dataSourceColumns <<- names(dataSource)
      ref <- length(unique(dataSource$Sample.Name))
      svalue(g0_ref_lbl) <- paste(" ", ref, "samples")
      
      f1_key_drp[] <- c(default_drp,
                        intersect(dataDestColumns,dataSourceColumns))
        
      f1_key2_drp[] <- c(default_drp,
                        intersect(dataDestColumns,dataSourceColumns))
    } else {
      
      dataSource <<- NULL
      svalue(refset_drp, index=TRUE) <- 1
      svalue(g0_ref_lbl) <- " 0 samples"
      f1_key_drp[] <- default_drp
      f1_key2_drp[] <- default_drp
      
    }
    
  } )  

  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 5,
               container = gv) 

  f1_exact_chk <- gcheckbox(text="Exact key matching",
                                    checked = TRUE,
                                    container = f1)
  
  
  glabel(text="Select key column:", container = f1, anchor=c(-1 ,0))
  f1_key_drp <- gdroplist(items=default_drp,
                          selected = 1,
                          editable = FALSE,
                          container = f1)
  
  glabel(text="Select second key column:", container = f1, anchor=c(-1 ,0))
  f1_key2_drp <- gdroplist(items=default_drp,
                          selected = 1,
                          editable = FALSE,
                          container = f1)

  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", container=f2)

  # BUTTON ####################################################################
  
  
  add_btn <- gbutton(text="Add new data",
                        border=TRUE,
                        container=gv)
  
  addHandlerChanged(add_btn, handler = function(h, ...) {
    
    val_exact <- svalue(f1_exact_chk)
    val_key <- svalue(f1_key_drp)
    val_key2 <- svalue(f1_key2_drp)
    val_name <- svalue(f2_save_edt)
    
    if(val_key == default_drp){
      val_key <- NULL
    }
    if(val_key2 == default_drp){
      val_key2 <- NULL
    }
    
    # Check dataset and first key (second key is optional)
    if(!is.null(dataDest) & !is.null(dataSource) & !is.null(val_key)){
      
      # Change button.
      svalue(add_btn) <- "Processing..."
      enabled(add_btn) <- FALSE
  
      datanew <- addData(data=dataDest,
                         newData=dataSource,
                         exact=val_exact,
                         byCol=val_key,
                         thenByCol=val_key2)
      
      # Save data.
      assign(val_name, datanew, envir=env)
      
      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
    
    } else {
      
      message <- "A destination and a source dataset have to be selected."
      
      gmessage(message, title="Datasets not selected",
               icon = "error",
               parent = w) 
      
    }
    
  } )

  
  # Show GUI.
  visible(w) <- TRUE
  
}
