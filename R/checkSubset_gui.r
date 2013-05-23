################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 17.05.2013: listDataFrames() -> listObjects()
# 27.04.2013: First version.


#' @title Check subsetting of a dataset
#'
#' @description
#' \code{checkSubset_gui} is a GUI wrapper for the \code{checkSubset} function.
#'
#' @details
#' Simplifies the use of the \code{checkSubset} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames.
#' @param debug logical indicating printing debug information.
#' 

checkSubset_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- NULL
  gRef <- NULL

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Check subsetting", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=15,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # DATASET ###################################################################
  
  group0 <- ggroup(horizontal=FALSE,
               spacing = 10,
               container = gv) 
  
  grid0 <- glayout(container = group0, spacing = 1)
  
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

        svalue(dataset_samples_lbl) <- "0 samples"
        
        message <- paste("The dataset is not typing data\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
        
        samples <- length(unique(gData$Sample.Name))
        svalue(dataset_samples_lbl) <- paste("", samples, "samples")
        
      }
      
    } else {
      
      gData <<- data.frame(No.Data=NA)
      svalue(dataset_samples_lbl) <- " 0 samples"
      
    }
  } )

  grid0[2,1] <- glabel(text="Select reference set:", container=grid0)
  
  grid0[2,2] <- dataset_ref_drp <- gdroplist(items=c("<Select dataset>",
                                                 listObjects(env=env,
                                                             objClass="data.frame")),
                                         selected = 1,
                                         editable = FALSE,
                                         container = grid0)
  
  grid0[2,3] <- dataset_ref_lbl <- glabel(text=" 0 reference samples",
                                              container=grid0)
  
  addHandlerChanged(dataset_ref_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_ref_drp)
    
    if(exists(val_obj, envir=env)){
      
      gRef <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Marker")
      
      if(!all(requiredCol %in% colnames(gRef))){
        
        gRef <<- data.frame(No.Data=NA)
        
        svalue(dataset_ref_lbl) <- "0 reference samples"
        
        message <- paste("The dataset is not typing data\n\n",
                         "The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
      } else {
        
        refs <- length(unique(gRef$Sample.Name))
        svalue(dataset_ref_lbl) <- paste("", refs, "reference samples")
        
      }
      
    } else {
      
      gRef <<- data.frame(No.Data=NA)
      svalue(dataset_ref_lbl) <- " 0 samples"
      
    }
  } )
  
  # BUTTON --------------------------------------------------------------------

  if(debug){
    print("BUTTON")
  }  
  
  check_btn <- gbutton(text="Subset",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(check_btn, handler = function(h, ...) {
    
    # Get values.
    val_data <- gData
    val_ref <- gRef
    
    if (!is.null(gData) || !is.null(gRef)){
      
      chksubset_w <- gwindow(title = "Check subsetting",
                             visible = FALSE, name=title,
                             width = NULL, height= NULL, parent=w,
                             handler = NULL, action = NULL)
      
      chksubset_txt <- checkSubset(data=val_data,
                                   ref=val_ref,
                                   console=FALSE)
      
      gtext (text = chksubset_txt, width = NULL, height = 300, font.attr = NULL, 
             wrap = FALSE, container = chksubset_w)
      
      visible(chksubset_w) <- TRUE
      
    } else {
      
      gmessage(message="Data frame is NULL!\n\n
               Make sure to select a dataset and a reference set",
               title="Error",
               icon = "error")      
      
    } 
    
  } )
  
  # Show GUI.
  visible(w) <- TRUE
  
} # End of GUI
