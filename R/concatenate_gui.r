################################################################################
# TODO LIST
# TODO: Option to remove old datasets

################################################################################
# CHANGE LOG
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 17.05.2013: First version.


#' @title Concatenate two datasets
#'
#' @description
#' \code{concatenate_gui} is a GUI for combining two datasets.
#'
#' @details
#' Simplifies the use of the \code{checkSubset} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames.
#' @param debug logical indicating printing debug information.
#' 

concatenate_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Global variables.
  .gData1 <- NULL
  .gData2 <- NULL
  .gData1Name <- NULL
  .gData2Name <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.
  w <- gwindow(title="Concatenate", visible=FALSE)
  
  # Vertical main group.
  gv <- ggroup(horizontal=FALSE,
               spacing=15,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # DATASET ###################################################################
  
  f1 <- ggroup(horizontal=FALSE,
               spacing = 10,
               container = gv) 
  
  f1g1 <- glayout(container = f1, spacing = 1)
  
  f1g1[1,1] <- glabel(text="Select dataset 1:", container=f1g1)
  
  f1g1[1,2] <- f1g1_data1_drp <- gdroplist(items=c("<Select dataset>",
                                                 listObjects(env=env,
                                                             objClass="data.frame")),
                                         selected = 1,
                                         editable = FALSE,
                                         container = f1g1)
  
  f1g1[1,3] <- f1g1_data1_col_lbl <- glabel(text=" 0 columns",
                                              container=f1g1)
  
  addHandlerChanged(f1g1_data1_drp, handler = function (h, ...) {
    
    val_obj <- svalue(f1g1_data1_drp)
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gData1 <<- get(val_obj, envir=env)
      .gData1Name <<- val_obj
      
      svalue(f1g1_data1_col_lbl) <- paste(" ", ncol(.gData1), " columns")
      svalue(f2_name) <- paste(.gData1Name, .gData2Name, sep="_")
        
    } else {
      
      .gData1 <<- NULL
      .gData1Name <<- NULL
      svalue(f1g1_data1_col_lbl) <- " 0 columns"
      svalue(f2_name) <- ""
      
    }
  } )

  f1g1[2,1] <- glabel(text="Select dataset 2:", container=f1g1)
  
  f1g1[2,2] <- f1g1_data2_drp <- gdroplist(items=c("<Select dataset>",
                                                 listObjects(env=env,
                                                             objClass="data.frame")),
                                         selected = 1,
                                         editable = FALSE,
                                         container = f1g1)
  
  f1g1[2,3] <- f1g1_data2_col_lbl <- glabel(text=" 0 columns",
                                              container=f1g1)
  
  addHandlerChanged(f1g1_data2_drp, handler = function (h, ...) {
    
    val_obj <- svalue(f1g1_data2_drp)
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gData2 <<- get(val_obj, envir=env)
      .gData2Name <<- val_obj
      
      svalue(f1g1_data2_col_lbl) <- paste(" ", ncol(.gData2), " columns")
      svalue(f2_name) <- paste(.gData1Name, .gData2Name, sep="_")
      
    } else {
      
      .gData2 <<- NULL
      .gData1Name <<- NULL
      svalue(f1g1_data2_col_lbl) <- " 0 samples"
      svalue(f2_name) <- ""
      
    }
  } )
  
  # NAME ######################################################################
  
  f2 <- ggroup(horizontal=TRUE,
                   spacing = 10,
                   container = gv) 
  
  glabel(text="Save as:", container=f2)
  f2_name <- gedit(text="", width=40, container=f2)
  
  # BUTTON ####################################################################

  if(debug){
    print("BUTTON")
  }  
  
  concatenate_btn <- gbutton(text="Concatenate",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(concatenate_btn, handler = function(h, ...) {
    
    colOk <- all(names(.gData1) == names(.gData2))
    
    if (colOk){
      
      datanew <- rbind(.gData1,.gData2)
      val_name <- svalue(f2_name)
      
      # Save data.
      saveObject(name=val_name, object=datanew, parent=w, env=env)
      
      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
      
    } else {
      
      gmessage(message="Datasets must have identical columns!",
               title="Error",
               icon = "error")      
      
    } 
    
  } )
  
  # Show GUI.
  visible(w) <- TRUE
  
} # End of GUI
