################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 21.05.2013: Fixed name on save as.
# 17.05.2013: listDataFrames() -> listObjects()
# 09.05.2013: .result removed, added save as group.
# 04.05.2013: First version.


#' @title Guess profile
#'
#' @description
#' \code{guessProfile_gui} is a GUI wrapper for the \code{guessProfile} function.
#'
#' @details
#' Simplifies the use of the \code{guessProfile} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames.
#' @param debug logical indicating printing debug information.
#' 

guessProfile_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Load dependencies.  
  require(gWidgets)
  options(guiToolkit="RGtk2")
  
  gData <- NULL
  gDataName <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  w <- gwindow(title="Guess profile", visible=FALSE)
  
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
      requiredCol <- c("Sample.Name", "Marker","Allele","Height")
      
      if(!all(requiredCol %in% colnames(gData))){
        
        gData <<- data.frame(No.Data=NA)
        gDataName <<- NULL

        svalue(dataset_samples_lbl) <- " 0 samples"
        svalue(f2_save_edt) <- ""
        
        message <- paste("The following columns are required:\n",
                         paste(requiredCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
        
        svalue(dataset_drp, index=TRUE) <- 1
        
      } else {
        
        gDataName <<- val_obj
        samples <- length(unique(gData$Sample.Name))
        svalue(dataset_samples_lbl) <- paste(" ", samples, "samples")
        svalue(f2_save_edt) <- paste(gDataName, "_profile", sep="")
        
      }
      
    } else {
      
      gData <<- data.frame(No.Data=NA)
      gDataName <<- NULL
      svalue(dataset_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )
  
  # OPTIONS ###################################################################
  
  group1 <- ggroup(horizontal=FALSE,
                   spacing = 10,
                   container = gv) 
  
  grid1 <- glayout(container = group1, spacing = 5)
  
  grid1[1,1] <- glabel(text="Accepted ratio >=", container=grid1)

  grid1[1,2] <- g1_ratio_spb <- gspinbutton(from = 0, to = 1,
                                            by = 0.01, value = 0.6,
                                            container = grid1)

  grid1[2,1] <- glabel(text="Accepted peak height >=", container=grid1)
  
  grid1[2,2] <- g1_height_edt <- gedit(width=6, container=grid1)
  
  grid1[3,1] <- g1_na_chk <- gcheckbox(text="Discard NA rows",
                                   checked=FALSE,
                                   container=grid1)

  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", width=25, container=f2)

  # BUTTON ####################################################################

  if(debug){
    print("BUTTON")
  }  
  
  check_btn <- gbutton(text="Guess",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(check_btn, handler = function(h, ...) {
    
    # Get values.
    val_data <- gData
    val_ratio <- as.numeric(svalue(g1_ratio_spb))
    val_height <- as.numeric(svalue(g1_height_edt))
    val_NA <- svalue(g1_na_chk)
    val_name <- svalue(f2_save_edt)
    
    if (!is.null(gData)){
      
      datanew <- guessProfile(data=val_data,
                   ratio=val_ratio,
                   height=val_height,
                   na.rm=val_NA)
      
      # Save data.
      assign(val_name, datanew, envir=env)
      
      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
      
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
