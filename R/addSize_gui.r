################################################################################
# TODO LIST
# TODO: Make a general function to add any (selected) kit information?

################################################################################
# CHANGE LOG
# 11.02.2014: First version.


#' @title Adds size information
#'
#' @description
#' \code{addSize_gui} is a GUI wrapper for the \code{addSize} function.
#'
#' @details
#' Simplifies the use of the \code{addSize} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

addSize_gui <- function(env=parent.frame(), debug=FALSE){
  
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gKit <- 1

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  
  w <- gwindow(title="Add size to dataset", visible=FALSE)
  
  gv <- ggroup(horizontal=FALSE,
               spacing=15,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 
  
  # DATASET ###################################################################
  
  group0 <- ggroup(horizontal=FALSE,
               spacing = 5,
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
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gData <<- get(val_obj, envir=env)
      requiredCol <- c("Marker", "Allele")
      
      if(!all(requiredCol %in% colnames(.gData))){
        
        missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]

        message <- paste("Additional columns required:\n",
                         paste(missingCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
      
        # Reset components.
        .gData <<- data.frame(No.Data=NA)
        .gDataName <<- NULL
        svalue(dataset_samples_lbl) <- " 0 samples"
        svalue(f2_save_edt) <- ""
        
      } else {

        # Load or change components.
        .gDataName <<- val_obj
        samples <- length(unique(.gData$Sample.Name))
        svalue(dataset_samples_lbl) <- paste(" ", samples, "samples")
        .gKit <<- detectKit(.gData, index=TRUE)
        svalue(kit_drp, index=TRUE) <- .gKit
        svalue(f2_save_edt) <- paste(.gDataName, "_size", sep="")
        
        if(debug){
          print("Detected kit index")
          print(.gKit)
        }
        
      }
      
    } else {
      
      # Reset components.
      .gData <<- data.frame(No.Data=NA)
      .gDataName <<- NULL
      svalue(dataset_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )
  
  # KIT -----------------------------------------------------------------------
  
  grid0[2,1] <- glabel(text="Kit:", container=grid0)
  
  kit_drp <- gdroplist(items=getKit(),
                           selected = 1,
                           editable = FALSE,
                           container = grid0)
  
  grid0[2,2] <- kit_drp
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", container=f2)

  # BUTTON ####################################################################

  if(debug){
    print("BUTTON")
  }  
  
  add_btn <- gbutton(text="Add size",
                      border=TRUE,
                      container=gv)
  
  addHandlerChanged(add_btn, handler = function(h, ...) {
    
    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_name <- svalue(f2_save_edt)
    val_data <- .gData
    
    if(debug){
      print("val_data:")
      print(str(val_data))
      print(head(val_data))
      print("val_kit")
      print(val_kit)
    }
    
    # Get kit with size information.
    val_size <- getKit(kit=val_kit, what="Size")

    # Change button.
    svalue(add_btn) <- "Processing..."
    enabled(add_btn) <- FALSE
    
    datanew <- addSize(data=val_data, kit=val_size, debug=debug)
    
    # Save data.
    saveObject(name=val_name, object=datanew, parent=w, env=env)
    
    # Close GUI.
    dispose(w)
    
  } )
  
  # Show GUI.
  visible(w) <- TRUE
  
} # End of GUI
