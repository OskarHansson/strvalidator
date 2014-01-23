################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.
# 20.05.2013: First version.


#' @title Calculate Heterozygous GUI
#'
#' @description
#' \code{calculateHeterozygous_gui} is a GUI wrapper for the
#'  \code{calculateHeterozygous} function.
#'
#' @details
#' Simplifies the use of the \code{calculateHeterozygous} function by
#' providing a graphical user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param debug logical indicating printing debug information.
#' 
#' @return data.frame in slim format.

calculateHeterozygous_gui <- function(env=parent.frame(), debug=FALSE){
  
  
  # Global variables.
  .gData <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.
  w <- gwindow(title="Calculate heterozygous", visible=FALSE)
  
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
  
  f0g0 <- glayout(container = f0, spacing = 1)
  
  # Datasets ------------------------------------------------------------------
  
  f0g0[1,1] <- glabel(text="Select dataset:", container=f0g0)
  
  f0g0[1,2] <- dataset_drp <- gdroplist(items=c("<Select dataset>",
                                              listObjects(env=env,
                                                          objClass="data.frame")), 
                                      selected = 1,
                                      editable = FALSE,
                                      container = f0g0)
  
  f0g0[1,3] <- f0g0_samples_lbl <- glabel(text=" 0 samples", container=f0g0)
  
  addHandlerChanged(dataset_drp, handler = function (h, ...) {
    
    val_obj <- svalue(dataset_drp)
    
    if(exists(val_obj, envir=env, inherits = FALSE)){
      
      .gData <<- get(val_obj, envir=env)
      requiredCol <- c("Sample.Name", "Marker", "Allele")
      
      if(!all(requiredCol %in% colnames(.gData))){
        
        missingCol <- requiredCol[!requiredCol %in% colnames(.gData)]

        message <- paste("Additional columns required:\n",
                         paste(missingCol, collapse="\n"), sep="")
        
        gmessage(message, title="Data error",
                 icon = "error",
                 parent = w) 
      
        # Reset components.
        .gData <<- NULL
        svalue(dataset_drp, index=TRUE) <- 1
        svalue(f0g0_samples_lbl) <- " 0 samples"
        svalue(f2_save_edt) <- ""
        
      } else {

        # Load or change components.
        samples <- length(unique(.gData$Sample.Name))
        svalue(f0g0_samples_lbl) <- paste("", samples, "samples")
        svalue(f2_save_edt) <- paste(val_obj, "_het", sep="")
        
      }
      
    } else {
      
      # Reset components.
      .gData <<- NULL
      svalue(dataset_drp, index=TRUE) <- 1
      svalue(f0g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )  
  
  # FRAME 2 ###################################################################
  
  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv) 
  
  glabel(text="Name for result:", container=f2)
  
  f2_save_edt <- gedit(text="", container=f2)
  
  # BUTTON ####################################################################
  
  
  calculate_btn <- gbutton(text="Calculate",
                        border=TRUE,
                        container=gv)
  
  addHandlerChanged(calculate_btn, handler = function(h, ...) {
    
    val_name <- svalue(f2_save_edt)
    
    if(!is.null(.gData)){
      
      # Change button.
      svalue(calculate_btn) <- "Processing..."
      enabled(calculate_btn) <- FALSE
      
      datanew <- calculateHeterozygous(data=.gData)
      
      # Save data.
      saveObject(name=val_name, object=datanew, parent=w, env=env)
      
      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
      
    } else {
      
      message <- "A dataset has to be selected."
      
      gmessage(message, title="Datasets not selected",
               icon = "error",
               parent = w) 
      
    }
    
  } )
  
  
  # Show GUI.
  visible(w) <- TRUE
  
}
