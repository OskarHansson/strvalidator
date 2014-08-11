################################################################################
# TODO LIST
# TODO: Implemented 'checkDataset'.
# TODO: Option to calculate sum of peak heights.

################################################################################
# CHANGE LOG
# 28.06.2014: Added help button and moved save gui checkbox.
# 25.02.2014: Implemented new options 'replace NA' and 'add to dataset'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.
# 20.05.2013: First version.


#' @title Calculate average peak height
#'
#' @description
#' \code{calculateH_gui} is a GUI wrapper for the \code{\link{calculateH}} 
#' function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateH}} function by providing a graphical 
#' user interface to it.
#' 
#' @param env environment in wich to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' 
#' @return TRUE
#' 
#' @export
#' 
#' @references
#' Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
#'  Journal of the Royal Statistical Society: Series C (Applied Statistics),
#'  Volume 59, Issue 5, 2010,
#'  Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
#' \url{http://dx.doi.org/10.1111/j.1467-9876.2010.00722.x}
#' 
#' @seealso \code{\link{calculateH}}

calculateH_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE){
  
  # Global variables.
  .gData <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }
  
  # Main window.
  w <- gwindow(title="Calculate average peak height", visible=FALSE)

  # Handler for saving GUI state.
  addHandlerDestroy(w, handler = function (h, ...) {
    .saveSettings()
  })
  
  # Vertical main group.
  gv <- ggroup(horizontal=FALSE,
               spacing=8,
               use.scrollwindow=FALSE,
               container = w,
               expand=TRUE) 

  # Help button group.
  gh <- ggroup(container = gv, expand=FALSE, fill="both")
  
  savegui_chk <- gcheckbox(text="Save GUI settings", checked=FALSE, container=gh)
  
  addSpring(gh)
  
  help_btn <- gbutton(text="Help", container=gh)
  
  addHandlerChanged(help_btn, handler = function(h, ...) {
    
    # Open help page for function.
    print(help("calculateH_gui", help_type="html"))
    
  })
  
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
      requiredCol <- c("Sample.Name", "Heterozygous", "Height")
      
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
        svalue(f2_save_edt) <- paste(val_obj, "_H", sep="")
        
      }
      
    } else {
      
      # Reset components.
      .gData <<- NULL
      svalue(dataset_drp, index=TRUE) <- 1
      svalue(f0g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
      
    }
  } )  
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = "Options",
               horizontal=FALSE,
               spacing = 10,
               container = gv) 
  
  f1_replace_chk <- gcheckbox(text="Replace NA with 0",
                              checked=TRUE,
                              container=f1)
  
  f1_add_chk <- gcheckbox(text="Add result to dataset",
                              checked=TRUE,
                              container=f1)
  
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
    val_add <- svalue(f1_add_chk)
    val_replace <- svalue(f1_replace_chk)
    
    if(val_replace){
      val_na <- 0
    } else {
      val_na <- NULL
    }
    
    if(!is.null(.gData)){
      
      # Change button.
      svalue(calculate_btn) <- "Processing..."
      enabled(calculate_btn) <- FALSE
      
      datanew <- calculateH(data=.gData, na=val_na, add=val_add, debug=debug)
      
      # Save data.
      saveObject(name=val_name, object=datanew, parent=w, env=env)
      
      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }
      
      # Close GUI.
      dispose(w)
      
    } else {
      
      message <- "A dataset and a reference dataset have to be selected."
      
      gmessage(message, title="Datasets not selected",
               icon = "error",
               parent = w) 
      
    }
    
  } )
  
  .loadSavedSettings <- function(){
    
    # First check status of save flag.
    if(!is.null(savegui)){
      svalue(savegui_chk) <- savegui
      enabled(savegui_chk) <- FALSE
      if(debug){
        print("Save GUI status set!")
      }  
    } else {
      # Load save flag.
      if(exists(".strvalidator_calculateH_gui_savegui", envir=env, inherits = FALSE)){
        svalue(savegui_chk) <- get(".strvalidator_calculateH_gui_savegui", envir=env)
      }
      if(debug){
        print("Save GUI status loaded!")
      }  
    }
    if(debug){
      print(svalue(savegui_chk))
    }  
    
    # Then load settings if true.
    if(svalue(savegui_chk)){
      if(exists(".strvalidator_calculateH_gui_replace", envir=env, inherits = FALSE)){
        svalue(f1_replace_chk) <- get(".strvalidator_calculateH_gui_replace", envir=env)
      }
      if(exists(".strvalidator_calculateH_gui_add", envir=env, inherits = FALSE)){
        svalue(f1_add_chk) <- get(".strvalidator_calculateH_gui_add", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }
    
  }
  
  .saveSettings <- function(){
    
    # Then save settings if true.
    if(svalue(savegui_chk)){
      
      assign(x=".strvalidator_calculateH_gui_savegui", value=svalue(savegui_chk), envir=env)
      assign(x=".strvalidator_calculateH_gui_replace", value=svalue(f1_replace_chk), envir=env)
      assign(x=".strvalidator_calculateH_gui_add", value=svalue(f1_add_chk), envir=env)
      
    } else { # or remove all saved values if false.
      
      if(exists(".strvalidator_calculateH_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateH_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_calculateH_gui_replace", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateH_gui_replace", envir = env)
      }
      if(exists(".strvalidator_calculateH_gui_add", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateH_gui_add", envir = env)
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
  
  # Show GUI.
  visible(w) <- TRUE
  
}
