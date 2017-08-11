################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 02.05.2016: Added attributes.
# 12.10.2015: First version.


#' @title Remove Spike
#'
#' @description
#' GUI wrapper for the \code{\link{removeSpike}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{removeSpike}} function by providing a
#'  graphical user interface to it.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help
#'
#' @return TRUE


removeSpike_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE, parent=NULL){

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gSpike <- NULL
  .gSpikeName <- NULL
  
  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title="Remove spikes", visible=FALSE)

  # Runs when window is closed.
  addHandlerDestroy(w, handler = function (h, ...) {

    # Save GUI state.
    .saveSettings()

    # Focus on parent window.
    if(!is.null(parent)){
      focus(parent)
    }

  })

  # Vertical main group.
  gv <- ggroup(horizontal=FALSE,
              spacing=15,
              use.scrollwindow=FALSE,
              container = w,
              expand=FALSE)

  # Help button group.
  gh <- ggroup(container = gv, expand=FALSE, fill="both")

  savegui_chk <- gcheckbox(text="Save GUI settings", checked=FALSE, container=gh)

  addSpring(gh)

  help_btn <- gbutton(text="Help", container=gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("removeSpike_gui", help_type="html"))

  })

  # DATASET ###################################################################

  f0 <- gframe(text = "Dataset",
               horizontal=FALSE,
               spacing = 10,
               container = gv)


  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1,1] <- glabel(text="Select dataset:", container=f0g0)

  f0g0[1,2] <- f0g0_data_drp <- gcombobox(items=c("<Select dataset>",
                                                 listObjects(env=env,
                                                             obj.class="data.frame")),
                                         selected = 1,
                                         editable = FALSE,
                                         container = f0g0,
                                         ellipsize = "none")

  f0g0[1,3] <- f0g0_data_col_lbl <- glabel(text=" 0 rows",
                                              container=f0g0)

  addHandlerChanged(f0g0_data_drp, handler = function (h, ...) {

    val_obj <- svalue(f0g0_data_drp)
    
    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Size", "File.Name")
    ok <- checkDataset(name=val_obj, reqcol=requiredCol,
                       env=env, parent=w, debug=debug)

    if(ok){

      # Load or change components.
      .gData <<- get(val_obj, envir=env)
      .gDataName <<- val_obj

      svalue(f0g0_data_col_lbl) <- paste(" ", nrow(.gData), " rows")
      svalue(f2_name) <- paste(.gDataName, "no_spikes", sep="_")

    } else {

      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- " 0 rows"
      svalue(f2_name) <- ""

    }

  } )

  f0g0[2,1] <- glabel(text="Select spike list:", container=f0g0)
  
  f0g0[2,2] <- f0g0_spike_drp <- gcombobox(items=c("<Select dataset>",
                                                   listObjects(env=env,
                                                               obj.class="data.frame")),
                                           selected = 1,
                                           editable = FALSE,
                                           container = f0g0,
                                           ellipsize = "none")
  
  f0g0[2,3] <- f0g0_spike_col_lbl <- glabel(text=" 0 samples",
                                           container=f0g0)
  
  addHandlerChanged(f0g0_spike_drp, handler = function (h, ...) {
    
    val_obj <- svalue(f0g0_spike_drp)
    
    # Check if suitable.
    requiredCol <- c("Allele", "Id", "Marker")
    ok <- checkDataset(name=val_obj, reqcol=requiredCol,
                       env=env, parent=w, debug=debug)
    
    if(ok){
      
      # Load or change components.
      .gSpike <<- get(val_obj, envir=env)
      .gSpikeName <<- val_obj
      
      svalue(f0g0_spike_col_lbl) <- paste(" ", length(unique(.gSpike$Id)), 
                                          " samples")

    } else {
      
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- " 0 samples"

    }
    
  } )
  
  # OPTIONS ###################################################################

  f1 <- gframe(text="Options", horizontal=FALSE, spacing=10, container=gv)

  f1_invert_chk <- gcheckbox(text="Invert (remove all but spikes)",
                             checked = FALSE, container = f1)
  

  # NAME ######################################################################

  f2 <- gframe(text = "Save as",
               horizontal=TRUE,
               spacing = 5,
               container = gv)

  glabel(text="Save as:", container=f2)
  f2_name <- gedit(text="", width=40, container=f2, expand = TRUE)

  # BUTTON ####################################################################

  if(debug){
    print("BUTTON")
  }

  remove_btn <- gbutton(text="Remove", container=gv)

  addHandlerChanged(remove_btn, handler = function(h, ...) {

    val_data <- .gData
    val_spike <- .gSpike
    val_name_data <- .gDataName
    val_name_spike <- .gSpikeName
    val_name <- svalue(f2_name)
    val_invert <- svalue(f1_invert_chk)

    if ((!is.na(val_data) && !is.null(val_data)) &
        (!is.na(val_spike) && !is.null(val_spike))){

      datanew <- removeSpike(data = val_data, spike = val_spike,
                             invert = val_invert, debug=debug)
      
      # Create key-value pairs to log.
      keys <- list("data", "spike", "invert")
      
      values <- list(val_name_data, val_name_spike, val_invert)
      
      # Update audit trail.
      datanew <- auditTrail(obj = datanew, key = keys, value = values,
                            label = "removeSpike_gui", arguments = FALSE,
                            package = "strvalidator")
      
      # Save data.
      saveObject(name=val_name, object=datanew, parent=w, env=env)

      if(debug){
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }

      # Close GUI.
      dispose(w)

    } else {

      gmessage(msg="Select a datasets!",
               title="Error",
               icon = "error")

    }

  } )

  # INTERNAL FUNCTIONS ########################################################

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
      if(exists(".strvalidator_removeSpike_gui_savegui", envir=env, inherits = FALSE)){
        svalue(savegui_chk) <- get(".strvalidator_removeSpike_gui_savegui", envir=env)
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
      if(exists(".strvalidator_removeSpike_gui_invert", envir=env, inherits = FALSE)){
        svalue(f1_invert_chk) <- get(".strvalidator_removeSpike_gui_invert", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }

  }

  .saveSettings <- function(){

    # Then save settings if true.
    if(svalue(savegui_chk)){

      assign(x=".strvalidator_removeSpike_gui_savegui", value=svalue(savegui_chk), envir=env)
      assign(x=".strvalidator_removeSpike_gui_invert", value=svalue(f1_invert_chk), envir=env)

    } else { # or remove all saved values if false.

      if(exists(".strvalidator_removeSpike_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_removeSpike_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_removeSpike_gui_invert", envir=env, inherits = FALSE)){
        remove(".strvalidator_removeSpike_gui_invert", envir = env)
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
  focus(w)

} # End of GUI