################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 02.05.2016: Added attributes.
# 02.05.2016: Added new options 'sex.rm' and 'kit'.
# 29.04.2016: First version.


#' @title Calculate Allele
#'
#' @description
#' GUI wrapper for the \code{\link{calculateAllele}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateAllele}} function by providing a
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


calculateAllele_gui <- function(env=parent.frame(), savegui=NULL, debug=FALSE, parent=NULL){

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title="Calculate allele", visible=FALSE)

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
    print(help("calculateAllele_gui", help_type="html"))

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
    requiredCol <- c("Marker", "Allele")
    ok <- checkDataset(name=val_obj, reqcol=requiredCol,
                       env=env, parent=w, debug=debug)

    if(ok){

      # Load or change components.
      .gData <<- get(val_obj, envir=env)
      .gDataName <<- val_obj

      svalue(f0g0_data_col_lbl) <- paste(" ", nrow(.gData), " rows")
      svalue(f2_name) <- paste(.gDataName, "allele", sep="_")

      # Autodetect kit.      
      svalue(f1_kit_drp) <- detectKit(data = .gData, index = FALSE,
                                      debug = debug)[1]

    } else {

      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- " 0 rows"
      svalue(f2_name) <- ""

    }

  } )

  # OPTIONS ###################################################################

  f1 <- gframe(text="Options", horizontal=FALSE, spacing=10, container=gv)
  
  f1g1 <- glayout(container = f1, spacing = 1)
  
  f1g1[1,1] <- f1_threshold_lbl <- glabel(text="Peak height threshold: ",
                                          container = f1g1)
  f1g1[1,2] <- f1_threshold_edt <- gedit(text="", width = 10, container = f1g1)
  tooltip(f1_threshold_edt) <- "Peaks with heights below this value will be removed."
  
  f1g1[2,1:2] <- f1_sex_chk <- gcheckbox(text="Remove sex markers defined in kit: ",
                                         checked=FALSE, container=f1g1)
  
  f1g1[2,3] <- f1_kit_drp <- gcombobox(items=getKit(), selected = 1,
                                       editable = FALSE, container = f1g1,
                                       ellipsize = "none") 

  addHandlerChanged(f1_sex_chk, handler = function (h, ...) {
    
    val_obj <- svalue(f1_sex_chk)
    
    if(val_obj){
      
      enabled(f1_kit_drp) <- TRUE

    } else {
      
      enabled(f1_kit_drp) <- FALSE
      
    }
    
  } )  
  
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

  button_btn <- gbutton(text="Calculate", container=gv)

  addHandlerChanged(button_btn, handler = function(h, ...) {

    val_data <- .gData
    val_name_data <- .gDataName
    val_name <- svalue(f2_name)
    val_threshold <- as.numeric(svalue(f1_threshold_edt))
    val_sex <- svalue(f1_sex_chk)
    val_kit <- svalue(f1_kit_drp)

    if (!is.na(val_data) && !is.null(val_data)){
      
      # Check status to set correct values for arguments.
      if(!val_sex){
        # If no filtering of sex markers kit should be NULL.
        val_kit <- NULL
      }
      if(is.na(val_threshold)){
        # If missing threshold should be NULL.
        val_threshold <- NULL
      }
      
      datanew <- calculateAllele(data = val_data, threshold = val_threshold,
                                 sex.rm = val_sex, kit = val_kit, debug=debug)
      
      # Add attributes to result.
      attr(datanew, which="kit") <- val_kit
      
      # Create key-value pairs to log.
      keys <- list("data", "threshold", "sex", "kit")
      
      values <- list(val_name_data, val_threshold, val_sex, val_kit)
      
      # Update audit trail.
      datanew <- auditTrail(obj = datanew, key = keys, value = values,
                            label = "calculateAllele_gui", arguments = FALSE,
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
      if(exists(".strvalidator_calculateAllele_gui_savegui", envir=env, inherits = FALSE)){
        svalue(savegui_chk) <- get(".strvalidator_calculateAllele_gui_savegui", envir=env)
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
      if(exists(".strvalidator_calculateAllele_gui_threshold", envir=env, inherits = FALSE)){
        svalue(f1_threshold_edt) <- get(".strvalidator_calculateAllele_gui_threshold", envir=env)
      }
      if(exists(".strvalidator_calculateAllele_gui_sex", envir=env, inherits = FALSE)){
        svalue(f1_sex_chk) <- get(".strvalidator_calculateAllele_gui_sex", envir=env)
      }
      if(debug){
        print("Saved settings loaded!")
      }
    }

  }

  .saveSettings <- function(){

    # Then save settings if true.
    if(svalue(savegui_chk)){

      assign(x=".strvalidator_calculateAllele_gui_savegui", value=svalue(savegui_chk), envir=env)
      assign(x=".strvalidator_calculateAllele_gui_threshold", value=svalue(f1_threshold_edt), envir=env)
      assign(x=".strvalidator_calculateAllele_gui_sex", value=svalue(f1_sex_chk), envir=env)
      
    } else { # or remove all saved values if false.

      if(exists(".strvalidator_calculateAllele_gui_savegui", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateAllele_gui_savegui", envir = env)
      }
      if(exists(".strvalidator_calculateAllele_gui_threshold", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateAllele_gui_threshold", envir = env)
      }
      if(exists(".strvalidator_calculateAllele_gui_sex", envir=env, inherits = FALSE)){
        remove(".strvalidator_calculateAllele_gui_sex", envir = env)
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