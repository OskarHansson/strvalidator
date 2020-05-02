################################################################################
# CHANGE LOG (last 20 changes)
# 02.05.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
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


removeSpike_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gSpike <- NULL
  .gSpikeName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Remove spikes"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblRows <- "rows"
  strLblSpikes <- "Spike list:"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strChkInvert <- "Invert (remove all but spikes)"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strMsgNull <- "Select datasets!"
  strMsgTitleError <- "Error"
  strBtnRemove <- "Remove"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.na(dtStrings)) {
    # Get language strings, use default if not found.

    strTmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)

    strTmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)

    strTmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strTmp <- dtStrings["strFrmDataset"]$value
    strFrmDataset <- ifelse(is.na(strtmp), strFrmDataset, strtmp)

    strTmp <- dtStrings["strLblDataset"]$value
    strLblDataset <- ifelse(is.na(strtmp), strLblDataset, strtmp)

    strTmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strTmp <- dtStrings["strLblRows"]$value
    strLblRows <- ifelse(is.na(strtmp), strLblRows, strtmp)

    strTmp <- dtStrings["strLblSpikes"]$value
    strLblSpikes <- ifelse(is.na(strtmp), strLblSpikes, strtmp)

    strTmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strTmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strTmp <- dtStrings["strChkInvert"]$value
    strChkInvert <- ifelse(is.na(strtmp), strChkInvert, strtmp)

    strTmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strTmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strTmp <- dtStrings["strMsgNull"]$value
    strMsgNull <- ifelse(is.na(strtmp), strMsgNull, strtmp)

    strTmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strTmp <- dtStrings["strBtnRemove"]$value
    strBtnRemove <- ifelse(is.na(strtmp), strBtnRemove, strtmp)
  }

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(title = strWinTitle, visible = FALSE)

  # Runs when window is closed.
  addHandlerUnrealize(w, handler = function(h, ...) {

    # Save GUI state.
    .saveSettings()

    # Focus on parent window.
    if (!is.null(parent)) {
      focus(parent)
    }

    # Check which toolkit we are using.
    if (gtoolkit() == "tcltk") {
      if (as.numeric(gsub("[^0-9]", "", packageVersion("gWidgets2tcltk"))) <= 106) {
        # Version <= 1.0.6 have the wrong implementation:
        # See: https://stackoverflow.com/questions/54285836/how-to-retrieve-checkbox-state-in-gwidgets2tcltk-works-in-gwidgets2rgtk2
        message("tcltk version <= 1.0.6, returned TRUE!")
        return(TRUE) # Destroys window under tcltk, but not RGtk2.
      } else {
        # Version > 1.0.6 will be fixed:
        # https://github.com/jverzani/gWidgets2tcltk/commit/9388900afc57454b6521b00a187ca4a16829df53
        message("tcltk version >1.0.6, returned FALSE!")
        return(FALSE) # Destroys window under tcltk, but not RGtk2.
      }
    } else {
      message("RGtk2, returned FALSE!")
      return(FALSE) # Destroys window under RGtk2, but not with tcltk.
    }
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 15,
    use.scrollwindow = FALSE,
    container = w,
    expand = FALSE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = strChkGui, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # DATASET ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 10,
    container = gv
  )


  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1, 1] <- glabel(text = strLblDataset, container = f0g0)

  f0g0[1, 2] <- f0g0_data_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none"
  )

  f0g0[1, 3] <- f0g0_data_col_lbl <- glabel(
    text = paste(" 0", strLblRows),
    container = f0g0
  )

  addHandlerChanged(f0g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Size", "File.Name")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      svalue(f0g0_data_col_lbl) <- paste(" ", nrow(.gData), " rows")
      svalue(f2_name) <- paste(.gDataName, "no_spikes", sep = "_")
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- paste(" 0", strLblRows)
      svalue(f2_name) <- ""
    }
  })

  f0g0[2, 1] <- glabel(text = strLblSpikes, container = f0g0)

  f0g0[2, 2] <- f0g0_spike_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none"
  )

  f0g0[2, 3] <- f0g0_spike_col_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g0
  )

  addHandlerChanged(f0g0_spike_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_spike_drp)

    # Check if suitable.
    requiredCol <- c("Allele", "Id", "Marker")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gSpike <<- get(val_obj, envir = env)
      .gSpikeName <<- val_obj

      svalue(f0g0_spike_col_lbl) <- paste(
        "", length(unique(.gSpike$Id)),
        strLblSamples
      )
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- paste(" 0", strLblSamples)
    }
  })

  # OPTIONS ###################################################################

  f1 <- gframe(text = strFrmOptions, horizontal = FALSE, spacing = 10, container = gv)

  f1_invert_chk <- gcheckbox(
    text = strChkInvert,
    checked = FALSE, container = f1
  )


  # NAME ######################################################################

  f2 <- gframe(
    text = strFrmSave,
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = strLblSave, container = f2)
  f2_name <- gedit(text = "", width = 40, container = f2, expand = TRUE)

  # BUTTON ####################################################################

  remove_btn <- gbutton(text = strBtnRemove, container = gv)

  addHandlerChanged(remove_btn, handler = function(h, ...) {
    val_data <- .gData
    val_spike <- .gSpike
    val_name_data <- .gDataName
    val_name_spike <- .gSpikeName
    val_name <- svalue(f2_name)
    val_invert <- svalue(f1_invert_chk)

    if ((!is.na(val_data) && !is.null(val_data)) &
      (!is.na(val_spike) && !is.null(val_spike))) {
      datanew <- removeSpike(
        data = val_data, spike = val_spike,
        invert = val_invert, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "spike", "invert")

      values <- list(val_name_data, val_name_spike, val_invert)

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(datanew)
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strMsgNull,
        title = strMsgTitleError,
        icon = "error"
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .loadSavedSettings <- function() {

    # First check status of save flag.
    if (!is.null(savegui)) {
      svalue(savegui_chk) <- savegui
      enabled(savegui_chk) <- FALSE
      if (debug) {
        print("Save GUI status set!")
      }
    } else {
      # Load save flag.
      if (exists(".strvalidator_removeSpike_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_removeSpike_gui_savegui", envir = env)
      }
      if (debug) {
        print("Save GUI status loaded!")
      }
    }
    if (debug) {
      print(svalue(savegui_chk))
    }

    # Then load settings if true.
    if (svalue(savegui_chk)) {
      if (exists(".strvalidator_removeSpike_gui_invert", envir = env, inherits = FALSE)) {
        svalue(f1_invert_chk) <- get(".strvalidator_removeSpike_gui_invert", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_removeSpike_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_removeSpike_gui_invert", value = svalue(f1_invert_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_removeSpike_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_removeSpike_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_removeSpike_gui_invert", envir = env, inherits = FALSE)) {
        remove(".strvalidator_removeSpike_gui_invert", envir = env)
      }

      if (debug) {
        print("Settings cleared!")
      }
    }

    if (debug) {
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
