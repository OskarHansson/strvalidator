################################################################################
# CHANGE LOG (last 20 changes)
# 20.06.2023: Fixed Error in !is.null(val_data) && !is.na(val_data) in coercion to 'logical(1)
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
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
  strMsgNotDf <- "Data set must be a data.frame!"
  strMsgTitleError <- "Error"
  strBtnRemove <- "Remove"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.null(dtStrings)) {
    # Get language strings, use default if not found.

    strtmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)

    strtmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)

    strtmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strtmp <- dtStrings["strFrmDataset"]$value
    strFrmDataset <- ifelse(is.na(strtmp), strFrmDataset, strtmp)

    strtmp <- dtStrings["strLblDataset"]$value
    strLblDataset <- ifelse(is.na(strtmp), strLblDataset, strtmp)

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strtmp <- dtStrings["strLblRows"]$value
    strLblRows <- ifelse(is.na(strtmp), strLblRows, strtmp)

    strtmp <- dtStrings["strLblSpikes"]$value
    strLblSpikes <- ifelse(is.na(strtmp), strLblSpikes, strtmp)

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkInvert"]$value
    strChkInvert <- ifelse(is.na(strtmp), strChkInvert, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strMsgNotDf"]$value
    strMsgNotDf <- ifelse(is.na(strtmp), strMsgNotDf, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strBtnRemove"]$value
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

    # Destroy window.
    return(FALSE)
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 1,
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
    spacing = 1,
    container = gv
  )

  # Datasets ------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g0)

  data_col_lbl <- glabel(
    text = paste(" 0", strLblRows),
    container = f0g0
  )

  data_drp <- gcombobox(
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
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)

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

      svalue(data_col_lbl) <- paste(" ", nrow(.gData), " rows")
      svalue(save_edt) <- paste(.gDataName, "no_spikes", sep = "_")
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_col_lbl) <- paste(" 0", strLblRows)
      svalue(save_edt) <- ""
    }
  })

  # Spikes --------------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblSpikes, container = f0g1)

  spike_col_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g1
  )

  spike_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g1,
    ellipsize = "none"
  )

  # Handlers ------------------------------------------------------------------

  addHandlerChanged(spike_drp, handler = function(h, ...) {
    val_obj <- svalue(spike_drp)

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

      svalue(spike_col_lbl) <- paste(
        "", length(unique(.gSpike$Id)),
        strLblSamples
      )
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_col_lbl) <- paste(" 0", strLblSamples)
    }
  })

  # OPTIONS ###################################################################

  f1 <- gframe(text = strFrmOptions, horizontal = FALSE, spacing = 1, container = gv)

  f1_invert_chk <- gcheckbox(
    text = strChkInvert,
    checked = FALSE, container = f1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  remove_btn <- gbutton(text = strBtnRemove, container = gv)

  addHandlerChanged(remove_btn, handler = function(h, ...) {
    val_data <- .gData
    val_spike <- .gSpike
    val_name_data <- .gDataName
    val_name_spike <- .gSpikeName
    val_name <- svalue(save_edt)
    val_invert <- svalue(f1_invert_chk)

    if (is.data.frame(val_data) & is.data.frame(val_spike)) {
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
        msg = strMsgNotDf,
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
