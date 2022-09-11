################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 07.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 19.05.2016: Implemented more accurat method and parameter 'quick'.
# 18.05.2016: Changed default of 'Round to nearest (bp)' from 1 to 1.5.
#             Also changed min from 1 to 0 and step by from 1 to 0.1.
# 12.10.2015: First version.

#' @title Detect Spike
#'
#' @description
#' GUI wrapper for the \code{\link{calculateSpike}} function.
#'
#' @details Simplifies the use of the \code{\link{calculateSpike}} function
#' by providing a graphical user interface.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @seealso \code{\link{calculateSpike}}

calculateSpike_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  .gData <- NULL
  .gDataName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate spikes"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Sample dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strLblThreshold <- "Threshold (number of peaks at similar size):"
  strLblTolerance <- "Tolerance (bp):"
  strChkQuick <- "Quick and dirty"
  strTipQuick <- "NB! The quick method may not catch all spikes since two peaks can be separated by rounding e.g. 200.5 and 200.6 becomes 200 and 201 respectively!"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A sample dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"

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

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblThreshold"]$value
    strLblThreshold <- ifelse(is.na(strtmp), strLblThreshold, strtmp)

    strtmp <- dtStrings["strLblTolerance"]$value
    strLblTolerance <- ifelse(is.na(strtmp), strLblTolerance, strtmp)

    strtmp <- dtStrings["strChkQuick"]$value
    strChkQuick <- ifelse(is.na(strtmp), strChkQuick, strtmp)

    strtmp <- dtStrings["strTipQuick"]$value
    strTipQuick <- ifelse(is.na(strtmp), strTipQuick, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strMsgDataset"]$value
    strMsgDataset <- ifelse(is.na(strtmp), strMsgDataset, strtmp)

    strtmp <- dtStrings["strMsgTitleDataset"]$value
    strMsgTitleDataset <- ifelse(is.na(strtmp), strMsgTitleDataset, strtmp)
  }

  # WINDOW ####################################################################

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

  gv <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = w,
    expand = TRUE
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

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # Datasets ------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

  samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = g0
  )

  dataset_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # Kit -----------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = g1)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "File.Name", "Size")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Size",
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(samples_lbl) <- paste("", samples, strLblSamples)
      svalue(f2_save_edt) <- paste(.gDataName, "_spikes", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste(" 0", strLblSamples)
      svalue(f2_save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(
    text = strLblThreshold,
    container = f1g1
  )
  f1g1[1, 2] <- f1_threshold_spn <- gspinbutton(
    from = 1, to = 10, by = 1,
    value = 3, container = f1g1
  )

  f1g1[2, 1] <- glabel(text = strLblTolerance, container = f1g1)
  f1g1[2, 2] <- f1_tolerance_spn <- gspinbutton(
    from = 0, to = 10, by = 0.1,
    value = 2, container = f1g1
  )

  f1_quick_chk <- gcheckbox(
    text = strChkQuick, checked = FALSE,
    container = f1
  )
  tooltip(f1_quick_chk) <- strTipQuick

  # FRAME 2 ###################################################################

  f2 <- gframe(text = strFrmSave, horizontal = TRUE, spacing = 1, container = gv)

  glabel(text = strLblSave, container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE, fill = TRUE)

  # BUTTON ####################################################################


  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_name_data <- .gDataName
    val_threshold <- svalue(f1_threshold_spn)
    val_tolerance <- svalue(f1_tolerance_spn)
    val_kit <- svalue(kit_drp)
    val_name <- svalue(f2_save_edt)
    val_quick <- svalue(f1_quick_chk)

    if (!is.null(val_data)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateSpike(
        data = val_data,
        threshold = val_threshold,
        tolerance = val_tolerance,
        kit = val_kit,
        quick = val_quick,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "threshold", "tolerance",
        "quick", "kit"
      )

      values <- list(
        val_name_data, val_threshold, val_tolerance,
        val_quick, val_kit
      )

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(str(datanew))
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strMsgDataset,
        title = strMsgTitleDataset,
        icon = "error",
        parent = w
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
      if (exists(".strvalidator_calculateSpike_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateSpike_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateSpike_gui_threshold", envir = env, inherits = FALSE)) {
        svalue(f1_threshold_spn) <- get(".strvalidator_calculateSpike_gui_threshold", envir = env)
      }
      if (exists(".strvalidator_calculateSpike_gui_tolerance", envir = env, inherits = FALSE)) {
        svalue(f1_tolerance_spn) <- get(".strvalidator_calculateSpike_gui_tolerance", envir = env)
      }
      if (exists(".strvalidator_calculateSpike_gui_quick", envir = env, inherits = FALSE)) {
        svalue(f1_quick_chk) <- get(".strvalidator_calculateSpike_gui_quick", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateSpike_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateSpike_gui_threshold", value = svalue(f1_threshold_spn), envir = env)
      assign(x = ".strvalidator_calculateSpike_gui_tolerance", value = svalue(f1_tolerance_spn), envir = env)
      assign(x = ".strvalidator_calculateSpike_gui_quick", value = svalue(f1_quick_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateSpike_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSpike_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateSpike_gui_threshold", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSpike_gui_threshold", envir = env)
      }
      if (exists(".strvalidator_calculateSpike_gui_tolerance", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSpike_gui_tolerance", envir = env)
      }
      if (exists(".strvalidator_calculateSpike_gui_quick", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSpike_gui_quick", envir = env)
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
}
