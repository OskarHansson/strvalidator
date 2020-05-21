################################################################################
# CHANGE LOG (last 20 changes)
# 04.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 18.09.2016: Added attributes to result.
# 18.09.2016: Changed labels, added tooltips, changed one widget.
# 29.04.2016: 'Save as' textbox expandable.
# 27.08.2015: Updated text for option to calculate per marker.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 11.01.2014: First version.

#' @title Calculate Peaks
#'
#' @description
#' GUI wrapper for the \code{\link{calculatePeaks}} function.
#'
#' @details Counts the number of peaks in samples and markers with option to
#'  discard off-ladder peaks and to label groups according to maximum number
#'  of peaks.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @seealso \code{\link{calculatePeaks}}

calculatePeaks_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  .gData <- NULL
  .gDataName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate peaks"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strChkExcludeOL <- "Exclude off-ladder peaks (OL alleles)."
  strRadBySample <- "Count peaks by sample"
  strRadByMarker <- "Count peaks by marker"
  strLblGroups <- "Define group labels (separated by comma):"
  strTipGroups <- "Number labels must be one more than the number of cut-off points. The last group is defined by > than the last cut-off point."
  strLblCutOff <- "Define cut-off points (<=) for the groups (separated by comma):"
  strTipCutOff <- "Number of cut-off points must be one less than the number of group labels. The last group is defined by > than the last cut-off point."
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

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strDrpDefault"]$value
    strDrpDefault <- ifelse(is.na(strtmp), strDrpDefault, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkExcludeOL"]$value
    strChkExcludeOL <- ifelse(is.na(strtmp), strChkExcludeOL, strtmp)

    strtmp <- dtStrings["strRadBySample"]$value
    strRadBySample <- ifelse(is.na(strtmp), strRadBySample, strtmp)

    strtmp <- dtStrings["strRadByMarker"]$value
    strRadByMarker <- ifelse(is.na(strtmp), strRadByMarker, strtmp)

    strtmp <- dtStrings["strLblGroups"]$value
    strLblGroups <- ifelse(is.na(strtmp), strLblGroups, strtmp)

    strtmp <- dtStrings["strTipGroups"]$value
    strTipGroups <- ifelse(is.na(strtmp), strTipGroups, strtmp)

    strtmp <- dtStrings["strLblCutOff"]$value
    strLblCutOff <- ifelse(is.na(strtmp), strLblCutOff, strtmp)

    strtmp <- dtStrings["strTipCutOff"]$value
    strTipCutOff <- ifelse(is.na(strtmp), strTipCutOff, strtmp)

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

  gv <- ggroup(
    horizontal = FALSE,
    spacing = 8,
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
    spacing = 5,
    container = gv
  )

  g0 <- glayout(container = f0, spacing = 1)

  # Datasets ------------------------------------------------------------------

  g0[1, 1] <- glabel(text = strLblDataset, container = g0)

  g0[1, 2] <- g0_dataset_drp <- gcombobox(
    items = c(
      strDrpDefault,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )

  g0[1, 3] <- g0_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = g0
  )

  addHandlerChanged(g0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Height")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Height",
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, strLblSamples)
      svalue(save_edt) <- paste(.gDataName, "_peaks", sep = "")
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(g0_dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f1_no_ol_chk <- gcheckbox(
    text = strChkExcludeOL,
    checked = FALSE, container = f1
  )

  f1_count_by_opt <- gradio(
    items = c(strRadBySample, strRadByMarker),
    selected = 1, container = f1
  )

  glabel(
    text = strLblGroups,
    container = f1, anchor = c(-1, 0)
  )
  f1_labels_edt <- gedit(
    text = "No contamination,Drop-in contamination,Gross contamination",
    width = 60, container = f1
  )
  tooltip(f1_labels_edt) <- strTipGroups

  glabel(text = strLblCutOff, container = f1, anchor = c(-1, 0))
  f1_bins_edt <- gedit(text = "0,2", width = 60, container = f1)
  tooltip(f1_bins_edt) <- strTipGroups

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################


  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_name_data <- .gDataName
    val_no_ol <- svalue(f1_no_ol_chk)
    val_per_marker <- ifelse(svalue(f1_count_by_opt, index = TRUE) == 1, FALSE, TRUE)
    val_labels <- svalue(f1_labels_edt)
    val_bins <- svalue(f1_bins_edt)
    val_name <- svalue(save_edt)

    # countPeaks require a vectors.
    val_labels <- unlist(strsplit(val_labels, ",", fixed = TRUE))
    val_bins <- as.numeric(unlist(strsplit(val_bins, ",", fixed = TRUE)))

    if (length(val_labels) == 0) {
      val_labels <- NULL
    }

    if (!is.null(val_data)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculatePeaks(
        data = val_data,
        bins = val_bins,
        labels = val_labels,
        ol.rm = val_no_ol,
        by.marker = val_per_marker,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "bins", "labels", "ol.rm",
        "by.marker"
      )

      values <- list(
        val_name_data, val_bins, val_labels, val_no_ol,
        val_per_marker
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
      if (exists(".strvalidator_calculatePeaks_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculatePeaks_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculatePeaks_gui_nool", envir = env, inherits = FALSE)) {
        svalue(f1_no_ol_chk) <- get(".strvalidator_calculatePeaks_gui_nool", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_labels", envir = env, inherits = FALSE)) {
        svalue(f1_labels_edt) <- get(".strvalidator_calculatePeaks_gui_labels", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_bins", envir = env, inherits = FALSE)) {
        svalue(f1_bins_edt) <- get(".strvalidator_calculatePeaks_gui_bins", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_countby", envir = env, inherits = FALSE)) {
        svalue(f1_count_by_opt) <- get(".strvalidator_calculatePeaks_gui_countby", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculatePeaks_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculatePeaks_gui_nool", value = svalue(f1_no_ol_chk), envir = env)
      assign(x = ".strvalidator_calculatePeaks_gui_labels", value = svalue(f1_labels_edt), envir = env)
      assign(x = ".strvalidator_calculatePeaks_gui_bins", value = svalue(f1_bins_edt), envir = env)
      assign(x = ".strvalidator_calculatePeaks_gui_countby", value = svalue(f1_count_by_opt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculatePeaks_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_nool", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_nool", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_labels", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_labels", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_bins", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_bins", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_countby", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_countby", envir = env)
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
