################################################################################
# CHANGE LOG (last 20 changes)
# 20.06.2023: Fixed Error in !is.null(val_data) && !is.na(val_data) in coercion to 'logical(1)
# 03.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 03.03.2020: Fixed reference to function name.
# 25.02.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
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


calculateAllele_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate summary statistics for alleles"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblRows <- "rows"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strLblThreshold <- "Peak height threshold: "
  strTipThreshold <- "Peaks with heights below this value will be removed."
  strChkSexMarkers <- "Remove sex markers defined in kit."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strMsgNotDf <- "Data set must be a data.frame!"
  strMsgTitleError <- "Error"

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

    strtmp <- dtStrings["strDrpDefault"]$value
    strDrpDefault <- ifelse(is.na(strtmp), strDrpDefault, strtmp)

    strtmp <- dtStrings["strLblRows"]$value
    strLblRows <- ifelse(is.na(strtmp), strLblRows, strtmp)

    strtmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblThreshold"]$value
    strLblThreshold <- ifelse(is.na(strtmp), strLblThreshold, strtmp)

    strtmp <- dtStrings["strTipThreshold"]$value
    strTipThreshold <- ifelse(is.na(strtmp), strTipThreshold, strtmp)

    strtmp <- dtStrings["strChkSexMarkers"]$value
    strChkSexMarkers <- ifelse(is.na(strtmp), strChkSexMarkers, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strMsgNotDf"]$value
    strMsgNotDf <- ifelse(is.na(strtmp), strMsgNotDf, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)
  }

  # ---------------------------------------------------------------------------

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

    return(FALSE) # Destroy window.
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

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv,
    expand = FALSE,
    fill = "x"
  )

  # DATASET -------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g0)

  f0g0_data_col_lbl <- glabel(
    text = paste(" 0", strLblRows),
    container = f0g0
  )

  f0g0_data_drp <- gcombobox(
    items = c(
      strDrpDefault,
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

  addHandlerChanged(f0g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_data_drp)

    # Check if suitable.
    requiredCol <- c("Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      svalue(f0g0_data_col_lbl) <- paste(" ", nrow(.gData), strLblRows)
      svalue(save_edt) <- paste(.gDataName, "allele", sep = "_")

      # Autodetect kit.
      svalue(kit_drp) <- detectKit(
        data = .gData, index = FALSE,
        debug = debug
      )[1]
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- paste(" 0", strLblRows)
      svalue(save_edt) <- ""
    }
  })

  # KIT -----------------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = f0g2)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = f0g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # OPTIONS ###################################################################

  f1 <- gframe(text = strFrmOptions, horizontal = FALSE, spacing = 1, container = gv)

  f1g1 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")

  f1_threshold_lbl <- glabel(
    text = strLblThreshold,
    container = f1g1
  )

  f1_threshold_edt <- gedit(text = "", width = 10, container = f1g1)
  tooltip(f1_threshold_edt) <- strTipThreshold

  f1g2 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")

  f1_sex_chk <- gcheckbox(
    text = strChkSexMarkers,
    checked = FALSE, container = f1g2
  )

  addHandlerChanged(f1_sex_chk, handler = function(h, ...) {
    .widgetState()
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  button_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerChanged(button_btn, handler = function(h, ...) {
    val_data <- .gData
    val_name_data <- .gDataName
    val_name <- svalue(save_edt)
    val_threshold <- as.numeric(svalue(f1_threshold_edt))
    val_sex <- svalue(f1_sex_chk)
    val_kit <- svalue(kit_drp)

    if (is.data.frame(val_data)) {
      # Check status to set correct values for arguments.
      if (!val_sex) {
        # If no filtering of sex markers kit should be NULL.
        val_kit <- NULL
      }
      if (is.na(val_threshold)) {
        # If missing threshold should be NULL.
        val_threshold <- NULL
      }

      datanew <- calculateAllele(
        data = val_data, threshold = val_threshold,
        sex.rm = val_sex, kit = val_kit, debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list("data", "threshold", "sex", "kit")

      values <- list(val_name_data, val_threshold, val_sex, val_kit)

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

  .widgetState <- function() {
    val_obj <- svalue(f1_sex_chk)

    if (val_obj) {
      enabled(kit_drp) <- TRUE
    } else {
      enabled(kit_drp) <- FALSE
    }
  }

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
      if (exists(".strvalidator_calculateAllele_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateAllele_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateAllele_gui_threshold", envir = env, inherits = FALSE)) {
        svalue(f1_threshold_edt) <- get(".strvalidator_calculateAllele_gui_threshold", envir = env)
      }
      if (exists(".strvalidator_calculateAllele_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateAllele_gui_sex", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateAllele_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateAllele_gui_threshold", value = svalue(f1_threshold_edt), envir = env)
      assign(x = ".strvalidator_calculateAllele_gui_sex", value = svalue(f1_sex_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateAllele_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllele_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateAllele_gui_threshold", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllele_gui_threshold", envir = env)
      }
      if (exists(".strvalidator_calculateAllele_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllele_gui_sex", envir = env)
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

  # Initiate widgets.
  .widgetState()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
} # End of GUI
