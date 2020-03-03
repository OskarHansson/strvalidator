################################################################################
# CHANGE LOG (last 20 changes)
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
  strFrmOptions <- "Options"
  strLblThreshold <- "Peak height threshold: "
  strTipThreshold <- "Peaks with heights below this value will be removed."
  strChkSexMarkers <- "Remove sex markers defined in kit: "
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strMsgMessage <- "Select a dataset!"
  strMsgTitle <- "Error"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.na(dtStrings)) {
    # Get language strings, use default if not found.

    strTmp <- dtStrings["strWinTitle"]$Value
    strWinTitle <- ifelse(is.na(strTmp), strWinTitle, strTmp)

    strTmp <- dtStrings["strChkGui"]$Value
    strChkGui <- ifelse(is.na(strTmp), strChkGui, strTmp)

    strTmp <- dtStrings["strBtnHelp"]$Value
    strBtnHelp <- ifelse(is.na(strTmp), strBtnHelp, strTmp)

    strTmp <- dtStrings["strFrmDataset"]$Value
    strFrmDataset <- ifelse(is.na(strTmp), strFrmDataset, strTmp)

    strTmp <- dtStrings["strLblDataset"]$Value
    strLblDataset <- ifelse(is.na(strTmp), strLblDataset, strTmp)

    strTmp <- dtStrings["strDrpDefault"]$Value
    strDrpDefault <- ifelse(is.na(strTmp), strDrpDefault, strTmp)

    strTmp <- dtStrings["strLblRows"]$Value
    strLblRows <- ifelse(is.na(strTmp), strLblRows, strTmp)

    strTmp <- dtStrings["strFrmOptions"]$Value
    strFrmOptions <- ifelse(is.na(strTmp), strFrmOptions, strTmp)

    strTmp <- dtStrings["strLblThreshold"]$Value
    strLblThreshold <- ifelse(is.na(strTmp), strLblThreshold, strTmp)

    strTmp <- dtStrings["strTipThreshold"]$Value
    strTipThreshold <- ifelse(is.na(strTmp), strTipThreshold, strTmp)

    strTmp <- dtStrings["strChkSexMarkers"]$Value
    strChkSexMarkers <- ifelse(is.na(strTmp), strChkSexMarkers, strTmp)

    strTmp <- dtStrings["strFrmSave"]$Value
    strFrmSave <- ifelse(is.na(strTmp), strFrmSave, strTmp)

    strTmp <- dtStrings["strLblSave"]$Value
    strLblSave <- ifelse(is.na(strTmp), strLblSave, strTmp)

    strTmp <- dtStrings["strBtnCalculate"]$Value
    strBtnCalculate <- ifelse(is.na(strTmp), strBtnCalculate, strTmp)

    strTmp <- dtStrings["strMsgMessage"]$Value
    strMsgMessage <- ifelse(is.na(strTmp), strMsgMessage, strTmp)

    strTmp <- dtStrings["strMsgTitle"]$Value
    strMsgTitle <- ifelse(is.na(strTmp), strMsgTitle, strTmp)
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
      strDrpDefault,
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
      svalue(f2_name) <- paste(.gDataName, "allele", sep = "_")

      # Autodetect kit.
      svalue(f1_kit_drp) <- detectKit(
        data = .gData, index = FALSE,
        debug = debug
      )[1]
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- paste(" 0", strLblRows)
      svalue(f2_name) <- ""
    }
  })

  # OPTIONS ###################################################################

  f1 <- gframe(text = strFrmOptions, horizontal = FALSE, spacing = 10, container = gv)

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- f1_threshold_lbl <- glabel(
    text = strLblThreshold,
    container = f1g1
  )
  f1g1[1, 2] <- f1_threshold_edt <- gedit(text = "", width = 10, container = f1g1)
  tooltip(f1_threshold_edt) <- strTipThreshold

  f1g1[2, 1:2] <- f1_sex_chk <- gcheckbox(
    text = strChkSexMarkers,
    checked = FALSE, container = f1g1
  )

  f1g1[2, 3] <- f1_kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f1g1,
    ellipsize = "none"
  )

  addHandlerChanged(f1_sex_chk, handler = function(h, ...) {
    val_obj <- svalue(f1_sex_chk)

    if (val_obj) {
      enabled(f1_kit_drp) <- TRUE
    } else {
      enabled(f1_kit_drp) <- FALSE
    }
  })

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

  button_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerChanged(button_btn, handler = function(h, ...) {
    val_data <- .gData
    val_name_data <- .gDataName
    val_name <- svalue(f2_name)
    val_threshold <- as.numeric(svalue(f1_threshold_edt))
    val_sex <- svalue(f1_sex_chk)
    val_kit <- svalue(f1_kit_drp)

    if (!is.na(val_data) && !is.null(val_data)) {

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
        msg = strMsgMessage,
        title = strMsgTitle,
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

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
} # End of GUI
