################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 07.03.2020: Added language support.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 01.03.2019: Compacting gui and made text box expand (tcltk)
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 11.07.2018: Fixed field 'Fixed value' not always disabled when it should.
# 10.08.2017: Fixed column dropdowns lose selection after selecting dataset.
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 09.05.2016: Added attributes to result.
# 09.05.2016: 'Save as' textbox expandable.
# 22.12.2015: Removed colum check to enable no selected column etc.
# 28.08.2015: Added importFrom.
# 23.05.2015: First version.

#' @title Column Actions
#'
#' @description
#' GUI wrapper for the \code{\link{columns}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{columns}} function by providing a
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


columns_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

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
  strWinTitle <- "Column actions"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblColumns <- "columns"
  strFrmColumns <- "Columns"
  strLblCol1 <- "Column 1:"
  strLblCol2 <- "Column 2:"
  strDrpColumn <- "<Select column>"
  strFrmOptions <- "Options"
  strLblFixed <- "Fixed value:"
  strLblNew <- "Column for new values:"
  strLblAction <- "Action:"
  strDrpAction <- "<Select action>"
  strLblStart <- "Start position:"
  strLblStop <- "Stop position:"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Execute"

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

    strtmp <- dtStrings["strLblColumns"]$value
    strLblColumns <- ifelse(is.na(strtmp), strLblColumns, strtmp)

    strtmp <- dtStrings["strLblCol1"]$value
    strLblCol1 <- ifelse(is.na(strtmp), strLblCol1, strtmp)

    strtmp <- dtStrings["strLblCol2"]$value
    strLblCol2 <- ifelse(is.na(strtmp), strLblCol2, strtmp)

    strtmp <- dtStrings["strDrpColumn"]$value
    strDrpColumn <- ifelse(is.na(strtmp), strDrpColumn, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblFixed"]$value
    strLblFixed <- ifelse(is.na(strtmp), strLblFixed, strtmp)

    strtmp <- dtStrings["strLblNew"]$value
    strLblNew <- ifelse(is.na(strtmp), strLblNew, strtmp)

    strtmp <- dtStrings["strLblAction"]$value
    strLblAction <- ifelse(is.na(strtmp), strLblAction, strtmp)

    strtmp <- dtStrings["strDrpAction"]$value
    strDrpAction <- ifelse(is.na(strtmp), strDrpAction, strtmp)

    strtmp <- dtStrings["strLblStart"]$value
    strLblStart <- ifelse(is.na(strtmp), strLblStart, strtmp)

    strtmp <- dtStrings["strLblStop"]$value
    strLblStop <- ifelse(is.na(strtmp), strLblStop, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)
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

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g0)

  data_col_lbl <- glabel(
    text = paste(" 0", strLblColumns),
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
    ok <- is.data.frame(get(val_obj, envir = env))

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      svalue(data_col_lbl) <- paste(" ", ncol(.gData), strLblColumns)
      svalue(save_edt) <- paste(.gDataName, "new", sep = "_")

      col1_drp[] <- c(strDrpColumn, names(.gData))
      svalue(col1_drp, index = TRUE) <- 1
      col2_drp[] <- c(strDrpColumn, names(.gData))
      svalue(col2_drp, index = TRUE) <- 1
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_col_lbl) <- paste(" 0", strLblColumns)
      svalue(save_edt) <- ""

      col1_drp[] <- c(strDrpColumn)
      svalue(col1_drp, index = TRUE) <- 1
      col2_drp[] <- c(strDrpColumn)
      svalue(col2_drp, index = TRUE) <- 1
    }
  })

  # COLUMNS ###################################################################

  f1 <- gframe(text = strFrmColumns, horizontal = FALSE, spacing = 1, container = gv)

  f1g1 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblCol1, container = f1g1)

  col1_drp <- gcombobox(
    items = c(strDrpColumn),
    editable = FALSE,
    container = f1g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  f1g2 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblCol2, container = f1g2)

  col2_drp <- gcombobox(
    items = c(strDrpColumn),
    editable = FALSE,
    container = f1g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(col1_drp, handler = function(h, ...) {
    val_col <- svalue(col1_drp)

    # Check if column exist.
    ok <- val_col %in% names(.gData)

    # Update target column.
    if (length(ok) > 0) {
      if (ok) {
        svalue(f3g1_col_edt) <- val_col
      } else {
        svalue(f3g1_col_edt) <- ""
      }
    }
  })

  addHandlerChanged(col2_drp, handler = function(h, ...) {
    if (svalue(col2_drp) %in% names(.gData)) {
      # If an existing colum gets selected.

      if (svalue(f3g1_action_drp) %in% "substr") {
        # Check if 'substr' is selected.

        # Reset action dropdown.
        svalue(f3g1_action_drp, index = TRUE) <- 1
      }
    }

    .updateGui()
  })

  # OPTIONS ###################################################################

  f3 <- gframe(
    text = strFrmOptions, horizontal = FALSE, spacing = 1,
    container = gv
  )

  f3g1 <- glayout(container = f3, spacing = 1)

  f3g1[1, 1] <- glabel(text = strLblFixed, container = f3g1)

  f3g1[1, 2] <- f3g1_val_edt <- gedit(text = "", width = 25, container = f3g1)

  f3g1[2, 1] <- glabel(text = strLblNew, container = f3g1)

  f3g1[2, 2] <- f3g1_col_edt <- gedit(text = "", width = 25, container = f3g1)

  f3g1[3, 1] <- glabel(text = strLblAction, container = f3g1)

  action_items <- c(strDrpAction, "&", "+", "*", "-", "/", "substr")
  f3g1[3, 2] <- f3g1_action_drp <- gcombobox(
    items = action_items, selected = 1,
    editable = FALSE, container = f3g1,
    ellipsize = "none"
  )

  f3g1[4, 1] <- glabel(text = strLblStart, container = f3g1)

  f3g1[4, 2] <- f3g1_start_edt <- gedit(text = "1", width = 25, container = f3g1)

  f3g1[5, 1] <- glabel(text = strLblStop, container = f3g1)

  f3g1[5, 2] <- f3g1_stop_edt <- gedit(text = "1", width = 25, container = f3g1)


  addHandlerChanged(f3g1_action_drp, handler = function(h, ...) {
    if (svalue(f3g1_action_drp) %in% "substr") {

      # Reset column 2 if 'substr' is selected.
      svalue(col2_drp, index = TRUE) <- 1
    }

    .updateGui()
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  combine_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerChanged(combine_btn, handler = function(h, ...) {
    val_col1 <- svalue(col1_drp)
    val_col2 <- svalue(col2_drp)
    val_action <- svalue(f3g1_action_drp)
    val_target <- svalue(f3g1_col_edt)
    val_fixed <- svalue(f3g1_val_edt)
    val_start <- as.integer(svalue(f3g1_start_edt))
    val_stop <- as.integer(svalue(f3g1_stop_edt))
    val_name <- svalue(save_edt)
    val_data_name <- .gDataName
    val_data <- .gData

    # Check values.
    if (val_col1 == strDrpColumn) {
      val_col1 <- NA
    }
    if (val_col2 == strDrpColumn) {
      val_col2 <- NA
    }

    datanew <- columns(
      data = val_data, col1 = val_col1, col2 = val_col2,
      operator = val_action, fixed = val_fixed,
      target = val_target, start = val_start, stop = val_stop,
      debug = debug
    )

    # Create key-value pairs to log.
    keys <- list(
      "data", "col1", "col2", "action", "target",
      "fixed", "start", "stop"
    )

    values <- list(
      val_data_name, val_col1, val_col2, val_action, val_target,
      val_fixed, val_start, val_stop
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
      print(datanew)
      print(paste("EXIT:", fnc))
    }

    # Close GUI.
    .saveSettings()
    dispose(w)
  })

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {
    substr_selected <- svalue(f3g1_action_drp) %in% "substr"
    no_action <- svalue(f3g1_action_drp) %in% strDrpAction
    second_col_selected <- svalue(col2_drp) %in% names(.gData)

    if (substr_selected || second_col_selected) {
      enabled(f3g1_val_edt) <- FALSE
    } else {
      enabled(f3g1_val_edt) <- TRUE
    }

    if (substr_selected) {
      enabled(f3g1_start_edt) <- TRUE
      enabled(f3g1_stop_edt) <- TRUE
    } else {
      enabled(f3g1_start_edt) <- FALSE
      enabled(f3g1_stop_edt) <- FALSE
    }

    if (no_action) {
      enabled(combine_btn) <- FALSE
    } else {
      enabled(combine_btn) <- TRUE
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
      if (exists(".strvalidator_columns_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_columns_gui_savegui", envir = env)
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
      if (exists(".strvalidator_columns_gui_import_opt", envir = env, inherits = FALSE)) {
        svalue(import_opt) <- get(".strvalidator_columns_gui_import_opt", envir = env)
      }
      if (exists(".strvalidator_columns_gui_fixed", envir = env, inherits = FALSE)) {
        svalue(f3g1_val_edt) <- get(".strvalidator_columns_gui_fixed", envir = env)
      }
      if (exists(".strvalidator_columns_gui_action", envir = env, inherits = FALSE)) {
        svalue(f3g1_action_drp) <- get(".strvalidator_columns_gui_action", envir = env)
      }
      if (exists(".strvalidator_columns_gui_start", envir = env, inherits = FALSE)) {
        svalue(f3g1_start_edt) <- get(".strvalidator_columns_gui_start", envir = env)
      }
      if (exists(".strvalidator_columns_gui_stop", envir = env, inherits = FALSE)) {
        svalue(f3g1_stop_edt) <- get(".strvalidator_columns_gui_stop", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_columns_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_columns_gui_fixed", value = svalue(f3g1_val_edt), envir = env)
      assign(x = ".strvalidator_columns_gui_action", value = svalue(f3g1_action_drp), envir = env)
      assign(x = ".strvalidator_columns_gui_start", value = svalue(f3g1_start_edt), envir = env)
      assign(x = ".strvalidator_columns_gui_stop", value = svalue(f3g1_stop_edt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_columns_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_columns_gui_fixed", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_fixed", envir = env)
      }
      if (exists(".strvalidator_columns_gui_action", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_action", envir = env)
      }
      if (exists(".strvalidator_columns_gui_start", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_start", envir = env)
      }
      if (exists(".strvalidator_columns_gui_stop", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_stop", envir = env)
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
  .updateGui()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
} # End of GUI
