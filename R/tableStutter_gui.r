################################################################################
# CHANGE LOG (last 20 changes)
# 07.06.2020: Added .Deprecated("calculateStatistics_gui")
# 03.05.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 01.08.2017: Added attributes to result.
# 31.07.2017: Save name text field added expand=TRUE.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 29.08.2015: Added importFrom.
# 07.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 10.06.2013: Fixed save GUI settings. New name sufix.
# 06.06.2013: Added save GUI settings.
# 04.06.2013: Fixed bug in 'missingCol'.
# 29.05.2013: Disabled button and adding "processing..." after press.

#' @title Table Stutter
#'
#' @description
#' GUI wrapper for the \code{\link{tableStutter}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{tableStutter}} function by providing a graphical
#' user interface to it.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help
#'
#' @seealso \code{\link{tableStutter}}

tableStutter_gui <- function(env = parent.frame(), savegui = NULL,
                             debug = FALSE, parent = NULL) {
  .Deprecated("calculateStatistics_gui")

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
  strWinTitle <- "Calculate summary statistics for stutters"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strLblQuantile <- "Calculate quantile"
  strLblBy <- "Statistics to calculate"
  strRadGlobal <- "global"
  strRadMarker <- "marker"
  strRadStutter <- "stutter"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgNull <- "Data frame is NULL or NA!"
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

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblQuantile"]$value
    strLblQuantile <- ifelse(is.na(strtmp), strLblQuantile, strtmp)

    strtmp <- dtStrings["strLblBy"]$value
    strLblBy <- ifelse(is.na(strtmp), strLblBy, strtmp)

    strtmp <- dtStrings["strRadGlobal"]$value
    strRadGlobal <- ifelse(is.na(strtmp), strRadGlobal, strtmp)

    strtmp <- dtStrings["strRadMarker"]$value
    strRadMarker <- ifelse(is.na(strtmp), strRadMarker, strtmp)

    strtmp <- dtStrings["strRadStutter"]$value
    strRadStutter <- ifelse(is.na(strtmp), strRadStutter, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strMsgNull"]$value
    strMsgNull <- ifelse(is.na(strtmp), strMsgNull, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)
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
    spacing = 10,
    container = gv
  )

  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1, 1] <- glabel(text = strLblDataset, container = f0g0)

  f0g0[1, 2] <- f0g0_dataset_drp <- gcombobox(
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

  f0g0[1, 3] <- f0g0_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g0
  )

  addHandlerChanged(f0g0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_dataset_drp)

    # Check if suitable.
    requiredCol <- c("Ratio", "Marker", "Allele", "Type")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(f0g0_samples_lbl) <- paste(" ", samples, strLblSamples)
      svalue(save_edt) <- paste(.gDataName,
        "_stat_",
        svalue(f1g1_scope_opt),
        sep = ""
      )
    } else {

      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(f0g0_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
      svalue(f0g0_dataset_drp, index = TRUE) <- 1
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 20,
    container = gv
  )

  f1g1 <- glayout(container = f1, spacing = 5)

  f1g1[1, 1] <- glabel(text = strLblQuantile, container = f1g1)

  f1g1[1, 2] <- f1g1_quant_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = 0.95,
    container = f1g1
  )

  f1g1[2, 1] <- glabel(text = strLblBy, container = f1g1)

  f1g1[3, 1] <- f1g1_scope_opt <- gradio(
    items = c(strRadGlobal, strRadMarker, strRadStutter),
    selected = 3,
    horizontal = FALSE,
    container = f1g1
  )

  addHandlerChanged(f1g1_scope_opt, handler = function(h, ...) {
    svalue(save_edt) <- paste(.gDataName,
      "_stat_",
      svalue(f1g1_scope_opt),
      sep = ""
    )
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  run_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(run_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_data_name <- .gDataName
    val_quant <- as.numeric(svalue(f1g1_quant_spb))
    val_scope <- svalue(f1g1_scope_opt)
    val_name <- svalue(save_edt)
    val_kit <- attr(x = .gData, which = "kit", exact = TRUE)

    if (!is.null(.gData)) {

      # Change button.
      blockHandlers(run_btn)
      svalue(run_btn) <- strBtnProcessing
      unblockHandlers(run_btn)
      enabled(run_btn) <- FALSE

      datanew <- tableStutter(
        data = val_data,
        quant = val_quant,
        scope = val_scope
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list("data", "quant", "scope")

      values <- list(val_data_name, val_quant, val_scope)

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

    # Set check state if provided.
    if (!is.null(savegui)) {
      svalue(savegui_chk) <- savegui
      enabled(savegui_chk) <- FALSE
      if (debug) {
        print("Save GUI status set!")
      }
    } else {
      # Load save flag.
      if (exists(".strvalidator_tableStutter_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_tableStutter_gui_savegui", envir = env)
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
      if (exists(".strvalidator_tableStutter_gui_quant", envir = env, inherits = FALSE)) {
        svalue(f1g1_quant_spb) <- get(".strvalidator_tableStutter_gui_quant", envir = env)
      }
      if (exists(".strvalidator_tableStutter_gui_scope", envir = env, inherits = FALSE)) {
        svalue(f1g1_scope_opt) <- get(".strvalidator_tableStutter_gui_scope", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_tableStutter_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_tableStutter_gui_quant", value = svalue(f1g1_quant_spb), envir = env)
      assign(x = ".strvalidator_tableStutter_gui_scope", value = svalue(f1g1_scope_opt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_tableStutter_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_tableStutter_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_tableStutter_gui_quant", envir = env, inherits = FALSE)) {
        remove(".strvalidator_tableStutter_gui_quant", envir = env)
      }
      if (exists(".strvalidator_tableStutter_gui_scope", envir = env, inherits = FALSE)) {
        remove(".strvalidator_tableStutter_gui_scope", envir = env)
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
