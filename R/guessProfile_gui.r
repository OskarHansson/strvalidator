################################################################################
# CHANGE LOG
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 16.03.2020: Added language support.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 26.07.2017: Added expand=TRUE to save name text field.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 23.02.2014: Implemented new option 'ol.rm'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 10.06.2013: New parameter 'savegui'.
# 06.06.2013: Added save GUI settings.
# 04.06.2013: Fixed bug in 'missingCol'.
# 29.05.2013: Disabled button and adding "processing..." after press.
# 24.05.2013: Improved error message for missing columns.

#' @title Guess Profile
#'
#' @description
#' GUI wrapper for the \code{\link{guessProfile}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{guessProfile}} function by providing
#' a graphical user interface to it.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @return TRUE
#'
#' @seealso \code{\link{guessProfile}}, \code{\link{checkSubset}}

guessProfile_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

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
  strWinTitle <- "Guess profile"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Sample dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strLblRatio <- "Accepted peak height ratio >="
  strLblHeight <- "Accepted peak height >="
  strChkNA <- "Discard NA rows"
  strChkOL <- "Ignore off-ladder (OL) alleles"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnGuess <- "Guess"
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

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblRatio"]$value
    strLblRatio <- ifelse(is.na(strtmp), strLblRatio, strtmp)

    strtmp <- dtStrings["strLblHeight"]$value
    strLblHeight <- ifelse(is.na(strtmp), strLblHeight, strtmp)

    strtmp <- dtStrings["strChkNA"]$value
    strChkNA <- ifelse(is.na(strtmp), strChkNA, strtmp)

    strtmp <- dtStrings["strChkOL"]$value
    strChkOL <- ifelse(is.na(strtmp), strChkOL, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnGuess"]$value
    strBtnGuess <- ifelse(is.na(strtmp), strBtnGuess, strtmp)

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
  addHandlerDestroy(w, handler = function(h, ...) {

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

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g0)

  samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g0
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
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(samples_lbl) <- paste(" ", samples, strLblSamples)
      svalue(save_edt) <- paste(val_obj, "_profile", sep = "")
    } else {

      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
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

  f1g1[1, 1] <- glabel(text = strLblRatio, container = f1g1)

  f1g1[1, 2] <- f1g1_ratio_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = 0.6,
    container = f1g1
  )

  f1g1[2, 1] <- glabel(text = strLblHeight, container = f1g1)

  f1g1[2, 2] <- f1g1_height_edt <- gedit(text = "100", width = 6, container = f1g1)

  f1g1[3, 1] <- f1g1_na_chk <- gcheckbox(
    text = strChkNA,
    checked = FALSE,
    container = f1g1
  )

  f1g1[4, 1] <- f1g1_ol_chk <- gcheckbox(
    text = strChkOL,
    checked = FALSE,
    container = f1g1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  check_btn <- gbutton(text = strBtnGuess, container = gv)

  addHandlerClicked(check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_name_data <- .gDataName
    val_ratio <- as.numeric(svalue(f1g1_ratio_spb))
    val_height <- as.numeric(svalue(f1g1_height_edt))
    val_NA <- svalue(f1g1_na_chk)
    val_OL <- svalue(f1g1_ol_chk)
    val_name <- svalue(save_edt)

    if (is.na(val_height)) {
      val_height <- 0
    }

    if (!is.null(.gData)) {

      # Change button.
      blockHandlers(check_btn)
      svalue(check_btn) <- strBtnProcessing
      unblockHandlers(check_btn)
      enabled(check_btn) <- FALSE

      datanew <- guessProfile(
        data = val_data,
        ratio = val_ratio,
        height = val_height,
        na.rm = val_NA,
        ol.rm = val_OL,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "ratio", "height", "na.rm", "ol.rm")

      values <- list(val_name_data, val_ratio, val_height, val_NA, val_OL)

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
        msg = strMsgDataset,
        title = strMsgTitleDataset,
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
      if (exists(".strvalidator_guessProfile_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_guessProfile_gui_savegui", envir = env)
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
      if (exists(".strvalidator_guessProfile_gui_ratio", envir = env, inherits = FALSE)) {
        svalue(f1g1_ratio_spb) <- get(".strvalidator_guessProfile_gui_ratio", envir = env)
      }
      if (exists(".strvalidator_guessProfile_gui_height", envir = env, inherits = FALSE)) {
        svalue(f1g1_height_edt) <- get(".strvalidator_guessProfile_gui_height", envir = env)
      }
      if (exists(".strvalidator_guessProfile_gui_na", envir = env, inherits = FALSE)) {
        svalue(f1g1_na_chk) <- get(".strvalidator_guessProfile_gui_na", envir = env)
      }
      if (exists(".strvalidator_guessProfile_gui_ol", envir = env, inherits = FALSE)) {
        svalue(f1g1_ol_chk) <- get(".strvalidator_guessProfile_gui_ol", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_guessProfile_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_guessProfile_gui_ratio", value = svalue(f1g1_ratio_spb), envir = env)
      assign(x = ".strvalidator_guessProfile_gui_height", value = svalue(f1g1_height_edt), envir = env)
      assign(x = ".strvalidator_guessProfile_gui_na", value = svalue(f1g1_na_chk), envir = env)
      assign(x = ".strvalidator_guessProfile_gui_ol", value = svalue(f1g1_ol_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_guessProfile_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_guessProfile_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_guessProfile_gui_ratio", envir = env, inherits = FALSE)) {
        remove(".strvalidator_guessProfile_gui_ratio", envir = env)
      }
      if (exists(".strvalidator_guessProfile_gui_height", envir = env, inherits = FALSE)) {
        remove(".strvalidator_guessProfile_gui_height", envir = env)
      }
      if (exists(".strvalidator_guessProfile_gui_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_guessProfile_gui_na", envir = env)
      }
      if (exists(".strvalidator_guessProfile_gui_ol", envir = env, inherits = FALSE)) {
        remove(".strvalidator_guessProfile_gui_ol", envir = env)
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
