################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 07.03.2020: Added language support.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 16.12.2015: Implemented option to type a reference sample name.
# 15.12.2015: Implemented 'exact' option.
# 04.12.2015: Removed 'Marker' from required columns.
# 28.08.2015: Added importFrom.
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 25.07.2013: Parameter 'fixed' changed to 'word'.
# 15.07.2013: Added save GUI settings.
# 15.07.2013: Added 'options' group.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.

#' @title Check Subset
#'
#' @description
#' GUI wrapper for the \code{\link{checkSubset}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{checkSubset}} function by providing
#' a graphical user interface to it.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help
#' @importFrom graphics title
#'
#' @return TRUE
#'
#' @seealso \code{\link{checkSubset}}

checkSubset_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gRef <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Check subsetting"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefDataset <- "Reference dataset:"
  strLblRef <- "references"
  strLblManual <- "Or type a reference name:"
  strFrmOptions <- "Options"
  strLblMatching <- "Reference sample name matching:"
  strChkIgnore <- "Ignore case ('A' will match 'A', 'B-a.2', and 'A2')"
  strChkWord <- "Add word boundaries ('A' will match 'A', 'B-A.2', and 'A 2' but not 'A2')"
  strChkExact <- "Exact matching ('A' will match 'A' but not 'B-A.2', 'A 2', or 'A2')"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Subset"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset and a reference dataset, or type a reference name"
  strWinTitleCheck <- "Check subsetting"
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

    strtmp <- dtStrings["strLblRefDataset"]$value
    strLblRefDataset <- ifelse(is.na(strtmp), strLblRefDataset, strtmp)

    strtmp <- dtStrings["strLblRef"]$value
    strLblRef <- ifelse(is.na(strtmp), strLblRef, strtmp)

    strtmp <- dtStrings["strLblManual"]$value
    strLblManual <- ifelse(is.na(strtmp), strLblManual, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblMatching"]$value
    strLblMatching <- ifelse(is.na(strtmp), strLblMatching, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strChkWord"]$value
    strChkWord <- ifelse(is.na(strtmp), strChkWord, strtmp)

    strtmp <- dtStrings["strChkExact"]$value
    strChkExact <- ifelse(is.na(strtmp), strChkExact, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strMsgCheck"]$value
    strMsgCheck <- ifelse(is.na(strtmp), strMsgCheck, strtmp)

    strtmp <- dtStrings["strWinTitleCheck"]$value
    strWinTitleCheck <- ifelse(is.na(strtmp), strWinTitleCheck, strtmp)

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

    # Destroy window.
    return(FALSE)
  })

  # Vertical main group.
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

  # DATASET ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # SAMPLE --------------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g1)

  dataset_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g1
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
    container = f0g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      samples <- length(unique(.gData$Sample.Name))
      svalue(dataset_samples_lbl) <- paste("", samples, strLblSamples)
    } else {
      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      svalue(dataset_samples_lbl) <- paste(" 0", strLblSamples)
    }
  })

  # REFERENCE -----------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblRefDataset, container = f0g2)

  dataset_ref_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = f0g2
  )

  dataset_ref_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # MANUAL --------------------------------------------------------------------

  f0g3 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblManual, container = f0g3)

  dataset_ref_edt <- gedit(container = f0g3, expand = TRUE, fill = "x")

  addHandlerChanged(dataset_ref_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_ref_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      refs <- length(unique(.gRef$Sample.Name))
      svalue(dataset_ref_lbl) <- paste("", refs, strLblRef)
    } else {
      # Reset components.
      .gRef <<- data.frame(No.Data = NA)
      svalue(dataset_ref_lbl) <- paste(" 0", strLblRef)
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  glabel(text = strLblMatching, anchor = c(-1, 0), container = f1)

  f1_ignore_case_chk <- gcheckbox(
    text = strChkIgnore,
    checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = strChkWord,
    checked = FALSE,
    container = f1
  )

  f1_exact_chk <- gcheckbox(
    text = strChkExact,
    checked = FALSE,
    container = f1
  )

  # BUTTON ####################################################################

  check_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ref_name <- svalue(dataset_ref_edt)
    val_ignore <- svalue(f1_ignore_case_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)

    if (is.null(.gRef)) {
      # If no reference dataset use given name.
      val_ref <- val_ref_name
    }

    # Check that data is available.
    if (!is.null(val_data) && !is.null(val_ref)) {
      chksubset_w <- gwindow(
        title = strWinTitleCheck,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- checkSubset(
        data = val_data,
        ref = val_ref,
        ignore.case = val_ignore,
        word = val_word,
        exact = val_exact,
        console = FALSE
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
        wrap = FALSE, container = chksubset_w
      )

      visible(chksubset_w) <- TRUE
    } else {
      gmessage(
        msg = strMsgCheck,
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
      if (exists(".strvalidator_checkSubset_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_checkSubset_gui_savegui", envir = env)
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
      if (exists(".strvalidator_checkSubset_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_case_chk) <- get(".strvalidator_checkSubset_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_checkSubset_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_checkSubset_gui_word", envir = env)
      }
      if (exists(".strvalidator_checkSubset_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_checkSubset_gui_exact", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_checkSubset_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_checkSubset_gui_ignore", value = svalue(f1_ignore_case_chk), envir = env)
      assign(x = ".strvalidator_checkSubset_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_checkSubset_gui_exact", value = svalue(f1_word_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_checkSubset_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_checkSubset_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_checkSubset_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_checkSubset_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_checkSubset_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_checkSubset_gui_word", envir = env)
      }
      if (exists(".strvalidator_checkSubset_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_checkSubset_gui_exact", envir = env)
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
