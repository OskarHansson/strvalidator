################################################################################
# CHANGE LOG (last 20 changes)
# 02.09.2022: Compacted the gui.
# 03.03.2020: Fixed reference to function name.
# 01.03.2020: Added language support.
# 10.01.2020: Changed "LDT" to "AT" following the book.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 17.07.2018: 'Save as' textbox expandable.
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.08.2016: Implemented new calculateHeight, removed calculateHeterozygous.
# 29.06.2016: Added option to remove sex markers and quality sensor.
# 16.06.2016: 'Save as' textbox expandable.
# 05.10.2015: Added more attributes to result.
# 04.10.2015: Added automatic calculation of average peak height 'H'.
# 28.08.2015: Added importFrom
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 13.12.2014: Added kit dropdown and kit attribute to result.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.

#' @title Calculate Dropout Events
#'
#' @description
#' GUI wrapper for the \code{\link{calculateDropout}} function.
#'
#' @details Scores dropouts for a dataset.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help head
#' @importFrom graphics title
#'
#' @seealso \code{\link{calculateDropout}}, \code{\link{checkSubset}}

calculateDropout_gui <- function(env = parent.frame(), savegui = NULL,
                                 debug = FALSE, parent = NULL) {
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
  strWinTitle <- "Calculate dropout"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset and kit"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefDataset <- "Reference dataset:"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strChkIgnore <- "Ignore case"
  strChkSex <- "Remove sex markers"
  strChkSensors <- "Remove quality sensors"
  strChkAverage <- "Calculate average peak height"
  strLblAt <- "Analytical threshold (AT):"
  strLblMethods <- "Select one or more dropout scoring methods:"
  strChkMethod1 <- "Score dropout relative to the low molecular weight allele"
  strChkMethod2 <- "Score dropout relative to the high molecular weight allele"
  strChkMethodX <- "Score dropout relative to a random allele"
  strChkMethodL <- "Score dropout per locus"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A sample dataset and a reference dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strWinTitleCheck <- "Check subsetting"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset and a reference dataset."
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

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strLblRefDataset"]$value
    strLblRefDataset <- ifelse(is.na(strtmp), strLblRefDataset, strtmp)

    strtmp <- dtStrings["strLblRef"]$value
    strLblRef <- ifelse(is.na(strtmp), strLblRef, strtmp)

    strtmp <- dtStrings["strBtnCheck"]$value
    strBtnCheck <- ifelse(is.na(strtmp), strBtnCheck, strtmp)

    strtmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strChkSex"]$value
    strChkSex <- ifelse(is.na(strtmp), strChkSex, strtmp)

    strtmp <- dtStrings["strChkSensors"]$value
    strChkSensors <- ifelse(is.na(strtmp), strChkSensors, strtmp)

    strtmp <- dtStrings["strChkAverage"]$value
    strChkAverage <- ifelse(is.na(strtmp), strChkAverage, strtmp)

    strtmp <- dtStrings["strLblMethods"]$value
    strLblMethods <- ifelse(is.na(strtmp), strLblMethods, strtmp)

    strtmp <- dtStrings["strChkMethod1"]$value
    strChkMethod1 <- ifelse(is.na(strtmp), strChkMethod1, strtmp)

    strtmp <- dtStrings["strChkMethod2"]$value
    strChkMethod2 <- ifelse(is.na(strtmp), strChkMethod2, strtmp)

    strtmp <- dtStrings["strChkMethodX"]$value
    strChkMethodX <- ifelse(is.na(strtmp), strChkMethodX, strtmp)

    strtmp <- dtStrings["strChkMethodL"]$value
    strChkMethodL <- ifelse(is.na(strtmp), strChkMethodL, strtmp)

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

    strtmp <- dtStrings["strMsgCheck"]$value
    strMsgCheck <- ifelse(is.na(strtmp), strMsgCheck, strtmp)

    strtmp <- dtStrings["strWinTitleCheck"]$value
    strWinTitleCheck <- ifelse(is.na(strtmp), strWinTitleCheck, strtmp)

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

    # Destroy window.
    return(FALSE)
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = w,
    # expand = TRUE,
    # fill = "x"
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "x")

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

  # Datasets ------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

  g0_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = g0
  )
  dataset_drp <- gcombobox(
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
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, strLblSamples)
      svalue(f1g1_at_edt) <- min(as.numeric(.gData$Height), na.rm = TRUE)

      # Suggest a name for result.
      svalue(f2_save_edt) <- paste(val_obj, "_dropout", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(f1g1_at_edt) <- ""
      svalue(f2_save_edt) <- ""
    }
  })

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblRefDataset, container = g1)

  ref_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = g1
  )

  refset_drp <- gcombobox(
    items = c(
      strDrpDefault,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(refset_drp, handler = function(h, ...) {
    val_obj <- svalue(refset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gRef <<- get(val_obj, envir = env)
      ref <- length(unique(.gRef$Sample.Name))
      svalue(ref_lbl) <- paste("", ref, strLblRef)
    } else {
      # Reset components.
      .gRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(ref_lbl) <- paste(" 0", strLblRef)
    }
  })

  # Kit -----------------------------------------------------------------------

  g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = g2)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # CHECK #####################################################################

  check_btn <- gbutton(text = strBtnCheck, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_case_chk)

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = strWinTitleCheck,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- checkSubset(
        data = val_data,
        ref = val_ref,
        console = FALSE,
        ignore.case = val_ignore,
        word = FALSE
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

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1_ignore_case_chk <- gcheckbox(
    text = strChkIgnore, checked = TRUE,
    container = f1
  )

  f1_sex_chk <- gcheckbox(
    text = strChkSex, checked = FALSE,
    container = f1
  )

  f1_qs_chk <- gcheckbox(
    text = strChkSensors, checked = TRUE,
    container = f1
  )

  f1_h_chk <- gcheckbox(
    text = strChkAverage, checked = TRUE,
    container = f1
  )

  f1g1 <- glayout(container = f1)

  f1g1[1, 1] <- glabel(
    text = strLblAt,
    container = f1g1, anchor = c(-1, 0)
  )

  f1g1[1, 2] <- f1g1_at_edt <- gedit(text = "", width = 6, container = f1g1)

  glabel(
    text = strLblMethods,
    container = f1, anchor = c(-1, 0)
  )

  f1_score1_chk <- gcheckbox(
    text = strChkMethod1,
    checked = TRUE, container = f1
  )

  f1_score2_chk <- gcheckbox(
    text = strChkMethod2,
    checked = TRUE, container = f1
  )

  f1_scorex_chk <- gcheckbox(
    text = strChkMethodX,
    checked = TRUE, container = f1
  )

  f1_scorel_chk <- gcheckbox(
    text = strChkMethodL,
    checked = TRUE, container = f1
  )

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strFrmSave,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strLblSave, container = f2)

  f2_save_edt <- gedit(expand = TRUE, fill = "x", container = f2)

  # BUTTON ####################################################################


  dropout_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(dropout_btn, handler = function(h, ...) {
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- svalue(dataset_drp)
    val_name_ref <- svalue(refset_drp)
    val_ignore_case <- svalue(f1_ignore_case_chk)
    val_h <- svalue(f1_h_chk)
    val_threshold <- as.numeric(svalue(f1g1_at_edt))
    val_name <- svalue(f2_save_edt)
    val_kit <- svalue(kit_drp)
    val_method <- vector()
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)

    # Get methods:
    if (svalue(f1_score1_chk)) {
      val_method <- c(val_method, "1")
    }
    if (svalue(f1_score2_chk)) {
      val_method <- c(val_method, "2")
    }
    if (svalue(f1_scorex_chk)) {
      val_method <- c(val_method, "X")
    }
    if (svalue(f1_scorel_chk)) {
      val_method <- c(val_method, "L")
    }

    if (debug) {
      print("GUI options:")
      print("val_ignore_case")
      print(val_ignore_case)
      print("val_threshold")
      print(val_threshold)
      print("val_name")
      print(val_name)
      print("val_method")
      print(val_method)
    }

    # No threshold is represented by NULL (not needed).
    if (length(val_threshold) == 0) {
      val_threshold <- NULL
    }

    if (debug) {
      print("Function arguments:")
      print("val_ignore_case")
      print(val_ignore_case)
      print("val_threshold")
      print(val_threshold)
      print("val_name")
      print(val_name)
      print("val_sex")
      print(val_sex)
      print("val_qs")
      print(val_qs)
    }

    if (!is.null(val_data) & !is.null(val_ref)) {
      # Change button.
      blockHandlers(dropout_btn)
      svalue(dropout_btn) <- strBtnProcessing
      unblockHandlers(dropout_btn)
      enabled(dropout_btn) <- FALSE

      datanew <- calculateDropout(
        data = val_data,
        ref = val_ref,
        threshold = val_threshold,
        method = val_method,
        ignore.case = val_ignore_case,
        sex.rm = val_sex,
        qs.rm = val_qs,
        kit = val_kit,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "threshold", "method",
        "ignore.case", "sex.rm", "qs.rm", "kit", "calculate.h"
      )

      values <- list(
        val_name_data, val_name_ref, val_threshold, val_method,
        val_ignore_case, val_sex, val_qs, val_kit, val_h
      )

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Calculate and add average peak height.
      if (val_h) {
        message("User selected to add average peak height...")

        # Calculate average peak height.
        dfH <- calculateHeight(
          data = val_data, ref = val_ref, na.replace = 0,
          add = FALSE, exclude = "OL", sex.rm = val_sex,
          qs.rm = val_qs, kit = val_kit,
          ignore.case = val_ignore_case, exact = FALSE,
          debug = debug
        )

        message("Average peak height calculated.")

        # Add average peak height to dataset.
        datanew <- addData(
          data = datanew, new.data = dfH,
          by.col = "Sample.Name", then.by.col = NULL,
          exact = TRUE, ignore.case = val_ignore_case,
          debug = debug
        )

        message("Average peak height added to result.")
      }

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(head(datanew))
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
      if (exists(".strvalidator_calculateDropout_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateDropout_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateDropout_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_case_chk) <- get(".strvalidator_calculateDropout_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateDropout_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_calculateDropout_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_h", envir = env, inherits = FALSE)) {
        svalue(f1_h_chk) <- get(".strvalidator_calculateDropout_gui_h", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score1", envir = env, inherits = FALSE)) {
        svalue(f1_score1_chk) <- get(".strvalidator_calculateDropout_gui_score1", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score2", envir = env, inherits = FALSE)) {
        svalue(f1_score2_chk) <- get(".strvalidator_calculateDropout_gui_score2", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorex", envir = env, inherits = FALSE)) {
        svalue(f1_scorex_chk) <- get(".strvalidator_calculateDropout_gui_scorex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorel", envir = env, inherits = FALSE)) {
        svalue(f1_scorel_chk) <- get(".strvalidator_calculateDropout_gui_scorel", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateDropout_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_ignore", value = svalue(f1_ignore_case_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_qs", value = svalue(f1_qs_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_h", value = svalue(f1_h_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_score1", value = svalue(f1_score1_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_score2", value = svalue(f1_score2_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_scorex", value = svalue(f1_scorex_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_scorel", value = svalue(f1_scorel_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateDropout_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_h", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_h", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score1", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_score1", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score2", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_score2", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_scorex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorel", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_scorel", envir = env)
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
