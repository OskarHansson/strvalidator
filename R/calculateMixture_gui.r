################################################################################
# CHANGE LOG (last 20 changes)
# 05.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 03.03.2020: Added language support.
# 03.05.2019: Compacted gui and expand text field under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 12.09.2018: 'Save as' textbox expandable.
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 28.08.2015: Added importFrom.
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.08.2014: Fixed bug in 'Check subsetting' showing extra combinations in many cases.
# 08.07.2014: First version.

#' @title Calculate Mixture
#'
#' @description
#' GUI wrapper for the \code{\link{calculateMixture}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateMixture}} function by
#' providing a graphical user interface.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help head str
#' @importFrom graphics title
#'
#' @seealso \code{\link{calculateMixture}}, \code{\link{checkSubset}}

calculateMixture_gui <- function(env = parent.frame(), savegui = NULL,
                                 debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gRef1 <- NULL
  .gRef2 <- NULL
  .gNameData <- NULL
  .gNameRef1 <- NULL
  .gNameRef2 <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate Mixture"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefMajor <- "Reference dataset (major):"
  strLblRefMinor <- "Reference dataset (minor):"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strFrmOptions <- "Options"
  strChkOL <- "Remove off-ladder alleles (affects number of drop-in)"
  strChkDrop <- "Ignore drop-out (calculate Mx anyway)"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A sample dataset and two reference datasets must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset."
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

    strtmp <- dtStrings["strDrpDefault"]$value
    strDrpDefault <- ifelse(is.na(strtmp), strDrpDefault, strtmp)

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strLblRefMajor"]$value
    strLblRefMajor <- ifelse(is.na(strtmp), strLblRefMajor, strtmp)

    strtmp <- dtStrings["strLblRefMinor"]$value
    strLblRefMinor <- ifelse(is.na(strtmp), strLblRefMinor, strtmp)

    strtmp <- dtStrings["strLblRef"]$value
    strLblRef <- ifelse(is.na(strtmp), strLblRef, strtmp)

    strtmp <- dtStrings["strBtnCheck"]$value
    strBtnCheck <- ifelse(is.na(strtmp), strBtnCheck, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkOL"]$value
    strChkOL <- ifelse(is.na(strtmp), strChkOL, strtmp)

    strtmp <- dtStrings["strChkDrop"]$value
    strChkDrop <- ifelse(is.na(strtmp), strChkDrop, strtmp)

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
    container = gv,
    expand = FALSE,
    fill = "x"
  )

  # Dataset -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

  data_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = g0
  )

  dfs <- c(strDrpDefault, listObjects(env = env, obj.class = "data.frame"))

  data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # get dataset.
      .gData <<- get(val_obj, envir = env)
      .gNameData <<- val_obj
      svalue(data_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strLblSamples
      )
      svalue(save_edt) <- paste(val_obj, "_mixture", sep = "")
    } else {

      # Reset components.
      .gData <<- NULL
      .gNameData <<- NULL
      svalue(data_drp, index = TRUE) <- 1
      svalue(data_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
    }
  })

  # Reference 1 ---------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblRefMajor, container = g1)

  ref1_samples_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = g1
  )

  # NB! dfs defined in previous section.
  ref1_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(ref1_drp, handler = function(h, ...) {
    val_obj <- svalue(ref1_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gRef1 <<- get(val_obj, envir = env)
      .gNameRef1 <<- val_obj
      svalue(ref1_samples_lbl) <- paste(
        length(unique(.gRef1$Sample.Name)),
        strLblRef
      )
    } else {

      # Reset components.
      .gRef1 <<- NULL
      .gNameRef1 <<- NULL
      svalue(ref1_drp, index = TRUE) <- 1
      svalue(ref1_samples_lbl) <- paste(" 0", strLblRef)
    }
  })

  # Reference 2 ---------------------------------------------------------------

  g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblRefMinor, container = g2)

  ref2_samples_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = g2
  )

  # NB! dfs defined in previous section.
  ref2_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(ref2_drp, handler = function(h, ...) {
    val_obj <- svalue(ref2_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gRef2 <<- get(val_obj, envir = env)
      .gNameRef2 <<- val_obj
      svalue(ref2_samples_lbl) <- paste(
        length(unique(.gRef2$Sample.Name)),
        strLblRef
      )
    } else {

      # Reset components.
      .gRef2 <<- NULL
      .gNameRef2 <<- NULL
      svalue(ref2_drp, index = TRUE) <- 1
      svalue(ref2_samples_lbl) <- paste(" 0", strLblRef)
    }
  })

  # CHECK #####################################################################

  check_btn <- gbutton(text = strBtnCheck, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref1 <- .gRef1
    val_ref2 <- .gRef2
    val_ignore <- FALSE
    val_word <- FALSE

    if (!is.null(.gData) || !is.null(.gRef1) || !is.null(.gRef2)) {
      chksubset_w <- gwindow(
        title = strWinTitleCheck,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      # Create pattern.
      tmp <- paste(".*", unique(.gRef1$Sample.Name), ".*",
        unique(.gRef2$Sample.Name), ".*",
        sep = ""
      )

      # Save as dataframe.
      val_pattern <- data.frame(Sample.Name = tmp, stringsAsFactors = FALSE)

      if (debug) {
        print("Pattern")
        print(val_pattern)
      }

      chksubset_txt <- checkSubset(
        data = val_data,
        ref = val_pattern,
        console = FALSE,
        ignore.case = val_ignore,
        word = val_word,
        debug = debug
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
        wrap = FALSE, container = chksubset_w
      )

      visible(chksubset_w) <- TRUE
    } else {
      gmessage(
        msg = strMsgDataset,
        title = strMsgTitleError,
        icon = "error"
      )
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions, horizontal = FALSE,
    spacing = 1, container = gv
  )

  f1_ol_chk <- gcheckbox(
    text = strChkOL,
    checked = TRUE, container = f1
  )

  f1_drop_chk <- gcheckbox(
    text = strChkDrop,
    checked = TRUE, container = f1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref1 <- .gRef1
    val_ref2 <- .gRef2
    val_name_data <- .gNameData
    val_name_ref1 <- .gNameRef1
    val_name_ref2 <- .gNameRef2
    val_name <- svalue(save_edt)
    val_ol <- svalue(f1_ol_chk)
    val_drop <- svalue(f1_drop_chk)

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_ref1")
      print(head(val_ref1))
      print("val_ref2")
      print(head(val_ref2))
      print("val_ol")
      print(val_ol)
      print("val_drop")
      print(val_drop)
      print("val_name")
      print(val_name)
    }

    # Check if data.
    if (!is.null(.gData) & !is.null(.gRef1) & !is.null(.gRef2)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateMixture(
        data = val_data,
        ref1 = val_ref1,
        ref2 = val_ref2,
        ol.rm = val_ol,
        ignore.dropout = val_drop,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref1", "ref2",
        "ol.rm", "ignore.dropout"
      )

      values <- list(
        val_name_data, val_name_ref1, val_name_ref2,
        val_ol, val_drop
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
      if (exists(".strvalidator_calculateMixture_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateMixture_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateMixture_gui_ol", envir = env, inherits = FALSE)) {
        svalue(f1_ol_chk) <- get(".strvalidator_calculateMixture_gui_ol", envir = env)
      }
      if (exists(".strvalidator_calculateMixture_gui_dropout", envir = env, inherits = FALSE)) {
        svalue(f1_drop_chk) <- get(".strvalidator_calculateMixture_gui_dropout", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateMixture_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateMixture_gui_ol", value = svalue(f1_ol_chk), envir = env)
      assign(x = ".strvalidator_calculateMixture_gui_dropout", value = svalue(f1_drop_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateMixture_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateMixture_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateMixture_gui_ol", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateMixture_gui_ol", envir = env)
      }
      if (exists(".strvalidator_calculateMixture_gui_dropout", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateMixture_gui_dropout", envir = env)
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
