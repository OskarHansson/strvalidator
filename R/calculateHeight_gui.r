################################################################################
# CHANGE LOG (last 20 changes)
# 05.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 07.07.2022: Fixed "...URLs which should use \doi (with the DOI name only)".
# 03.03.2020: Fixed reference to function name.
# 02.03.2020: Added language support.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.08.2016: Implemented new calculateHeight, selection of reference, and check subsetting.
# 08.07.2016: Fixed options 'sex.rm' and 'qs.rm' not saved.
# 29.06.2016: Implement 'checkDataset'.
# 29.06.2016: Added option to remove sex markers and quality sensor.
# 29.04.2016: 'Save as' textbox expandable.
# 06.01.2016: Added attributes to result.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 26.09.2014: Implemented text field for 'exclude'.

#' @title Calculate Peak Height
#'
#' @description
#' GUI wrapper for the \code{\link{calculateHeight}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateHeight}} function by providing a graphical
#' user interface to it.
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
#' @importFrom utils help
#'
#' @references
#' Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
#'  Journal of the Royal Statistical Society: Series C (Applied Statistics),
#'  Volume 59, Issue 5, 2010,
#'  Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
#' \doi{10.1111/j.1467-9876.2010.00722.x}
#'
#' @seealso \code{\link{calculateHeight}}

calculateHeight_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gRef <- NULL
  .gRefName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate peak height"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefDataset <- "Reference dataset:"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strLblPre <- "Pre-processing:"
  strChkSex <- "Remove sex markers"
  strChkSensors <- "Remove quality sensors"
  strChkExclude <- "Exclude values in 'Allele' column"
  strChkExcludeInfo <- "Case sensitive values separated by comma:"
  strLblMatching <- "Reference sample name matching:"
  strChkIgnore <- "Ignore case"
  strChkExact <- "Exact matching"
  strLblPost <- "Post-processing:"
  strChkReplace <- "Replace NA in the result with 0"
  strChkAdd <- "Add result to dataset"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A sample dataset and a reference dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset and a reference dataset."
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

    strtmp <- dtStrings["strLblPre"]$value
    strLblPre <- ifelse(is.na(strtmp), strLblPre, strtmp)

    strtmp <- dtStrings["strChkSex"]$value
    strChkSex <- ifelse(is.na(strtmp), strChkSex, strtmp)

    strtmp <- dtStrings["strChkSensors"]$value
    strChkSensors <- ifelse(is.na(strtmp), strChkSensors, strtmp)

    strtmp <- dtStrings["strChkExclude"]$value
    strChkExclude <- ifelse(is.na(strtmp), strChkExclude, strtmp)

    strtmp <- dtStrings["strChkExcludeInfo"]$value
    strChkExcludeInfo <- ifelse(is.na(strtmp), strChkExcludeInfo, strtmp)

    strtmp <- dtStrings["strLblMatching"]$value
    strLblMatching <- ifelse(is.na(strtmp), strLblMatching, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strChkExact"]$value
    strChkExact <- ifelse(is.na(strtmp), strChkExact, strtmp)

    strtmp <- dtStrings["strLblPost"]$value
    strLblPost <- ifelse(is.na(strtmp), strLblPost, strtmp)

    strtmp <- dtStrings["strChkReplace"]$value
    strChkReplace <- ifelse(is.na(strtmp), strChkReplace, strtmp)

    strtmp <- dtStrings["strChkAdd"]$value
    strChkAdd <- ifelse(is.na(strtmp), strChkAdd, strtmp)

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

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # Samples -------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g0)

  samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g0
  )

  dataset_drp <- gcombobox(
    items = c(
      strDrpDefault,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1, editable = FALSE,
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Height")
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
      svalue(samples_lbl) <- paste("", samples, strLblSamples)

      # Suggest name for the result.
      svalue(save_edt) <- paste(val_obj, "_height", sep = "")

      # Detect kit.
      kitIndex <- detectKit(data = .gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
    }
  })

  # References ----------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblRefDataset, container = f0g1)

  ref_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = f0g1
  )

  refset_drp <- gcombobox(
    items = c(
      strDrpDefault,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1, editable = FALSE,
    container = f0g1,
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
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj
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

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = f0g2)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f0g2,
    ellipsize = "none", expand = TRUE, fill = "x"
  )

  # CHECK #####################################################################

  check_btn <- gbutton(text = strBtnCheck, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_exact <- svalue(f1_exact_chk)
    val_word <- FALSE

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
        exact = val_exact,
        word = val_word
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

  glabel(text = strLblPre, anchor = c(-1, 0), container = f1)

  f1_sex_chk <- gcheckbox(
    text = strChkSex,
    checked = FALSE, container = f1
  )

  f1_qs_chk <- gcheckbox(
    text = strChkSensors,
    checked = TRUE, container = f1
  )

  f1_exclude_chk <- gcheckbox(
    text = strChkExclude,
    checked = TRUE, container = f1
  )

  f1_exclude_lbl <- glabel(
    text = strChkExcludeInfo,
    anchor = c(-1, 0), container = f1
  )

  f1_exclude_edt <- gedit(text = "OL", container = f1)

  glabel(text = strLblMatching, anchor = c(-1, 0), container = f1)

  f1_ignore_chk <- gcheckbox(
    text = strChkIgnore,
    checked = TRUE, container = f1
  )

  f1_exact_chk <- gcheckbox(
    text = strChkExact,
    checked = FALSE, container = f1
  )

  glabel(text = strLblPost, anchor = c(-1, 0), container = f1)

  f1_replace_chk <- gcheckbox(
    text = strChkReplace,
    checked = TRUE, container = f1
  )

  f1_add_chk <- gcheckbox(
    text = strChkAdd,
    checked = TRUE, container = f1
  )

  addHandlerChanged(f1_exclude_chk, handler = function(h, ...) {
    if (svalue(f1_exclude_chk)) {
      enabled(f1_exclude_lbl) <- TRUE
      enabled(f1_exclude_edt) <- TRUE
    } else {
      enabled(f1_exclude_lbl) <- FALSE
      enabled(f1_exclude_edt) <- FALSE
    }
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################


  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    val_data <- .gData
    val_data_name <- .gDataName
    val_ref <- .gRef
    val_ref_name <- .gRefName
    val_name <- svalue(save_edt)
    val_add <- svalue(f1_add_chk)
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)
    val_kit <- svalue(kit_drp)
    val_ignore <- svalue(f1_ignore_chk)
    val_exact <- svalue(f1_exact_chk)
    val_replace <- svalue(f1_replace_chk)
    val_exclude <- svalue(f1_exclude_chk)
    val_ex_values <- svalue(f1_exclude_edt)
    val_na <- NULL
    val_ex <- NULL

    if (val_replace) {
      val_na <- 0
    } else {
      val_na <- NULL
    }

    if (val_exclude) {
      val_ex <- unlist(strsplit(val_ex_values, ",", fixed = TRUE))
    } else {
      val_ex <- NULL
    }

    if (!is.null(val_data)) {
      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateHeight(
        data = val_data, ref = val_ref, na.replace = val_na,
        add = val_add, sex.rm = val_sex, qs.rm = val_qs,
        kit = val_kit, exclude = val_ex,
        ignore.case = val_ignore, exact = val_exact,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "na.replace", "add", "sex.rm",
        "qs.rm", "kit", "exclude", "ignore.case", "exact"
      )

      values <- list(
        val_data_name, val_ref_name, val_na, val_add, val_sex,
        val_qs, val_kit, val_ex, val_ignore, val_exact
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
    } else {
      gmessage(
        msg = strMsgDataset,
        title = strMsgTitleDataset,
        icon = "error",
        parent = w
      )
    }
  })

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
      if (exists(".strvalidator_calculateHeight_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateHeight_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateHeight_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateHeight_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_calculateHeight_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculateHeight_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_calculateHeight_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_replace", envir = env, inherits = FALSE)) {
        svalue(f1_replace_chk) <- get(".strvalidator_calculateHeight_gui_replace", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_add", envir = env, inherits = FALSE)) {
        svalue(f1_add_chk) <- get(".strvalidator_calculateHeight_gui_add", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exclude", envir = env, inherits = FALSE)) {
        svalue(f1_exclude_chk) <- get(".strvalidator_calculateHeight_gui_exclude", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exclude_edt", envir = env, inherits = FALSE)) {
        svalue(f1_exclude_edt) <- get(".strvalidator_calculateHeight_gui_exclude_edt", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateHeight_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_qs", value = svalue(f1_qs_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_replace", value = svalue(f1_replace_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_add", value = svalue(f1_add_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_exclude", value = svalue(f1_exclude_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_exclude_edt", value = svalue(f1_exclude_edt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateHeight_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_replace", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_replace", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_add", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_add", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exclude", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_exclude", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exclude_edt", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_exclude_edt", envir = env)
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
