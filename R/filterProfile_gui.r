################################################################################
# CHANGE LOG (last 20 changes)
# 02.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 14.03.2020: Added language support and minor gui adjustments.
# 02.03.2019: Tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 17.09.2016: Updated to pass 'kit' option. Dropdown always active.
# 07.09.2016: Updated to include new filterProfile options.
# 28.04.2016: 'Save as' textbox expandable.
# 15.12.2015: Added attributes to result.
# 15.12.2015: Added option 'exact' sample name matching.
# 29.08.2015: Added importFrom.
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 09.04.2015: Added option 'invert' to filter peaks NOT in reference.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 12.01.2014: Replaced 'subset' with native code.

#' @title Filter Profile
#'
#' @description
#' GUI wrapper for the \code{\link{filterProfile}} function.
#'
#' @details Simplifies the use of the \code{\link{filterProfile}} function
#' by providing a graphical user interface to it.
#' All data not matching/matching the reference will be discarded.
#' Useful for filtering stutters and artifacts from raw typing data or
#' to identify drop-ins.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help str
#' @importFrom graphics title
#'
#' @return TRUE
#'
#' @seealso \code{\link{filterProfile}}, \code{\link{checkSubset}}

filterProfile_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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
  strWinTitle <- "Filter profile"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefDataset <- "Reference dataset:"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strLblKit <- "Kit:"
  strChkVirtual <- "Exclude virtual bins"
  strFrmOptions <- "Options"
  strLblPre <- "Pre-processing:"
  strChkSex <- "Remove sex markers"
  strTipSex <- "Removes sex markers defined in the selected kit."
  strChkSensors <- "Remove quality sensors"
  strTipSensors <- "Removes quality sensors defined in the selected kit."
  strLblMethod <- "Filter options:"
  strRadRef <- "Filter by reference profiles"
  strRadBins <- "Filter by kit bins (allelic ladder)"
  strChkInvert <- "Invert (remove peaks matching)"
  strLblMatching <- "Reference sample name matching:"
  strChkIgnore <- "Ignore case"
  strTipIgnore <- "'A' will match 'A', 'B-a.2', and 'A2'"
  strChkWord <- "Add word boundaries"
  strTipWord <- "'A' will match 'A', 'B-A.2', and 'A 2' but not 'A2'"
  strChkExact <- "Exact matching"
  strTipExact <- "'A' will match 'A' but not 'B-A.2', 'A 2', or 'A2'"
  strLblPost <- "Post-processing:"
  strChkAdd <- "Add missing loci (markers)"
  strTipAdd <- "This option will be slower."
  strChkKeep <- "Keep loci/sample even if no matching allele"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnFilter <- "Filter"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A sample dataset and a reference dataset must be selected."
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

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

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

    strtmp <- dtStrings["strChkVirtual"]$value
    strChkVirtual <- ifelse(is.na(strtmp), strChkVirtual, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblPre"]$value
    strLblPre <- ifelse(is.na(strtmp), strLblPre, strtmp)

    strtmp <- dtStrings["strChkSex"]$value
    strChkSex <- ifelse(is.na(strtmp), strChkSex, strtmp)

    strtmp <- dtStrings["strTipSex"]$value
    strTipSex <- ifelse(is.na(strtmp), strTipSex, strtmp)

    strtmp <- dtStrings["strChkSensors"]$value
    strChkSensors <- ifelse(is.na(strtmp), strChkSensors, strtmp)

    strtmp <- dtStrings["strTipSensors"]$value
    strTipSensors <- ifelse(is.na(strtmp), strTipSensors, strtmp)

    strtmp <- dtStrings["strLblMethod"]$value
    strLblMethod <- ifelse(is.na(strtmp), strLblMethod, strtmp)

    strtmp <- dtStrings["strRadRef"]$value
    strRadRef <- ifelse(is.na(strtmp), strRadRef, strtmp)

    strtmp <- dtStrings["strRadBins"]$value
    strRadBins <- ifelse(is.na(strtmp), strRadBins, strtmp)

    strtmp <- dtStrings["strChkInvert"]$value
    strChkInvert <- ifelse(is.na(strtmp), strChkInvert, strtmp)

    strtmp <- dtStrings["strLblMatching"]$value
    strLblMatching <- ifelse(is.na(strtmp), strLblMatching, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strTipIgnore"]$value
    strTipIgnore <- ifelse(is.na(strtmp), strTipIgnore, strtmp)

    strtmp <- dtStrings["strChkWord"]$value
    strChkWord <- ifelse(is.na(strtmp), strChkWord, strtmp)

    strtmp <- dtStrings["strTipWord"]$value
    strTipWord <- ifelse(is.na(strtmp), strTipWord, strtmp)

    strtmp <- dtStrings["strChkExact"]$value
    strChkExact <- ifelse(is.na(strtmp), strChkExact, strtmp)

    strtmp <- dtStrings["strTipExact"]$value
    strTipExact <- ifelse(is.na(strtmp), strTipExact, strtmp)

    strtmp <- dtStrings["strLblPost"]$value
    strLblPost <- ifelse(is.na(strtmp), strLblPost, strtmp)

    strtmp <- dtStrings["strChkAdd"]$value
    strChkAdd <- ifelse(is.na(strtmp), strChkAdd, strtmp)

    strtmp <- dtStrings["strTipAdd"]$value
    strTipAdd <- ifelse(is.na(strtmp), strTipAdd, strtmp)

    strtmp <- dtStrings["strChkKeep"]$value
    strChkKeep <- ifelse(is.na(strtmp), strChkKeep, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnFilter"]$value
    strBtnFilter <- ifelse(is.na(strtmp), strBtnFilter, strtmp)

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
    expand = TRUE,
    fill = "x"
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

  # Datasets ------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

  g0_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = g0
  )

  g0_dataset_drp <- gcombobox(
    items = c(
      strDrpDataset,
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

  addHandlerChanged(g0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, strLblSamples)
      svalue(save_edt) <- paste(.gDataName, "_filter", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(g0_dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
    }
  })

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  refset_lbl <- glabel(
    text = strLblRefDataset,
    container = g1
  )

  g1_ref_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = g1
  )

  refset_drp <- gcombobox(
    items = c(
      strDrpDataset,
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
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj
      ref <- length(unique(.gRef$Sample.Name))
      svalue(g1_ref_lbl) <- paste("", ref, strLblRef)
    } else {
      # Reset components.
      .gRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(g1_ref_lbl) <- paste(" 0", strLblRef)
    }
  })

  # Kit -------------------------------------------------------------------

  g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  kit_lbl <- glabel(text = strLblKit, container = g2)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  kit_chk <- gcheckbox(
    text = strChkVirtual,
    checked = TRUE,
    container = f0
  )

  # CHECK ###############################################################33####

  check_btn <- gbutton(text = strBtnCheck, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_case_chk)
    val_exact <- svalue(f1_exact_chk)
    val_word <- svalue(f1_word_chk)

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
    text = strFrmOptions, horizontal = FALSE,
    spacing = 1, container = gv
  )

  # PRE-PROCESSING ------------------------------------------------------------

  glabel(text = strLblPre, anchor = c(-1, 0), container = f1)

  f1_sex_chk <- gcheckbox(
    text = strChkSex,
    checked = FALSE, container = f1
  )
  tooltip(f1_sex_chk) <- strTipSex

  f1_qs_chk <- gcheckbox(
    text = strChkSensors,
    checked = FALSE, container = f1
  )
  tooltip(f1_qs_chk) <- strTipSensors

  # METHOD --------------------------------------------------------------------

  glabel(text = strLblMethod, anchor = c(-1, 0), container = f1)

  f1_options <- c(strRadRef, strRadBins)

  f1_filter_opt <- gradio(
    items = f1_options, selected = 1,
    horizontal = FALSE, container = f1
  )

  f1_invert_chk <- gcheckbox(
    text = strChkInvert,
    checked = FALSE, container = f1
  )

  f1_keep_na_chk <- gcheckbox(
    text = strChkKeep,
    checked = TRUE, container = f1
  )

  # MATCHING ------------------------------------------------------------------

  glabel(text = strLblMatching, anchor = c(-1, 0), container = f1)

  f1_ignore_case_chk <- gcheckbox(
    text = strChkIgnore,
    checked = TRUE, container = f1
  )
  tooltip(f1_ignore_case_chk) <- strTipIgnore

  f1_exact_chk <- gcheckbox(
    text = strChkExact,
    checked = FALSE, container = f1
  )
  tooltip(f1_exact_chk) <- strTipExact

  f1_word_chk <- gcheckbox(
    text = strChkWord,
    checked = FALSE, container = f1
  )
  tooltip(f1_word_chk) <- strTipWord

  # POST-PROCESSING -----------------------------------------------------------

  glabel(text = strLblPost, anchor = c(-1, 0), container = f1)

  f1_add_missing_loci_chk <- gcheckbox(
    text = strChkAdd,
    checked = TRUE, container = f1
  )
  tooltip(f1_add_missing_loci_chk) <- strTipAdd

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f1_filter_opt, handler = function(h, ...) {
    .refreshOptions()
  })

  addHandlerChanged(f1_exact_chk, handler = function(h, ...) {
    .refreshOptions()
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  filter_btn <- gbutton(text = strBtnFilter, container = gv)

  addHandlerClicked(filter_btn, handler = function(h, ...) {
    val_data <- .gData
    val_name_data <- .gDataName
    val_ref <- .gRef
    val_name_ref <- .gRefName
    val_invert <- svalue(f1_invert_chk)
    val_add_missing_loci <- svalue(f1_add_missing_loci_chk)
    val_keep_na <- svalue(f1_keep_na_chk)
    val_ignore_case <- svalue(f1_ignore_case_chk)
    val_exact <- svalue(f1_exact_chk)
    val_word <- svalue(f1_word_chk)
    val_name <- svalue(save_edt)
    val_filter <- svalue(f1_filter_opt, index = TRUE)
    val_kit <- svalue(kit_drp)
    val_exclude <- svalue(kit_chk)
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)

    # Check if filter by kit bins.
    if (val_filter == 2) {
      # Get markers, bins and flag for virtual bins.
      val_ref <- getKit(kit = val_kit, what = "VIRTUAL")

      if (val_exclude) {
        # Remove virtual bins.
        val_ref <- val_ref[val_ref$Virtual == 0, ]
      }
    }

    if (!is.null(val_data) & !is.null(val_ref)) {
      # Change button.
      blockHandlers(filter_btn)
      svalue(filter_btn) <- strBtnProcessing
      unblockHandlers(filter_btn)
      enabled(filter_btn) <- FALSE

      datanew <- filterProfile(
        data = val_data,
        ref = val_ref,
        add.missing.loci = val_add_missing_loci,
        keep.na = val_keep_na,
        ignore.case = val_ignore_case,
        exact = val_exact,
        word = val_word,
        invert = val_invert,
        sex.rm = val_sex,
        qs.rm = val_qs,
        kit = val_kit,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "add.missing.loci",
        "keep.na", "ignore.case", "exact", "word",
        "invert", "sex", "qs", "kit"
      )

      values <- list(
        val_name_data, val_name_ref, val_add_missing_loci,
        val_keep_na, val_ignore_case, val_exact, val_word,
        val_invert, val_sex, val_qs, val_kit
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

  .refreshOptions <- function() {
    val_opt <- svalue(f1_filter_opt, index = TRUE)
    val_exact <- svalue(f1_exact_chk)

    if (val_opt == 1) {
      enabled(refset_lbl) <- TRUE
      enabled(refset_drp) <- TRUE
      enabled(check_btn) <- TRUE

      enabled(kit_chk) <- FALSE
    } else if (val_opt == 2) {
      enabled(refset_lbl) <- FALSE
      enabled(refset_drp) <- FALSE
      enabled(check_btn) <- FALSE

      enabled(kit_chk) <- TRUE
    }

    # Disable 'word' if 'exact' is TRUE.
    if (val_exact) {
      enabled(f1_word_chk) <- FALSE
    } else {
      enabled(f1_word_chk) <- TRUE
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
      if (exists(".strvalidator_filterProfile_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_filterProfile_gui_savegui", envir = env)
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
      if (exists(".strvalidator_filterProfile_gui_invert", envir = env, inherits = FALSE)) {
        svalue(f1_invert_chk) <- get(".strvalidator_filterProfile_gui_invert", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_add_loci", envir = env, inherits = FALSE)) {
        svalue(f1_add_missing_loci_chk) <- get(".strvalidator_filterProfile_gui_add_loci", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_keep_na", envir = env, inherits = FALSE)) {
        svalue(f1_keep_na_chk) <- get(".strvalidator_filterProfile_gui_keep_na", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_ignore_case", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_case_chk) <- get(".strvalidator_filterProfile_gui_ignore_case", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_filterProfile_gui_exact", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_filterProfile_gui_word", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_by", envir = env, inherits = FALSE)) {
        svalue(f1_filter_opt) <- get(".strvalidator_filterProfile_gui_by", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_filterProfile_gui_sex", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_filterProfile_gui_qs", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_filterProfile_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_invert", value = svalue(f1_invert_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_add_loci", value = svalue(f1_add_missing_loci_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_keep_na", value = svalue(f1_keep_na_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_ignore_case", value = svalue(f1_ignore_case_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_by", value = svalue(f1_filter_opt), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_qs", value = svalue(f1_qs_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_filterProfile_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_invert", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_invert", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_add_loci", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_add_loci", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_keep_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_keep_na", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_ignore_case", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_ignore_case", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_exact", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_word", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_by", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_by", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_sex", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_qs", envir = env)
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
  .refreshOptions()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
