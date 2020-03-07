################################################################################
# CHANGE LOG (last 20 changes)
# 07.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 06.09.2016: Implemented the 'word' option.
# 24.04.2016: First version.

#' @title Calculate Profile Slope
#'
#' @description
#' GUI wrapper for the \code{\link{calculateSlope}} function.
#'
#' @details Simplifies the use of the \code{\link{calculateSlope}} function
#' by providing a graphical user interface.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help str
#'
#' @seealso \code{\link{calculateSlope}}

calculateSlope_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  .gData <- NULL
  .gRef <- NULL
  .gDataName <- NULL
  .gRefName <- NULL

  # Language ------------------------------------------------------------------
  
  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])
  
  if (debug) {
    print(paste("IN:", fnc))
  }
  
  # Default strings.
  strWinTitle <- "Calculate profile slope"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Sample dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefDataset <- "Reference dataset:"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strFrmOptions <- "Options"
  strLblGroups <- "Groups:"
  strLblConf <- "Confidence limit:"
  strLblKit <- "Kit to calculate size from:"
  strChkKit <- "Autodetect"
  strTipKit <- "Must be checked for multiple kits."
  strTipKitDrp <- "Not needed if 'Size' is provided in the dataset."
  strLblMatching <- "Reference sample name matching:"
  strChkIgnore <- "Ignore case"
  strChkWord <- "Add word boundaries"
  strChkExact <- "Exact matching"
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
    
    strTmp <- dtStrings["strDrpDataset"]$Value
    strDrpDataset <- ifelse(is.na(strTmp), strDrpDataset, strTmp)
    
    strTmp <- dtStrings["strLblSamples"]$Value
    strLblSamples <- ifelse(is.na(strTmp), strLblSamples, strTmp)
    
    strTmp <- dtStrings["strLblRefDataset"]$Value
    strLblRefDataset <- ifelse(is.na(strTmp), strLblRefDataset, strTmp)
    
    strTmp <- dtStrings["strLblRef"]$Value
    strLblRef <- ifelse(is.na(strTmp), strLblRef, strTmp)
    
    strTmp <- dtStrings["strBtnCheck"]$Value
    strBtnCheck <- ifelse(is.na(strTmp), strBtnCheck, strTmp)
    
    strTmp <- dtStrings["strFrmOptions"]$Value
    strFrmOptions <- ifelse(is.na(strTmp), strFrmOptions, strTmp)
    
    strTmp <- dtStrings["strLblGroups"]$Value
    strLblGroups <- ifelse(is.na(strTmp), strLblGroups, strTmp)
    
    strTmp <- dtStrings["strLblConf"]$Value
    strLblConf <- ifelse(is.na(strTmp), strLblConf, strTmp)
    
    strTmp <- dtStrings["strLblKit"]$Value
    strLblKit <- ifelse(is.na(strTmp), strLblKit, strTmp)
    
    strTmp <- dtStrings["strChkKit"]$Value
    strChkKit <- ifelse(is.na(strTmp), strChkKit, strTmp)
    
    strTmp <- dtStrings["strTipKit"]$Value
    strTipKit <- ifelse(is.na(strTmp), strTipKit, strTmp)
    
    strTmp <- dtStrings["strTipKitDrp"]$Value
    strTipKitDrp <- ifelse(is.na(strTmp), strTipKitDrp, strTmp)
    
    strTmp <- dtStrings["strLblMatching"]$Value
    strLblMatching <- ifelse(is.na(strTmp), strLblMatching, strTmp)
    
    strTmp <- dtStrings["strChkIgnore"]$Value
    strChkIgnore <- ifelse(is.na(strTmp), strChkIgnore, strTmp)
    
    strTmp <- dtStrings["strChkWord"]$Value
    strChkWord <- ifelse(is.na(strTmp), strChkWord, strTmp)
    
    strTmp <- dtStrings["strChkExact"]$Value
    strChkExact <- ifelse(is.na(strTmp), strChkExact, strTmp)
    
    strTmp <- dtStrings["strFrmSave"]$Value
    strFrmSave <- ifelse(is.na(strTmp), strFrmSave, strTmp)
    
    strTmp <- dtStrings["strLblSave"]$Value
    strLblSave <- ifelse(is.na(strTmp), strLblSave, strTmp)
    
    strTmp <- dtStrings["strBtnCalculate"]$Value
    strBtnCalculate <- ifelse(is.na(strTmp), strBtnCalculate, strTmp)
    
    strTmp <- dtStrings["strBtnProcessing"]$Value
    strBtnProcessing <- ifelse(is.na(strTmp), strBtnProcessing, strTmp)
    
    strTmp <- dtStrings["strMsgDataset"]$Value
    strMsgDataset <- ifelse(is.na(strTmp), strMsgDataset, strTmp)
    
    strTmp <- dtStrings["strMsgTitleDataset"]$Value
    strMsgTitleDataset <- ifelse(is.na(strTmp), strMsgTitleDataset, strTmp)
    
    strTmp <- dtStrings["strMsgCheck"]$Value
    strMsgCheck <- ifelse(is.na(strTmp), strMsgCheck, strTmp)
    
    strTmp <- dtStrings["strWinTitleCheck"]$Value
    strWinTitleCheck <- ifelse(is.na(strTmp), strWinTitleCheck, strTmp)
    
    strTmp <- dtStrings["strMsgTitleError"]$Value
    strMsgTitleError <- ifelse(is.na(strTmp), strMsgTitleError, strTmp)
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

  gv <- ggroup(
    horizontal = FALSE,
    spacing = 8,
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
    spacing = 5,
    container = gv
  )

  f0g0 <- glayout(container = f0, spacing = 1)

  # Datasets ------------------------------------------------------------------

  f0g0[1, 1] <- glabel(text = strLblDataset, container = f0g0)

  f0g0[1, 2] <- f0_dataset_drp <- gcombobox(
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

  f0g0[1, 3] <- f0_samples_lbl <- glabel(text = paste(" 0", strLblSamples),
                                         container = f0g0)

  addHandlerChanged(f0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(f0_dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
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
      svalue(f0_samples_lbl) <- paste("", samples, strLblSamples)
      svalue(f2_save_edt) <- paste(.gDataName, "_slope", sep = "")
      svalue(f1_groups_lbl) <- paste(strLblGroups, unique(.gData$Group))

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(f1_kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f0_dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(f2_save_edt) <- ""
      svalue(f1_groups_lbl) <- paste(strLblGroups, "")
    }
  })

  f0g0[2, 1] <- glabel(text = strLblRefDataset, container = f0g0)

  f0g0[2, 2] <- f0_refset_drp <- gcombobox(
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

  f0g0[2, 3] <- f0_ref_lbl <- glabel(text = paste(" 0", strLblRef),
                                     container = f0g0)

  addHandlerChanged(f0_refset_drp, handler = function(h, ...) {
    val_obj <- svalue(f0_refset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj
      refs <- length(unique(.gRef$Sample.Name))
      svalue(f0_ref_lbl) <- paste("", refs, strLblRef)
    } else {

      # Reset components.
      .gRef <<- NULL
      svalue(f0_refset_drp, index = TRUE) <- 1
      svalue(f0_ref_lbl) <- paste(" 0", strLblRef)
    }
  })

  # CHECK ---------------------------------------------------------------------

  f0g0[3, 2] <- f0_check_btn <- gbutton(text = strBtnCheck, container = f0g0)

  addHandlerChanged(f0_check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)

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
        word = val_word,
        exact = val_exact
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
    spacing = 5, 
    anchor = c(-1, 0),
    container = gv
  )

  #----------------------------------------------------------------------------
  
  f1_groups_lbl <- glabel(text = strLblGroups, anchor = c(-1, 0), container = f1)
  
  #----------------------------------------------------------------------------
  
  f1g1 <- glayout(container = f1, spacing = 1, anchor = c(-1, 0))

  f1g1[1, 1] <- glabel(
    text = strLblConf, 
    anchor = c(-1, 0),
    container = f1g1
  )
  f1g1[1, 2] <- f1_conf_spn <- gspinbutton(
    from = 0, to = 1, by = 0.005,
    value = 0.975, container = f1g1
  )

  #----------------------------------------------------------------------------

  f1g1[2, 1] <- glabel(
    text = strLblKit, anchor = c(-1, 0),
    container = f1g1
  )

  f1g1[3, 1] <- f1_auto_chk <- gcheckbox(
    text = strChkKit, checked = FALSE,
    container = f1g1
  )
  tooltip(f1_auto_chk) <- strTipKit

  f1g1[3, 2] <- f1_kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f1g1,
    ellipsize = "none"
  )
  tooltip(f1_kit_drp) <- strTipKitDrp

  #----------------------------------------------------------------------------
  glabel(
    text = strLblMatching, anchor = c(-1, 0),
    container = f1
  )

  f1_ignore_chk <- gcheckbox(
    text = strChkIgnore, checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = strChkWord, checked = FALSE,
    container = f1
  )

  f1_exact_chk <- gcheckbox(
    text = strChkExact, checked = FALSE,
    container = f1
  )

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f1_auto_chk, handler = function(h, ...) {

    # Get values.
    val_auto <- svalue(f1_auto_chk)

    if (val_auto) {
      enabled(f1_kit_drp) <- FALSE
    } else {
      enabled(f1_kit_drp) <- TRUE
    }
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(text = strFrmSave, horizontal = TRUE, spacing = 5, container = gv)

  glabel(text = strLblSave, container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE, fill = TRUE)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- .gDataName
    val_name_ref <- .gRefName
    val_conf <- svalue(f1_conf_spn)
    val_kit <- svalue(f1_kit_drp)
    val_name <- svalue(f2_save_edt)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    val_auto <- svalue(f1_auto_chk)

    if (!is.null(val_data)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      # Check if automatic kit detection.
      if (val_auto) {
        val_kit <- NULL
      }

      datanew <- calculateSlope(
        data = val_data, ref = val_ref, conf = val_conf,
        kit = val_kit,
        ignore.case = val_ignore, exact = val_exact,
        word = val_word, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "conf", "kit", "auto",
        "ignore.case", "word", "exact"
      )

      values <- list(
        val_name_data, val_name_ref, val_conf, val_kit, val_auto,
        val_ignore, val_word, val_exact
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
      gmessage(msg = strMsgDataset,
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
      if (exists(".strvalidator_calculateSlope_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateSlope_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateSlope_gui_conf", envir = env, inherits = FALSE)) {
        svalue(f1_conf_spn) <- get(".strvalidator_calculateSlope_gui_conf", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculateSlope_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_calculateSlope_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_calculateSlope_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_auto", envir = env, inherits = FALSE)) {
        svalue(f1_auto_chk) <- get(".strvalidator_calculateSlope_gui_auto", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateSlope_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_conf", value = svalue(f1_conf_spn), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_auto", value = svalue(f1_auto_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateSlope_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_conf", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_conf", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_auto", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_auto", envir = env)
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
