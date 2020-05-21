################################################################################
# CHANGE LOG (last 20 changes)
# 03.03.2020: Added language support.
# 03.03.2020: Expand scrollable checkbox view under RGtk2.
# 19.02.2019: Expand text field under tcltk. Scrollable checkbox view.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 06.01.2014: Fixed button name used as 'save as' name.
# 20.11.2013: Fixed result now stored in variable 'datanew' insted of 'val_name'.
# 29.09.2013: First version.


#' @title Calculate Bins Overlap
#'
#' @description
#' GUI wrapper for the \code{\link{calculateOverlap}} function.
#'
#' @details By analysis of the bins overlap between dye channels a measure of
#' the risk for spectral pull-up artefacts can be obtain. The default result
#' is a matrix with the total bins overlap in number of base pairs. If an allele
#' frequency database is provided the overlap at each bin is multiplied with the
#' frequency of the corresponding allele. If no frequence exist for that allele
#' a frequency of 5/2N will be used. X and Y alleles is given the frequency 1.
#' A scoring matrix can be supplied to reduce the effect by spectral distance,
#' meaning that overlap with the neighboring dye can be counted in full (100%)
#' while a non neighbor dye get its overlap reduced (to e.g. 10%).
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
#' @seealso \code{\link{calculateOverlap}}

calculateOverlap_gui <- function(env = parent.frame(), savegui = NULL, debug = TRUE, parent = NULL) {

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Analyse bins overlap"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmKits <- "Select kits"
  strFrmOptions <- "Options"
  strChkMultiply <- "Multiply overlap with allele frequency"
  strChkVirtual <- "Include virtual bins in analysis"
  strTipVirtual <- "NB! Not all vendors specify which alleles are virtual in the bins file. This can be done manually in the kit.txt file."
  strChkPenalty <- "Apply spectral channel penalty"
  strFrmPenalty <- "Penalty"
  strLblPenalty <- "Define penalty by the distance between dye channels  (1st neighbor to 5th neighbor)"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgColor <- "Kit color set must be identical for multiple kit comparison!\nAnalyse one kit at a time!"
  strMsgKit <- "At least one kit must be selected."
  strMsgTitleKit <- "No kit selected"
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

    strtmp <- dtStrings["strFrmKits"]$value
    strFrmKits <- ifelse(is.na(strtmp), strFrmKits, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkMultiply"]$value
    strChkMultiply <- ifelse(is.na(strtmp), strChkMultiply, strtmp)

    strtmp <- dtStrings["strChkVirtual"]$value
    strChkVirtual <- ifelse(is.na(strtmp), strChkVirtual, strtmp)

    strtmp <- dtStrings["strTipVirtual"]$value
    strTipVirtual <- ifelse(is.na(strtmp), strTipVirtual, strtmp)

    strtmp <- dtStrings["strChkPenalty"]$value
    strChkPenalty <- ifelse(is.na(strtmp), strChkPenalty, strtmp)

    strtmp <- dtStrings["strFrmPenalty"]$value
    strFrmPenalty <- ifelse(is.na(strtmp), strFrmPenalty, strtmp)

    strtmp <- dtStrings["strLblPenalty"]$value
    strLblPenalty <- ifelse(is.na(strtmp), strLblPenalty, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strMsgColor"]$value
    strMsgColor <- ifelse(is.na(strtmp), strMsgColor, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strMsgKit"]$value
    strMsgKit <- ifelse(is.na(strtmp), strMsgKit, strtmp)

    strtmp <- dtStrings["strMsgTitleKit"]$value
    strMsgTitleKit <- ifelse(is.na(strtmp), strMsgTitleKit, strtmp)
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
    text = strFrmKits,
    horizontal = TRUE,
    spacing = 5,
    container = gv,
    expand = TRUE,
    fill = TRUE
  )

  scroll_view <- ggroup(
    horizontal = FALSE,
    use.scrollwindow = TRUE,
    container = f0,
    expand = TRUE,
    fill = TRUE
  )

  kit_checkbox_group <- gcheckboxgroup(
    items = getKit(),
    checked = FALSE,
    horizontal = FALSE,
    container = scroll_view
  )

  addHandlerChanged(kit_checkbox_group, handler = function(h, ...) {
    val_kits <- svalue(kit_checkbox_group)

    if (debug) {
      print("val_kits")
      print(val_kits)
    }

    # Initiate.
    kitColor <- list()

    # check if any selected kit.
    if (length(val_kits) > 0) {

      # Get selected kits.
      for (k in seq(along = val_kits)) {
        kit <- getKit(val_kits[k], what = "Color")
        kitColor[k] <- list(unique(kit$Color))
      }

      # Check if identical.
      if (all(sapply(kitColor, identical, kitColor[[1]]))) {

        # Calculate number of penalty elements.
        elements <- length(kitColor[[1]]) - 1

        # Enable spin buttons, simple and effective...
        if (elements == 1) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- FALSE
          enabled(f2_penalty3_spb) <- FALSE
          enabled(f2_penalty4_spb) <- FALSE
          enabled(f2_penalty5_spb) <- FALSE
        }
        if (elements == 2) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- TRUE
          enabled(f2_penalty3_spb) <- FALSE
          enabled(f2_penalty4_spb) <- FALSE
          enabled(f2_penalty5_spb) <- FALSE
        }
        if (elements == 3) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- TRUE
          enabled(f2_penalty3_spb) <- TRUE
          enabled(f2_penalty4_spb) <- FALSE
          enabled(f2_penalty5_spb) <- FALSE
        }
        if (elements == 4) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- TRUE
          enabled(f2_penalty3_spb) <- TRUE
          enabled(f2_penalty4_spb) <- TRUE
          enabled(f2_penalty5_spb) <- FALSE
        }
        if (elements == 5) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- TRUE
          enabled(f2_penalty3_spb) <- TRUE
          enabled(f2_penalty4_spb) <- TRUE
          enabled(f2_penalty5_spb) <- TRUE
        }

        # Enable analyse button.
        enabled(analyse_btn) <- TRUE

        # Suggest a save name.
        svalue(save_edt) <- paste(paste(val_kits, collapse = "_"),
          "_overlap",
          sep = ""
        )
      } else {
        # Show error message.
        gmessage(
          msg = strMsgColor,
          title = strMsgTitleError,
          icon = "error", parent = w
        )

        # Disable analyse button.
        enabled(analyse_btn) <- FALSE
      }
    } else {

      # Disable all.
      enabled(f2_penalty1_spb) <- FALSE
      enabled(f2_penalty2_spb) <- FALSE
      enabled(f2_penalty3_spb) <- FALSE
      enabled(f2_penalty4_spb) <- FALSE
      enabled(f2_penalty5_spb) <- FALSE

      # Disable analyse button.
      enabled(analyse_btn) <- FALSE

      # Empty save name.
      svalue(save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f1_db_chk <- gcheckbox(
    text = strChkMultiply,
    checked = TRUE,
    container = f1
  )

  f1_db_names <- getDb()

  f1_db_drp <- gcombobox(
    items = f1_db_names, fill = FALSE, selected = 1,
    container = f1, ellipsize = "none"
  )

  f1_virtual_chk <- gcheckbox(
    text = strChkVirtual,
    checked = TRUE,
    container = f1
  )
  tooltip(f1_virtual_chk) <- strTipVirtual

  f1_penalty_chk <- gcheckbox(
    text = strChkPenalty,
    checked = TRUE,
    container = f1
  )

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f1_db_chk, handler = function(h, ...) {
    val_chk <- svalue(f1_db_chk)
    if (val_chk) {
      enabled(f1_db_drp) <- TRUE
    } else {
      enabled(f1_db_drp) <- FALSE
    }
  })

  addHandlerChanged(f1_penalty_chk, handler = function(h, ...) {
    val_chk <- svalue(f1_penalty_chk)
    if (val_chk) {
      enabled(f2) <- TRUE
    } else {
      enabled(f2) <- FALSE
    }
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strFrmPenalty,
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  glabel(
    text = strLblPenalty,
    container = f2
  )

  f2g1 <- ggroup(
    horizontal = TRUE,
    spacing = 5,
    container = f2
  )

  # Penalty vector elements.
  f2_penalty1_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 1, container = f2g1
  )
  f2_penalty2_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0.1, container = f2g1
  )
  f2_penalty3_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0.01, container = f2g1
  )
  f2_penalty4_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0, container = f2g1
  )
  f2_penalty5_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0, container = f2g1
  )

  # Disable all until a kit is selected.
  enabled(f2_penalty1_spb) <- FALSE
  enabled(f2_penalty2_spb) <- FALSE
  enabled(f2_penalty3_spb) <- FALSE
  enabled(f2_penalty4_spb) <- FALSE
  enabled(f2_penalty5_spb) <- FALSE

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  analyse_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(analyse_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)
    val_kits <- svalue(kit_checkbox_group)
    val_kitData <- data.frame() # Filled further down.
    val_db_chk <- svalue(f1_db_chk)
    val_db_selected <- svalue(f1_db_drp)
    val_db <- NULL # Filled further down.
    val_virtual <- svalue(f1_virtual_chk)
    val_penalty_chk <- svalue(f1_penalty_chk)
    val_penalty <- NULL

    if (length(val_kits) > 0) {

      # Change button.
      blockHandlers(analyse_btn)
      svalue(analyse_btn) <- strBtnProcessing
      unblockHandlers(analyse_btn)
      enabled(analyse_btn) <- FALSE

      # Get kits.
      for (k in seq(along = val_kits)) {
        tmp <- getKit(val_kits[k])
        val_kitData <- rbind(val_kitData, tmp)
      }

      # Get allele frequency database.
      if (val_db_chk) {
        val_db <- getDb(val_db_selected)
      } else {
        val_db <- NULL
      }

      # Get penalty.
      if (val_penalty_chk) {
        # Simple but not so nice solution...
        if (enabled(f2_penalty1_spb)) {
          val_penalty[1] <- svalue(f2_penalty1_spb)
        }
        if (enabled(f2_penalty2_spb)) {
          val_penalty[2] <- svalue(f2_penalty2_spb)
        }
        if (enabled(f2_penalty3_spb)) {
          val_penalty[3] <- svalue(f2_penalty3_spb)
        }
        if (enabled(f2_penalty4_spb)) {
          val_penalty[4] <- svalue(f2_penalty4_spb)
        }
        if (enabled(f2_penalty5_spb)) {
          val_penalty[5] <- svalue(f2_penalty5_spb)
        }
      } else {
        val_penalty <- NULL
      }

      if (debug) {
        print("val_kits")
        print(val_kits)
        print("val_db_chk")
        print(val_db_chk)
        print("val_penalty_chk")
        print(val_penalty_chk)
        print("val_penalty")
        print(val_penalty)
        print("val_virtual")
        print(val_virtual)
      }

      # Analyse bins overlap.
      datanew <- calculateOverlap(
        data = val_kitData,
        db = val_db,
        penalty = val_penalty,
        virtual = val_virtual,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "db", "penalty", "virtual")

      values <- list(val_kits, val_db_selected, val_penalty, val_virtual)

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
        msg = strMsgKit,
        title = strMsgTitleKit,
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
      if (exists(".strvalidator_calculateOverlap_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateOverlap_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateOverlap_gui_db", envir = env, inherits = FALSE)) {
        svalue(f1_db_chk) <- get(".strvalidator_calculateOverlap_gui_db", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_db_name", envir = env, inherits = FALSE)) {
        svalue(f1_db_drp) <- get(".strvalidator_calculateOverlap_gui_db_name", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_virtual", envir = env, inherits = FALSE)) {
        svalue(f1_virtual_chk) <- get(".strvalidator_calculateOverlap_gui_virtual", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_penalty", envir = env, inherits = FALSE)) {
        svalue(f1_penalty_chk) <- get(".strvalidator_calculateOverlap_gui_penalty", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p1", envir = env, inherits = FALSE)) {
        svalue(f2_penalty1_spb) <- get(".strvalidator_calculateOverlap_gui_kit_p1", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p2", envir = env, inherits = FALSE)) {
        svalue(f2_penalty2_spb) <- get(".strvalidator_calculateOverlap_gui_kit_p2", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p3", envir = env, inherits = FALSE)) {
        svalue(f2_penalty3_spb) <- get(".strvalidator_calculateOverlap_gui_kit_p3", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p4", envir = env, inherits = FALSE)) {
        svalue(f2_penalty4_spb) <- get(".strvalidator_calculateOverlap_gui_kit_p4", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p5", envir = env, inherits = FALSE)) {
        svalue(f2_penalty5_spb) <- get(".strvalidator_calculateOverlap_gui_kit_p5", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateOverlap_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_db", value = svalue(f1_db_chk), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_db_name", value = svalue(f1_db_drp), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_virtual", value = svalue(f1_virtual_chk), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_penalty", value = svalue(f1_penalty_chk), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_kit_p1", value = svalue(f2_penalty1_spb), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_kit_p2", value = svalue(f2_penalty2_spb), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_kit_p3", value = svalue(f2_penalty3_spb), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_kit_p4", value = svalue(f2_penalty4_spb), envir = env)
      assign(x = ".strvalidator_calculateOverlap_gui_kit_p5", value = svalue(f2_penalty5_spb), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateOverlap_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_db", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_db", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_db_name", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_db_name", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_virtual", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_virtual", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_penalty", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_penalty", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p1", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_kit_p1", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p2", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_kit_p2", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p3", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_kit_p3", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p4", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_kit_p4", envir = env)
      }
      if (exists(".strvalidator_calculateOverlap_gui_kit_p5", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOverlap_gui_kit_p5", envir = env)
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
