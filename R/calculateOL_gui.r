################################################################################
# CHANGE LOG (last 20 changes)
# 03.03.2020: Added language support.
# 03.03.2020: Expand scrollable checkbox view.
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
# 21.01.2014: Added parameter 'limit'.
# 06.01.2014: Fixed button name used as 'save as' name.
# 20.11.2013: Fixed result now stored in variable 'datanew' insted of 'val_name'.
# 29.09.2013: First version.


#' @title Analyze Off-ladder Alleles
#'
#' @description
#' GUI wrapper for the \code{\link{calculateOL}} function.
#'
#' @details By analysis of the allelic ladder the risk for getting off-ladder
#' (OL) alleles are calculated. The frequencies from a provided population
#' database is used to calculate the risk per marker and in total for the given
#' kit(s). Virtual alleles can be excluded from the calculation.
#' Small frequencies can be limited to the estimate 5/2N.
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
#' @importFrom utils help str
#'
#' @seealso \code{\link{calculateOL}}

calculateOL_gui <- function(env = parent.frame(), savegui = NULL, debug = TRUE, parent = NULL) {

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Analyse off-ladder alleles"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmKits <- "Select kits"
  strFrmOptions <- "Options"
  strLblDatabase <- "Select allele frequency database:"
  strChkVirtual <- "Include virtual bins in analysis"
  strTipVirtual <- "NB! Not all vendors specify which alleles are virtual in the bins file. This can be done manually in the kit.txt file."
  strChkLowFreq <- "Limit small frequencies to 5/2N"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgKit <- "At least one kit must be selected."
  strMsgTitleKit <- "No kit selected"

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

    strTmp <- dtStrings["strFrmKits"]$Value
    strFrmKits <- ifelse(is.na(strTmp), strFrmKits, strTmp)

    strTmp <- dtStrings["strFrmOptions"]$Value
    strFrmOptions <- ifelse(is.na(strTmp), strFrmOptions, strTmp)

    strTmp <- dtStrings["strLblDatabase"]$Value
    strLblDatabase <- ifelse(is.na(strTmp), strLblDatabase, strTmp)

    strTmp <- dtStrings["strChkVirtual"]$Value
    strChkVirtual <- ifelse(is.na(strTmp), strChkVirtual, strTmp)

    strTmp <- dtStrings["strTipVirtual"]$Value
    strTipVirtual <- ifelse(is.na(strTmp), strTipVirtual, strTmp)

    strTmp <- dtStrings["strFrmSave"]$Value
    strFrmSave <- ifelse(is.na(strTmp), strFrmSave, strTmp)

    strTmp <- dtStrings["strLblSave"]$Value
    strLblSave <- ifelse(is.na(strTmp), strLblSave, strTmp)

    strTmp <- dtStrings["strBtnCalculate"]$Value
    strBtnCalculate <- ifelse(is.na(strTmp), strBtnCalculate, strTmp)

    strTmp <- dtStrings["strBtnProcessing"]$Value
    strBtnProcessing <- ifelse(is.na(strTmp), strBtnProcessing, strTmp)

    strTmp <- dtStrings["strMsgKit"]$Value
    strMsgKit <- ifelse(is.na(strTmp), strMsgKit, strTmp)

    strTmp <- dtStrings["strMsgTitleKit"]$Value
    strMsgTitleKit <- ifelse(is.na(strTmp), strMsgTitleKit, strTmp)
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

    # check if any selected kit.
    if (length(val_kits) > 0) {

      # Enable analyse button.
      enabled(analyse_btn) <- TRUE

      # Suggest a save name.
      svalue(save_edt) <- paste(paste(val_kits, collapse = "_"),
        "_OL",
        sep = ""
      )
    } else {

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

  glabel(
    text = strLblDatabase,
    anchor = c(-1, 0), container = f1
  )

  f1_db_names <- getDb()

  f1_db_drp <- gcombobox(
    items = f1_db_names, fill = FALSE,
    selected = 1, container = f1, ellipsize = "none"
  )

  f1_virtual_chk <- gcheckbox(
    text = strChkVirtual,
    checked = TRUE,
    container = f1
  )
  tooltip(f1_virtual_chk) <- strTipVirtual

  f1_limit_chk <- gcheckbox(
    text = strChkLowFreq,
    checked = TRUE,
    container = f1
  )

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
    val_db_selected <- svalue(f1_db_drp)
    val_db <- NULL # Filled further down.
    val_virtual <- svalue(f1_virtual_chk)
    val_limit <- svalue(f1_limit_chk)

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
      val_db <- getDb(val_db_selected)

      if (debug) {
        print("val_kits")
        print(val_kits)
        print("val_virtual")
        print(val_virtual)
        print("val_limit")
        print(val_limit)
      }

      # Analyse bins overlap.
      datanew <- calculateOL(
        kit = val_kitData,
        db = val_db,
        virtual = val_virtual,
        limit = val_limit,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("kit", "db", "virtual", "limit")

      values <- list(val_kits, val_db_selected, val_virtual, val_limit)

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
        mmsg = strMsgKit,
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
      if (exists(".strvalidator_calculateOL_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateOL_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateOL_gui_db_name", envir = env, inherits = FALSE)) {
        svalue(f1_db_drp) <- get(".strvalidator_calculateOL_gui_db_name", envir = env)
      }
      if (exists(".strvalidator_calculateOL_gui_virtual", envir = env, inherits = FALSE)) {
        svalue(f1_virtual_chk) <- get(".strvalidator_calculateOL_gui_virtual", envir = env)
      }
      if (exists(".strvalidator_calculateOL_gui_limit", envir = env, inherits = FALSE)) {
        svalue(f1_limit_chk) <- get(".strvalidator_calculateOL_gui_limit", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateOL_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateOL_gui_db_name", value = svalue(f1_db_drp), envir = env)
      assign(x = ".strvalidator_calculateOL_gui_virtual", value = svalue(f1_virtual_chk), envir = env)
      assign(x = ".strvalidator_calculateOL_gui_limit", value = svalue(f1_limit_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateOL_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOL_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateOL_gui_db_name", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOL_gui_db_name", envir = env)
      }
      if (exists(".strvalidator_calculateOL_gui_virtual", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOL_gui_virtual", envir = env)
      }
      if (exists(".strvalidator_calculateOL_gui_limit", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateOL_gui_limit", envir = env)
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
