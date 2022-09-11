################################################################################
# CHANGE LOG (last 20 changes)
# 05.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 03.03.2020: Fixed reference to function name.
# 01.03.2020: Added language support.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.08.2016: Added save settings.
# 14.08.2016: Renamed to calculateCopies_gui and implementing new calculateCopies.
# 16.06.2016: 'Save as' textbox expandable.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.

#' @title Calculate Allele Copies
#'
#' @description
#' GUI wrapper for the \code{link{calculateCopies}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateCopies}} function by
#' providing a graphical user interface to it.
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
#' @seealso \code{\link{calculateCopies}}

calculateCopies_gui <- function(env = parent.frame(), savegui = NULL,
                                debug = FALSE, parent = NULL) {


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
  strWinTitle <- "Calculate allele copies"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strChkObserved <- "Add number of unique alleles (count number of peaks)"
  strChkCopies <- "Add number of allele copies (for known complete profiles)"
  strTipCopies <- "'1' for heterozygotes and '2' for homozygotes."
  strChkHeterozygosity <- "Add heterozygote indicator (for known complete profiles)"
  strTipHeterozygosity <- "'1' for heterozygous loci and '0' for homozygous loci."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A dataset has to be selected."
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

    strtmp <- dtStrings["strDrpDefault"]$value
    strDrpDefault <- ifelse(is.na(strtmp), strDrpDefault, strtmp)

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkObserved"]$value
    strChkObserved <- ifelse(is.na(strtmp), strChkObserved, strtmp)

    strtmp <- dtStrings["strChkCopies"]$value
    strChkCopies <- ifelse(is.na(strtmp), strChkCopies, strtmp)

    strtmp <- dtStrings["strTipCopies"]$value
    strTipCopies <- ifelse(is.na(strtmp), strTipCopies, strtmp)

    strtmp <- dtStrings["strChkHeterozygosity"]$value
    strChkHeterozygosity <- ifelse(is.na(strtmp), strChkHeterozygosity, strtmp)

    strtmp <- dtStrings["strTipHeterozygosity"]$value
    strTipHeterozygosity <- ifelse(is.na(strtmp), strTipHeterozygosity, strtmp)

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

    # Destroys window.
    return(FALSE)
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = w,
    expand = FALSE,
    fill = "x"
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(
    text = strChkGui, checked = FALSE,
    container = gh
  )
  enabled(savegui_chk) <- FALSE

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset, horizontal = TRUE, spacing = 1,
    container = gv, expand = FALSE, fill = "x"
  )

  # Datasets ------------------------------------------------------------------

  glabel(text = strLblDataset, container = f0)

  samples_lbl <- glabel(text = paste(" 0", strLblSamples), container = f0)

  dataset_drp <- gcombobox(
    items = c(
      strDrpDefault,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1, editable = FALSE, container = f0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
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
      svalue(save_edt) <- paste(val_obj, "_cop", sep = "")
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions, horizontal = FALSE,
    spacing = 1, container = gv
  )

  f1_observed_chk <- gcheckbox(
    text = strChkObserved,
    checked = FALSE, container = f1
  )

  f1_copies_chk <- gcheckbox(
    text = strChkCopies,
    checked = TRUE, container = f1
  )
  tooltip(f1_copies_chk) <- strTipCopies

  f1_het_chk <- gcheckbox(
    text = strChkHeterozygosity,
    checked = FALSE, container = f1
  )
  tooltip(f1_het_chk) <- strTipHeterozygosity

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)
    val_data <- .gData
    val_data_name <- .gDataName
    val_obs <- svalue(f1_observed_chk)
    val_cop <- svalue(f1_copies_chk)
    val_het <- svalue(f1_het_chk)

    if (!is.null(val_data)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateCopies(
        data = val_data, observed = val_obs,
        copies = val_cop, heterozygous = val_het,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "observed", "copies", "heterozygous")

      values <- list(val_data_name, val_obs, val_cop, val_het)

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
        msg = strMsgDataset, title = strMsgTitleDataset,
        icon = "error", parent = w
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
      if (exists(".strvalidator_calculateCopies_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateCopies_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateCopies_gui_observed", envir = env, inherits = FALSE)) {
        svalue(f1_observed_chk) <- get(".strvalidator_calculateCopies_gui_observed", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_copies", envir = env, inherits = FALSE)) {
        svalue(f1_copies_chk) <- get(".strvalidator_calculateCopies_gui_copies", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_het", envir = env, inherits = FALSE)) {
        svalue(f1_het_chk) <- get(".strvalidator_calculateCopies_gui_het", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateCopies_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateCopies_gui_observed", value = svalue(f1_observed_chk), envir = env)
      assign(x = ".strvalidator_calculateCopies_gui_copies", value = svalue(f1_copies_chk), envir = env)
      assign(x = ".strvalidator_calculateCopies_gui_het", value = svalue(f1_het_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateCopies_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateCopies_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_observed", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateCopies_gui_observed", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_copies", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateCopies_gui_copies", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_het", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateCopies_gui_het", envir = env)
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
