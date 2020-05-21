################################################################################
# CHANGE LOG (last 20 changes)
# 03.03.2020: Fixed reference to function name.
# 27.02.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 25.04.2016: 'Save as' textbox expandable.
# 30.11.2015: Added warning.
# 28.08.2015: Added importFrom
# 10.06.2015: Added missing label 'Significance level:'.
# 26.05.2015: First version.

#' @title Calculate Analytical Threshold
#'
#' @description
#' GUI wrapper for the \code{\link{calculateAT6}} function.
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
#' @seealso \code{\link{calculateAT6}}, \code{\link{calculateAT}},
#'  \code{\link{calculateAT_gui}}, \code{\link{checkSubset}}

calculateAT6_gui <- function(env = parent.frame(), savegui = NULL,
                             debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gRef <- NULL
  .gAm <- NULL
  .gNameData <- NULL
  .gNameRef <- NULL
  .gNameAm <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate analytical threshold"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblDatasetRef <- "Reference dataset:"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strLblAmount <- "Amount dataset:"
  strFrmOptions <- "Options"
  strLblText1 <- "NB! This is an indirect method not recommended."
  strLblText2 <- "See 'Help' or reference for limitations."
  strChkIgnore <- "Ignore case"
  strRadItem1 <- "Linear regression"
  strRadItem2 <- "Weighted linear regression"
  strLblLevel <- "Significance level:"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strWinCheck <- "Check subsetting"
  strMsgTitleError <- "Error"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a dataset and a reference set"
  strMsgDataset <- "A dataset and a reference dataset must be selected."
  strMsgTitleDataset <- "Datasets not selected"

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

    strtmp <- dtStrings["strLblDatasetRef"]$value
    strLblDatasetRef <- ifelse(is.na(strtmp), strLblDatasetRef, strtmp)

    strtmp <- dtStrings["strLblRef"]$value
    strLblRef <- ifelse(is.na(strtmp), strLblRef, strtmp)

    strtmp <- dtStrings["strBtnCheck"]$value
    strBtnCheck <- ifelse(is.na(strtmp), strBtnCheck, strtmp)

    strtmp <- dtStrings["strLblAmount"]$value
    strLblAmount <- ifelse(is.na(strtmp), strLblAmount, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblText1"]$value
    strLblText1 <- ifelse(is.na(strtmp), strLblText1, strtmp)

    strtmp <- dtStrings["strLblText1"]$value
    strLblText1 <- ifelse(is.na(strtmp), strLblText1, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strRadItem1"]$value
    strRadItem1 <- ifelse(is.na(strtmp), strRadItem1, strtmp)

    strtmp <- dtStrings["strRadItem2"]$value
    strRadItem2 <- ifelse(is.na(strtmp), strRadItem2, strtmp)

    strtmp <- dtStrings["strLblLevel"]$value
    strLblLevel <- ifelse(is.na(strtmp), strLblLevel, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strWinCheck"]$value
    strWinCheck <- ifelse(is.na(strtmp), strWinCheck, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strMsgCheck"]$value
    strMsgCheck <- ifelse(is.na(strtmp), strMsgCheck, strtmp)

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

  # Vertical main group.
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

  g0 <- glayout(container = f0, spacing = 1)

  # Datasets ------------------------------------------------------------------

  g0[1, 1] <- glabel(text = strLblDataset, container = g0)

  g0[1, 2] <- dataset_drp <- gcombobox(
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
    ellipsize = "none"
  )

  g0[1, 3] <- g0_samples_lbl <- glabel(text = paste(" 0", strLblSamples), container = g0)

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
      .gNameData <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, strLblSamples)

      # Suggest a name for result.
      svalue(f2_save_edt) <- paste(val_obj, "_at6", sep = "")
    } else {

      # Reset components.
      .gData <<- NULL
      .gNameData <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(f2_save_edt) <- ""
    }
  })

  g0[2, 1] <- glabel(text = strLblDatasetRef, container = g0)

  g0[2, 2] <- refset_drp <- gcombobox(
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
    ellipsize = "none"
  )

  g0[2, 3] <- g0_ref_lbl <- glabel(text = paste(" 0", strLblRef), container = g0)

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
      .gNameRef <<- val_obj
      ref <- length(unique(.gRef$Sample.Name))
      svalue(g0_ref_lbl) <- paste("", ref, strLblRef)
    } else {

      # Reset components.
      .gRef <<- NULL
      .gNameRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(g0_ref_lbl) <- paste(" 0", strLblRef)
    }
  })

  # CHECK ---------------------------------------------------------------------

  g0[3, 2] <- g0_check_btn <- gbutton(text = strBtnCheck, container = g0)

  addHandlerChanged(g0_check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_case_chk)

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = strWinCheck,
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

  # AMOUNT --------------------------------------------------------------------

  g0[4, 1] <- glabel(text = strLblAmount, container = g0)

  g0[4, 2] <- amset_drp <- gcombobox(
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
    ellipsize = "none"
  )

  g0[4, 3] <- g0_am_lbl <- glabel(text = paste(" 0", strLblSamples), container = g0)

  addHandlerChanged(amset_drp, handler = function(h, ...) {
    val_obj <- svalue(amset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Amount")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gAm <<- get(val_obj, envir = env)
      .gNameAm <<- val_obj
      am <- length(unique(.gAm$Sample.Name))
      svalue(g0_am_lbl) <- paste("", am, strLblSamples)
    } else {

      # Reset components.
      .gAm <<- NULL
      .gNameAm <<- NULL
      svalue(amset_drp, index = TRUE) <- 1
      svalue(g0_am_lbl) <- paste(" 0", strLblSamples)
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
    text = strLblText1,
    anchor = c(-1, 0), container = f1
  )
  glabel(
    text = strLblText2,
    anchor = c(-1, 0), container = f1
  )

  f1_ignore_case_chk <- gcheckbox(
    text = strChkIgnore, checked = TRUE,
    container = f1
  )

  f1_items <- c(strRadItem1, strRadItem2)
  f1_weighted_opt <- gradio(items = f1_items, selected = 2, container = f1)

  glabel(text = strLblLevel, anchor = c(-1, 0), container = f1)
  f1_alpha_spn <- gspinbutton(from = 0, to = 1, by = 0.01, value = 0.05, container = f1)

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strFrmSave,
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = strLblSave, container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE, fill = TRUE)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    val_ignore_case <- svalue(f1_ignore_case_chk)
    val_weighted <- ifelse(svalue(f1_weighted_opt, index = TRUE) == 1, FALSE, TRUE)
    val_alpha <- svalue(f1_alpha_spn)
    val_name <- svalue(f2_save_edt)
    val_name_data <- .gNameData
    val_name_ref <- .gNameRef
    val_name_amount <- .gNameAm

    if (debug) {
      print("GUI options:")
      print("val_ignore_case")
      print(val_ignore_case)
      print("val_weighted")
      print(val_weighted)
      print("val_alpha")
      print(val_alpha)
      print("val_name")
      print(val_name)
    }

    if (!is.null(.gData) & !is.null(.gRef)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateAT6(
        data = .gData,
        ref = .gRef,
        amount = .gAm,
        weighted = val_weighted,
        alpha = val_alpha,
        ignore.case = val_ignore_case,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "amount",
        "weighted", "alpha", "ignore.case"
      )

      values <- list(
        val_name_data, val_name_ref, val_name_amount,
        val_weighted, val_alpha, val_ignore_case
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
      if (exists(".strvalidator_calculateAT6_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateAT6_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateAT6_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_case_chk) <- get(".strvalidator_calculateAT6_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateAT6_gui_weighted", envir = env, inherits = FALSE)) {
        svalue(f1_weighted_opt) <- get(".strvalidator_calculateAT6_gui_weighted", envir = env)
      }
      if (exists(".strvalidator_calculateAT6_gui_alpha", envir = env, inherits = FALSE)) {
        svalue(f1_alpha_spn) <- get(".strvalidator_calculateAT6_gui_alpha", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateAT6_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateAT6_gui_ignore", value = svalue(f1_ignore_case_chk), envir = env)
      assign(x = ".strvalidator_calculateAT6_gui_weighted", value = svalue(f1_weighted_opt), envir = env)
      assign(x = ".strvalidator_calculateAT6_gui_alpha", value = svalue(f1_alpha_spn), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateAT6_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAT6_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateAT6_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAT6_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateAT6_gui_weighted", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAT6_gui_weighted", envir = env)
      }
      if (exists(".strvalidator_calculateAT6_gui_alpha", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAT6_gui_alpha", envir = env)
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
