################################################################################
# CHANGE LOG (last 20 changes)
# 03.03.2020: Fixed reference to function name.
# 25.02.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 25.07.2018: Added option to remove sex markers.
# 17.07.2018: First version.

#' @title Calculate Stochastic Thresholds
#'
#' @description
#' GUI wrapper to the \code{\link{calculateAllT}} function.
#'
#' @details
#' Convenience GUI for the use of \code{\link{calculateAllT}} to calculate
#' point estimates for the stochastic threshold using multiple models.
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
#' @importFrom utils head help
#'
#' @seealso \code{\link{calculateAllT}}

calculateAllT_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- data.frame(No.Data = NA)
  .gDataName <- NULL
  .gKit <- 1

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate stochastic thresholds"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset and kit"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strLblT <- "Calculate point estimates at P(dropout)="
  strLblTcons1 <- "Calculate conservative point estimates at P(dropout>"
  strLblTcons2 <- ")<"
  strChkSexMarkers <- "Remove sex markers"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."

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

    strTmp <- dtStrings["strDrpDefault"]$Value
    strDrpDefault <- ifelse(is.na(strTmp), strDrpDefault, strTmp)

    strTmp <- dtStrings["strLblSamples"]$Value
    strLblSamples <- ifelse(is.na(strTmp), strLblSamples, strTmp)

    strTmp <- dtStrings["strLblKit"]$Value
    strLblKit <- ifelse(is.na(strTmp), strLblKit, strTmp)

    strTmp <- dtStrings["strFrmOptions"]$Value
    strFrmOptions <- ifelse(is.na(strTmp), strFrmOptions, strTmp)

    strTmp <- dtStrings["strLblT"]$Value
    strLblT <- ifelse(is.na(strTmp), strLblT, strTmp)

    strTmp <- dtStrings["strLblTcons1"]$Value
    strLblTcons1 <- ifelse(is.na(strTmp), strLblTcons1, strTmp)

    strTmp <- dtStrings["strLblTcons2"]$Value
    strLblTcons2 <- ifelse(is.na(strTmp), strLblTcons2, strTmp)

    strTmp <- dtStrings["strChkSexMarkers"]$Value
    strChkSexMarkers <- ifelse(is.na(strTmp), strChkSexMarkers, strTmp)

    strTmp <- dtStrings["strFrmSave"]$Value
    strFrmSave <- ifelse(is.na(strTmp), strFrmSave, strTmp)

    strTmp <- dtStrings["strLblSave"]$Value
    strLblSave <- ifelse(is.na(strTmp), strLblSave, strTmp)

    strTmp <- dtStrings["strBtnCalculate"]$Value
    strBtnCalculate <- ifelse(is.na(strTmp), strBtnCalculate, strTmp)

    strTmp <- dtStrings["strBtnProcessing"]$Value
    strBtnProcessing <- ifelse(is.na(strTmp), strBtnProcessing, strTmp)
  }

  # ---------------------------------------------------------------------------

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
    horizontal = FALSE, spacing = 15, use.scrollwindow = FALSE,
    container = w, expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(
    text = strChkGui,
    checked = FALSE, container = gh
  )

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # DATASET ###################################################################

  f0 <- gframe(
    text = strFrmDataset, horizontal = FALSE,
    spacing = 5, container = gv
  )

  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1, 1] <- glabel(text = strLblDataset, container = f0g0)

  f0g0[1, 2] <- dataset_drp <- gcombobox(
    items = c(
      strDrpDefault,
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

  f0g0[1, 3] <- dataset_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g0
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c(
      "Sample.Name", "MethodX", "Method1", "Method2", "MethodL",
      "Height", "H", "MethodL.Ph"
    )
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(dataset_samples_lbl) <- paste(" ", samples, strLblSamples)
      .gKit <<- detectKit(.gData, index = TRUE)
      svalue(kit_drp, index = TRUE) <- .gKit
      svalue(f2_save_edt) <- paste(.gDataName, "_t", sep = "")
    } else {

      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(dataset_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(f2_save_edt) <- ""
    }
  })

  # KIT -----------------------------------------------------------------------

  f0g0[2, 1] <- glabel(text = strLblKit, container = f0g0)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1, editable = FALSE,
    container = f0g0, ellipsize = "none"
  )

  f0g0[2, 2] <- kit_drp

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions, horizontal = FALSE,
    spacing = 5, container = gv
  )
  f1g1 <- ggroup(container = f1, horizontal = TRUE)
  f1g2 <- ggroup(container = f1, horizontal = TRUE)

  glabel(text = strLblT, container = f1g1)
  f1_p_dropout <- gspinbutton(
    from = 0, to = 1, by = 0.01,
    value = 0.01, container = f1g1
  )

  label_init <- svalue(f1_p_dropout)
  label_conservative <- glabel(
    text = paste(strLblTcons1, label_init,
      strLblTcons2,
      sep = ""
    ),
    container = f1g2
  )
  f1_p_conservative <- gspinbutton(
    from = 0, to = 1, by = 0.01,
    value = 0.05, container = f1g2
  )

  f1_sex_chk <- gcheckbox(
    text = strChkSexMarkers, checked = TRUE,
    container = f1
  )

  addHandlerChanged(f1_p_dropout, handler = function(h, ...) {
    label_p <- svalue(f1_p_dropout)
    svalue(label_conservative) <- paste(strLblTcons1, label_p,
      strLblTcons2,
      sep = ""
    )
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(text = strFrmSave, horizontal = TRUE, spacing = 5, container = gv)

  glabel(text = strLblSave, container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE, fill = TRUE)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_data_name <- .gDataName
    val_name <- svalue(f2_save_edt)
    val_p <- svalue(f1_p_dropout)
    val_pcons <- svalue(f1_p_conservative)
    val_sex <- svalue(f1_sex_chk)

    if (debug) {
      print("val_data")
      print(names(.gData))
      print("val_kit")
      print(val_kit)
      print("val_p")
      print(val_p)
      print("val_pcons")
      print(val_pcons)
      print("val_sex")
      print(val_sex)
    }

    # Change button.
    blockHandlers(calculate_btn)
    svalue(calculate_btn) <- strBtnProcessing
    unblockHandlers(calculate_btn)
    enabled(calculate_btn) <- FALSE

    # Calculate stochastic thresholds.
    datanew <- calculateAllT(
      data = val_data, kit = val_kit, p.dropout = val_p,
      p.conservative = val_pcons, rm.sex = val_sex,
      debug = debug
    )


    # Add attributes to result.
    attr(datanew, which = "kit") <- val_kit

    # Create key-value pairs to log.
    keys <- list("data", "kit", "p.dropout", "p.conservative", "rm.sex")

    values <- list(val_data_name, val_kit, val_p, val_pcons, val_sex)

    # Update audit trail.
    datanew <- auditTrail(
      obj = datanew, key = keys, value = values,
      label = fnc, arguments = FALSE,
      package = "strvalidator"
    )

    # Save data.
    saveObject(name = val_name, object = datanew, parent = w, env = env)

    # Close GUI.
    .saveSettings()
    dispose(w)
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
      if (exists(".strvalidator_calculateAllT_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateAllT_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateAllT_gui_pdrop", envir = env, inherits = FALSE)) {
        svalue(f1_p_dropout) <- get(".strvalidator_calculateAllT_gui_pdrop", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_pcons", envir = env, inherits = FALSE)) {
        svalue(f1_p_conservative) <- get(".strvalidator_calculateAllT_gui_pcons", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateAllT_gui_sex", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateAllT_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateAllT_gui_pdrop", value = svalue(f1_p_dropout), envir = env)
      assign(x = ".strvalidator_calculateAllT_gui_pcons", value = svalue(f1_p_conservative), envir = env)
      assign(x = ".strvalidator_calculateAllT_gui_sex", value = svalue(f1_sex_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateAllT_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllT_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_pdrop", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllT_gui_pdrop", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_pcons", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllT_gui_pcons", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllT_gui_sex", envir = env)
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
