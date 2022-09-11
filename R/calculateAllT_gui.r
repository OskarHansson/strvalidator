################################################################################
# CHANGE LOG (last 20 changes)
# 03.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
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
  strLblTcons <- "Calculate conservative point estimates at P(dropout>"
  strChkSexMarkers <- "Remove sex markers"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."

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

    strtmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblT"]$value
    strLblT <- ifelse(is.na(strtmp), strLblT, strtmp)

    strtmp <- dtStrings["strLblTcons"]$value
    strLblTcons <- ifelse(is.na(strtmp), strLblTcons, strtmp)

    strtmp <- dtStrings["strChkSexMarkers"]$value
    strChkSexMarkers <- ifelse(is.na(strtmp), strChkSexMarkers, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)
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

    # Destroy window.
    return(FALSE)
  })

  gv <- ggroup(
    horizontal = FALSE, spacing = 1, use.scrollwindow = FALSE,
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

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset, horizontal = FALSE,
    spacing = 1, expand = FALSE, fill = "x", container = gv
  )

  # DATASET -------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g0)

  dataset_samples_lbl <- glabel(
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
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
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

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = f0g1)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1, editable = FALSE,
    container = f0g1, ellipsize = "none", expand = TRUE, fill = "x"
  )

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions, horizontal = FALSE,
    spacing = 1, container = gv, expand = FALSE, fill = "x"
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
    text = paste(strLblTcons, label_init,
      ")<",
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
    svalue(label_conservative) <- paste(strLblTcons, label_p,
      ")<",
      sep = ""
    )
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(text = strFrmSave, horizontal = TRUE, spacing = 1, container = gv)

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
