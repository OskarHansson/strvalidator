################################################################################
# CHANGE LOG (last 20 changes)
# 01.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 03.03.2020: Fixed reference to function name.
# 23.02.2020: Added language support.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 09.02.2017: New options to add color, r.color, and marker order.
# 27.06.2016: Added expand=TRUE to save as field.
# 09.01.2016: Added attributes to result.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 11.05.2014: Implemented new option 'Ignore case' and save user settings functions.
# 06.05.2014: Implemented 'checkDataset'.
# 23.02.2014: Removed requirement for 'Sample.Name'.
# 11.02.2014: Pass debug to 'addColor'.
# 27.11.2013: Added parameter 'overwrite=TRUE'.

#' @title Add Dye Information
#'
#' @description
#' GUI wrapper to the \code{\link{addColor}} function.
#'
#' @details
#' Convenience GUI for the use of \code{\link{addColor}} and
#' \code{\link{addOrder}} to add 'Dye', 'Color', 'R.Color', and marker 'Order'
#' to a dataset.
#' 'Dye' is the one letter abbreviations for the fluorophores commonly used
#' to label primers in forensic STR typing kits (e.g. R and Y),
#' 'Color' is the corresponding color name (e.g. red and yellow),
#' 'R.Color' is the plot color used in R (e.g. red and black).
#' 'Order' is the marker order in the selected kit.
#' NB! Existing columns will be overwritten.
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
#' @seealso \code{\link{addColor}}

addDye_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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
  strWinTitle <- "Add dye to dataset"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset and kit"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strChkIgnore <- "Ignore case in marker name"
  strChkDye <- "Add dye information"
  strChkColor <- "Add color information"
  strChkR <- "Add R color information"
  strChkMarker <- "Add marker order"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnAdd <- "Add"
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

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strChkDye"]$value
    strChkDye <- ifelse(is.na(strtmp), strChkDye, strtmp)

    strtmp <- dtStrings["strChkColor"]$value
    strChkColor <- ifelse(is.na(strtmp), strChkColor, strtmp)

    strtmp <- dtStrings["strChkR"]$value
    strChkR <- ifelse(is.na(strtmp), strChkR, strtmp)

    strtmp <- dtStrings["strChkMarker"]$value
    strChkMarker <- ifelse(is.na(strtmp), strChkMarker, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnAdd"]$value
    strBtnAdd <- ifelse(is.na(strtmp), strBtnAdd, strtmp)

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

  # DATASET ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

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
    requiredCol <- c("Marker")
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
      svalue(save_edt) <- paste(.gDataName, sep = "")
    } else {
      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(dataset_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
    }
  })

  # KIT -----------------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = f0g1)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = f0g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # FRAME 1 ###################################################################

  f1 <- gframe(text = strFrmOptions, horizontal = FALSE, spacing = 1, container = gv)

  f1_ignore_chk <- gcheckbox(
    text = strChkIgnore,
    checked = FALSE, container = f1
  )

  f1_dye_chk <- gcheckbox(
    text = strChkDye,
    checked = TRUE, container = f1
  )

  f1_color_chk <- gcheckbox(
    text = strChkColor,
    checked = FALSE, container = f1
  )

  f1_r_chk <- gcheckbox(
    text = strChkR,
    checked = FALSE, container = f1
  )

  f1_order_chk <- gcheckbox(
    text = strChkMarker,
    checked = FALSE, container = f1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  add_btn <- gbutton(text = strBtnAdd, container = gv)

  addHandlerClicked(add_btn, handler = function(h, ...) {
    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_data_name <- .gDataName
    val_name <- svalue(save_edt)
    val_ignore <- svalue(f1_ignore_chk)
    val_dye <- svalue(f1_dye_chk)
    val_color <- svalue(f1_color_chk)
    val_r <- svalue(f1_r_chk)
    val_order <- svalue(f1_order_chk)

    # Initialise what information is needed from the addColor function.
    need <- NULL
    if (val_dye) {
      need <- c(need, "Dye")
    }
    if (val_color) {
      need <- c(need, "Color")
    }
    if (val_r) {
      need <- c(need, "R.Color")
    }

    if (debug) {
      print(".gData")
      print(names(.gData))
      print("val_kit")
      print(val_kit)
      print("val_ignore")
      print(val_ignore)
      print("val_dye")
      print(val_dye)
      print("val_color")
      print(val_color)
      print("val_r")
      print(val_r)
      print("val_order")
      print(val_order)
    }

    # Change button.
    blockHandlers(add_btn)
    svalue(add_btn) <- strBtnProcessing
    unblockHandlers(add_btn)
    enabled(add_btn) <- FALSE

    if (!is.null(need)) {
      message(
        "Adding the following color information: ",
        paste(need, collapse = ", ")
      )

      val_data <- addColor(
        data = val_data, kit = val_kit, need = need,
        overwrite = TRUE, ignore.case = val_ignore,
        debug = debug
      )
    }

    if (val_order) {
      val_data <- addOrder(
        data = val_data, kit = val_kit, overwrite = TRUE,
        ignore.case = val_ignore, debug = debug
      )
    }

    # Save to new variable.
    datanew <- val_data

    # Add attributes to result.
    attr(datanew, which = "kit") <- val_kit

    # Create key-value pairs to log.
    keys <- list(
      "data", "kit", "ignore.case", "dye", "color",
      "r.color", "order"
    )

    values <- list(
      val_data_name, val_kit, val_ignore, val_dye, val_color,
      val_r, val_order
    )

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
      if (exists(".strvalidator_addDye_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_addDye_gui_savegui", envir = env)
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
      if (exists(".strvalidator_addDye_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_addDye_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_dye", envir = env, inherits = FALSE)) {
        svalue(f1_dye_chk) <- get(".strvalidator_addDye_gui_dye", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_color", envir = env, inherits = FALSE)) {
        svalue(f1_color_chk) <- get(".strvalidator_addDye_gui_color", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_r", envir = env, inherits = FALSE)) {
        svalue(f1_r_chk) <- get(".strvalidator_addDye_gui_r", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_order", envir = env, inherits = FALSE)) {
        svalue(f1_order_chk) <- get(".strvalidator_addDye_gui_order", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_addDye_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_dye", value = svalue(f1_dye_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_color", value = svalue(f1_color_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_r", value = svalue(f1_r_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_order", value = svalue(f1_order_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_addDye_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_dye", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_dye", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_color", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_color", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_r", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_r", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_order", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_order", envir = env)
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
