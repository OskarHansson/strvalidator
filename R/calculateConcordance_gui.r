################################################################################
# CHANGE LOG (last 20 changes)
# 03.03.2020: Fixed reference to function name.
# 01.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 10.02.2019: Adjusted GUI for tcltk.
# 10.02.2019: Fixed tcltk Error in structure(.External(.C_dotTclObjv, objv)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 20.07.2016: Added attributes to result.
# 20.07.2016: Added new option 'list.all' to include missing samples in result.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 22.06.2014: First version.

#' @title Calculate Concordance
#'
#' @description
#' GUI wrapper for the \code{\link{calculateConcordance}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateConcordance}} function by
#' providing a graphical user interface.
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
#' @importFrom utils help head str
#'
#' @seealso \code{\link{calculateConcordance}}

calculateConcordance_gui <- function(env = parent.frame(), savegui = NULL,
                                     debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate concordance"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset and kit"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblKit <- "Kit:"
  strBtnAdd <- "Add"
  strFrmOptions <- "Options"
  strLblDelimiter <- "Delimiter for alleles in genotype:"
  strLblNoSample <- "String for missing samples:"
  strLblNoMarker <- "String for missing markers:"
  strChkInclude <- "Include missing samples in result."
  strTipInclude <- "Samples not in all datasets will always be included in the result."
  strFrmSelected <- "Selected datasets"
  strLblSelected <- "Name for datasets to analyse (separated by comma):"
  strLblUsedKits <- "Name for analysis kit (separated by comma):"
  strFrmSave <- "Save as"
  strLblSave <- "Name for discordance table:"
  strLblSave2 <- "Name for concordance table:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgAddMessage <- "Data frame is NULL!\n\nMake sure to select a dataset"
  strMsgTitleError <- "Error"
  strMsgMessage <- "A dataset must be selected."
  strMsgTitle <- "Datasets not selected"

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

    strTmp <- dtStrings["strBtnAdd"]$Value
    strBtnAdd <- ifelse(is.na(strTmp), strBtnAdd, strTmp)

    strTmp <- dtStrings["strFrmOptions"]$Value
    strFrmOptions <- ifelse(is.na(strTmp), strFrmOptions, strTmp)

    strTmp <- dtStrings["strLblDelimiter"]$Value
    strLblDelimiter <- ifelse(is.na(strTmp), strLblDelimiter, strTmp)

    strTmp <- dtStrings["strLblNoSample"]$Value
    strLblNoSample <- ifelse(is.na(strTmp), strLblNoSample, strTmp)

    strTmp <- dtStrings["strLblNoMarker"]$Value
    strLblNoMarker <- ifelse(is.na(strTmp), strLblNoMarker, strTmp)

    strTmp <- dtStrings["strChkInclude"]$Value
    strChkInclude <- ifelse(is.na(strTmp), strChkInclude, strTmp)

    strTmp <- dtStrings["strTipInclude"]$Value
    strTipInclude <- ifelse(is.na(strTmp), strTipInclude, strTmp)

    strTmp <- dtStrings["strFrmSelected"]$Value
    strFrmSelected <- ifelse(is.na(strTmp), strFrmSelected, strTmp)

    strTmp <- dtStrings["strLblSelected"]$Value
    strLblSelected <- ifelse(is.na(strTmp), strLblSelected, strTmp)

    strTmp <- dtStrings["strLblUsedKits"]$Value
    strLblUsedKits <- ifelse(is.na(strTmp), strLblUsedKits, strTmp)

    strTmp <- dtStrings["strFrmSave"]$Value
    strFrmSave <- ifelse(is.na(strTmp), strFrmSave, strTmp)

    strTmp <- dtStrings["strLblSave"]$Value
    strLblSave <- ifelse(is.na(strTmp), strLblSave, strTmp)

    strTmp <- dtStrings["strLblSave2"]$Value
    strLblSave2 <- ifelse(is.na(strTmp), strLblSave2, strTmp)

    strTmp <- dtStrings["strBtnCalculate"]$Value
    strBtnCalculate <- ifelse(is.na(strTmp), strBtnCalculate, strTmp)

    strTmp <- dtStrings["strBtnProcessing"]$Value
    strBtnProcessing <- ifelse(is.na(strTmp), strBtnProcessing, strTmp)

    strTmp <- dtStrings["strMsgAddMessage"]$Value
    strMsgAddMessage <- ifelse(is.na(strTmp), strMsgAddMessage, strTmp)

    strTmp <- dtStrings["strMsgTitleError"]$Value
    strMsgTitleError <- ifelse(is.na(strTmp), strMsgTitleError, strTmp)

    strTmp <- dtStrings["strMsgMessage"]$Value
    strMsgMessage <- ifelse(is.na(strTmp), strMsgMessage, strTmp)

    strTmp <- dtStrings["strMsgTitle"]$Value
    strMsgTitle <- ifelse(is.na(strTmp), strMsgTitle, strTmp)
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
    spacing = 5,
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
    text = strFrmDataset, horizontal = FALSE,
    spacing = 2, container = gv
  )

  f0g0 <- glayout(container = f0, expand = TRUE, fill = "both", spacing = 2)

  f0g0[1, 1] <- glabel(text = strLblDataset, container = f0g0)

  f0_list <- c(strDrpDefault, listObjects(env = env, obj.class = "data.frame"))

  f0g0[1, 2] <- dataset_drp <- gcombobox(
    items = f0_list, selected = 1,
    editable = FALSE, container = f0g0,
    ellipsize = "none"
  )

  f0g0[1, 3] <- f0_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g0
  )

  f0g0[2, 1] <- glabel(text = strLblKit, container = f0g0)

  f0g0[2, 2] <- kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f0g0,
    ellipsize = "none"
  )

  f0_add_btn <- gbutton(text = strBtnAdd, container = f0)

  # HANDLERS ------------------------------------------------------------------

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

      # Get data.
      .gData <<- get(val_obj, envir = env)

      svalue(f0_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strLblSamples
      )

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f4_save1_edt) <- ""
      svalue(f4_save2_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste("0", strLblSamples)
    }
  })

  addHandlerChanged(f0_add_btn, handler = function(h, ...) {

    # Get values.
    val_obj <- svalue(dataset_drp)
    val_dataset <- svalue(f3_dataset_edt)
    val_kit <- svalue(f3_kit_edt)
    val_new_kit <- svalue(kit_drp)

    if (!is.null(.gData)) {

      # Add new value to selected.
      new <- ifelse(nchar(val_dataset) > 0,
        paste(val_dataset, val_obj, sep = ","),
        val_obj
      )

      # Update text box.
      svalue(f3_dataset_edt) <- new

      # Add new value to selected.
      new <- ifelse(nchar(val_kit) > 0,
        paste(val_kit, val_new_kit, sep = ","),
        val_new_kit
      )

      # Update text box.
      svalue(f3_kit_edt) <- new
    } else {
      gmessage(
        msg = strMsgAddMessage,
        title = strMsgTitleError,
        icon = "error"
      )
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(text = strFrmOptions, horizontal = FALSE, spacing = 10, container = gv)

  f1g0 <- glayout(container = f1, expand = TRUE, fill = "both", spacing = 2)

  f1g0[1, 1] <- glabel(
    text = strLblDelimiter,
    anchor = c(-1, 0), container = f1g0
  )
  f1g0[1, 2] <- f1_delimeter_edt <- gedit(
    text = ",",
    width = 15, container = f1g0
  )

  f1g0[2, 1] <- glabel(
    text = strLblNoSample,
    anchor = c(-1, 0), container = f1g0
  )
  f1g0[2, 2] <- f1_no_sample_edt <- gedit(
    text = "NO SAMPLE",
    width = 15, container = f1g0
  )

  f1g0[3, 1] <- glabel(
    text = strLblNoMarker,
    anchor = c(-1, 0), container = f1g0
  )
  f1g0[3, 2] <- f1_no_marker_edt <- gedit(
    text = "NO MARKER",
    width = 15, container = f1g0
  )

  f1_all_chk <- gcheckbox(
    text = strChkInclude,
    checked = FALSE, container = f1
  )
  tooltip(f1_all_chk) <- strTipInclude

  # FRAME 3 ###################################################################

  f3 <- gframe(
    text = strFrmSelected,
    horizontal = FALSE,
    spacing = 2,
    container = gv
  )

  glabel(
    text = strLblSelected,
    anchor = c(-1, 0), container = f3
  )

  f3_dataset_edt <- gedit(container = f3)

  glabel(
    text = strLblUsedKits,
    anchor = c(-1, 0), container = f3
  )

  f3_kit_edt <- gedit(container = f3)


  # FRAME 4 ###################################################################

  f4 <- gframe(
    text = strFrmSave,
    horizontal = FALSE,
    spacing = 2,
    container = gv
  )

  glabel(text = strLblSave, anchor = c(-1, 0), container = f4)

  f4_save1_edt <- gedit(text = "table_discordance", container = f4)

  glabel(text = strLblSave2, anchor = c(-1, 0), container = f4)

  f4_save2_edt <- gedit(text = "table_concordance", container = f4)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_datasets <- svalue(f3_dataset_edt)
    val_kits <- svalue(f3_kit_edt)
    val_name1 <- svalue(f4_save1_edt)
    val_name2 <- svalue(f4_save2_edt)
    val_del <- svalue(f1_delimeter_edt)
    val_nosample <- svalue(f1_no_sample_edt)
    val_nomarker <- svalue(f1_no_marker_edt)
    val_all <- svalue(f1_all_chk)
    val_list <- list()

    if (debug) {
      print("Read Values:")
      print("val_datasets")
      print(val_datasets)
      print("val_kits")
      print(val_kits)
      print("val_name1")
      print(val_name1)
      print("val_name2")
      print(val_name2)
      print("val_delimeter")
      print(val_del)
      print("val_nosample")
      print(val_nosample)
      print("val_nomarker")
      print(val_nomarker)
      print("val_all")
      print(val_all)
    }

    # Check if data.
    if (!val_datasets == "") {

      # Create list of datasets.
      val_datasets <- unlist(strsplit(val_datasets, ","))
      for (d in seq(along = val_datasets)) {
        # Get data and store in list.
        val_list[[d]] <- get(val_datasets[d], envir = env)
      }

      # Create vector of kit names.
      val_kits <- unlist(strsplit(val_kits, ","))

      if (debug) {
        print("Sent Values:")
        print("val_list")
        print(str(val_list))
        print("val_kits")
        print(val_kits)
        print("val_del")
        print(val_del)
        print("val_nosample")
        print(val_nosample)
        print("val_nomarker")
        print(val_nomarker)
        print("val_all")
        print(val_all)
      }

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateConcordance(
        data = val_list,
        kit.name = val_kits,
        no.sample = val_nosample,
        no.marker = val_nomarker,
        delimeter = val_del,
        list.all = val_all,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "kit.name", "no.sample", "no.marker",
        "delimeter", "list.all"
      )

      values <- list(
        val_datasets, val_kits, val_nosample, val_nomarker,
        val_del, val_all
      )

      # Update audit trail.
      datanew[[1]] <- auditTrail(
        obj = datanew[[1]], key = keys, value = values,
        label = fnc,
        arguments = FALSE, package = "strvalidator"
      )

      datanew[[2]] <- auditTrail(
        obj = datanew[[2]], key = keys, value = values,
        label = fnc,
        arguments = FALSE, package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name1, object = datanew[[1]], parent = w, env = env)
      saveObject(name = val_name2, object = datanew[[2]], parent = w, env = env)

      if (debug) {
        print(str(datanew))
        print(head(datanew))
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(strMsgMessage,
        title = strMsgTitle,
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
      if (exists(".strvalidator_calculateConcordance_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateConcordance_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateConcordance_gui_delimeter", envir = env, inherits = FALSE)) {
        svalue(f1_delimeter_edt) <- get(".strvalidator_calculateConcordance_gui_delimeter", envir = env)
      }
      if (exists(".strvalidator_calculateConcordance_gui_sample", envir = env, inherits = FALSE)) {
        svalue(f1_no_sample_edt) <- get(".strvalidator_calculateConcordance_gui_sample", envir = env)
      }
      if (exists(".strvalidator_calculateConcordance_gui_marker", envir = env, inherits = FALSE)) {
        svalue(f1_no_marker_edt) <- get(".strvalidator_calculateConcordance_gui_marker", envir = env)
      }
      if (exists(".strvalidator_calculateConcordance_gui_all", envir = env, inherits = FALSE)) {
        svalue(f1_all_chk) <- get(".strvalidator_calculateConcordance_gui_all", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateConcordance_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateConcordance_gui_delimeter", value = svalue(f1_delimeter_edt), envir = env)
      assign(x = ".strvalidator_calculateConcordance_gui_sample", value = svalue(f1_no_sample_edt), envir = env)
      assign(x = ".strvalidator_calculateConcordance_gui_marker", value = svalue(f1_no_marker_edt), envir = env)
      assign(x = ".strvalidator_calculateConcordance_gui_all", value = svalue(f1_all_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateConcordance_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateConcordance_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateConcordance_gui_delimeter", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateConcordance_gui_delimeter", envir = env)
      }
      if (exists(".strvalidator_calculateConcordance_gui_sample", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateConcordance_gui_sample", envir = env)
      }
      if (exists(".strvalidator_calculateConcordance_gui_marker", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateConcordance_gui_marker", envir = env)
      }
      if (exists(".strvalidator_calculateConcordance_gui_all", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateConcordance_gui_all", envir = env)
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
