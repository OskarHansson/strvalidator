# NOTE: As of version 2.2.0 of strvalidator gdf generates an error when gui is closed.
# (R version 3.6.2, gWidgets2RGtk2_1.0-7, gWidgets2_1.0-8, RGtk2_2.20.36)
# (rsession.exe:20768): Gtk-CRITICAL **: gtk_tree_view_unref_tree_helper: assertion `node != NULL' failed
# Issue reported here: https://github.com/jverzani/gWidgets2/issues/11

################################################################################
# CHANGE LOG (last 20 changes)
# 07.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 28.04.2016: 'Save as' textbox expandable.
# 26.10.2015: Added attributes.
# 28.08.2015: Added importFrom.
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 05.01.2015: Added kit dropdown and kit attribute to result.
# 07.10.2014: Added 'focus', added 'parent' parameter.
# 03.08.2014: Added detection of kit and add attribute to result.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 26.07.2013: Changed parameter 'fixed' to 'word' for 'checkSubset' function.
# 18.07.2013: Check before overwrite object.
# 17.07.2013: Added save GUI settings.
# 17.07.2013: 'false' allele checkboxes replaced by gdf table.

#' @title Calculate Stutter
#'
#' @description
#' GUI wrapper for the \code{\link{calculateStutter}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateStutter}} function by providing
#' a graphical user interface to it.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help head
#' @importFrom graphics title
#'
#' @return TRUE
#'
#' @seealso \code{\link{calculateStutter}}, \code{\link{checkSubset}}

calculateStutter_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
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
  strWinTitle <- "Calculate stutter ratio"
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
  strFrmOptions <- "Options"
  strLblRange <- "Calculate stutter ratio within the following analysis range:"
  strLblBack <- "back stutters to"
  strLblForward <- "forward stutters."
  strLblNB <- "NB! Additive effects outside the analysis range cannot be controlled.\nA narrow range like 0 to 1 can be greately affected by neighbouring -1 stutters."
  strLblInterference <- "Level of interference within the given range:"
  strRadNone <- "no overlap between stutters and alleles"
  strRadStutter <- "stutter-stutter overlap allowed"
  strRadAllele <- "stutter-allele overlap allowed"
  strLblReplace <- "Replace 'false' stutters:"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A sample dataset and a reference dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset."
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

    strTmp <- dtStrings["strLblKit"]$Value
    strLblKit <- ifelse(is.na(strTmp), strLblKit, strTmp)

    strTmp <- dtStrings["strFrmOptions"]$Value
    strFrmOptions <- ifelse(is.na(strTmp), strFrmOptions, strTmp)

    strTmp <- dtStrings["strLblRange"]$Value
    strLblRange <- ifelse(is.na(strTmp), strLblRange, strTmp)

    strTmp <- dtStrings["strLblBack"]$Value
    strLblBack <- ifelse(is.na(strTmp), strLblBack, strTmp)

    strTmp <- dtStrings["strLblForward"]$Value
    strLblForward <- ifelse(is.na(strTmp), strLblForward, strTmp)

    strTmp <- dtStrings["strLblNB"]$Value
    strLblNB <- ifelse(is.na(strTmp), strLblNB, strTmp)

    strTmp <- dtStrings["strLblInterference"]$Value
    strLblInterference <- ifelse(is.na(strTmp), strLblInterference, strTmp)

    strTmp <- dtStrings["strRadNone"]$Value
    strRadNone <- ifelse(is.na(strTmp), strRadNone, strTmp)

    strTmp <- dtStrings["strRadStutter"]$Value
    strRadStutter <- ifelse(is.na(strTmp), strRadStutter, strTmp)

    strTmp <- dtStrings["strRadAllele"]$Value
    strRadAllele <- ifelse(is.na(strTmp), strRadAllele, strTmp)

    strTmp <- dtStrings["strLblReplace"]$Value
    strLblReplace <- ifelse(is.na(strTmp), strLblReplace, strTmp)

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
    spacing = 5,
    use.scrollwindow = FALSE,
    container = w,
    expand = FALSE
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

  f0g0[1, 3] <- f0_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g0
  )

  addHandlerChanged(f0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(f0_dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(f0_samples_lbl) <- paste("", samples, strLblSamples)
      svalue(save_edt) <- paste(val_obj, "_stutter", sep = "")
      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0_dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
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

  f0g0[2, 3] <- f0_ref_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = f0g0
  )

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
      .gRefName <<- NULL
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
        ignore.case = TRUE,
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

  # Kit -----------------------------------------------------------------------

  f0g0[4, 1] <- glabel(text = strLblKit, container = f0g0)

  f0g0[4, 2] <- kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f0g0,
    ellipsize = "none"
  )

  # OPTIONS ###################################################################

  f1 <- gframe(strFrmOptions,
    horizontal = FALSE, container = gv,
    expand = TRUE, fill = TRUE
  )

  glabel(text = strLblRange, container = f1, anchor = c(-1, 0))

  f1g1 <- ggroup(
    horizontal = TRUE,
    spacing = 5,
    use.scrollwindow = FALSE,
    container = f1
  )

  f1g1_range_b_spb <- gspinbutton(
    from = 0, to = 3, by = 1,
    value = 1, digits = 0,
    container = f1g1
  )

  glabel(text = strLblBack, container = f1g1, anchor = c(-1, 0))

  f1g1_range_f_spb <- gspinbutton(
    from = 0, to = 3, by = 1,
    value = 1, digits = 0,
    container = f1g1
  )

  glabel(text = strLblForward, container = f1g1, anchor = c(-1, 0))

  glabel(text = strLblNB, container = f1, anchor = c(-1, 0))

  # INTERFERENCE --------------------------------------------------------------

  glabel(text = "", container = f1, anchor = c(-1, 0))

  glabel(text = strLblInterference, container = f1, anchor = c(-1, 0))

  options <- c(strRadNone, strRadStutter, strRadAllele)

  interference_opt <- gradio(
    items = options, selected = 1,
    horizontal = FALSE, container = f1
  )

  # FALSE STUTTERS ------------------------------------------------------------

  glabel(text = "", container = f1, anchor = c(-1, 0))

  glabel(text = strLblReplace, container = f1, anchor = c(-1, 0))

  # Create default data frame.
  replace_val <- c(-1.9, -1.8, -1.7, -0.9, -0.8, -0.7, 0.9, 0.8, 0.7)
  by_val <- c(-1.3, -1.2, -1.1, -0.3, -0.2, -0.1, 0.3, 0.2, 0.1)
  default <- data.frame(
    False.Stutter = replace_val,
    True.Stutter = by_val,
    Replace = TRUE,
    stringsAsFactors = FALSE
  )

  # gdf generates an error when gui is closed.
  # (rsession.exe:20768): Gtk-CRITICAL **: gtk_tree_view_unref_tree_helper: assertion `node != NULL' failed
  # Issue reported here: https://github.com/jverzani/gWidgets2/issues/11
  default_gdf <- gdf(items = default, container = f1, expand = TRUE)

  # Set initial minimum size.
  size(default_gdf) <- c(100, 100)

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_back <- svalue(f1g1_range_b_spb)
    val_forward <- svalue(f1g1_range_f_spb)
    val_interference <- svalue(interference_opt, index = TRUE) - 1 # NB! range [0-2]
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- .gDataName
    val_name_ref <- .gRefName
    val_name <- svalue(save_edt)
    val_replace_df <- default_gdf[]
    val_chk <- val_replace_df$Replace
    val_replace <- val_replace_df$False.Stutter
    val_by <- val_replace_df$True.Stutter
    val_kit <- svalue(kit_drp)

    # Get selected values.
    val_replace <- val_replace[val_chk]
    val_by <- val_by[val_chk]
    if (length(val_replace) == 0) {
      val_replace <- NULL
    }
    if (length(val_by) == 0) {
      val_by <- NULL
    }

    if (!is.null(val_data) & !is.null(val_ref)) {
      if (debug) {
        print("val_data")
        print(head(val_data))
        print("val_ref")
        print(head(val_ref))
        print("val_back")
        print(val_back)
        print("val_forward")
        print(val_forward)
        print("val_interference")
        print(val_interference)
        print("val_replace")
        print(val_replace)
        print("val_by")
        print(val_by)
      }

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      # Calculate stutter.
      datanew <- calculateStutter(
        data = val_data, ref = val_ref,
        back = val_back, forward = val_forward,
        interference = val_interference,
        replace.val = val_replace,
        by.val = val_by,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "back", "forward",
        "interference", "replace.val", "by.val"
      )

      values <- list(
        val_name_data, val_name_ref, val_back, val_forward,
        val_interference, val_replace, val_by
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
        print("datanew")
        print(head(datanew))
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
      if (exists(".strvalidator_calculateStutter_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateStutter_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateStutter_gui_back", envir = env, inherits = FALSE)) {
        svalue(f1g1_range_b_spb) <- get(".strvalidator_calculateStutter_gui_back", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_forward", envir = env, inherits = FALSE)) {
        svalue(f1g1_range_f_spb) <- get(".strvalidator_calculateStutter_gui_forward", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_interference", envir = env, inherits = FALSE)) {
        svalue(interference_opt) <- get(".strvalidator_calculateStutter_gui_interference", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_replace", envir = env, inherits = FALSE)) {
        default_gdf[, ] <- get(".strvalidator_calculateStutter_gui_replace", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateStutter_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateStutter_gui_back", value = svalue(f1g1_range_b_spb), envir = env)
      assign(x = ".strvalidator_calculateStutter_gui_forward", value = svalue(f1g1_range_f_spb), envir = env)
      assign(x = ".strvalidator_calculateStutter_gui_interference", value = svalue(interference_opt), envir = env)
      assign(x = ".strvalidator_calculateStutter_gui_replace", value = default_gdf[], envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateStutter_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_back", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_back", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_forward", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_forward", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_interference", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_interference", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_replace", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_replace", envir = env)
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
