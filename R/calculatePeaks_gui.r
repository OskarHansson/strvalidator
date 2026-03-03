################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 04.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 18.09.2016: Added attributes to result.
# 18.09.2016: Changed labels, added tooltips, changed one widget.
# 29.04.2016: 'Save as' textbox expandable.
# 27.08.2015: Updated text for option to calculate per marker.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 11.01.2014: First version.

#' @title Calculate Peaks
#'
#' @description
#' GUI wrapper for the \code{\link{calculatePeaks}} function.
#'
#' @details Counts the number of peaks in samples and markers with option to
#'  discard off-ladder peaks and to label groups according to maximum number
#'  of peaks.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @seealso \code{\link{calculatePeaks}}

calculatePeaks_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  .gData <- NULL
  .gDataName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Calculate peaks",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_EXCLUDE_OL      = "Exclude off-ladder peaks (OL alleles).",
    STR_RAD_BY_SAMPLE       = "Count peaks by sample",
    STR_RAD_BY_MARKER       = "Count peaks by marker",
    STR_LBL_GROUPS          = "Define group labels (separated by comma):",
    STR_TIP_GROUPS          = "Number labels must be one more than the number of cut-off points. The last group is defined by > than the last cut-off point.",
    STR_LBL_CUT_OFF         = "Define cut-off points (<=) for the groups (separated by comma):",
    STR_TIP_CUT_OFF         = "Number of cut-off points must be one less than the number of group labels. The last group is defined by > than the last cut-off point.",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A sample dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # WINDOW ####################################################################

  w <- gwindow(title = strings$STR_WIN_TITLE, visible = FALSE)

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

  savegui_chk <- gcheckbox(text = strings$STR_CHK_GUI, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # Datasets ------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = g0
  )

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Height")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Height",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(samples_lbl) <- paste("", samples, strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- paste(.gDataName, "_peaks", sep = "")
    } else {
      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1_no_ol_chk <- gcheckbox(
    text = strings$STR_CHK_EXCLUDE_OL,
    checked = FALSE, container = f1
  )

  f1_count_by_opt <- gradio(
    items = c(strings$STR_RAD_BY_SAMPLE, strings$STR_RAD_BY_MARKER),
    selected = 1, container = f1
  )

  glabel(
    text = strings$STR_LBL_GROUPS,
    container = f1, anchor = c(-1, 0)
  )
  f1_labels_edt <- gedit(
    text = "No contamination,Drop-in contamination,Gross contamination",
    width = 60, container = f1
  )
  tooltip(f1_labels_edt) <- strings$STR_TIP_GROUPS

  glabel(text = strings$STR_LBL_CUT_OFF, container = f1, anchor = c(-1, 0))
  f1_bins_edt <- gedit(text = "0,2", width = 60, container = f1)
  tooltip(f1_bins_edt) <- strings$STR_TIP_GROUPS

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################


  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_name_data <- .gDataName
    val_no_ol <- svalue(f1_no_ol_chk)
    val_per_marker <- ifelse(svalue(f1_count_by_opt, index = TRUE) == 1, FALSE, TRUE)
    val_labels <- svalue(f1_labels_edt)
    val_bins <- svalue(f1_bins_edt)
    val_name <- svalue(save_edt)

    # countPeaks require a vectors.
    val_labels <- unlist(strsplit(val_labels, ",", fixed = TRUE))
    val_bins <- as.numeric(unlist(strsplit(val_bins, ",", fixed = TRUE)))

    if (length(val_labels) == 0) {
      val_labels <- NULL
    }

    if (!is.null(val_data)) {
      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculatePeaks(
        data = val_data,
        bins = val_bins,
        labels = val_labels,
        ol.rm = val_no_ol,
        by.marker = val_per_marker,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "bins", "labels", "ol.rm",
        "by.marker"
      )

      values <- list(
        val_name_data, val_bins, val_labels, val_no_ol,
        val_per_marker
      )

      # Update audit trail.
      datanew <- audit_trail(
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
        msg = strings$STR_MSG_DATASET,
        title = strings$STR_MSG_TITLE_DATASET,
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
      if (exists(".strvalidator_calculatePeaks_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculatePeaks_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculatePeaks_gui_nool", envir = env, inherits = FALSE)) {
        svalue(f1_no_ol_chk) <- get(".strvalidator_calculatePeaks_gui_nool", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_labels", envir = env, inherits = FALSE)) {
        svalue(f1_labels_edt) <- get(".strvalidator_calculatePeaks_gui_labels", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_bins", envir = env, inherits = FALSE)) {
        svalue(f1_bins_edt) <- get(".strvalidator_calculatePeaks_gui_bins", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_countby", envir = env, inherits = FALSE)) {
        svalue(f1_count_by_opt) <- get(".strvalidator_calculatePeaks_gui_countby", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculatePeaks_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculatePeaks_gui_nool", value = svalue(f1_no_ol_chk), envir = env)
      assign(x = ".strvalidator_calculatePeaks_gui_labels", value = svalue(f1_labels_edt), envir = env)
      assign(x = ".strvalidator_calculatePeaks_gui_bins", value = svalue(f1_bins_edt), envir = env)
      assign(x = ".strvalidator_calculatePeaks_gui_countby", value = svalue(f1_count_by_opt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculatePeaks_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_nool", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_nool", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_labels", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_labels", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_bins", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_bins", envir = env)
      }
      if (exists(".strvalidator_calculatePeaks_gui_countby", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePeaks_gui_countby", envir = env)
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
