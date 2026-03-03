################################################################################
# CHANGE LOG (last 20 changes)
# 01.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 04.07.2020: Added missing string variable.
# 03.03.2020: Fixed reference to function name.
# 29.02.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 16.06.2016: 'Save as' textbox expandable.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 06.05.2014: Implemented 'checkDataset'.
# 28.10.2013: First version.

#' @title Calculate Capillary Balance
#'
#' @description
#' GUI wrapper for the \code{\link{calculateCapillary}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateCapillary}} function by providing
#' a graphical user interface.
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
#' @importFrom utils help head
#'
#' @seealso \code{\link{calculateCapillary}}

calculateCapillary_gui <- function(env = parent.frame(), savegui = NULL,
                                   debug = FALSE, parent = NULL) {
  # Global variables.
  .gSamples <- NULL
  .gSamplesName <- NULL
  .gPlot <- NULL
  .gPlotName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Calculate capillary balance",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATA_SAMPLE     = "Samples Table:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_DATA_SIZING     = "Sample Plot Sizing Table:",
    STR_LBL_SAMPLE_FILES    = "sample files",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_RUN             = "Run name:",
    STR_LBL_RUN_MSG         = "Optional run name",
    STR_LBL_THRESHOLD       = "Sizing quality threshold:",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A 'Samples Table' dataset and a 'SamplePlotSizing' dataset have to be selected.",
    STR_MSG_TITLE_DATASET   = "Datasets not selected"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # WINDOW ####################################################################

  # Main window.
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
    container = gv,
    expand = TRUE,
    fill = "x"
  )

  # Samples -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATA_SAMPLE, container = g0)

  dfs <- c(strings$STR_DRP_DEFAULT, listObjects(env = env, obj.class = "data.frame"))

  g0_data_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = g0
  )

  g0_data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_data_drp)

    # Check if suitable.
    requiredCol <- c(
      "Sample.File", "Sample.Name", "Size.Standard",
      "Instrument.Type", "Instrument.ID", "Cap", "Well", "SQ"
    )
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gSamples <<- get(val_obj, envir = env)
      .gSamplesName <<- val_obj
      svalue(g0_data_samples_lbl) <- paste(
        length(unique(.gSamples$Sample.Name)),
        strings$STR_LBL_SAMPLES
      )
      svalue(save_edt) <- paste(val_obj, "_cap", sep = "")
    } else {
      # Reset components.
      .gSamples <<- NULL
      .gSamplesName <<- NULL
      svalue(g0_data_drp, index = TRUE) <- 1
      svalue(g0_data_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  # Plot ----------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATA_SIZING, container = g1)

  g0_ref_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLE_FILES),
    container = g1
  )

  # NB! dfs defined in previous section.
  g0_ref_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(g0_ref_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_ref_drp)

    # Check if suitable.
    requiredCol <- c("Sample.File.Name", "Size", "Height")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gPlot <<- get(val_obj, envir = env)
      .gPlotName <<- val_obj
      svalue(g0_ref_samples_lbl) <- paste(
        length(unique(.gPlot$Sample.File.Name)),
        "sample files."
      )
    } else {
      # Reset components.
      .gPlot <<- NULL
      .gPlotName <<- NULL
      svalue(g0_ref_drp, index = TRUE) <- 1
      svalue(g0_ref_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLE_FILES)
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv,
    expand = TRUE,
    fill = "x"
  )

  f1g1 <- ggroup(
    horizontal = TRUE, spacing = 1,
    expand = TRUE, fill = "x", container = f1
  )
  glabel(
    text = strings$STR_LBL_RUN, initial.msg = strings$STR_LBL_RUN_MSG,
    anchor = c(-1, 0), container = f1g1
  )

  f1_run_edt <- gedit(text = "", fill = "x", container = f1g1)

  f1g2 <- ggroup(horizontal = TRUE, spacing = 1, container = f1)
  glabel(
    text = strings$STR_LBL_THRESHOLD,
    anchor = c(-1, 0), container = f1g2
  )
  f1_sq_spb <- gspinbutton(
    from = 0, to = 1, by = 0.01, value = 0, digits = 4,
    container = f1g2
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    val_samples <- .gSamples
    val_name_samples <- .gSamplesName
    val_plot <- .gPlot
    val_name_plot <- .gPlotName
    val_sq <- as.numeric(svalue(f1_sq_spb))
    val_run <- svalue(f1_run_edt)
    val_name <- svalue(save_edt)

    if (debug) {
      print("Read Values:")
      print("val_run")
      print(val_run)
      print("val_name")
      print(val_name)
      print("val_sq")
      print(val_sq)
      print("val_samples")
      print(head(val_samples))
      print("val_plot")
      print(head(val_plot))
    }

    if (!is.null(.gSamples) & !is.null(.gPlot)) {
      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateCapillary(
        samples.table = val_samples,
        plot.table = val_plot,
        sq = val_sq,
        run = val_run,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("sample.table", "plot.table", "sq", "run")

      values <- list(val_name_samples, val_name_plot, val_sq, val_run)

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(head(datanew))
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
      if (exists(".strvalidator_capillaryBalance_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_capillaryBalance_gui_savegui", envir = env)
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
      if (exists(".strvalidator_capillaryBalance_gui_sq", envir = env, inherits = FALSE)) {
        svalue(f1_sq_spb) <- get(".strvalidator_capillaryBalance_gui_sq", envir = env)
      }
      if (exists(".strvalidator_capillaryBalance_gui_run", envir = env, inherits = FALSE)) {
        svalue(f1_run_edt) <- get(".strvalidator_capillaryBalance_gui_run", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_capillaryBalance_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_capillaryBalance_gui_sq", value = svalue(f1_sq_spb), envir = env)
      assign(x = ".strvalidator_capillaryBalance_gui_run", value = svalue(f1_run_edt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_capillaryBalance_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_capillaryBalance_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_capillaryBalance_gui_sq", envir = env, inherits = FALSE)) {
        remove(".strvalidator_capillaryBalance_gui_sq", envir = env)
      }
      if (exists(".strvalidator_capillaryBalance_gui_run", envir = env, inherits = FALSE)) {
        remove(".strvalidator_capillaryBalance_gui_run", envir = env)
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
