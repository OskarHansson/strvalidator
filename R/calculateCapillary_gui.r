################################################################################
# CHANGE LOG (last 20 changes)
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

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(title = "Calculate capillary balance", visible = FALSE)

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

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("calculateCapillary_gui", help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = "Select datasets",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  g0 <- glayout(container = f0, spacing = 1)

  # Samples -------------------------------------------------------------------

  g0[1, 1] <- glabel(text = "Samples Table:", container = g0)

  dfs <- c("<Select a dataset>", listObjects(env = env, obj.class = "data.frame"))

  g0[1, 2] <- g0_data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )
  g0[1, 3] <- g0_data_samples_lbl <- glabel(text = " 0 samples", container = g0)

  addHandlerChanged(g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_data_drp)

    # Check if suitable.
    requiredCol <- c(
      "Sample.File", "Sample.Name", "Size.Standard",
      "Instrument.Type", "Instrument.ID", "Cap", "Well", "SQ"
    )
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gSamples <<- get(val_obj, envir = env)
      .gSamplesName <<- val_obj
      svalue(g0_data_samples_lbl) <- paste(
        length(unique(.gSamples$Sample.Name)),
        "samples."
      )
      svalue(f4_save_edt) <- paste(val_obj, "_cap", sep = "")
    } else {

      # Reset components.
      .gSamples <<- NULL
      .gSamplesName <<- NULL
      svalue(g0_data_drp, index = TRUE) <- 1
      svalue(g0_data_samples_lbl) <- " 0 samples"
      svalue(f4_save_edt) <- ""
    }
  })

  # Plot ----------------------------------------------------------------------

  g0[2, 1] <- glabel(text = "Sample Plot Sizing Table:", container = g0)

  # NB! dfs defined in previous section.
  g0[2, 2] <- g0_ref_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )

  g0[2, 3] <- g0_ref_samples_lbl <- glabel(text = " 0 sample files", container = g0)

  addHandlerChanged(g0_ref_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_ref_drp)

    # Check if suitable.
    requiredCol <- c("Sample.File.Name", "Size", "Height")
    ok <- checkDataset(
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
      svalue(g0_ref_samples_lbl) <- " 0 sample files"
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = "Options",
    horizontal = FALSE,
    spacing = 10,
    container = gv
  )

  f1g1 <- ggroup(horizontal = TRUE, spacing = 5, container = f1)
  glabel(
    text = "Run name:", initial.msg = "Optional run name",
    anchor = c(-1, 0), container = f1g1
  )
  f1_run_edt <- gedit(text = "", width = 45, container = f1g1)

  f1g2 <- ggroup(horizontal = TRUE, spacing = 5, container = f1)
  glabel(
    text = "Sizing quality threshold:",
    anchor = c(-1, 0), container = f1g2
  )
  f1_sq_spb <- gspinbutton(
    from = 0, to = 1, by = 0.01, value = 0, digits = 4,
    container = f1g2
  )

  # FRAME 4 ###################################################################

  f4 <- gframe(
    text = "Save as",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = "Name for result:", container = f4)

  f4_save_edt <- gedit(text = "", expand = TRUE, container = f4)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = "Calculate", container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_samples <- .gSamples
    val_name_samples <- .gSamplesName
    val_plot <- .gPlot
    val_name_plot <- .gPlotName
    val_sq <- as.numeric(svalue(f1_sq_spb))
    val_run <- svalue(f1_run_edt)
    val_name <- svalue(f4_save_edt)

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
      svalue(calculate_btn) <- "Processing..."
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
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = "calculateCapillary_gui", arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(head(datanew))
        print(paste("EXIT:", match.call()[[1]]))
      }

      # Close GUI.
      dispose(w)
    } else {
      message <- "A 'Samples Table' dataset and a 'SamplePlotSizing' dataset have to be selected."

      gmessage(message,
        title = "Datasets not selected",
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
