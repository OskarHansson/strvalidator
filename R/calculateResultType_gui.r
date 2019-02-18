###############################################################################
# CHANGE LOG (last 20 changes)
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 18.09.2016: Added attributes to result.
# 29.04.2016: 'Save as' textbox expandable.
# 28.08.2015: Added importFrom.
# 04.06.2015: Changed button label 'Calculate result type' to 'Calculate'.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 15.01.2014: Added option for parameter to add missing markers.
# 10.11.2013: Added kit dropbox.
# 03.11.2013: First version.

#' @title Calculate Result Type
#'
#' @description
#' GUI wrapper for the \code{\link{calculateResultType}} function.
#'
#' @details Simplifies the use of \code{\link{calculateResultType}} by providing a
#' graphical user interface.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help
#'
#' @return TRUE
#'
#' @seealso \code{\link{calculateResultType}}

calculateResultType_gui <- function(env = parent.frame(), savegui = NULL,
                                    debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Calculate result type", visible = FALSE)

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

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("calculateResultType_gui", help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = "Dataset and kit",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  g0 <- glayout(container = f0, spacing = 1)

  # Datasets ------------------------------------------------------------------

  g0[1, 1] <- glabel(text = "Select dataset:", container = g0)

  g0[1, 2] <- g0_dataset_drp <- gcombobox(
    items = c(
      "<Select dataset>",
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

  g0[1, 3] <- g0_samples_lbl <- glabel(text = " 0 samples", container = g0)

  g0[2, 1] <- glabel(text = " and the kit used:", container = g0)

  g0[2, 2] <- f0_kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )


  addHandlerChanged(g0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_dataset_drp)

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
      svalue(g0_samples_lbl) <- paste("", samples, "samples")
      svalue(f2_save_edt) <- paste(val_obj, "_type", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(f0_kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(g0_dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = "Options",
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f1_add_chk <- gcheckbox(
    text = "Add missing markers (can be slow on large datasets)",
    checked = TRUE, container = f1
  )

  glabel(text = "NB! All markers must be present in each sample for correct results.")

  glabel(
    text = "Peak height threshold (RFU):",
    container = f1, anchor = c(-1, 0)
  )

  f1_rfu_edt <- gedit(text = "", width = 6, container = f1)

  glabel(
    text = "Define subtypes of mixtures by number of markers with >2 detected peaks:",
    container = f1, anchor = c(-1, 0)
  )

  f1_mix_edt <- gedit(text = "", width = 6, container = f1)

  glabel(
    text = "Define subtypes of partial profiles by number of detected peaks:",
    container = f1, anchor = c(-1, 0)
  )

  f1_par_edt <- gedit(text = "", width = 6, container = f1)

  glabel(
    text = "Define subtypes of partial profiles by kit:",
    container = f1, anchor = c(-1, 0)
  )

  f1_kit_drp <- gcombobox(
    items = c("<Select kit>", getKit()),
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = "Save as",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = "Name for result:", container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE, fill = TRUE)

  # BUTTON ####################################################################


  calculate_btn <- gbutton(text = "Calculate", container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    val_data <- .gData
    val_name_data <- .gDataName
    val_threshold <- as.numeric(svalue(f1_rfu_edt))
    val_mix <- svalue(f1_mix_edt)
    val_par <- svalue(f1_par_edt)
    val_subkit <- svalue(f1_kit_drp)
    val_name <- svalue(f2_save_edt)
    val_marker <- NULL
    val_kit <- svalue(f0_kit_drp)
    val_add <- svalue(f1_add_chk)

    if (debug) {
      print("GUI options:")
      print("val_threshold")
      print(val_threshold)
      print("val_mix")
      print(val_mix)
      print("val_par")
      print(val_par)
      print("val_subkit")
      print(val_subkit)
      print("val_marker")
      print(val_marker)
      print("val_name")
      print(val_name)
      print("val_kit")
      print(val_kit)
      print("val_add")
      print(val_add)
    }

    # Prepare arguments -------------------------------------------------------

    # No threshold is represented by NULL.
    if (is.na(val_threshold)) {
      val_threshold <- NULL
    }

    # Check if empty.
    if (nchar(val_mix) == 0) {
      # No limit is represented by NULL.
      val_mix <- NULL
    } else {
      # Convert string to numeric vector.
      val_mix <- unlist(strsplit(x = val_mix, split = ",", fixed = TRUE))
      val_mix <- as.numeric(val_mix)
    }

    # Check if empty.
    if (nchar(val_par) == 0) {
      # No limit is represented by NULL.
      val_par <- NULL
    } else {
      # Convert string to numeric vector.
      val_par <- unlist(strsplit(x = val_par, split = ",", fixed = TRUE))
      val_par <- as.numeric(val_par)
    }

    # Check if kit is provided and available.
    if (val_subkit %in% getKit()) {
      # Get marker names and create string.
      val_marker <- paste(getKit(kit = val_subkit, what = "Marker"), collapse = "|")
    } else {
      # Set to NA.
      val_subkit <- NA
    }

    if (debug) {
      print("Function arguments:")
      print("val_threshold")
      print(val_threshold)
      print("val_mix")
      print(val_mix)
      print("val_par")
      print(val_par)
      print("val_subkit")
      print(val_subkit)
      print("val_marker")
      print(val_marker)
      print("val_name")
      print(val_name)
      print("val_kit")
      print(val_kit)
    }

    if (!is.null(val_data)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- "Processing..."
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateResultType(
        data = val_data,
        kit = val_kit,
        add.missing.marker = val_add,
        threshold = val_threshold,
        mixture.limits = val_mix,
        partial.limits = val_par,
        subset.name = val_subkit,
        marker.subset = val_marker,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "add.missing.marker", "threshold", "mixture.limits",
        "partial.limits", "subset.name", "marker.subset"
      )

      values <- list(
        val_name_data, val_add, val_threshold, val_mix,
        val_par, val_subkit, val_marker
      )

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = "calculateResultType_gui",
        arguments = FALSE, package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(paste("EXIT:", match.call()[[1]]))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      message <- "A dataset and a reference dataset have to be selected."

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
      if (exists(".strvalidator_calculateResultType_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateResultType_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateResultType_gui_rfu", envir = env, inherits = FALSE)) {
        svalue(f1_rfu_edt) <- get(".strvalidator_calculateResultType_gui_rfu", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_mix", envir = env, inherits = FALSE)) {
        svalue(f1_mix_edt) <- get(".strvalidator_calculateResultType_gui_mix", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_par", envir = env, inherits = FALSE)) {
        svalue(f1_par_edt) <- get(".strvalidator_calculateResultType_gui_par", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_kit", envir = env, inherits = FALSE)) {
        svalue(f1_kit_drp) <- get(".strvalidator_calculateResultType_gui_kit", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_add", envir = env, inherits = FALSE)) {
        svalue(f1_add_chk) <- get(".strvalidator_calculateResultType_gui_add", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateResultType_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateResultType_gui_rfu", value = svalue(f1_rfu_edt), envir = env)
      assign(x = ".strvalidator_calculateResultType_gui_mix", value = svalue(f1_mix_edt), envir = env)
      assign(x = ".strvalidator_calculateResultType_gui_par", value = svalue(f1_par_edt), envir = env)
      assign(x = ".strvalidator_calculateResultType_gui_kit", value = svalue(f1_kit_drp), envir = env)
      assign(x = ".strvalidator_calculateResultType_gui_add", value = svalue(f1_add_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateResultType_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateResultType_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_rfu", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateResultType_gui_rfu", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_mix", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateResultType_gui_mix", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_par", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateResultType_gui_par", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_kit", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateResultType_gui_kit", envir = env)
      }
      if (exists(".strvalidator_calculateResultType_gui_add", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateResultType_gui_add", envir = env)
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
