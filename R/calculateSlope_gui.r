################################################################################
# CHANGE LOG (last 20 changes)
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 06.09.2016: Implemented the 'word' option.
# 24.04.2016: First version.

#' @title Calculate Profile Slope
#'
#' @description
#' GUI wrapper for the \code{\link{calculateSlope}} function.
#'
#' @details Simplifies the use of the \code{\link{calculateSlope}} function
#' by providing a graphical user interface.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help str
#'
#' @seealso \code{\link{calculateSlope}}

calculateSlope_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  .gData <- NULL
  .gRef <- NULL
  .gDataName <- NULL
  .gRefName <- NULL

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  w <- gwindow(title = "Calculate profile slope", visible = FALSE)

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
    print(help("calculateSlope_gui", help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = "Dataset",
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f0g0 <- glayout(container = f0, spacing = 1)

  # Datasets ------------------------------------------------------------------

  f0g0[1, 1] <- glabel(text = "Select dataset:", container = f0g0)

  f0g0[1, 2] <- f0_dataset_drp <- gcombobox(
    items = c(
      "<Select dataset>",
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

  f0g0[1, 3] <- f0_samples_lbl <- glabel(text = " 0 samples", container = f0g0)

  addHandlerChanged(f0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(f0_dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Height",
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(f0_samples_lbl) <- paste("", samples, "samples")
      svalue(f2_save_edt) <- paste(.gDataName, "_slope", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(f1_kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f0_dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
    }
  })

  f0g0[2, 1] <- glabel(text = "Select reference dataset:", container = f0g0)

  f0g0[2, 2] <- f0_refset_drp <- gcombobox(
    items = c(
      "<Select dataset>",
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

  f0g0[2, 3] <- f0_ref_lbl <- glabel(text = " 0 references", container = f0g0)

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
      svalue(f0_ref_lbl) <- paste("", refs, "references")
    } else {

      # Reset components.
      .gRef <<- NULL
      svalue(f0_refset_drp, index = TRUE) <- 1
      svalue(f0_ref_lbl) <- " 0 references"
    }
  })

  # CHECK ---------------------------------------------------------------------

  if (debug) {
    print("CHECK")
  }

  f0g0[3, 2] <- f0_check_btn <- gbutton(text = "Check subsetting", container = f0g0)

  addHandlerChanged(f0_check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = "Check subsetting",
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- checkSubset(
        data = val_data,
        ref = val_ref,
        console = FALSE,
        ignore.case = val_ignore,
        word = val_word,
        exact = val_exact
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
        wrap = FALSE, container = chksubset_w
      )

      visible(chksubset_w) <- TRUE
    } else {
      gmessage(
        msg = "Data frame is NULL!\n\n
               Make sure to select a dataset and a reference set",
        title = "Error",
        icon = "error"
      )
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = "Options",
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(
    text = "Confidence limit:",
    container = f1g1
  )
  f1g1[1, 2] <- f1_conf_spn <- gspinbutton(
    from = 0, to = 1, by = 0.005,
    value = 0.975, container = f1g1
  )

  #----------------------------------------------------------------------------

  f1g1[2, 1] <- glabel(
    text = "Kit to calculate size from:", anchor = c(-1, 0),
    container = f1g1
  )

  f1g1[3, 1] <- f1_auto_chk <- gcheckbox(
    text = "Autodetect", checked = FALSE,
    container = f1g1
  )
  tooltip(f1_auto_chk) <- "Must be checked for multiple kits."

  f1g1[3, 2] <- f1_kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f1g1,
    ellipsize = "none"
  )
  tooltip(f1_kit_drp) <- "Not needed if 'Size' is provided in the dataset."

  #----------------------------------------------------------------------------
  glabel(
    text = "Reference sample name matching:", anchor = c(-1, 0),
    container = f1
  )

  f1_ignore_chk <- gcheckbox(
    text = "Ignore case", checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = "Add word boundaries", checked = FALSE,
    container = f1
  )

  f1_exact_chk <- gcheckbox(
    text = "Exact matching", checked = FALSE,
    container = f1
  )

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f1_auto_chk, handler = function(h, ...) {

    # Get values.
    val_auto <- svalue(f1_auto_chk)

    if (val_auto) {
      enabled(f1_kit_drp) <- FALSE
    } else {
      enabled(f1_kit_drp) <- TRUE
    }
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(text = "Save as", horizontal = TRUE, spacing = 5, container = gv)

  glabel(text = "Name for result:", container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE, fill = TRUE)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = "Calculate", container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- .gDataName
    val_name_ref <- .gRefName
    val_conf <- svalue(f1_conf_spn)
    val_kit <- svalue(f1_kit_drp)
    val_name <- svalue(f2_save_edt)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    val_auto <- svalue(f1_auto_chk)

    if (!is.null(val_data)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- "Processing..."
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      # Check if automatic kit detection.
      if (val_auto) {
        val_kit <- NULL
      }

      datanew <- calculateSlope(
        data = val_data, ref = val_ref, conf = val_conf,
        kit = val_kit,
        ignore.case = val_ignore, exact = val_exact,
        word = val_word, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "conf", "kit", "auto",
        "ignore.case", "word", "exact"
      )

      values <- list(
        val_name_data, val_name_ref, val_conf, val_kit, val_auto,
        val_ignore, val_word, val_exact
      )

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = "calculateSlope_gui", arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(str(datanew))
        print(paste("EXIT:", match.call()[[1]]))
      }

      # Close GUI.
      dispose(w)
    } else {
      message <- "A dataset must be selected."

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
      if (exists(".strvalidator_calculateSlope_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateSlope_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateSlope_gui_conf", envir = env, inherits = FALSE)) {
        svalue(f1_conf_spn) <- get(".strvalidator_calculateSlope_gui_conf", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculateSlope_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_calculateSlope_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_calculateSlope_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_auto", envir = env, inherits = FALSE)) {
        svalue(f1_auto_chk) <- get(".strvalidator_calculateSlope_gui_auto", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateSlope_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_conf", value = svalue(f1_conf_spn), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_calculateSlope_gui_auto", value = svalue(f1_auto_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateSlope_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_conf", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_conf", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateSlope_gui_auto", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateSlope_gui_auto", envir = env)
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
