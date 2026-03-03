################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 07.03.2020: Added language support.
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

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Calculate profile slope",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_REF_DATASET     = "Reference dataset:",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_GROUPS          = "Groups:",
    STR_LBL_CONF            = "Confidence limit:",
    STR_LBL_KIT             = "Kit to calculate size from:",
    STR_CHK_KIT             = "Autodetect",
    STR_TIP_KIT             = "Must be checked for multiple kits.",
    STR_TIP_KIT_DRP         = "Not needed if 'Size' is provided in the dataset.",
    STR_LBL_MATCHING        = "Reference sample name matching:",
    STR_CHK_IGNORE          = "Ignore case",
    STR_CHK_WORD            = "Add word boundaries",
    STR_CHK_EXACT           = "Exact matching",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A sample dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected",
    STR_MSG_CHECK           = "Data frame is NULL!\n\nMake sure to select a sample dataset and a reference dataset.",
    STR_WIN_TITLE_CHECK     = "Check subsetting",
    STR_MSG_TITLE_ERROR     = "Error"
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

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g0)

  samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0g0
  )

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
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
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
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
      svalue(f2_save_edt) <- paste(.gDataName, "_slope", sep = "")
      svalue(f1_groups_lbl) <- paste(strings$STR_LBL_GROUPS, unique(.gData$Group))

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(f1_kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(f2_save_edt) <- ""
      svalue(f1_groups_lbl) <- paste(strings$STR_LBL_GROUPS, "")
    }
  })

  # Reference -----------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_REF_DATASET, container = f0g1)

  ref_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = f0g1
  )

  refset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(refset_drp, handler = function(h, ...) {
    val_obj <- svalue(refset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj
      refs <- length(unique(.gRef$Sample.Name))
      svalue(ref_lbl) <- paste("", refs, strings$STR_LBL_REF)
    } else {
      # Reset components.
      .gRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(ref_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # CHECK #####################################################################

  check_btn <- gbutton(text = strings$STR_BTN_CHECK, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = strings$STR_WIN_TITLE_CHECK,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- check_subset(
        data = val_data,
        ref = val_ref,
        console = FALSE,
        ignore_case = val_ignore,
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
        msg = strings$STR_MSG_CHECK,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    anchor = c(-1, 0),
    container = gv
  )

  #----------------------------------------------------------------------------

  f1_groups_lbl <- glabel(text = strings$STR_LBL_GROUPS, anchor = c(-1, 0), container = f1)

  #----------------------------------------------------------------------------

  f1g1 <- glayout(container = f1, spacing = 1, anchor = c(-1, 0))

  f1g1[1, 1] <- glabel(
    text = strings$STR_LBL_CONF,
    anchor = c(-1, 0),
    container = f1g1
  )
  f1g1[1, 2] <- f1_conf_spn <- gspinbutton(
    from = 0, to = 1, by = 0.005,
    value = 0.975, container = f1g1
  )

  #----------------------------------------------------------------------------

  f1g1[2, 1] <- glabel(
    text = strings$STR_LBL_KIT, anchor = c(-1, 0),
    container = f1g1
  )

  f1g1[3, 1] <- f1_auto_chk <- gcheckbox(
    text = strings$STR_CHK_KIT, checked = FALSE,
    container = f1g1
  )
  tooltip(f1_auto_chk) <- strings$STR_TIP_KIT

  f1g1[3, 2] <- f1_kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f1g1,
    ellipsize = "none"
  )
  tooltip(f1_kit_drp) <- strings$STR_TIP_KIT_DRP

  #----------------------------------------------------------------------------
  glabel(
    text = strings$STR_LBL_MATCHING, anchor = c(-1, 0),
    container = f1
  )

  f1_ignore_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE, checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = strings$STR_CHK_WORD, checked = FALSE,
    container = f1
  )

  f1_exact_chk <- gcheckbox(
    text = strings$STR_CHK_EXACT, checked = FALSE,
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

  f2 <- gframe(text = strings$STR_FRM_SAVE, horizontal = TRUE, spacing = 1, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE, fill = TRUE)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

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
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
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
