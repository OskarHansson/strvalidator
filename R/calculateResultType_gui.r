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

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Calculate result type",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset and kit",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_NB              = "NB! All markers must be present in each sample for correct results.",
    STR_LBL_PRE             = "Pre-processing:",
    STR_CHK_MISSING         = "Add missing markers (can be slow on large datasets)",
    STR_LBL_METHOD          = "Calculate result type:",
    STR_LBL_HEIGHT          = "Define subtypes of complete profiles by all peaks > peak height threshold (RFU):",
    STR_TIP_HEIGHT          = "Complete >[RFU]",
    STR_LBL_MARKER          = "Define subtypes of mixtures by number of markers with > 2 detected peaks:",
    STR_TIP_MARKER          = "Mixture >[markers]",
    STR_LBL_PEAK            = "Define subtypes of partial profiles by number of detected peaks:",
    STR_TIP_PEAK            = "Partial >[peaks]",
    STR_LBL_FULL_KIT        = "Define subtypes of partial profiles by kit:",
    STR_DRP_KIT             = "<Select kit>",
    STR_TIP_FULL_KIT        = "Partial Complete [kit]",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A sample dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected"
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

  # Vertical main group.
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
      strings$STR_DRP_DATASET,
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

  # Kit -----------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_KIT, container = g1)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = g1,
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
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(samples_lbl) <- paste("", samples, strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- paste(val_obj, "_type", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
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

  glabel(text = strings$STR_LBL_NB, container = f1, anchor = c(-1, 0))

  glabel(text = strings$STR_LBL_PRE, container = f1, anchor = c(-1, 0))

  f1_add_chk <- gcheckbox(text = strings$STR_CHK_MISSING, checked = TRUE, container = f1)

  glabel(text = strings$STR_LBL_METHOD, container = f1, anchor = c(-1, 0))

  glabel(text = strings$STR_LBL_HEIGHT, container = f1, anchor = c(-1, 0))

  f1_rfu_edt <- gedit(text = "", width = 6, container = f1)
  tooltip(f1_rfu_edt) <- strings$STR_TIP_HEIGHT

  glabel(text = strings$STR_LBL_MARKER, container = f1, anchor = c(-1, 0))

  f1_mix_edt <- gedit(text = "", width = 6, container = f1)
  tooltip(f1_mix_edt) <- strings$STR_TIP_MARKER

  glabel(text = strings$STR_LBL_PEAK, container = f1, anchor = c(-1, 0))

  f1_par_edt <- gedit(text = "", width = 6, container = f1)
  tooltip(f1_par_edt) <- strings$STR_TIP_PEAK

  glabel(text = strings$STR_LBL_FULL_KIT, container = f1, anchor = c(-1, 0))

  f1_kit_drp <- gcombobox(
    items = c(strings$STR_DRP_KIT, getKit()),
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################


  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    val_data <- .gData
    val_name_data <- .gDataName
    val_threshold <- as.numeric(svalue(f1_rfu_edt))
    val_mix <- svalue(f1_mix_edt)
    val_par <- svalue(f1_par_edt)
    val_subkit <- svalue(f1_kit_drp)
    val_name <- svalue(save_edt)
    val_marker <- NULL
    val_kit <- svalue(kit_drp)
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
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
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
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc,
        arguments = FALSE, package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
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

  settings_prefix <- ".strvalidator_calculateResultType_gui_"
  settings_widgets <- list(
    rfu = f1_rfu_edt,
    mix = f1_mix_edt,
    par = f1_par_edt,
    kit = f1_kit_drp,
    add = f1_add_chk
  )

  settings_key <- function(name) {
    paste0(settings_prefix, name)
  }

  get_saved_setting <- function(name) {
    key <- settings_key(name)
    if (exists(key, envir = env, inherits = FALSE)) {
      return(get(key, envir = env))
    }
    NULL
  }

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
      saved_savegui <- get_saved_setting("savegui")
      if (!is.null(saved_savegui)) {
        svalue(savegui_chk) <- saved_savegui
      }
      if (debug) {
        print("Save GUI status loaded!")
      }
    }
    if (debug) {
      print(svalue(savegui_chk))
    }

    # Then load settings if true.
    if (isTRUE(svalue(savegui_chk))) {
      for (name in names(settings_widgets)) {
        value <- get_saved_setting(name)
        if (!is.null(value)) {
          svalue(settings_widgets[[name]]) <- value
        }
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (isTRUE(svalue(savegui_chk))) {
      assign(x = settings_key("savegui"), value = svalue(savegui_chk), envir = env)
      for (name in names(settings_widgets)) {
        assign(x = settings_key(name), value = svalue(settings_widgets[[name]]), envir = env)
      }
    } else { # or remove all saved values if false.
      for (name in c("savegui", names(settings_widgets))) {
        key <- settings_key(name)
        if (exists(key, envir = env, inherits = FALSE)) {
          remove(key, envir = env)
        }
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
