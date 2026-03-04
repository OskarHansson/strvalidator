
#' @title Calculate Allele
#'
#' @description
#' GUI wrapper for the \code{\link{calculateAllele}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateAllele}} function by providing a
#'  graphical user interface to it.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help
#'
#' @return TRUE


calculate_allele_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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
    STR_WIN_TITLE           = "Calculate summary statistics for alleles",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_ROWS            = "rows",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_THRESHOLD       = "Peak height threshold: ",
    STR_TIP_THRESHOLD       = "Peaks with heights below this value will be removed.",
    STR_CHK_SEX_MARKERS     = "Remove sex markers defined in kit.",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_MSG_NOT_DF          = "Data set must be a data.frame!",
    STR_MSG_TITLE_ERROR     = "Error"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # ---------------------------------------------------------------------------

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

    return(FALSE) # Destroy window.
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = w,
    expand = FALSE
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
    expand = FALSE,
    fill = "x"
  )

  # DATASET -------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g0)

  f0g0_data_col_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_ROWS),
    container = f0g0
  )

  f0g0_data_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      list_objects(
        env = env,
        obj_class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(f0g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_data_drp)

    # Check if suitable.
    requiredCol <- c("Marker", "Allele")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      svalue(f0g0_data_col_lbl) <- paste(" ", nrow(.gData), strings$STR_LBL_ROWS)
      svalue(save_edt) <- paste(.gDataName, "allele", sep = "_")

      # Autodetect kit.
      svalue(kit_drp) <- detect_kit(
        data = .gData, index = FALSE,
        debug = debug
      )[1]
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- paste(" 0", strings$STR_LBL_ROWS)
      svalue(save_edt) <- ""
    }
  })

  # KIT -----------------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_KIT, container = f0g2)

  kit_drp <- gcombobox(
    items = get_kit(),
    selected = 1,
    editable = FALSE,
    container = f0g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # OPTIONS ###################################################################

  f1 <- gframe(text = strings$STR_FRM_OPTIONS, horizontal = FALSE, spacing = 1, container = gv)

  f1g1 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")

  f1_threshold_lbl <- glabel(
    text = strings$STR_LBL_THRESHOLD,
    container = f1g1
  )

  f1_threshold_edt <- gedit(text = "", width = 10, container = f1g1)
  tooltip(f1_threshold_edt) <- strings$STR_TIP_THRESHOLD

  f1g2 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")

  f1_sex_chk <- gcheckbox(
    text = strings$STR_CHK_SEX_MARKERS,
    checked = FALSE, container = f1g2
  )

  addHandlerChanged(f1_sex_chk, handler = function(h, ...) {
    .widgetState()
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  button_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerChanged(button_btn, handler = function(h, ...) {
    val_data <- .gData
    val_name_data <- .gDataName
    val_name <- svalue(save_edt)
    val_threshold <- as.numeric(svalue(f1_threshold_edt))
    val_sex <- svalue(f1_sex_chk)
    val_kit <- svalue(kit_drp)

    if (is.data.frame(val_data)) {
      # Check status to set correct values for arguments.
      if (!val_sex) {
        # If no filtering of sex markers kit should be NULL.
        val_kit <- NULL
      }
      if (is.na(val_threshold)) {
        # If missing threshold should be NULL.
        val_threshold <- NULL
      }

      datanew <- calculate_allele(
        data = val_data, threshold = val_threshold,
        sex_rm = val_sex, kit = val_kit, debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list("data", "threshold", "sex", "kit")

      values <- list(val_name_data, val_threshold, val_sex, val_kit)

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      save_object(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(datanew)
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strings$STR_MSG_NOT_DF,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .widgetState <- function() {
    val_obj <- svalue(f1_sex_chk)

    if (val_obj) {
      enabled(kit_drp) <- TRUE
    } else {
      enabled(kit_drp) <- FALSE
    }
  }

  settings_prefix <- ".strvalidator_calculateAllele_gui_"
  settings_widgets <- list(
    threshold = f1_threshold_edt,
    sex = f1_sex_chk
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

  # Initiate widgets.
  .widgetState()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
} # End of GUI
