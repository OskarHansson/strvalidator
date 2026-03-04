#' @title Column Actions
#'
#' @description
#' GUI wrapper for the \code{\link{columns}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{columns}} function by providing a
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


columns_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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
    STR_WIN_TITLE           = "Column actions",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_COLUMNS         = "columns",
    STR_FRM_COLUMNS         = "Columns",
    STR_LBL_COL1            = "Column 1:",
    STR_LBL_COL2            = "Column 2:",
    STR_DRP_COLUMN          = "<Select column>",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_FIXED           = "Fixed value:",
    STR_LBL_NEW             = "Column for new values:",
    STR_LBL_ACTION          = "Action:",
    STR_DRP_ACTION          = "<Select action>",
    STR_LBL_START           = "Start position:",
    STR_LBL_STOP            = "Stop position:",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Execute"
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

  # DATASET ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g0)

  data_col_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_COLUMNS),
    container = f0g0
  )

  data_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
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

  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)

    # Check if suitable.
    ok <- is.data.frame(get(val_obj, envir = env))

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      svalue(data_col_lbl) <- paste(" ", ncol(.gData), strings$STR_LBL_COLUMNS)
      svalue(save_edt) <- paste(.gDataName, "new", sep = "_")

      col1_drp[] <- c(strings$STR_DRP_COLUMN, names(.gData))
      svalue(col1_drp, index = TRUE) <- 1
      col2_drp[] <- c(strings$STR_DRP_COLUMN, names(.gData))
      svalue(col2_drp, index = TRUE) <- 1
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_col_lbl) <- paste(" 0", strings$STR_LBL_COLUMNS)
      svalue(save_edt) <- ""

      col1_drp[] <- c(strings$STR_DRP_COLUMN)
      svalue(col1_drp, index = TRUE) <- 1
      col2_drp[] <- c(strings$STR_DRP_COLUMN)
      svalue(col2_drp, index = TRUE) <- 1
    }
  })

  # COLUMNS ###################################################################

  f1 <- gframe(text = strings$STR_FRM_COLUMNS, horizontal = FALSE, spacing = 1, container = gv)

  f1g1 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_COL1, container = f1g1)

  col1_drp <- gcombobox(
    items = c(strings$STR_DRP_COLUMN),
    editable = FALSE,
    container = f1g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  f1g2 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_COL2, container = f1g2)

  col2_drp <- gcombobox(
    items = c(strings$STR_DRP_COLUMN),
    editable = FALSE,
    container = f1g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(col1_drp, handler = function(h, ...) {
    val_col <- svalue(col1_drp)

    # Check if column exist.
    ok <- val_col %in% names(.gData)

    # Update target column.
    if (length(ok) > 0) {
      if (ok) {
        svalue(f3g1_col_edt) <- val_col
      } else {
        svalue(f3g1_col_edt) <- ""
      }
    }
  })

  addHandlerChanged(col2_drp, handler = function(h, ...) {
    if (svalue(col2_drp) %in% names(.gData)) {
      # If an existing colum gets selected.

      if (svalue(f3g1_action_drp) %in% "substr") {
        # Check if 'substr' is selected.

        # Reset action dropdown.
        svalue(f3g1_action_drp, index = TRUE) <- 1
      }
    }

    .updateGui()
  })

  # OPTIONS ###################################################################

  f3 <- gframe(
    text = strings$STR_FRM_OPTIONS, horizontal = FALSE, spacing = 1,
    container = gv
  )

  f3g1 <- glayout(container = f3, spacing = 1)

  f3g1[1, 1] <- glabel(text = strings$STR_LBL_FIXED, container = f3g1)

  f3g1[1, 2] <- f3g1_val_edt <- gedit(text = "", width = 25, container = f3g1)

  f3g1[2, 1] <- glabel(text = strings$STR_LBL_NEW, container = f3g1)

  f3g1[2, 2] <- f3g1_col_edt <- gedit(text = "", width = 25, container = f3g1)

  f3g1[3, 1] <- glabel(text = strings$STR_LBL_ACTION, container = f3g1)

  action_items <- c(strings$STR_DRP_ACTION, "&", "+", "*", "-", "/", "substr")
  f3g1[3, 2] <- f3g1_action_drp <- gcombobox(
    items = action_items, selected = 1,
    editable = FALSE, container = f3g1,
    ellipsize = "none"
  )

  f3g1[4, 1] <- glabel(text = strings$STR_LBL_START, container = f3g1)

  f3g1[4, 2] <- f3g1_start_edt <- gedit(text = "1", width = 25, container = f3g1)

  f3g1[5, 1] <- glabel(text = strings$STR_LBL_STOP, container = f3g1)

  f3g1[5, 2] <- f3g1_stop_edt <- gedit(text = "1", width = 25, container = f3g1)


  addHandlerChanged(f3g1_action_drp, handler = function(h, ...) {
    if (svalue(f3g1_action_drp) %in% "substr") {
      # Reset column 2 if 'substr' is selected.
      svalue(col2_drp, index = TRUE) <- 1
    }

    .updateGui()
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  combine_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerChanged(combine_btn, handler = function(h, ...) {
    val_col1 <- svalue(col1_drp)
    val_col2 <- svalue(col2_drp)
    val_action <- svalue(f3g1_action_drp)
    val_target <- svalue(f3g1_col_edt)
    val_fixed <- svalue(f3g1_val_edt)
    val_start <- as.integer(svalue(f3g1_start_edt))
    val_stop <- as.integer(svalue(f3g1_stop_edt))
    val_name <- svalue(save_edt)
    val_data_name <- .gDataName
    val_data <- .gData

    # Check values.
    if (val_col1 == strings$STR_DRP_COLUMN) {
      val_col1 <- NA
    }
    if (val_col2 == strings$STR_DRP_COLUMN) {
      val_col2 <- NA
    }

    datanew <- columns(
      data = val_data, col1 = val_col1, col2 = val_col2,
      operator = val_action, fixed = val_fixed,
      target = val_target, start = val_start, stop = val_stop,
      debug = debug
    )

    # Create key-value pairs to log.
    keys <- list(
      "data", "col1", "col2", "action", "target",
      "fixed", "start", "stop"
    )

    values <- list(
      val_data_name, val_col1, val_col2, val_action, val_target,
      val_fixed, val_start, val_stop
    )

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
  })

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {
    substr_selected <- svalue(f3g1_action_drp) %in% "substr"
    no_action <- svalue(f3g1_action_drp) %in% strings$STR_DRP_ACTION
    second_col_selected <- svalue(col2_drp) %in% names(.gData)

    if (substr_selected || second_col_selected) {
      enabled(f3g1_val_edt) <- FALSE
    } else {
      enabled(f3g1_val_edt) <- TRUE
    }

    if (substr_selected) {
      enabled(f3g1_start_edt) <- TRUE
      enabled(f3g1_stop_edt) <- TRUE
    } else {
      enabled(f3g1_start_edt) <- FALSE
      enabled(f3g1_stop_edt) <- FALSE
    }

    if (no_action) {
      enabled(combine_btn) <- FALSE
    } else {
      enabled(combine_btn) <- TRUE
    }
  }

  settings_prefix <- ".strvalidator_columns_gui_"
  settings_widgets <- list(
    fixed = f3g1_val_edt,
    action = f3g1_action_drp,
    start = f3g1_start_edt,
    stop = f3g1_stop_edt
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
  .updateGui()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
} # End of GUI
