################################################################################
# TODO LIST
# TODO: Separate action and save with buttons "Execute" and "Save" to allow multiple actions.

################################################################################
# CHANGE LOG (last 20 changes)
# 11.07.2018: Fixed field 'Fixed value' not always disabled when it should.
# 10.08.2017: Fixed column dropdowns lose selection after selecting dataset.
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 09.05.2016: Added attributes to result.
# 09.05.2016: 'Save as' textbox expandable.
# 22.12.2015: Removed colum check to enable no selected column etc.
# 28.08.2015: Added importFrom.
# 23.05.2015: First version.


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
  .columnActionDefault <- "<Select action>"
  .columnDropDefault <- "<Select column>"

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Column actions", visible = FALSE)

  # Runs when window is closed.
  addHandlerDestroy(w, handler = function(h, ...) {

    # Save GUI state.
    .saveSettings()

    # Focus on parent window.
    if (!is.null(parent)) {
      focus(parent)
    }
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 15,
    use.scrollwindow = FALSE,
    container = w,
    expand = FALSE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("columns_gui", help_type = "html"))
  })

  # DATASET ###################################################################

  f0 <- gframe(
    text = "Dataset",
    horizontal = FALSE,
    spacing = 10,
    container = gv
  )


  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1, 1] <- glabel(text = "Select dataset:", container = f0g0)

  f0g0[1, 2] <- f0g0_data_drp <- gcombobox(
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

  f0g0[1, 3] <- f0g0_data_col_lbl <- glabel(
    text = " 0 columns",
    container = f0g0
  )

  addHandlerChanged(f0g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_data_drp)

    # Check if suitable.
    ok <- is.data.frame(get(val_obj, envir = env))

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      svalue(f0g0_data_col_lbl) <- paste(" ", ncol(.gData), " columns")
      svalue(f2_name) <- paste(.gDataName, "new", sep = "_")

      f1g1_col1_drp[] <- c(.columnDropDefault, names(.gData))
      svalue(f1g1_col1_drp, index = TRUE) <- 1
      f1g1_col2_drp[] <- c(.columnDropDefault, names(.gData))
      svalue(f1g1_col2_drp, index = TRUE) <- 1
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- " 0 columns"
      svalue(f2_name) <- ""

      f1g1_col1_drp[] <- c(.columnDropDefault)
      svalue(f1g1_col1_drp, index = TRUE) <- 1
      f1g1_col2_drp[] <- c(.columnDropDefault)
      svalue(f1g1_col2_drp, index = TRUE) <- 1
    }
  })

  # COLUMNS ###################################################################

  f1 <- gframe(text = "Columns", horizontal = FALSE, spacing = 10, container = gv)

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(text = "Select column 1:", container = f1g1)

  f1g1[1, 2] <- f1g1_col1_drp <- gcombobox(
    items = c(.columnDropDefault),
    editable = FALSE,
    container = f1g1,
    ellipsize = "none"
  )

  f1g1[2, 1] <- glabel(text = "Select column 2:", container = f1g1)

  f1g1[2, 2] <- f1g1_col2_drp <- gcombobox(
    items = c(.columnDropDefault),
    editable = FALSE,
    container = f1g1,
    ellipsize = "none"
  )

  addHandlerChanged(f1g1_col1_drp, handler = function(h, ...) {
    val_col <- svalue(f1g1_col1_drp)

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

  addHandlerChanged(f1g1_col2_drp, handler = function(h, ...) {
    if (svalue(f1g1_col2_drp) %in% names(.gData)) {
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

  f3 <- gframe(text = "Options", horizontal = FALSE, spacing = 10, container = gv)

  f3g1 <- glayout(container = f3, spacing = 1)

  f3g1[1, 1] <- glabel(text = "Fixed value:", container = f3g1)

  f3g1[1, 2] <- f3g1_val_edt <- gedit(text = "", width = 25, container = f3g1)

  f3g1[2, 1] <- glabel(text = "Column for new values:", container = f3g1)

  f3g1[2, 2] <- f3g1_col_edt <- gedit(text = "", width = 25, container = f3g1)

  f3g1[3, 1] <- glabel(text = "Action:", container = f3g1)

  action_items <- c(.columnActionDefault, "&", "+", "*", "-", "/", "substr")
  f3g1[3, 2] <- f3g1_action_drp <- gcombobox(
    items = action_items, selected = 1,
    editable = FALSE, container = f3g1,
    ellipsize = "none"
  )

  f3g1[4, 1] <- glabel(text = "Start position:", container = f3g1)

  f3g1[4, 2] <- f3g1_start_edt <- gedit(text = "1", width = 25, container = f3g1)

  f3g1[5, 1] <- glabel(text = "Stop position:", container = f3g1)

  f3g1[5, 2] <- f3g1_stop_edt <- gedit(text = "1", width = 25, container = f3g1)


  addHandlerChanged(f3g1_action_drp, handler = function(h, ...) {
    if (svalue(f3g1_action_drp) %in% "substr") {

      # Reset column 2 if 'substr' is selected.
      svalue(f1g1_col2_drp, index = TRUE) <- 1
    }

    .updateGui()
  })

  # NAME ######################################################################

  f2 <- gframe(
    text = "Save as",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = "Save as:", container = f2)
  f2_name <- gedit(text = "", container = f2, expand = TRUE)

  # BUTTON ####################################################################

  if (debug) {
    print("BUTTON")
  }

  combine_btn <- gbutton(text = "Execute", container = gv)

  addHandlerChanged(combine_btn, handler = function(h, ...) {
    val_col1 <- svalue(f1g1_col1_drp)
    val_col2 <- svalue(f1g1_col2_drp)
    val_action <- svalue(f3g1_action_drp)
    val_target <- svalue(f3g1_col_edt)
    val_fixed <- svalue(f3g1_val_edt)
    val_start <- as.integer(svalue(f3g1_start_edt))
    val_stop <- as.integer(svalue(f3g1_stop_edt))
    val_name <- svalue(f2_name)
    val_data_name <- .gDataName
    val_data <- .gData

    # Check values.
    if (val_col1 == .columnDropDefault) {
      val_col1 <- NA
    }
    if (val_col2 == .columnDropDefault) {
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
    datanew <- auditTrail(
      obj = datanew, key = keys, value = values,
      label = "columns_gui", arguments = FALSE,
      package = "strvalidator"
    )

    # Save data.
    saveObject(name = val_name, object = datanew, parent = w, env = env)

    if (debug) {
      print(datanew)
      print(paste("EXIT:", match.call()[[1]]))
    }

    # Close GUI.
    dispose(w)
  })

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {
    substr_selected <- svalue(f3g1_action_drp) %in% "substr"
    no_action <- svalue(f3g1_action_drp) %in% .columnActionDefault
    second_col_selected <- svalue(f1g1_col2_drp) %in% names(.gData)

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
      if (exists(".strvalidator_columns_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_columns_gui_savegui", envir = env)
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
      if (exists(".strvalidator_columns_gui_import_opt", envir = env, inherits = FALSE)) {
        svalue(import_opt) <- get(".strvalidator_columns_gui_import_opt", envir = env)
      }
      if (exists(".strvalidator_columns_gui_fixed", envir = env, inherits = FALSE)) {
        svalue(f3g1_val_edt) <- get(".strvalidator_columns_gui_fixed", envir = env)
      }
      if (exists(".strvalidator_columns_gui_action", envir = env, inherits = FALSE)) {
        svalue(f3g1_action_drp) <- get(".strvalidator_columns_gui_action", envir = env)
      }
      if (exists(".strvalidator_columns_gui_start", envir = env, inherits = FALSE)) {
        svalue(f3g1_start_edt) <- get(".strvalidator_columns_gui_start", envir = env)
      }
      if (exists(".strvalidator_columns_gui_stop", envir = env, inherits = FALSE)) {
        svalue(f3g1_stop_edt) <- get(".strvalidator_columns_gui_stop", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_columns_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_columns_gui_fixed", value = svalue(f3g1_val_edt), envir = env)
      assign(x = ".strvalidator_columns_gui_action", value = svalue(f3g1_action_drp), envir = env)
      assign(x = ".strvalidator_columns_gui_start", value = svalue(f3g1_start_edt), envir = env)
      assign(x = ".strvalidator_columns_gui_stop", value = svalue(f3g1_stop_edt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_columns_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_columns_gui_fixed", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_fixed", envir = env)
      }
      if (exists(".strvalidator_columns_gui_action", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_action", envir = env)
      }
      if (exists(".strvalidator_columns_gui_start", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_start", envir = env)
      }
      if (exists(".strvalidator_columns_gui_stop", envir = env, inherits = FALSE)) {
        remove(".strvalidator_columns_gui_stop", envir = env)
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
