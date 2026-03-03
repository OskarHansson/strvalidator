################################################################################
# CHANGE LOG (last 20 changes)
# 20.06.2023: Fixed Error in !is.null(val_data) && !is.na(val_data) in coercion to 'logical(1)
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 02.05.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 02.05.2016: Added attributes.
# 12.10.2015: First version.

#' @title Remove Spike
#'
#' @description
#' GUI wrapper for the \code{\link{removeSpike}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{removeSpike}} function by providing a
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


removeSpike_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gSpike <- NULL
  .gSpikeName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Remove spikes",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_ROWS            = "rows",
    STR_LBL_SPIKES          = "Spike list:",
    STR_LBL_SAMPLES         = "samples",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_INVERT          = "Invert (remove all but spikes)",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_MSG_NOT_DF          = "Data set must be a data.frame!",
    STR_MSG_TITLE_ERROR     = "Error",
    STR_BTN_REMOVE          = "Remove"
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

  # Datasets ------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g0)

  data_col_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_ROWS),
    container = f0g0
  )

  data_drp <- gcombobox(
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

  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Size", "File.Name")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      svalue(data_col_lbl) <- paste(" ", nrow(.gData), " rows")
      svalue(save_edt) <- paste(.gDataName, "no_spikes", sep = "_")
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_col_lbl) <- paste(" 0", strings$STR_LBL_ROWS)
      svalue(save_edt) <- ""
    }
  })

  # Spikes --------------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_SPIKES, container = f0g1)

  spike_col_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0g1
  )

  spike_drp <- gcombobox(
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
    ellipsize = "none"
  )

  # Handlers ------------------------------------------------------------------

  addHandlerChanged(spike_drp, handler = function(h, ...) {
    val_obj <- svalue(spike_drp)

    # Check if suitable.
    requiredCol <- c("Allele", "Id", "Marker")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gSpike <<- get(val_obj, envir = env)
      .gSpikeName <<- val_obj

      svalue(spike_col_lbl) <- paste(
        "", length(unique(.gSpike$Id)),
        strings$STR_LBL_SAMPLES
      )
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_col_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
    }
  })

  # OPTIONS ###################################################################

  f1 <- gframe(text = strings$STR_FRM_OPTIONS, horizontal = FALSE, spacing = 1, container = gv)

  f1_invert_chk <- gcheckbox(
    text = strings$STR_CHK_INVERT,
    checked = FALSE, container = f1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  remove_btn <- gbutton(text = strings$STR_BTN_REMOVE, container = gv)

  addHandlerChanged(remove_btn, handler = function(h, ...) {
    val_data <- .gData
    val_spike <- .gSpike
    val_name_data <- .gDataName
    val_name_spike <- .gSpikeName
    val_name <- svalue(save_edt)
    val_invert <- svalue(f1_invert_chk)

    if (is.data.frame(val_data) & is.data.frame(val_spike)) {
      datanew <- removeSpike(
        data = val_data, spike = val_spike,
        invert = val_invert, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "spike", "invert")

      values <- list(val_name_data, val_name_spike, val_invert)

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

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
      if (exists(".strvalidator_removeSpike_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_removeSpike_gui_savegui", envir = env)
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
      if (exists(".strvalidator_removeSpike_gui_invert", envir = env, inherits = FALSE)) {
        svalue(f1_invert_chk) <- get(".strvalidator_removeSpike_gui_invert", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_removeSpike_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_removeSpike_gui_invert", value = svalue(f1_invert_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_removeSpike_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_removeSpike_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_removeSpike_gui_invert", envir = env, inherits = FALSE)) {
        remove(".strvalidator_removeSpike_gui_invert", envir = env)
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
} # End of GUI
