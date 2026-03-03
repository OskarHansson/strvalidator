#' @title Add Missing Markers
#'
#' @description
#' GUI wrapper for the \code{\link{addMarker}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{addMarker}} function by providing a graphical
#' user interface to it.
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
#' @importFrom utils head help
#'
#' @seealso \code{\link{addMarker}}

addMarker_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- data.frame(No.Data = NA)
  .gDataName <- NULL
  .gKit <- 1

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Add missing markers to dataset",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset and kit",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_IGNORE          = "Ignore case in marker name",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_ADD             = "Add",
    STR_BTN_PROCESSING      = "Processing..."
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

    # Destroys window.
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

  # DATASET ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv,
    expand = FALSE,
    fill = "x"
  )

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g1)

  dataset_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0g1
  )

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
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

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # Get data.
      .gData <<- get(val_obj, envir = env)

      # Load or change components.
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(dataset_samples_lbl) <- paste(" ", samples, strings$STR_LBL_SAMPLES)
      .gKit <<- detectKit(.gData, index = TRUE)
      svalue(kit_drp, index = TRUE) <- .gKit
      svalue(save_edt) <- paste(.gDataName, "_marker", sep = "")
    } else {
      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(dataset_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  # KIT -----------------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_KIT, container = f0g2)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = f0g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # FRAME 1 ###################################################################

  # OPTIONS -----------------------------------------------------------------------

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1_ignore_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE,
    checked = FALSE, container = f1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  add_btn <- gbutton(text = strings$STR_BTN_ADD, container = gv)

  addHandlerClicked(add_btn, handler = function(h, ...) {
    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_data_name <- .gDataName
    val_ignore <- svalue(f1_ignore_chk)
    val_name <- svalue(save_edt)

    if (debug) {
      print(".gData")
      print(names(.gData))
      print("val_kit")
      print(val_kit)
    }

    # Change button.
    blockHandlers(add_btn)
    svalue(add_btn) <- strings$STR_BTN_PROCESSING
    unblockHandlers(add_btn)
    enabled(add_btn) <- FALSE

    datanew <- addMarker(
      data = val_data,
      marker = getKit(val_kit, what = "Marker"),
      ignore.case = val_ignore, debug = debug
    )

    # Add attributes to result.
    attr(datanew, which = "kit") <- val_kit

    # Create key-value pairs to log.
    keys <- list("data", "kit", "ignore.case")

    values <- list(val_data_name, val_kit, val_ignore)

    # Update audit trail.
    datanew <- audit_trail(
      obj = datanew, key = keys, value = values,
      label = fnc, arguments = FALSE,
      package = "strvalidator"
    )

    # Save data.
    saveObject(name = val_name, object = datanew, parent = w, env = env)

    # Close GUI.
    .saveSettings()
    dispose(w)
  })

  # INTERNAL FUNCTIONS ########################################################

  settings_prefix <- ".strvalidator_addMarker_gui_"
  settings_widgets <- list(
    ignore = f1_ignore_chk
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
} # End of GUI
