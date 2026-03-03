#' @title Add Dye Information
#'
#' @description
#' GUI wrapper to the \code{\link{add_color}} function.
#'
#' @details
#' Convenience GUI for the use of \code{\link{add_color}} and
#' \code{\link{addOrder}} to add 'Dye', 'Color', 'R.Color', and marker 'Order'
#' to a dataset.
#' 'Dye' is the one letter abbreviations for the fluorophores commonly used
#' to label primers in forensic STR typing kits (e.g. R and Y),
#' 'Color' is the corresponding color name (e.g. red and yellow),
#' 'R.Color' is the plot color used in R (e.g. red and black).
#' 'Order' is the marker order in the selected kit.
#' NB! Existing columns will be overwritten.
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
#' @seealso \code{\link{add_color}}

addDye_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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
    STR_WIN_TITLE           = "Add dye to dataset",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset and kit",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_IGNORE          = "Ignore case in marker name",
    STR_CHK_DYE             = "Add dye information",
    STR_CHK_COLOR           = "Add color information",
    STR_CHK_R               = "Add R color information",
    STR_CHK_MARKER          = "Add marker order",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_ADD             = "Add",
    STR_BTN_PROCESSING      = "Processing..."
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # ---------------------------------------------------------------------------

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

  # DATASET ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g0)

  dataset_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0g0
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
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Marker")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(dataset_samples_lbl) <- paste(" ", samples, strings$STR_LBL_SAMPLES)
      .gKit <<- detectKit(.gData, index = TRUE)
      svalue(kit_drp, index = TRUE) <- .gKit
      svalue(save_edt) <- paste(.gDataName, sep = "")
    } else {
      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(dataset_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  # KIT -----------------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_KIT, container = f0g1)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = f0g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # FRAME 1 ###################################################################

  f1 <- gframe(text = strings$STR_FRM_OPTIONS, horizontal = FALSE, spacing = 1, container = gv)

  f1_ignore_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE,
    checked = FALSE, container = f1
  )

  f1_dye_chk <- gcheckbox(
    text = strings$STR_CHK_DYE,
    checked = TRUE, container = f1
  )

  f1_color_chk <- gcheckbox(
    text = strings$STR_CHK_COLOR,
    checked = FALSE, container = f1
  )

  f1_r_chk <- gcheckbox(
    text = strings$STR_CHK_R,
    checked = FALSE, container = f1
  )

  f1_order_chk <- gcheckbox(
    text = strings$STR_CHK_MARKER,
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
    val_name <- svalue(save_edt)
    val_ignore <- svalue(f1_ignore_chk)
    val_dye <- svalue(f1_dye_chk)
    val_color <- svalue(f1_color_chk)
    val_r <- svalue(f1_r_chk)
    val_order <- svalue(f1_order_chk)

    # Initialise what information is needed from the add_color function.
    need <- NULL
    if (val_dye) {
      need <- c(need, "Dye")
    }
    if (val_color) {
      need <- c(need, "Color")
    }
    if (val_r) {
      need <- c(need, "R.Color")
    }

    if (debug) {
      print(".gData")
      print(names(.gData))
      print("val_kit")
      print(val_kit)
      print("val_ignore")
      print(val_ignore)
      print("val_dye")
      print(val_dye)
      print("val_color")
      print(val_color)
      print("val_r")
      print(val_r)
      print("val_order")
      print(val_order)
    }

    # Change button.
    blockHandlers(add_btn)
    svalue(add_btn) <- strings$STR_BTN_PROCESSING
    unblockHandlers(add_btn)
    enabled(add_btn) <- FALSE

    if (!is.null(need)) {
      message(
        "Adding the following color information: ",
        paste(need, collapse = ", ")
      )

      val_data <- add_color(
        data = val_data, kit = val_kit, need = need,
        overwrite = TRUE, ignore_case = val_ignore,
        debug = debug
      )
    }

    if (val_order) {
      val_data <- addOrder(
        data = val_data, kit = val_kit, overwrite = TRUE,
        ignore.case = val_ignore, debug = debug
      )
    }

    # Save to new variable.
    datanew <- val_data

    # Add attributes to result.
    attr(datanew, which = "kit") <- val_kit

    # Create key-value pairs to log.
    keys <- list(
      "data", "kit", "ignore.case", "dye", "color",
      "r.color", "order"
    )

    values <- list(
      val_data_name, val_kit, val_ignore, val_dye, val_color,
      val_r, val_order
    )

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
      if (exists(".strvalidator_addDye_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_addDye_gui_savegui", envir = env)
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
      if (exists(".strvalidator_addDye_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_addDye_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_dye", envir = env, inherits = FALSE)) {
        svalue(f1_dye_chk) <- get(".strvalidator_addDye_gui_dye", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_color", envir = env, inherits = FALSE)) {
        svalue(f1_color_chk) <- get(".strvalidator_addDye_gui_color", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_r", envir = env, inherits = FALSE)) {
        svalue(f1_r_chk) <- get(".strvalidator_addDye_gui_r", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_order", envir = env, inherits = FALSE)) {
        svalue(f1_order_chk) <- get(".strvalidator_addDye_gui_order", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_addDye_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_dye", value = svalue(f1_dye_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_color", value = svalue(f1_color_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_r", value = svalue(f1_r_chk), envir = env)
      assign(x = ".strvalidator_addDye_gui_order", value = svalue(f1_order_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_addDye_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_dye", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_dye", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_color", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_color", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_r", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_r", envir = env)
      }
      if (exists(".strvalidator_addDye_gui_order", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addDye_gui_order", envir = env)
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
