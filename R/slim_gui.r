#' @title Slim Data Frames
#'
#' @description
#' GUI wrapper for the \code{\link{slim}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{slim}} function by providing a graphical
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
#' @importFrom utils help
#'
#' @seealso \code{\link{slim}}

slim_gui <- function(env = parent.frame(), savegui = NULL,
                     debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- data.frame(No.Data = NA)
  .gDataName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Slim dataset",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_COLUMNS         = "columns",
    STR_LBL_ROWS            = "rows",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_KEEP            = "Keep rows in fixed columns even if no data in stacked columns",
    STR_LBL_KEEP            = "(i.e. keep one row per marker for each sample even if no peak)",
    STR_TIP_COLUMNS         = "Tip: Manually edit the columns to fix and stack (e.g. stack='Allele' will stack 'Allele.1', 'Allele.2'...)",
    STR_FRM_FIX             = "Fix",
    STR_LBL_FIX             = "Columns to keep fixed (separate by pipe |):",
    STR_EDT_MSG             = "Doubleklick or drag column names to list",
    STR_FRM_STACK           = "Stack",
    STR_LBL_STACK           = "Columns to stack (separate by pipe |):",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_MSG_NAME            = "A file name must be provided!",
    STR_BTN_SLIM            = "Slim dataset",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_TITLE_ERROR     = "Error"
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


  # Vertical sub group.
  g0 <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = gv,
    expand = FALSE,
    fill = TRUE
  )

  # Horizontal sub group.
  g1 <- ggroup(
    horizontal = TRUE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = gv,
    expand = TRUE,
    fill = TRUE
  )

  # Vertical sub group.
  g2 <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = gv,
    expand = FALSE,
    fill = TRUE
  )


  # DATASET ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = TRUE,
    spacing = 1,
    container = g0
  )

  glabel(text = strings$STR_LBL_DATASET, container = f0)

  samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0
  )
  columns_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_COLUMNS),
    container = f0
  )
  rows_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_ROWS),
    container = f0
  )

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      list_objects(
        env = env,
        obj_class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0,
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
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      .refresh_fix_tbl()
      .refresh_stack_tbl()

      samples <- length(unique(.gData$Sample.Name))
      # Info.
      if ("Sample.Name" %in% names(.gData)) {
        samples <- length(unique(.gData$Sample.Name))
        svalue(samples_lbl) <- paste(" ", samples, " ",
          strings$STR_LBL_SAMPLES, ", ",
          sep = ""
        )
      } else {
        svalue(samples_lbl) <- paste(" ", "<NA> ",
          strings$STR_LBL_SAMPLES, ", ",
          sep = ""
        )
      }
      svalue(columns_lbl) <- paste(" ", ncol(.gData), " ",
        strings$STR_LBL_COLUMNS, ", ",
        sep = ""
      )
      svalue(rows_lbl) <- paste(" ", nrow(.gData), " ",
        strings$STR_LBL_ROWS, ", ",
        sep = ""
      )
      # Result name.
      svalue(save_edt) <- paste(val_obj, "_slim", sep = "")

      # Guess column names to keep fixed.
      svalue(fix_edt) <- col_names(
        data = .gData, slim = TRUE,
        numbered = TRUE, concatenate = "|"
      )

      # Guess column names to stack.
      svalue(stack_edt) <- col_names(
        data = .gData, slim = FALSE,
        numbered = TRUE, concatenate = "|"
      )

      # Reset button.
      svalue(slim_btn) <- strings$STR_BTN_SLIM
      enabled(slim_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(fix_edt) <- ""
      svalue(stack_edt) <- ""
      .refresh_fix_tbl()
      .refresh_stack_tbl()
      svalue(samples_lbl) <- paste(" ", "<NA> ",
        strings$STR_LBL_SAMPLES, ", ",
        sep = ""
      )
      svalue(columns_lbl) <- paste(" ", "<NA> ",
        strings$STR_LBL_COLUMNS, ", ",
        sep = ""
      )
      svalue(rows_lbl) <- paste(" ", "<NA> ",
        strings$STR_LBL_ROWS, ", ",
        sep = ""
      )
      svalue(save_edt) <- ""
    }
  })

  # FIX #######################################################################

  if (debug) {
    print("COLUMNS")
    print(names(.gData))
  }

  fix_f <- gframe(
    text = strings$STR_FRM_FIX,
    horizontal = FALSE, container = g1,
    expand = TRUE, fill = TRUE
  )

  fix_lbl <- glabel(
    text = strings$STR_LBL_FIX,
    container = fix_f,
    anchor = c(-1, 0)
  )

  fix_edt <- gedit(
    initial.msg = strings$STR_EDT_MSG,
    width = 40,
    container = fix_f
  )
  tooltip(fix_edt) <- strings$STR_TIP_COLUMNS

  fix_tbl <- gWidgets2::gtable(
    items = names(.gData),
    container = fix_f,
    expand = TRUE
  )

  # Set initial size (only height is important here).
  size(fix_tbl) <- c(100, 200)

  addDropTarget(fix_edt, handler = function(h, ...) {
    if (debug) {
      print("SAMPLES:DROPTARGET")
    }

    # Get values.
    drp_val <- h$dropdata
    fix_val <- svalue(h$obj)

    # Add new value to selected.
    new <- ifelse(nchar(fix_val) > 0,
      paste(fix_val, drp_val, sep = "|"),
      drp_val
    )

    # Update text box.
    svalue(h$obj) <- new

    # Update sample name table.
    tmp_tbl <- fix_tbl[, ] # Get all values.
    print(tmp_tbl)
    tmp_tbl <- tmp_tbl[tmp_tbl != drp_val] # Remove value added to selected.
    fix_tbl[, ] <- tmp_tbl # Update table.
  })

  # STACK #####################################################################

  if (debug) {
    print("STACK")
  }

  stack_f <- gframe(
    text = strings$STR_FRM_STACK,
    horizontal = FALSE, container = g1,
    expand = TRUE, fill = TRUE
  )

  stack_lbl <- glabel(
    text = strings$STR_LBL_STACK,
    container = stack_f,
    anchor = c(-1, 0)
  )

  stack_edt <- gedit(
    initial.msg = strings$STR_EDT_MSG,
    width = 40,
    container = stack_f
  )
  tooltip(stack_edt) <- strings$STR_TIP_COLUMNS

  stack_tbl <- gWidgets2::gtable(
    items = names(.gData),
    container = stack_f,
    expand = TRUE
  )

  addDropTarget(stack_edt, handler = function(h, ...) {
    # Get values.
    drp_val <- h$dropdata
    stack_val <- svalue(h$obj)

    # Add new value to selected.
    new <- ifelse(nchar(stack_val) > 0,
      paste(stack_val, drp_val, sep = "|"),
      drp_val
    )

    # Update text box.
    svalue(h$obj) <- new

    # Update column name table.
    tmp_tbl <- stack_tbl[, ] # Get all values.
    print(tmp_tbl)
    tmp_tbl <- tmp_tbl[tmp_tbl != drp_val] # Remove value added to selected.
    stack_tbl[, ] <- tmp_tbl # Update table.
  })

  # OPTIONS ###################################################################

  option_frm <- gframe(text = strings$STR_FRM_OPTIONS, horizontal = FALSE, container = g0)

  keep_chk <- gcheckbox(text = strings$STR_CHK_KEEP, checked = TRUE, container = option_frm)

  glabel(text = strings$STR_LBL_KEEP, container = option_frm, anchor = c(-1, 0))

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = g2)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  slim_btn <- gbutton(text = strings$STR_BTN_SLIM, container = g2)

  addHandlerClicked(slim_btn, handler = function(h, ...) {
    # Get new dataset name.
    val_name <- svalue(save_edt)
    val_data <- .gData
    val_data_name <- .gDataName

    if (nchar(val_name) > 0) {
      # Get values.
      fix_val <- svalue(fix_edt)
      stack_val <- svalue(stack_edt)
      keep_val <- svalue(keep_chk)

      # Slim require a vector of strings.
      fix_val <- unlist(strsplit(fix_val, "|", fixed = TRUE))
      stack_val <- unlist(strsplit(stack_val, "|", fixed = TRUE))

      if (debug) {
        print("val_data")
        print(names(val_data))
        print("fix_val")
        print(fix_val)
        print("stack_val")
        print(stack_val)
        print("keep_val")
        print(keep_val)
      }

      # Change button.
      blockHandlers(slim_btn)
      svalue(slim_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(slim_btn)
      enabled(slim_btn) <- FALSE

      datanew <- slim(
        data = val_data, fix = fix_val, stack = stack_val,
        keep_na = keep_val, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "fix", "stack", "keep_na")

      values <- list(val_data_name, fix_val, stack_val, keep_val)

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      save_object(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strings$STR_MSG_NAME,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error",
        parent = w
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .refresh_fix_tbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Refresh widget by removing it and...
    delete(fix_f, fix_tbl)

    # ...creating a new table.
    fix_tbl <<- gWidgets2::gtable(
      items = names(.gData),
      container = fix_f,
      expand = TRUE
    )

    addDropSource(fix_tbl, handler = function(h, ...) svalue(h$obj))

    addHandlerDoubleclick(fix_tbl, handler = function(h, ...) {
      # Get values.
      tbl_val <- svalue(h$obj)
      fix_val <- svalue(fix_edt)

      # Add new value to selected.
      new <- ifelse(nchar(fix_val) > 0,
        paste(fix_val, tbl_val, sep = "|"),
        tbl_val
      )

      # Update text box.
      svalue(fix_edt) <- new

      # Update sample name table.
      tmp_tbl <- fix_tbl[, ] # Get all values.
      tmp_tbl <- tmp_tbl[tmp_tbl != tbl_val] # Remove value added to selected.
      fix_tbl[, ] <- tmp_tbl # Update table.
    })
  }

  .refresh_stack_tbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Refresh widget by removing it and...
    delete(stack_f, stack_tbl)

    # ...creating a new table.
    stack_tbl <<- gWidgets2::gtable(
      items = names(.gData),
      container = stack_f,
      expand = TRUE
    )

    addDropSource(stack_tbl, handler = function(h, ...) svalue(h$obj))

    addHandlerDoubleclick(stack_tbl, handler = function(h, ...) {
      # Get values.
      tbl_val <- svalue(h$obj)
      stack_val <- svalue(stack_edt)

      # Add new value to selected.
      new <- ifelse(nchar(stack_val) > 0,
        paste(stack_val, tbl_val, sep = "|"),
        tbl_val
      )

      # Update text box.
      svalue(stack_edt) <- new

      # Update column name table.
      tmp_tbl <- stack_tbl[, ] # Get all values.
      tmp_tbl <- tmp_tbl[tmp_tbl != tbl_val] # Remove value added to selected.
      stack_tbl[, ] <- tmp_tbl # Update table.
    })
  }

  settings_prefix <- ".strvalidator_slim_gui_"
  settings_widgets <- list(
    title = keep_chk
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
