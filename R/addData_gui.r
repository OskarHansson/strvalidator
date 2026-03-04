#' @title Add Data
#'
#' @description
#' GUI wrapper for \code{\link{addData}}.
#'
#' @details
#' Simplifies the use of the \code{\link{addData}} function by providing a graphical
#' user interface to it.
#' @param env environment in which to search for data frames.
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
#' @seealso  \code{\link{addData}}

add_data_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gDataDest <- NULL
  .gDataDestName <- NULL
  .gDataDestColumns <- NULL
  .gDataSource <- NULL
  .gDataSourceColumns <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Add data",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Datasets",
    STR_LBL_DESTINATION     = "Select destination dataset:",
    STR_DRP_DEFAULT         = "<Select column>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_SOURCE          = "Select source dataset:",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_EXACT           = "Exact key matching",
    STR_CHK_IGNORE          = "Ignore case in marker name",
    STR_LBL_KEY1            = "Select primary key column:",
    STR_LBL_KEY2            = "Select secondary key column (optional):",
    STR_LBL_COLUMNS         = "Select columns to add to the new dataset:",
    STR_EDT_MSG             = "Default is all columns",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_ADD             = "Add",
    STR_BTN_ADD_ACTIVE      = "Processing...",
    STR_MSG_DATASET         = "A destination and source dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Datasets not selected"
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
    container = gv,
    expand = FALSE,
    fill = "x"
  )

  # Datasets ------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DESTINATION, container = g0)

  g0_samples_lbl <- glabel(text = paste(" 0", strings$STR_LBL_SAMPLES), container = g0)

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      list_objects(
        env = env,
        obj_class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    ok <- check_dataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gDataDest <<- get(val_obj, envir = env)
      .gDataDestName <<- val_obj
      .gDataDestColumns <<- names(.gDataDest)

      samples <- length(unique(.gDataDest$Sample.Name))
      svalue(g0_samples_lbl) <- paste(" ", samples, strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- paste(.gDataDestName, "_new", sep = "")

      # Update dropdown menues.
      f1_key_drp[] <- c(
        strings$STR_DRP_DEFAULT,
        intersect(.gDataDestColumns, .gDataSourceColumns)
      )
      f1_key2_drp[] <- c(
        strings$STR_DRP_DEFAULT,
        intersect(.gDataDestColumns, .gDataSourceColumns)
      )

      # Select default value.
      svalue(f1_key_drp, index = TRUE) <- 1
      svalue(f1_key2_drp, index = TRUE) <- 1
    } else {
      # Reset components.
      .gDataDest <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""

      # Update dropdown menues.
      f1_key_drp[] <- strings$STR_DRP_DEFAULT
      f1_key2_drp[] <- strings$STR_DRP_DEFAULT

      # Select default value.
      svalue(f1_key_drp, index = TRUE) <- 1
      svalue(f1_key2_drp, index = TRUE) <- 1
    }
  })

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_SOURCE, container = g1)

  g1_ref_lbl <- glabel(text = paste(" 0", strings$STR_LBL_SAMPLES), container = g1)

  refset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      list_objects(
        env = env,
        obj_class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(refset_drp, handler = function(h, ...) {
    val_obj <- svalue(refset_drp)

    # Check if suitable.
    ok <- check_dataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gDataSource <<- get(val_obj, envir = env)
      .gDataSourceColumns <<- names(.gDataSource)
      ref <- length(unique(.gDataSource$Sample.Name))
      svalue(g1_ref_lbl) <- paste(" ", ref, strings$STR_LBL_SAMPLES)

      # Update dropdown menues.
      f1_key_drp[] <- c(
        strings$STR_DRP_DEFAULT,
        intersect(.gDataDestColumns, .gDataSourceColumns)
      )

      f1_key2_drp[] <- c(
        strings$STR_DRP_DEFAULT,
        intersect(.gDataDestColumns, .gDataSourceColumns)
      )

      f1_col_drp[] <- c(strings$STR_DRP_DEFAULT, .gDataSourceColumns)

      # Select default value.
      svalue(f1_key_drp, index = TRUE) <- 1
      svalue(f1_key2_drp, index = TRUE) <- 1
      svalue(f1_col_drp, index = TRUE) <- 1
    } else {
      .gDataSource <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(g1_ref_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)

      # Update dropdown menues.
      f1_key_drp[] <- strings$STR_DRP_DEFAULT
      f1_key2_drp[] <- strings$STR_DRP_DEFAULT
      f1_col_drp[] <- strings$STR_DRP_DEFAULT

      # Select default value.
      svalue(f1_key_drp, index = TRUE) <- 1
      svalue(f1_key2_drp, index = TRUE) <- 1
      svalue(f1_col_drp, index = TRUE) <- 1
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1_exact_chk <- gcheckbox(
    text = strings$STR_CHK_EXACT,
    checked = TRUE, container = f1
  )

  f1_ignore_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE,
    checked = TRUE, container = f1
  )

  enabled(f1_ignore_chk) <- !svalue(f1_exact_chk)

  glabel(text = strings$STR_LBL_KEY1, container = f1, anchor = c(-1, 0))
  f1_key_drp <- gcombobox(
    items = strings$STR_DRP_DEFAULT,
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  glabel(
    text = strings$STR_LBL_KEY2, container = f1,
    anchor = c(-1, 0)
  )
  f1_key2_drp <- gcombobox(
    items = strings$STR_DRP_DEFAULT,
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  glabel(
    text = strings$STR_LBL_COLUMNS, container = f1,
    anchor = c(-1, 0)
  )
  f1_col_drp <- gcombobox(
    items = strings$STR_DRP_DEFAULT,
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  f1_col_edt <- gedit(
    text = "", initial.msg = strings$STR_EDT_MSG,
    container = f1
  )

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f1_exact_chk, handler = function(h, ...) {
    val_obj <- svalue(f1_exact_chk)

    if (val_obj) {
      enabled(f1_ignore_chk) <- FALSE
    } else {
      enabled(f1_ignore_chk) <- TRUE
    }
  })

  addHandlerChanged(f1_col_drp, handler = function(h, ...) {
    val_drp <- svalue(f1_col_drp)
    val_edt <- svalue(f1_col_edt)

    if (!is.null(val_drp) && val_drp != strings$STR_DRP_DEFAULT) {
      if (nchar(val_edt) == 0) {
        svalue(f1_col_edt) <- val_drp
      } else {
        svalue(f1_col_edt) <- paste(val_edt, val_drp, sep = ",")
      }
    }
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################


  add_btn <- gbutton(text = strings$STR_BTN_ADD, container = gv)

  addHandlerClicked(add_btn, handler = function(h, ...) {
    # Get values.
    val_destination <- svalue(dataset_drp)
    val_source <- svalue(refset_drp)
    val_exact <- svalue(f1_exact_chk)
    val_key <- svalue(f1_key_drp)
    val_key2 <- svalue(f1_key2_drp)
    val_what <- svalue(f1_col_edt)
    val_ignore <- svalue(f1_ignore_chk)
    val_name <- svalue(save_edt)

    # Check if default.
    if (val_key == strings$STR_DRP_DEFAULT) {
      val_key <- NULL
    }
    if (val_key2 == strings$STR_DRP_DEFAULT) {
      val_key2 <- NULL
    }

    # Prepare columns to add.
    if (nchar(val_what) == 0) {
      # Default is all columns.
      val_what <- NULL
    } else {
      # Create vector of column names to add.
      delimeters <- ",|, | |;|; |:|: "
      val_what <- strsplit(x = val_what, split = delimeters, fixed = FALSE)
      val_what <- unlist(val_what)
    }

    if (debug) {
      print("val_exact")
      print(val_exact)
      print("val_key")
      print(val_key)
      print("val_key2")
      print(val_key2)
      print("val_what")
      print(val_what)
      print("val_name")
      print(val_name)
      print("val_ignore")
      print(val_ignore)
    }

    # Check dataset and first key (second key is optional)
    if (!is.null(.gDataDest) & !is.null(.gDataSource) & !is.null(val_key)) {
      # Change button.
      blockHandlers(add_btn)
      svalue(add_btn) <- strings$STR_BTN_ADD_ACTIVE
      unblockHandlers(add_btn)
      enabled(add_btn) <- FALSE

      datanew <- add_data(
        data = .gDataDest,
        new_data = .gDataSource,
        exact = val_exact,
        by_col = val_key,
        then_by_col = val_key2,
        what = val_what,
        ignore_case = val_ignore
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "new_data", "exact", "by_col", "then_by_col",
        "what", "ignore_case"
      )

      values <- list(
        val_destination, val_source, val_exact, val_key, val_key2,
        val_what, val_ignore
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

  settings_prefix <- ".strvalidator_addData_gui_"
  settings_widgets <- list(
    exact = f1_exact_chk
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
