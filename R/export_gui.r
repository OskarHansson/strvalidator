#' @title Export
#'
#' @description
#' GUI wrapper for the \code{\link{export}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{export}} function by providing a
#' graphical user interface to it. Currently all available objects provided
#' are selected by default.
#'
#' @param env environment where the objects exist.
#' Default is the current environment.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#' @param obj character vector with object names.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help
#'
#' @seealso \code{\link{export}}


export_gui <- function(obj = listObjects(env = env, obj.class = c("data.frame", "ggplot")),
                       env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Export objects as files or images",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_OBJECTS         = "Objects",
    STR_LBL_OBJECTS         = "objects selected for export:",
    STR_LBL_VALUES          = "Values",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_FILES           = "Files",
    STR_CHK_NAMES           = "Use object names as file names",
    STR_LBL_NAMES           = "File names (separated by | or ,):",
    STR_CHK_OVERWRITE       = "Overwrite existing files",
    STR_LBL_EXTENSION       = "File extension:",
    STR_LBL_DELIMITER       = "Delimiter:",
    STR_DRP_TAB             = "TAB",
    STR_DRP_SPACE           = "SPACE",
    STR_DRP_COMMA           = "COMMA",
    STR_LBL_SETTINGS        = "Image settings:",
    STR_LBL_WIDTH           = "Width:",
    STR_LBL_HEIGHT          = "Height:",
    STR_LBL_RESOLUTION      = "Resolution:",
    STR_FRM_LOCATION        = "Location",
    STR_LBL_LOCATION        = "Save files to path:",
    STR_BTN_OPEN            = "Open",
    STR_BTN_EXPORT          = "Export",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_OBJECT          = "At least one object must be selected.\nFile name and path must be provided.\n\nThis error may also occur the first time the function is used.\nPlease locate the folder using the 'Open' button.",
    STR_MSG_TITLE_ERROR     = "Error",
    STR_MSG_FAILED          = "\nThe following objects were not saved because the file names existed.\nMake sure to exit the last edited cell before continuing.",
    STR_MSG_TITLE_FAILED    = "Export failed!",
    STR_LBL_OBJECT          = "Object",
    STR_LBL_NEW_NAME        = "New.Name",
    STR_BTN_CANCEL          = "Cancel",
    STR_BTN_OVERWRITE       = "Overwrite",
    STR_BTN_RETRY           = "Retry"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(
    title = strings$STR_WIN_TITLE,
    visible = FALSE
  )

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
    horizontal = FALSE, spacing = 1, use.scrollwindow = FALSE,
    container = w, expand = TRUE
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

  # OBJECTS ###################################################################

  object_frm <- gframe(
    text = strings$STR_FRM_OBJECTS, horizontal = FALSE, spacing = 1, expand = TRUE,
    fill = TRUE, container = gv
  )

  # Active selection label.
  selection_lbl <- glabel(
    text = paste(length(obj), strings$STR_LBL_OBJECTS),
    anchor = c(-1, 0), container = object_frm
  )

  # Create list of objects.
  object_tbl <- gWidgets2::gtable(
    items = obj, multiple = TRUE,
    expand = TRUE, container = object_frm
  )

  # Initiate with all objects selected.
  # (works best from within the main GUI but not ideal for the separate wrapper)
  if (length(obj) > 0) {
    svalue(object_tbl) <- seq(1, length(obj))
  }

  # Set initial minimal size.
  size(object_tbl) <- c(100, 150)

  # OPTIONS ###################################################################

  option_frm <- gframe(
    text = strings$STR_FRM_OPTIONS, horizontal = FALSE, spacing = 1,
    container = gv
  )

  name_chk <- gcheckbox(
    text = strings$STR_CHK_NAMES,
    checked = TRUE, container = option_frm
  )

  glabel(text = strings$STR_LBL_NAMES, container = option_frm, anchor = c(-1, 0))

  name_edt <- gedit(container = option_frm, expand = TRUE, fill = TRUE)

  # Defult is disabled.
  enabled(name_edt) <- FALSE

  replace_chk <- gcheckbox(
    text = strings$STR_CHK_OVERWRITE,
    checked = TRUE, container = option_frm
  )

  # FILE ----------------------------------------------------------------------

  glabel(text = "", container = option_frm) # Spacer.

  file_grp <- ggroup(spacing = 1, container = option_frm)

  glabel(text = strings$STR_LBL_EXTENSION, container = file_grp, anchor = c(-1, 0))

  ext_drp <- gcombobox(
    items = c("auto", ".RData"),
    selected = 1, editable = FALSE,
    container = file_grp, ellipsize = "none"
  )

  glabel(text = strings$STR_LBL_DELIMITER, container = file_grp, anchor = c(-1, 0))

  del_drp <- gcombobox(
    items = c(strings$STR_DRP_TAB, strings$STR_DRP_SPACE, strings$STR_DRP_COMMA),
    selected = 1, editable = FALSE,
    container = file_grp, ellipsize = "none"
  )

  # IMAGE ---------------------------------------------------------------------

  glabel(text = "", container = option_frm) # Spacer.

  glabel(text = strings$STR_LBL_SETTINGS, container = option_frm, anchor = c(-1, 0))

  image_grp <- ggroup(spacing = 1, container = option_frm)

  glabel(text = strings$STR_LBL_WIDTH, container = image_grp, anchor = c(-1, 0))
  width_edt <- gedit(text = "3000", width = 6, container = image_grp)

  glabel(text = strings$STR_LBL_HEIGHT, container = image_grp, anchor = c(-1, 0))
  height_edt <- gedit(text = "2000", width = 6, container = image_grp)

  glabel(text = strings$STR_LBL_RESOLUTION, container = image_grp, anchor = c(-1, 0))
  res_edt <- gedit(text = "250", width = 6, container = image_grp)

  # PATH ----------------------------------------------------------------------

  glabel(text = "", container = option_frm) # Spacer.

  path_grp <- gframe(text = strings$STR_FRM_LOCATION, horizontal = FALSE, spacing = 1, container = gv)

  glabel(text = strings$STR_LBL_LOCATION, container = path_grp, anchor = c(-1, 0))

  # NB! text = getwd() does not always work (e.g. when 'Documents' are localized).
  # https://stackoverflow.com/questions/45231928/initiate-gfilebrowse-with-a-valid-path-gwidgets2
  # Unable to find a solution other then using the "Open" button to browse...
  save_brw <- gfilebrowse(
    text = getwd(), quote = FALSE, type = "selectdir",
    initial.dir = getwd(), expand = TRUE, container = path_grp
  )

  # BUTTON ####################################################################

  export_btn <- gbutton(text = strings$STR_BTN_EXPORT, container = gv)

  # HANDLERS ##################################################################

  addHandlerSelectionChanged(object_tbl, handler = function(h, ...) {
    # Get selected values.
    val <- svalue(object_tbl)

    # Update current selection label.
    svalue(selection_lbl) <- paste(length(val), strings$STR_LBL_OBJECTS)
  })

  addHandlerChanged(name_chk, handler = function(h, ...) {
    .updateGui()
  })

  addHandlerChanged(ext_drp, handler = function(h, ...) {
    # Get values.
    val <- svalue(ext_drp)

    if (val == ".RData") {
      enabled(del_drp) <- FALSE
      enabled(image_grp) <- FALSE
    } else {
      enabled(del_drp) <- TRUE
      enabled(image_grp) <- TRUE
    }
  })

  addHandlerChanged(export_btn, handler = function(h, ...) {
    # Get values.
    val_object <- svalue(object_tbl)
    val_use_obj <- svalue(name_chk)
    val_name <- svalue(name_edt)
    val_replace <- svalue(replace_chk)
    val_ext <- svalue(ext_drp)
    val_del <- svalue(del_drp, index = TRUE)
    val_w <- as.numeric(svalue(width_edt))
    val_h <- as.numeric(svalue(height_edt))
    val_r <- as.numeric(svalue(res_edt))
    val_path <- svalue(save_brw)

    # Assign a delimiter character.
    if (val_del == 1) {
      val_delimiter <- "\t"
    } else if (val_del == 2) {
      val_delimiter <- " "
    } else if (val_del == 3) {
      val_delimiter <- ","
    }

    # Check file name.
    if (nchar(val_name) == 0) {
      val_name <- NA
    }

    # Check path.
    if (length(val_path) == 0) {
      val_path <- NA
    }

    if (debug) {
      print("val_object")
      print(val_object)
      print("val_use_obj")
      print(val_use_obj)
      print("val_name")
      print(val_name)
      print("val_ext")
      print(val_ext)
      print("val_del")
      print(val_del)
      print("val_w")
      print(val_w)
      print("val_h")
      print(val_h)
      print("val_r")
      print(val_r)
      print("val_path")
      print(val_path)
    }

    # Check for file name and path.
    ok <- val_use_obj || !is.na(val_name)
    ok <- ok && !is.na(val_path)

    # Check if any objects have been selected.
    if (length(val_object) == 0) {
      ok <- FALSE
    }

    if (ok) {
      blockHandlers(export_btn)
      svalue(export_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(export_btn)

      repeat{
        fail <- export(
          object = val_object, name = val_name, use.object.name = val_use_obj,
          env = env, path = val_path,
          ext = val_ext, delim = val_delimiter,
          width = val_w, height = val_h, res = val_r,
          overwrite = val_replace, debug = debug
        )

        if (is.data.frame(fail)) {
          if (debug) {
            print("The following objects failed:")
            print(fail)
          }

          dialog <- gbasicdialog(
            title = strings$STR_MSG_TITLE_FAILED, parent = w,
            do.buttons = FALSE,
            width = 200, height = 200, horizontal = FALSE
          )

          group <- ggroup(horizontal = FALSE, container = dialog)

          msg <- glabel(text = strings$STR_MSG_FAILED, anchor = c(-1, 0), container = group)

          tbl <- gdf(items = fail, container = group, expand = TRUE)
          size(tbl) <- c(100, 200)

          gg <- ggroup(container = group)
          btn_cancel <- gbutton(strings$STR_BTN_CANCEL, container = gg, handler = function(h, ...) {
            fail <<- NULL
            dispose(dialog)
          })

          btn_replace <- gbutton(strings$STR_BTN_OVERWRITE, container = gg, handler = function(h, ...) {
            val_replace <<- TRUE
            dispose(dialog)
          })

          btn_retry <- gbutton(strings$STR_BTN_RETRY, container = gg, handler = function(h, ...) {
            val_object <<- tbl[, strings$STR_LBL_OBJECT]
            val_name <<- tbl[, strings$STR_LBL_NEW_NAME]
            val_use_obj <<- FALSE

            if (debug) {
              print("val_object")
              print(val_object)
              print("val_name")
              print(val_name)
            }

            dispose(dialog)
          })

          visible(dialog, set = TRUE)
        } else {
          break
        }

        if (is.null(fail)) {
          break
        }
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strings$STR_MSG_OBJECT,
        title = strings$STR_MSG_TITLE_ERROR,
        parent = w,
        icon = "error"
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {
    # Get values.
    val <- svalue(name_chk)

    if (val) {
      enabled(name_edt) <- FALSE
    } else {
      enabled(name_edt) <- TRUE
    }
  }

  settings_prefix <- ".strvalidator_export_gui_"
  settings_widgets <- list(
    objName = name_chk,
    replace = replace_chk,
    ext = ext_drp,
    del = del_drp,
    width = width_edt,
    height = height_edt,
    res = res_edt,
    path = save_brw
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
}
