################################################################################
# CHANGE LOG (last 20 changes)
# 03.05.2019: Fixed "Error in structure(.External(.C_dotTclObjv, objv).." (tcltk).
# 03.05.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 24.07.2018: Added instruction in error message.
# 24.07.2018: Fixed: Error in if (nchar(val_path) == 0) { : argument is of length zero
# 21.07.2017: Function now exported. Fixed number of selected objects.
# 19.07.2017: Now closes after export.
# 18.07.2017: Fixed button infinite loop issue.
# 18.07.2017: Implemented last export path used.
# 18.07.2017: Fixed "Error in if (nchar(val_path) == 0 || val_path == expDefText)".
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 07.07.2017: Replaced gWidgets:: with gWidgets2::
# 29.08.2015: Added importFrom.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 20.11.2013: Specified package for function 'gtable' -> 'gWidgets::gtable'
# 27.10.2013: Added warning when no object selected.
# 15.07.2013: Added save GUI settings.

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
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(
    title = "Export objects as files or images",
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

    # Check which toolkit we are using.
    if (gtoolkit() == "tcltk") {
      if (as.numeric(gsub("[^0-9]", "", packageVersion("gWidgets2tcltk"))) <= 106) {
        # Version <= 1.0.6 have the wrong implementation:
        # See: https://stackoverflow.com/questions/54285836/how-to-retrieve-checkbox-state-in-gwidgets2tcltk-works-in-gwidgets2rgtk2
        message("tcltk version <= 1.0.6, returned TRUE!")
        return(TRUE) # Destroys window under tcltk, but not RGtk2.
      } else {
        # Version > 1.0.6 will be fixed:
        # https://github.com/jverzani/gWidgets2tcltk/commit/9388900afc57454b6521b00a187ca4a16829df53
        message("tcltk version >1.0.6, returned FALSE!")
        return(FALSE) # Destroys window under tcltk, but not RGtk2.
      }
    } else {
      message("RGtk2, returned FALSE!")
      return(FALSE) # Destroys window under RGtk2, but not with tcltk.
    }
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE, spacing = 5, use.scrollwindow = FALSE,
    container = w, expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("export_gui", help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = "Objects", horizontal = FALSE, spacing = 2, expand = TRUE,
    fill = TRUE, container = gv
  )

  # Active selection label.
  f0_selection_lbl <- glabel(
    text = paste(length(obj), " objects selected for export:"),
    anchor = c(-1, 0), container = f0
  )

  # Create list of objects.
  f0_object_tbl <- gWidgets2::gtable(
    items = obj, multiple = TRUE,
    expand = TRUE, container = f0
  )

  # Initiate with all objects selected.
  # (works best from within the main GUI but not ideal for the separate wrapper)
  if (length(obj) > 0) {
    svalue(f0_object_tbl) <- seq(1, length(obj))
  }

  # Set initial minimal size.
  size(f0_object_tbl) <- c(100, 150)

  # FRAME 1 ###################################################################

  f1 <- gframe(text = "File name", horizontal = FALSE, spacing = 2, container = gv)

  f1_name_chk <- gcheckbox(
    text = "Use object names",
    checked = TRUE,
    container = f1
  )

  glabel(
    text = "File name (separated by | or ,):",
    container = f1, anchor = c(-1, 0)
  )

  f1_name_edt <- gedit(container = f1, expand = TRUE, fill = TRUE)

  # Defult is disabled.
  enabled(f1_name_edt) <- FALSE

  f1_replace_chk <- gcheckbox(
    text = "Overwrite existing files",
    checked = TRUE, container = f1
  )

  # FRAME 2 ###################################################################

  f2 <- gframe(text = "Options", horizontal = TRUE, spacing = 2, container = gv)

  glabel(
    text = "File extension: ", container = f2,
    anchor = c(-1, 0)
  )

  f2_ext_drp <- gcombobox(
    items = c("auto", ".RData"),
    selected = 1, editable = FALSE,
    container = f2, ellipsize = "none"
  )

  glabel(text = " Delimiter: ", container = f2, anchor = c(-1, 0))

  f2_del_drp <- gcombobox(
    items = c("TAB", "SPACE", "COMMA"),
    selected = 1, editable = FALSE,
    container = f2, ellipsize = "none"
  )

  # FRAME 3 ###################################################################

  f3 <- gframe(
    text = "Image settings", horizontal = TRUE,
    spacing = 2, container = gv
  )

  glabel(text = "Width: ", container = f3, anchor = c(-1, 0))

  f3_width_edt <- gedit(
    text = "3000", width = 6, container = f3
  )

  glabel(text = " Height: ", container = f3, anchor = c(-1, 0))
  f3_height_edt <- gedit(
    text = "2000", width = 6, container = f3
  )

  glabel(text = " Resolution: ", container = f3, anchor = c(-1, 0))

  f3_res_edt <- gedit(
    text = "250", width = 6,
    container = f3
  )


  # FRAME 4 ###################################################################

  f4 <- gframe(text = "Location", horizontal = FALSE, spacing = 2, container = gv)

  glabel(text = "File path:", container = f4, anchor = c(-1, 0))

  # NB! text = getwd() does not always work (e.g. when 'Documents' are localized).
  # https://stackoverflow.com/questions/45231928/initiate-gfilebrowse-with-a-valid-path-gwidgets2
  # Unable to find a solution other then using the "Open" button to browse...
  f4_save_brw <- gfilebrowse(
    text = getwd(), quote = FALSE, type = "selectdir",
    initial.dir = getwd(), expand = TRUE, container = f4
  )

  # BUTTON ####################################################################

  g_export_btn <- gbutton(text = "Export", container = gv)

  # HANDLERS ##################################################################

  addHandlerSelectionChanged(f0_object_tbl, handler = function(h, ...) {

    # Get selected values.
    val <- svalue(f0_object_tbl)

    # Update current selection label.
    svalue(f0_selection_lbl) <- paste(
      length(val),
      " objects selected for export:"
    )
  })

  addHandlerChanged(f1_name_chk, handler = function(h, ...) {
    .updateGui()
  })

  addHandlerChanged(f2_ext_drp, handler = function(h, ...) {

    # Get values.
    val <- svalue(f2_ext_drp)

    if (val == ".RData") {
      enabled(f2_del_drp) <- FALSE
      enabled(f3g1) <- FALSE
    } else {
      enabled(f2_del_drp) <- TRUE
      enabled(f3g1) <- TRUE
    }
  })

  addHandlerChanged(g_export_btn, handler = function(h, ...) {

    # Get values.
    val_object <- svalue(f0_object_tbl)
    val_use_obj <- svalue(f1_name_chk)
    val_name <- svalue(f1_name_edt)
    val_replace <- svalue(f1_replace_chk)
    val_ext <- svalue(f2_ext_drp)
    val_del <- svalue(f2_del_drp, index = TRUE)
    val_w <- as.numeric(svalue(f3_width_edt))
    val_h <- as.numeric(svalue(f3_height_edt))
    val_r <- as.numeric(svalue(f3_res_edt))
    val_path <- svalue(f4_save_brw)

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
      blockHandlers(g_export_btn)
      svalue(g_export_btn) <- "Processing..."
      unblockHandlers(g_export_btn)

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
            title = "Export failed!", parent = w,
            do.buttons = FALSE,
            width = 200, height = 200, horizontal = FALSE
          )

          group <- ggroup(horizontal = FALSE, container = dialog)

          msgtxt <- paste(
            "\nThe following objects were not saved because the file names existed.\n",
            "Make sure to exit the last edited cell before continuing."
          )

          msg <- glabel(text = msgtxt, anchor = c(-1, 0), container = group)

          tbl <- gdf(items = fail, container = group, expand = TRUE)
          size(tbl) <- c(100, 200)

          gg <- ggroup(container = group)
          btn_cancel <- gbutton("Cancel", container = gg, handler = function(h, ...) {
            fail <<- NULL
            dispose(dialog)
          })

          btn_replace <- gbutton("Overwrite", container = gg, handler = function(h, ...) {
            val_replace <<- TRUE
            dispose(dialog)
          })

          btn_retry <- gbutton("Retry", container = gg, handler = function(h, ...) {
            val_object <<- tbl[, "Object"]
            val_name <<- tbl[, "New.Name"]
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
        msg = paste(
          "At least one object must be selected.",
          "\nFile name and path must be provided.",
          "\n\nThis error may also occur the first time the function is used.",
          "\nPlease locate the folder using the 'Open' button."
        ),
        title = "Error",
        parent = w,
        icon = "error"
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {

    # Get values.
    val <- svalue(f1_name_chk)

    if (val) {
      enabled(f1_name_edt) <- FALSE
    } else {
      enabled(f1_name_edt) <- TRUE
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
      if (exists(".strvalidator_export_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_export_gui_savegui", envir = env)
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
      if (exists(".strvalidator_export_gui_objName", envir = env, inherits = FALSE)) {
        svalue(f1_name_chk) <- get(".strvalidator_export_gui_objName", envir = env)
      }
      if (exists(".strvalidator_export_gui_replace", envir = env, inherits = FALSE)) {
        svalue(f1_replace_chk) <- get(".strvalidator_export_gui_replace", envir = env)
      }
      if (exists(".strvalidator_export_gui_ext", envir = env, inherits = FALSE)) {
        svalue(f2_ext_drp) <- get(".strvalidator_export_gui_ext", envir = env)
      }
      if (exists(".strvalidator_export_gui_del", envir = env, inherits = FALSE)) {
        svalue(f2_del_drp) <- get(".strvalidator_export_gui_del", envir = env)
      }
      if (exists(".strvalidator_export_gui_width", envir = env, inherits = FALSE)) {
        svalue(f3_width_edt) <- get(".strvalidator_export_gui_width", envir = env)
      }
      if (exists(".strvalidator_export_gui_height", envir = env, inherits = FALSE)) {
        svalue(f3_height_edt) <- get(".strvalidator_export_gui_height", envir = env)
      }
      if (exists(".strvalidator_export_gui_res", envir = env, inherits = FALSE)) {
        svalue(f3_res_edt) <- get(".strvalidator_export_gui_res", envir = env)
      }
      if (exists(".strvalidator_export_gui_path", envir = env, inherits = FALSE)) {
        svalue(f4_save_brw) <- get(".strvalidator_export_gui_path", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_export_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_export_gui_objName", value = svalue(f1_name_chk), envir = env)
      assign(x = ".strvalidator_export_gui_replace", value = svalue(f1_replace_chk), envir = env)
      assign(x = ".strvalidator_export_gui_ext", value = svalue(f2_ext_drp), envir = env)
      assign(x = ".strvalidator_export_gui_del", value = svalue(f2_del_drp), envir = env)
      assign(x = ".strvalidator_export_gui_width", value = svalue(f3_width_edt), envir = env)
      assign(x = ".strvalidator_export_gui_height", value = svalue(f3_height_edt), envir = env)
      assign(x = ".strvalidator_export_gui_res", value = svalue(f3_res_edt), envir = env)
      assign(x = ".strvalidator_export_gui_path", value = svalue(f4_save_brw), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_export_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_export_gui_objName", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_objName", envir = env)
      }
      if (exists(".strvalidator_export_gui_replace", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_replace", envir = env)
      }
      if (exists(".strvalidator_export_gui_ext", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_ext", envir = env)
      }
      if (exists(".strvalidator_export_gui_del", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_del", envir = env)
      }
      if (exists(".strvalidator_export_gui_width", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_width", envir = env)
      }
      if (exists(".strvalidator_export_gui_height", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_height", envir = env)
      }
      if (exists(".strvalidator_export_gui_res", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_res", envir = env)
      }
      if (exists(".strvalidator_export_gui_path", envir = env, inherits = FALSE)) {
        remove(".strvalidator_export_gui_path", envir = env)
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
