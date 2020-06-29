################################################################################
# CHANGE LOG (last 20 changes)
# 28.06.2020: Expanded path field under tcltk.
# 14.03.2020: Added language support.
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

  # Default strings.
  strWinTitle <- "Export objects as files or images"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmObjects <- "Objects"
  strLblObjects <- "objects selected for export:"
  strLblValues <- "Values"
  strFrmOptions <- "Options"
  strLblFiles <- "Files"
  strChkNames <- "Use object names as file names"
  strLblNames <- "File names (separated by | or ,):"
  strChkOverwrite <- "Overwrite existing files"
  strLblExtension <- "File extension:"
  strLblDelimiter <- "Delimiter:"
  strDrpTab <- "TAB"
  strDrpSpace <- "SPACE"
  strDrpComma <- "COMMA"
  strLblSettings <- "Image settings:"
  strLblWidth <- "Width:"
  strLblHeight <- "Height:"
  strLblResolution <- "Resolution:"
  strFrmLocation <- "Location"
  strLblLocation <- "Save files to path:"
  strBtnOpen <- "Open"
  strBtnExport <- "Export"
  strBtnProcessing <- "Processing..."
  strMsgObject <- "At least one object must be selected.\nFile name and path must be provided.\n\nThis error may also occur the first time the function is used.\nPlease locate the folder using the 'Open' button."
  strMsgTitleError <- "Error"
  strMsgFailed <- "\nThe following objects were not saved because the file names existed.\nMake sure to exit the last edited cell before continuing."
  strMsgTitleFailed <- "Export failed!"
  strLblObject <- "Object"
  strLblNewName <- "New.Name"
  strBtnCancel <- "Cancel"
  strBtnOverwrite <- "Overwrite"
  strBtnRetry <- "Retry"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.null(dtStrings)) {
    # Get language strings, use default if not found.

    strtmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)

    strtmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)

    strtmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strtmp <- dtStrings["strFrmObjects"]$value
    strFrmObjects <- ifelse(is.na(strtmp), strFrmObjects, strtmp)

    strtmp <- dtStrings["strLblObjects"]$value
    strLblObjects <- ifelse(is.na(strtmp), strLblObjects, strtmp)

    strtmp <- dtStrings["strLblValues"]$value
    strLblValues <- ifelse(is.na(strtmp), strLblValues, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblFiles"]$value
    strLblFiles <- ifelse(is.na(strtmp), strLblFiles, strtmp)

    strtmp <- dtStrings["strChkNames"]$value
    strChkNames <- ifelse(is.na(strtmp), strChkNames, strtmp)

    strtmp <- dtStrings["strLblNames"]$value
    strLblNames <- ifelse(is.na(strtmp), strLblNames, strtmp)

    strtmp <- dtStrings["strChkOverwrite"]$value
    strChkOverwrite <- ifelse(is.na(strtmp), strChkOverwrite, strtmp)

    strtmp <- dtStrings["strLblExtension"]$value
    strLblExtension <- ifelse(is.na(strtmp), strLblExtension, strtmp)

    strtmp <- dtStrings["strLblDelimiter"]$value
    strLblDelimiter <- ifelse(is.na(strtmp), strLblDelimiter, strtmp)

    strtmp <- dtStrings["strDrpTab"]$value
    strDrpTab <- ifelse(is.na(strtmp), strDrpTab, strtmp)

    strtmp <- dtStrings["strDrpComma"]$value
    strDrpComma <- ifelse(is.na(strtmp), strDrpComma, strtmp)

    strtmp <- dtStrings["strDrpSpace"]$value
    strDrpSpace <- ifelse(is.na(strtmp), strDrpSpace, strtmp)

    strtmp <- dtStrings["strLblSettings"]$value
    strLblSettings <- ifelse(is.na(strtmp), strLblSettings, strtmp)

    strtmp <- dtStrings["strLblWidth"]$value
    strLblWidth <- ifelse(is.na(strtmp), strLblWidth, strtmp)

    strtmp <- dtStrings["strLblHeight"]$value
    strLblHeight <- ifelse(is.na(strtmp), strLblHeight, strtmp)

    strtmp <- dtStrings["strLblResolution"]$value
    strLblResolution <- ifelse(is.na(strtmp), strLblResolution, strtmp)

    strtmp <- dtStrings["strFrmLocation"]$value
    strFrmLocation <- ifelse(is.na(strtmp), strFrmLocation, strtmp)

    strtmp <- dtStrings["strLblLocation"]$value
    strLblLocation <- ifelse(is.na(strtmp), strLblLocation, strtmp)

    strtmp <- dtStrings["strBtnOpen"]$value
    strBtnOpen <- ifelse(is.na(strtmp), strBtnOpen, strtmp)

    strtmp <- dtStrings["strBtnExport"]$value
    strBtnExport <- ifelse(is.na(strtmp), strBtnExport, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strMsgObject"]$value
    strMsgObject <- ifelse(is.na(strtmp), strMsgObject, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strMsgTitleFailed"]$value
    strMsgTitleFailed <- ifelse(is.na(strtmp), strMsgTitleFailed, strtmp)

    strtmp <- dtStrings["strLblObject"]$value
    strLblObject <- ifelse(is.na(strtmp), strLblObject, strtmp)

    strtmp <- dtStrings["strLblNewName"]$value
    strLblNewName <- ifelse(is.na(strtmp), strLblNewName, strtmp)

    strtmp <- dtStrings["strBtnCancel"]$value
    strBtnCancel <- ifelse(is.na(strtmp), strBtnCancel, strtmp)

    strtmp <- dtStrings["strBtnOverwrite"]$value
    strBtnOverwrite <- ifelse(is.na(strtmp), strBtnOverwrite, strtmp)

    strtmp <- dtStrings["strBtnRetry"]$value
    strBtnRetry <- ifelse(is.na(strtmp), strBtnRetry, strtmp)
  }

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(
    title = strWinTitle,
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

  savegui_chk <- gcheckbox(text = strChkGui, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # OBJECTS ###################################################################

  object_frm <- gframe(
    text = strFrmObjects, horizontal = FALSE, spacing = 2, expand = TRUE,
    fill = TRUE, container = gv
  )

  # Active selection label.
  selection_lbl <- glabel(
    text = paste(length(obj), strLblObjects),
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
    text = strFrmOptions, horizontal = FALSE, spacing = 2,
    container = gv
  )

  name_chk <- gcheckbox(
    text = strChkNames,
    checked = TRUE, container = option_frm
  )

  glabel(text = strLblNames, container = option_frm, anchor = c(-1, 0))

  name_edt <- gedit(container = option_frm, expand = TRUE, fill = TRUE)

  # Defult is disabled.
  enabled(name_edt) <- FALSE

  replace_chk <- gcheckbox(
    text = strChkOverwrite,
    checked = TRUE, container = option_frm
  )

  # FILE ----------------------------------------------------------------------

  glabel(text = "", container = option_frm) # Spacer.

  file_grp <- ggroup(spacing = 2, container = option_frm)

  glabel(text = strLblExtension, container = file_grp, anchor = c(-1, 0))

  ext_drp <- gcombobox(
    items = c("auto", ".RData"),
    selected = 1, editable = FALSE,
    container = file_grp, ellipsize = "none"
  )

  glabel(text = strLblDelimiter, container = file_grp, anchor = c(-1, 0))

  del_drp <- gcombobox(
    items = c(strDrpTab, strDrpSpace, strDrpComma),
    selected = 1, editable = FALSE,
    container = file_grp, ellipsize = "none"
  )

  # IMAGE ---------------------------------------------------------------------

  glabel(text = "", container = option_frm) # Spacer.

  glabel(text = strLblSettings, container = option_frm, anchor = c(-1, 0))

  image_grp <- ggroup(spacing = 2, container = option_frm)

  glabel(text = strLblWidth, container = image_grp, anchor = c(-1, 0))
  width_edt <- gedit(text = "3000", width = 6, container = image_grp)

  glabel(text = strLblHeight, container = image_grp, anchor = c(-1, 0))
  height_edt <- gedit(text = "2000", width = 6, container = image_grp)

  glabel(text = strLblResolution, container = image_grp, anchor = c(-1, 0))
  res_edt <- gedit(text = "250", width = 6, container = image_grp)

  # PATH ----------------------------------------------------------------------

  glabel(text = "", container = option_frm) # Spacer.

  path_grp <- gframe(text = strFrmLocation, horizontal = FALSE, spacing = 2, container = gv)

  glabel(text = strLblLocation, container = path_grp, anchor = c(-1, 0))

  # NB! text = getwd() does not always work (e.g. when 'Documents' are localized).
  # https://stackoverflow.com/questions/45231928/initiate-gfilebrowse-with-a-valid-path-gwidgets2
  # Unable to find a solution other then using the "Open" button to browse...
  save_brw <- gfilebrowse(
    text = getwd(), quote = FALSE, type = "selectdir",
    initial.dir = getwd(), expand = TRUE, container = path_grp
  )

  # BUTTON ####################################################################

  export_btn <- gbutton(text = strBtnExport, container = gv)

  # HANDLERS ##################################################################

  addHandlerSelectionChanged(object_tbl, handler = function(h, ...) {

    # Get selected values.
    val <- svalue(object_tbl)

    # Update current selection label.
    svalue(selection_lbl) <- paste(length(val), strLblObjects)
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
      svalue(export_btn) <- strBtnProcessing
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
            title = strMsgTitleFailed, parent = w,
            do.buttons = FALSE,
            width = 200, height = 200, horizontal = FALSE
          )

          group <- ggroup(horizontal = FALSE, container = dialog)

          msg <- glabel(text = strMsgFailed, anchor = c(-1, 0), container = group)

          tbl <- gdf(items = fail, container = group, expand = TRUE)
          size(tbl) <- c(100, 200)

          gg <- ggroup(container = group)
          btn_cancel <- gbutton(strBtnCancel, container = gg, handler = function(h, ...) {
            fail <<- NULL
            dispose(dialog)
          })

          btn_replace <- gbutton(strBtnOverwrite, container = gg, handler = function(h, ...) {
            val_replace <<- TRUE
            dispose(dialog)
          })

          btn_retry <- gbutton(strBtnRetry, container = gg, handler = function(h, ...) {
            val_object <<- tbl[, strLblObject]
            val_name <<- tbl[, strLblNewName]
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
        msg = strMsgObject,
        title = strMsgTitleError,
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
        svalue(name_chk) <- get(".strvalidator_export_gui_objName", envir = env)
      }
      if (exists(".strvalidator_export_gui_replace", envir = env, inherits = FALSE)) {
        svalue(replace_chk) <- get(".strvalidator_export_gui_replace", envir = env)
      }
      if (exists(".strvalidator_export_gui_ext", envir = env, inherits = FALSE)) {
        svalue(ext_drp) <- get(".strvalidator_export_gui_ext", envir = env)
      }
      if (exists(".strvalidator_export_gui_del", envir = env, inherits = FALSE)) {
        svalue(del_drp) <- get(".strvalidator_export_gui_del", envir = env)
      }
      if (exists(".strvalidator_export_gui_width", envir = env, inherits = FALSE)) {
        svalue(width_edt) <- get(".strvalidator_export_gui_width", envir = env)
      }
      if (exists(".strvalidator_export_gui_height", envir = env, inherits = FALSE)) {
        svalue(height_edt) <- get(".strvalidator_export_gui_height", envir = env)
      }
      if (exists(".strvalidator_export_gui_res", envir = env, inherits = FALSE)) {
        svalue(res_edt) <- get(".strvalidator_export_gui_res", envir = env)
      }
      if (exists(".strvalidator_export_gui_path", envir = env, inherits = FALSE)) {
        svalue(save_brw) <- get(".strvalidator_export_gui_path", envir = env)
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
      assign(x = ".strvalidator_export_gui_objName", value = svalue(name_chk), envir = env)
      assign(x = ".strvalidator_export_gui_replace", value = svalue(replace_chk), envir = env)
      assign(x = ".strvalidator_export_gui_ext", value = svalue(ext_drp), envir = env)
      assign(x = ".strvalidator_export_gui_del", value = svalue(del_drp), envir = env)
      assign(x = ".strvalidator_export_gui_width", value = svalue(width_edt), envir = env)
      assign(x = ".strvalidator_export_gui_height", value = svalue(height_edt), envir = env)
      assign(x = ".strvalidator_export_gui_res", value = svalue(res_edt), envir = env)
      assign(x = ".strvalidator_export_gui_path", value = svalue(save_brw), envir = env)
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
