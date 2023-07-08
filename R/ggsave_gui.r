################################################################################
# CHANGE LOG (last 20 changes)
# 12.09.2022: Reworked to avoid 'dispose' error https://github.com/jverzani/gWidgets2/issues/103
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 15.03.2020: Added language support.
# 15.02.2019: Minor adjustments to tcltk gui. Expand gedit.
# 11.02.2019: Minor adjustments to tcltk gui.
# 11.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 25.07.2018: Added instruction in error message.
# 25.07.2018: Fixed: Error in if (nchar(val_path) == 0) { : argument is of length zero
# 17.07.2017: Minor GUI improvements.
# 17.07.2017: Added store path, and block/unblock when button text is changed.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 16.06.2016: File name/path textbox made expandable.
# 05.01.2016: Fixed 'dev' not find error in ggplot2 2.0.
# 29.08.2015: Added importFrom.
# 11.10.2014: Added 'focus'.
# 06.10.2014: Correct pixel dimensions are now shown.
# 28.06.2014: Added help button and moved save gui checkbox.
# 25.02.2014: Pixel info now update when textbox is changed.
# 09.02.2014: Added info for size in pixel.
# 09.02.2014: Removed unsupported unit 'px'.

#' @title Save Image
#'
#' @description
#' A simple GUI wrapper for \code{\link{ggsave}}.
#'
#' @details
#' Simple GUI wrapper for ggsave.
#'
#' @param ggplot plot object.
#' @param name optional string providing a file name.
#' @param env environment where the objects exist.
#' Default is the current environment.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent object specifying the parent widget to center the message box,
#' and to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom ggplot2 ggsave last_plot
#' @importFrom utils help
#' @importFrom grDevices dev.cur dev.list dev.size
#'
#' @seealso \code{\link{ggsave}}

ggsave_gui <- function(ggplot = NULL, name = "", env = parent.frame(),
                       savegui = NULL, debug = FALSE, parent = NULL) {
  # Constants.
  .separator <- .Platform$file.sep # Platform dependent path separator.

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Save as image"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmOptions <- "Options"
  strLblFileName <- "File name and extension:"
  strChkOverwrite <- "Overwrite existing file"
  strChkSize <- "Load size from plot device"
  strBtnSize <- "Get size from plot device"
  strLblSettings <- "Image settings:"
  strLblUnit <- "Unit:"
  strLblWidth <- "Width:"
  strLblPixels <- "pixels"
  strLblHeight <- "Height:"
  strLblRes <- "Resolution:"
  strLblScale <- "Scaling factor"
  strLblPath <- "Save file to path:"
  strBtnSave <- "Save"
  strBtnProcessing <- "Processing..."
  strMsgPath <- "File path must be provided!\n\nThis error may also occur the first time the function is used.\nPlease locate the folder using the [file] button."
  strMsgName <- "File name must be provided!"
  strMsgTitleError <- "Error"
  strMsgTitleSaveError <- "Save error"
  strMsgFileExist <- "The file already exist!\n\nClick [Yes] to overwrite or [No] to cancel and type a new name."

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

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblFileName"]$value
    strLblFileName <- ifelse(is.na(strtmp), strLblFileName, strtmp)

    strtmp <- dtStrings["strChkOverwrite"]$value
    strChkOverwrite <- ifelse(is.na(strtmp), strChkOverwrite, strtmp)

    strtmp <- dtStrings["strChkSize"]$value
    strChkSize <- ifelse(is.na(strtmp), strChkSize, strtmp)

    strtmp <- dtStrings["strBtnSize"]$value
    strBtnSize <- ifelse(is.na(strtmp), strBtnSize, strtmp)

    strtmp <- dtStrings["strLblSettings"]$value
    strLblSettings <- ifelse(is.na(strtmp), strLblSettings, strtmp)

    strtmp <- dtStrings["strLblUnit"]$value
    strLblUnit <- ifelse(is.na(strtmp), strLblUnit, strtmp)

    strtmp <- dtStrings["strLblWidth"]$value
    strLblWidth <- ifelse(is.na(strtmp), strLblWidth, strtmp)

    strtmp <- dtStrings["strLblPixels"]$value
    strLblPixels <- ifelse(is.na(strtmp), strLblPixels, strtmp)

    strtmp <- dtStrings["strLblHeight"]$value
    strLblHeight <- ifelse(is.na(strtmp), strLblHeight, strtmp)

    strtmp <- dtStrings["strLblRes"]$value
    strLblRes <- ifelse(is.na(strtmp), strLblRes, strtmp)

    strtmp <- dtStrings["strLblScale"]$value
    strLblScale <- ifelse(is.na(strtmp), strLblScale, strtmp)

    strtmp <- dtStrings["strLblPath"]$value
    strLblPath <- ifelse(is.na(strtmp), strLblPath, strtmp)

    strtmp <- dtStrings["strBtnSave"]$value
    strBtnSave <- ifelse(is.na(strtmp), strBtnSave, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strMsgPath"]$value
    strMsgPath <- ifelse(is.na(strtmp), strMsgPath, strtmp)

    strtmp <- dtStrings["strMsgName"]$value
    strMsgName <- ifelse(is.na(strtmp), strMsgName, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strMsgTitleSaveError"]$value
    strMsgTitleSaveError <- ifelse(is.na(strtmp), strMsgTitleSaveError, strtmp)

    strtmp <- dtStrings["strMsgFileExist"]$value
    strMsgFileExist <- ifelse(is.na(strtmp), strMsgFileExist, strtmp)
  }

  # WINDOW ####################################################################

  if (debug) {
    print(paste("IN:", fnc))
    print("Current device")
    print(dev.cur())
    print("Device list")
    print(dev.list())
  }

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

  savegui_chk <- gcheckbox(text = strChkGui, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  glabel(
    text = strLblFileName, container = f1, anchor = c(-1, 0),
    expand = TRUE
  )

  f1g1 <- ggroup(horizontal = TRUE, spacing = 1, container = f1)

  f1g1_name_edt <- gedit(text = name, container = f1g1, expand = TRUE, fill = TRUE)

  f1g1_ext_drp <- gcombobox(
    items = c(
      "eps", "ps", "tex", "pdf", "jpeg", "tiff",
      "png", "bmp", "svg", "wmf"
    ),
    selected = 4, container = f1g1, ellipsize = "none"
  )

  f1_replace_chk <- gcheckbox(
    text = strChkOverwrite, checked = TRUE,
    container = f1
  )

  f1_load_chk <- gcheckbox(
    text = strChkSize, checked = TRUE,
    container = f1
  )

  f1_get_btn <- gbutton(text = strBtnSize, container = f1)

  addHandlerChanged(f1_load_chk, handler = function(h, ...) {
    val <- svalue(f1_load_chk)

    if (val) {
      # Read size from device.
      .readSize()
    } else {
      # Could load saved settings...
    }
  })

  addHandlerChanged(f1_get_btn, handler = function(h, ...) {
    # Read size from device.
    .readSize()
  })

  # GRID 2 --------------------------------------------------------------------

  f1g2 <- glayout(container = f1, spacing = 1)

  f1g2[1, 1] <- glabel(
    text = strLblSettings,
    container = f1g2, anchor = c(-1, 0)
  )

  f1g2[2, 1] <- glabel(
    text = strLblUnit,
    container = f1g2,
    anchor = c(-1, 0)
  )

  f1g2[2, 2] <- f1g2_unit_drp <- gcombobox(
    items = c("in", "cm"),
    selected = 2,
    container = f1g2,
    ellipsize = "none"
  )

  addHandlerChanged(f1g2_unit_drp, handler = function(h, ...) {
    # Read size from device.
    .readSize()
  })

  f1g2[3, 1] <- glabel(text = strLblWidth, container = f1g2, anchor = c(-1, 0))

  f1g2[3, 2] <- f1g2_width_edt <- gedit(
    text = "",
    width = 6,
    initial.msg = "",
    container = f1g2
  )

  f1g2[3, 3] <- f1g2_width_lbl <- glabel(
    text = paste(" NA", strLblPixels),
    container = f1g2, anchor = c(-1, 0)
  )

  f1g2[4, 1] <- glabel(text = strLblHeight, container = f1g2, anchor = c(-1, 0))

  f1g2[4, 2] <- f1g2_height_edt <- gedit(
    text = "",
    width = 6,
    initial.msg = "",
    container = f1g2
  )

  f1g2[4, 3] <- f1g2_height_lbl <- glabel(
    text = paste(" NA", strLblPixels),
    container = f1g2, anchor = c(-1, 0)
  )

  f1g2[5, 1] <- glabel(text = strLblRes, container = f1g2, anchor = c(-1, 0))

  f1g2[5, 2] <- f1g2_res_edt <- gedit(
    text = "300",
    width = 4,
    initial.msg = "",
    container = f1g2
  )

  f1g2[6, 1] <- glabel(text = strLblScale, container = f1g2, anchor = c(-1, 0))

  f1g2[6, 2] <- f1g2_scale_edt <- gedit(
    text = "1",
    width = 4,
    initial.msg = "",
    container = f1g2
  )

  addHandlerKeystroke(f1g2_width_edt, handler = function(h, ...) {
    # Get values.
    val_w <- as.numeric(svalue(f1g2_width_edt))
    val_u <- svalue(f1g2_unit_drp)
    val_d <- as.numeric(svalue(f1g2_res_edt))
    val_s <- as.numeric(svalue(f1g2_scale_edt))

    # Convert to pixel.
    pixels <- .toPixel(unit = val_u, val = val_w, dpi = val_d, scale = val_s)

    # Update label.
    svalue(f1g2_width_lbl) <- paste(" ", pixels, strLblPixels)
  })

  addHandlerKeystroke(f1g2_height_edt, handler = function(h, ...) {
    # Get values.
    val_h <- as.numeric(svalue(f1g2_height_edt))
    val_u <- svalue(f1g2_unit_drp)
    val_d <- as.numeric(svalue(f1g2_res_edt))
    val_s <- as.numeric(svalue(f1g2_scale_edt))

    # Convert to pixel.
    pixels <- .toPixel(unit = val_u, val = val_h, dpi = val_d, scale = val_s)

    svalue(f1g2_height_lbl) <- paste(" ", pixels, strLblPixels)
  })

  addHandlerChanged(f1g2_width_edt, handler = function(h, ...) {
    # Get values.
    val_w <- as.numeric(svalue(f1g2_width_edt))
    val_u <- svalue(f1g2_unit_drp)
    val_d <- as.numeric(svalue(f1g2_res_edt))
    val_s <- as.numeric(svalue(f1g2_scale_edt))

    # Convert to pixel.
    pixels <- .toPixel(unit = val_u, val = val_w, dpi = val_d, scale = val_s)

    # Update label.
    svalue(f1g2_width_lbl) <- paste(" ", pixels, strLblPixels)
  })

  addHandlerChanged(f1g2_height_edt, handler = function(h, ...) {
    val_h <- as.numeric(svalue(f1g2_height_edt))
    val_u <- svalue(f1g2_unit_drp)
    val_d <- as.numeric(svalue(f1g2_res_edt))
    val_s <- as.numeric(svalue(f1g2_scale_edt))

    # Convert to pixel.
    pixels <- .toPixel(unit = val_u, val = val_h, dpi = val_d, scale = val_s)

    svalue(f1g2_height_lbl) <- paste(" ", pixels, strLblPixels)
  })

  addHandlerKeystroke(f1g2_res_edt, handler = function(h, ...) {
    # Get values.
    val_w <- as.numeric(svalue(f1g2_width_edt))
    val_h <- as.numeric(svalue(f1g2_height_edt))
    val_u <- svalue(f1g2_unit_drp)
    val_d <- as.numeric(svalue(f1g2_res_edt))
    val_s <- as.numeric(svalue(f1g2_scale_edt))

    # Convert to pixel.
    pixels_w <- .toPixel(unit = val_u, val = val_w, dpi = val_d, scale = val_s)
    pixels_h <- .toPixel(unit = val_u, val = val_h, dpi = val_d, scale = val_s)

    # Update label.
    svalue(f1g2_width_lbl) <- paste(" ", pixels_w, strLblPixels)
    svalue(f1g2_height_lbl) <- paste(" ", pixels_h, strLblPixels)
  })

  addHandlerChanged(f1g2_res_edt, handler = function(h, ...) {
    # Get values.
    val_w <- as.numeric(svalue(f1g2_width_edt))
    val_h <- as.numeric(svalue(f1g2_height_edt))
    val_u <- svalue(f1g2_unit_drp)
    val_d <- as.numeric(svalue(f1g2_res_edt))
    val_s <- as.numeric(svalue(f1g2_scale_edt))

    # Convert to pixel.
    pixels_w <- .toPixel(unit = val_u, val = val_w, dpi = val_d, scale = val_s)
    pixels_h <- .toPixel(unit = val_u, val = val_h, dpi = val_d, scale = val_s)

    # Update label.
    svalue(f1g2_width_lbl) <- paste(" ", pixels_w, strLblPixels)
    svalue(f1g2_height_lbl) <- paste(" ", pixels_h, strLblPixels)
  })

  addHandlerKeystroke(f1g2_scale_edt, handler = function(h, ...) {
    # Get values.
    val_w <- as.numeric(svalue(f1g2_width_edt))
    val_h <- as.numeric(svalue(f1g2_height_edt))
    val_u <- svalue(f1g2_unit_drp)
    val_d <- as.numeric(svalue(f1g2_res_edt))
    val_s <- as.numeric(svalue(f1g2_scale_edt))

    # Convert to pixel.
    pixels_w <- .toPixel(unit = val_u, val = val_w, dpi = val_d, scale = val_s)
    pixels_h <- .toPixel(unit = val_u, val = val_h, dpi = val_d, scale = val_s)

    # Update label.
    svalue(f1g2_width_lbl) <- paste(" ", pixels_w, strLblPixels)
    svalue(f1g2_height_lbl) <- paste(" ", pixels_h, strLblPixels)
  })

  addHandlerChanged(f1g2_scale_edt, handler = function(h, ...) {
    # Get values.
    val_w <- as.numeric(svalue(f1g2_width_edt))
    val_h <- as.numeric(svalue(f1g2_height_edt))
    val_u <- svalue(f1g2_unit_drp)
    val_d <- as.numeric(svalue(f1g2_res_edt))
    val_s <- as.numeric(svalue(f1g2_scale_edt))

    # Convert to pixel.
    pixels_w <- .toPixel(unit = val_u, val = val_w, dpi = val_d, scale = val_s)
    pixels_h <- .toPixel(unit = val_u, val = val_h, dpi = val_d, scale = val_s)

    # Update label.
    svalue(f1g2_width_lbl) <- paste(" ", pixels_w, strLblPixels)
    svalue(f1g2_height_lbl) <- paste(" ", pixels_h, strLblPixels)
  })

  # GRID 3 --------------------------------------------------------------------

  # f1g3 <- glayout(container = f1, spacing = 5)
  #
  # f1g3[1,1] <- glabel(text = "File path:", container = f1g3,
  #                     anchor = c(-1 ,0), expand = TRUE)
  #
  # f1g3[2,1:2] <- f1g3_save_brw <- gfilebrowse(text=getwd(),
  #                                             quote=FALSE,
  #                                             type="selectdir",
  #                                             container=f1g3,
  #                                             expand=TRUE,
  #                                             initial.dir=getwd())

  glabel(text = strLblPath, container = f1, anchor = c(-1, 0), expand = TRUE)

  f1g3_save_brw <- gfilebrowse(
    text = getwd(), quote = FALSE, type = "selectdir",
    container = f1, expand = TRUE, initial.dir = getwd()
  )

  # BUTTON ####################################################################

  g_save_btn <- gbutton(text = strBtnSave, container = gv)

  # HANDLERS ##################################################################



  addHandlerChanged(g_save_btn, handler = function(h, ...) {
    # Change button label.
    blockHandlers(g_save_btn)
    svalue(g_save_btn) <- strBtnProcessing
    unblockHandlers(g_save_btn)

    # Get values.
    val_name <- svalue(f1g1_name_edt)
    val_ggplot <- ggplot
    val_ext <- paste(".", svalue(f1g1_ext_drp), sep = "")
    val_scale <- as.numeric(svalue(f1g2_scale_edt))
    val_unit <- svalue(f1g2_unit_drp)
    val_replace <- svalue(f1_replace_chk)
    val_w <- as.numeric(svalue(f1g2_width_edt))
    val_h <- as.numeric(svalue(f1g2_height_edt))
    val_r <- as.numeric(svalue(f1g2_res_edt))
    val_path <- svalue(f1g3_save_brw)

    if (debug) {
      print("val_name")
      print(val_name)
      print("val_ext")
      print(val_ext)
      print("val_replace")
      print(val_replace)
      print("val_w")
      print(val_w)
      print("val_h")
      print(val_h)
      print("val_r")
      print(val_r)
      print("val_path")
      print(val_path)
    }

    # If no plot is provided, default to last plot.
    if (is.null(val_ggplot)) {
      val_ggplot <- last_plot()
      message("No plot provided. Defaults to last plot.")
      str(val_ggplot)
    }

    # Initiate logical.
    okPathAndName <- FALSE

    if (is.na(val_path) | nchar(val_path) == 0) {
      # Check if path is provided.

      message("val_path=NA or length(val_path)=0")

      gmessage(
        msg = strMsgPath,
        title = strMsgTitleError,
        parent = w,
        icon = "error"
      )
    } else if (is.na(val_name) | nchar(val_name) == 0) {
      # Check if file name is provided.

      message("val_name=NA or length(val_name)=0")

      gmessage(
        msg = strMsgName,
        title = strMsgTitleError,
        parent = w,
        icon = "error"
      )
    } else {
      okPathAndName <- TRUE
    }

    if (okPathAndName) {
      # Add trailing path separator if not present.
      if (substr(val_path, nchar(val_path), nchar(val_path) + 1) != .separator) {
        val_path <- paste(val_path, .separator, sep = "")
      }

      # Create filename (NB! used further below).
      val_filename <- paste(val_name, val_ext, sep = "")
      # Construct complete file name.
      fullFileName <- paste(val_path, val_filename, sep = "")

      # Initiate logical.
      okToSave <- FALSE

      if (val_replace) {
        # Ok to overwrite.
        okToSave <- TRUE

        if (debug) {
          print("Replace=TRUE. Ok to save!")
        }
      } else {
        # Not ok to overwrite.

        message("Replace=FALSE. Check if file exist!")

        # Check if file exist.
        if (file.exists(fullFileName)) {
          message(paste("file '", val_name, "' already exist!", sep = ""))

          # Create modal confirm dialog (YES/NO).
          okToSave <- gconfirm(
            title = strMsgTitleSaveError, msg = strMsgFileExist, parent = w
          )

          message("Overwrite? ", okToSave)
        } else {
          # File does not exist.
          okToSave <- TRUE
        }
      }

      if (okToSave) {
        # Save plot device as image.
        ggsave(
          filename = val_filename,
          plot = val_ggplot,
          path = val_path,
          scale = val_scale,
          width = val_w, height = val_h,
          units = val_unit, dpi = val_r
        )

        # Confirm to console.
        message("Image saved: ", val_path, val_filename)

        # Close GUI.
        .saveSettings()
        dispose(w)
      } else {
        # Confirm to console.
        message("Image not saved!")
      }
    }

    # Change button label.
    blockHandlers(g_save_btn)
    svalue(g_save_btn) <- strBtnSave
    unblockHandlers(g_save_btn)
  })

  # INTERNAL FUNCTIONS ########################################################

  .toPixel <- function(unit, val, dpi = 72, scale = 1) {
    # Convert to pixel.
    if (unit == "cm") {
      pixels <- (val / (2.54 / dpi)) * scale
    } else if (unit == "in") {
      pixels <- val * dpi * scale
    } else {
      pixels <- NA
    }

    return(round(pixels, 0))
  }

  .readSize <- function() {
    # Get values.
    val_unit <- svalue(f1g2_unit_drp)
    val_size <- round(dev.size(val_unit), 2)
    val_px <- dev.size("px")

    # Update.
    svalue(f1g2_width_edt) <- val_size[1]
    svalue(f1g2_height_edt) <- val_size[2]

    #     svalue(f1g2_width_lbl) <- paste(" ", val_px[1],"pixels")
    #     svalue(f1g2_height_lbl) <- paste(" ", val_px[2],"pixels")
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
      if (exists(".strvalidator_ggsave_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_ggsave_gui_savegui", envir = env)
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
      if (exists(".strvalidator_ggsave_gui_ext", envir = env, inherits = FALSE)) {
        svalue(f1g1_ext_drp) <- get(".strvalidator_ggsave_gui_ext", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_replace", envir = env, inherits = FALSE)) {
        svalue(f1_replace_chk) <- get(".strvalidator_ggsave_gui_replace", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_load", envir = env, inherits = FALSE)) {
        svalue(f1_load_chk) <- get(".strvalidator_ggsave_gui_load", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_unit", envir = env, inherits = FALSE)) {
        svalue(f1g2_unit_drp) <- get(".strvalidator_ggsave_gui_unit", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_width", envir = env, inherits = FALSE)) {
        svalue(f1g2_width_edt) <- get(".strvalidator_ggsave_gui_width", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_height", envir = env, inherits = FALSE)) {
        svalue(f1g2_height_edt) <- get(".strvalidator_ggsave_gui_height", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_res", envir = env, inherits = FALSE)) {
        svalue(f1g2_res_edt) <- get(".strvalidator_ggsave_gui_res", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_scale", envir = env, inherits = FALSE)) {
        svalue(f1g2_scale_edt) <- get(".strvalidator_ggsave_gui_scale", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_path", envir = env, inherits = FALSE)) {
        svalue(f1g3_save_brw) <- get(".strvalidator_ggsave_gui_path", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_ggsave_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_ggsave_gui_ext", value = svalue(f1g1_ext_drp), envir = env)
      assign(x = ".strvalidator_ggsave_gui_replace", value = svalue(f1_replace_chk), envir = env)
      assign(x = ".strvalidator_ggsave_gui_load", value = svalue(f1_load_chk), envir = env)
      assign(x = ".strvalidator_ggsave_gui_unit", value = svalue(f1g2_unit_drp), envir = env)
      assign(x = ".strvalidator_ggsave_gui_width", value = svalue(f1g2_width_edt), envir = env)
      assign(x = ".strvalidator_ggsave_gui_height", value = svalue(f1g2_height_edt), envir = env)
      assign(x = ".strvalidator_ggsave_gui_res", value = svalue(f1g2_res_edt), envir = env)
      assign(x = ".strvalidator_ggsave_gui_scale", value = svalue(f1g2_scale_edt), envir = env)
      assign(x = ".strvalidator_ggsave_gui_path", value = svalue(f1g3_save_brw), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_ggsave_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_ext", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_ext", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_replace", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_replace", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_load", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_load", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_unit", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_unit", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_width", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_width", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_height", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_height", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_res", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_res", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_scale", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_scale", envir = env)
      }
      if (exists(".strvalidator_ggsave_gui_path", envir = env, inherits = FALSE)) {
        remove(".strvalidator_ggsave_gui_path", envir = env)
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

  # Read size.
  if (svalue(f1_load_chk)) {
    .readSize()
  }

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
