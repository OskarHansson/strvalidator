################################################################################
# CHANGE LOG (last 20 changes)
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
#' @importFrom ggplot2 ggsave
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
  strMsgSave <- "Plot object, file name and path must be provided.\n\nThis error may also occur the first time the function is used.\nPlease locate the folder using the 'Open' button."
  strMsgTitleError <- "Error"
  strMsgTitleSaveError <- "Save error"
  strMsgFileExist <- "The file already exist!/n/nChose to cancel, overwrite or give a new name."
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

    strtmp <- dtStrings["strMsgSave"]$value
    strMsgSave <- ifelse(is.na(strtmp), strMsgSave, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strMsgTitleSaveError"]$value
    strMsgTitleSaveError <- ifelse(is.na(strtmp), strMsgTitleSaveError, strtmp)

    strtmp <- dtStrings["strMsgFileExist"]$value
    strMsgFileExist <- ifelse(is.na(strtmp), strMsgFileExist, strtmp)

    strtmp <- dtStrings["strBtnCancel"]$value
    strBtnCancel <- ifelse(is.na(strtmp), strBtnCancel, strtmp)

    strtmp <- dtStrings["strBtnOverwrite"]$value
    strBtnOverwrite <- ifelse(is.na(strtmp), strBtnOverwrite, strtmp)

    strtmp <- dtStrings["strBtnRetry"]$value
    strBtnRetry <- ifelse(is.na(strtmp), strBtnRetry, strtmp)
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

    # Check file name.
    if (nchar(val_name) == 0) {
      val_name <- NA
    }

    # Check path.
    if (length(val_path) == 0) {
      val_path <- NA
    }

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

    # Check for file name and path.
    ok <- !is.na(val_name) && !is.na(val_path) && !is.null(val_ggplot)

    if (ok) {
      blockHandlers(g_save_btn)
      svalue(g_save_btn) <- strBtnProcessing
      unblockHandlers(g_save_btn)

      # Add trailing path separator if not present.
      if (substr(val_path, nchar(val_path), nchar(val_path) + 1) != .separator) {
        val_path <- paste(val_path, .separator, sep = "")
      }

      # Repeat until saved or cancel.
      okToSave <- FALSE
      cancel <- FALSE
      repeat{

        # Construct complete file name.
        fullFileName <- paste(val_path, val_name, val_ext, sep = "")

        if (val_replace) {
          # Ok to overwrite.
          okToSave <- TRUE

          if (debug) {
            print("Replace=TRUE. Ok to save!")
          }
        } else {
          # Not ok to overwrite.

          if (debug) {
            print("Replace=FALSE. Check if file exist!")
          }

          # Check if file exist.
          if (file.exists(fullFileName)) {
            if (debug) {
              print(paste("file '", name, "' already exist!", sep = ""))
            }

            # Create dialog.
            dialog <- gbasicdialog(
              title = strMsgTitleSaveError, parent = w,
              do.buttons = FALSE, width = 200, height = 200
            )

            # Vertical container.
            gg <- ggroup(container = dialog, horizontal = FALSE)

            glabel(
              text = strMsgFileExist,
              anchor = c(-1, 0), container = gg
            )

            # Edit box for new name.
            newName <- gedit(container = gg)

            # Container for buttons.
            buttcont <- ggroup(container = gg)

            btn_cancel <- gbutton(strBtnCancel,
              container = buttcont,
              handler = function(h, ...) {
                cancel <<- TRUE
                dispose(dialog)
              }
            )

            btn_replace <- gbutton(strBtnOverwrite,
              container = buttcont,
              handler = function(h, ...) {
                val_replace <<- TRUE
                dispose(dialog)
              }
            )

            btn_retry <- gbutton(strBtnRetry,
              container = buttcont,
              handler = function(h, ...) {
                val_name <<- svalue(newName)
                if (debug) {
                  print("val_name")
                  print(val_name)
                }
                dispose(dialog)
              }
            )

            # Show dialog.
            visible(dialog, set = TRUE)
          } else {
            okToSave <- TRUE
          }
        }

        if (cancel) {
          # Chose to cancel.

          if (debug) {
            print("Chose to cancel!")
          }

          break ## EXIT REPEAT.
        }

        if (okToSave) {

          # Save plot device as image.
          ggsave(
            filename = paste(val_name, val_ext, sep = ""),
            plot = val_ggplot,
            path = val_path,
            scale = val_scale,
            width = val_w, height = val_h,
            units = val_unit, dpi = val_r
          )


          if (debug) {
            print("Image saved!")
          }

          break ## EXIT REPEAT.
        }
      } ## END REPEAT.

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strMsgSave,
        title = strMsgTitleError,
        parent = w,
        icon = "error"
      )
    }
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
