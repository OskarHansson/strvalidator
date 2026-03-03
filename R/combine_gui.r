################################################################################
# CHANGE LOG (last 20 changes)
# 15.10.2025: Specified the package anchor in link.
# 09.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 08.03.2020: Added check for data selected.
# 08.03.2020: Added language support.
# 16.03.2019: Fixed R Check note.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 26.07.2017: Added expand=TRUE to save name text field.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 10.10.2016: Changed to rbind.fill
# 10.10.2016: Check for column names no longer require identical order.
# 28.08.2015: Added importFrom.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 29.07.2014: Changed name concatenate_gui -> combine_gui.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.

#' @title Combine Datasets
#'
#' @description
#' GUI for combining two datasets.
#'
#' @details
#' Simple GUI to combine two datasets using the \code{\link[plyr:rbind.fill]{rbind.fill}}
#' function.
#' NB! Datasets must have identical column names but not necessarily
#' in the same order.
#'
#' @param env environment in which to search for data frames.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help
#' @importFrom plyr rbind.fill
#'
#' @return TRUE


combine_gui <- function(env = parent.frame(), debug = FALSE, parent = NULL) {
  # Global variables.
  .gData1 <- NULL
  .gData2 <- NULL
  .gData1Name <- NULL
  .gData2Name <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Combine datasets",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_SET1            = "Dataset 1:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_COLUMNS         = "columns",
    STR_LBL_SET2            = "Dataset 2:",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Combine",
    STR_MSG_COLUMNS         = "Datasets must have identical column names!",
    STR_MSG_TITLE_ERROR     = "Error",
    STR_MSG_DATASET         = "Two datasets must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(title = strings$STR_WIN_TITLE, visible = FALSE)

  # Runs when window is closed.
  addHandlerUnrealize(w, handler = function(h, ...) {
    # Save GUI state.
    # .saveSettings()

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


  # DATASET 1 -----------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_SET1, container = f0g0)

  data1_col_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_COLUMNS),
    container = f0g0
  )

  data1_drp <- gcombobox(
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

  addHandlerChanged(data1_drp, handler = function(h, ...) {
    val_obj <- svalue(data1_drp)

    # Check if suitable.
    ok <- check_dataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData1 <<- get(val_obj, envir = env)
      .gData1Name <<- val_obj

      svalue(data1_col_lbl) <- paste(" ", ncol(.gData1), strings$STR_LBL_COLUMNS)
      svalue(save_edt) <- paste(.gData1Name, .gData2Name, sep = "_")
    } else {
      .gData1 <<- NULL
      .gData1Name <<- NULL
      svalue(data1_col_lbl) <- paste(" 0", strings$STR_LBL_COLUMNS)
      svalue(save_edt) <- ""
    }
  })

  # DATASET 2 -----------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_SET2, container = f0g1)

  data2_col_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_COLUMNS),
    container = f0g1
  )

  data2_drp <- gcombobox(
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
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(data2_drp, handler = function(h, ...) {
    val_obj <- svalue(data2_drp)

    if (exists(val_obj, envir = env, inherits = FALSE)) {
      .gData2 <<- get(val_obj, envir = env)
      .gData2Name <<- val_obj

      svalue(data2_col_lbl) <- paste(" ", ncol(.gData2), strings$STR_LBL_COLUMNS)
      svalue(save_edt) <- paste(.gData1Name, .gData2Name, sep = "_")
    } else {
      .gData2 <<- NULL
      .gData1Name <<- NULL
      svalue(data2_col_lbl) <- paste(" 0", strings$STR_LBL_COLUMNS)
      svalue(save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################
  # # No options yet.
  #   f1 <- gframe("Options", horizontal=FALSE, container=gv)
  #
  #   f1g0 <- glayout(container = f1, expand=TRUE, fill="both")

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  combine_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerChanged(combine_btn, handler = function(h, ...) {
    datOk <- all(!is.null(.gData1), !is.null(.gData2))
    colOk <- all(names(.gData1) %in% names(.gData2))
    val_data_1 <- .gData1Name
    val_data_2 <- .gData2Name
    val_name <- svalue(save_edt)

    if (colOk && datOk) {
      # Combine the datasets.
      datanew <- plyr::rbind.fill(.gData1, .gData2)

      # Create key-value pairs to log.
      keys <- list("data1", "data2")

      values <- list(val_data_1, val_data_2)

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
      # .saveSettings()
      dispose(w)
    } else if (!colOk) {
      gmessage(
        msg = strings$STR_MSG_COLUMNS,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    } else if (!datOk) {
      gmessage(
        msg = strings$STR_MSG_DATASET,
        title = strings$STR_MSG_TITLE_DATASET,
        icon = "error"
      )
    }
  })

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
} # End of GUI
