################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 20.08.2022: Fixed retaining new option decimals to quantile widget.
# 09.08.2022: Added saving/retaining the new option decimals.
# 05.08.2022: Decreased spacing in options. Added new option decimals.
# 02.08.2022: Fixed bug "Error in if (length(value) == 0 || value == "")...".
# 02.08.2022: Fixed saved gui state cleared when opening with pre set parameters.
# 28.06.2020: Fixed bug dropdowns not populated in tcltk.
# 13.06.2020: Fixed bug "The column <Select Columns> was not found in the dataset."
# 12.06.2020: Fixed bug in call to checkDataset.
# 09.06.2020: Added parameter count.
# 06.06.2020: Fixed vector support for parameter group. Removed edit field for target.
# 23.05.2020: First version.

#' @title Calculate Statistics
#'
#' @description
#' GUI wrapper for the \code{\link{calculateStatistics}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateStatistics}} function
#' by providing a graphical user interface. Preselected values can be provided
#' as arguments.
#'
#' @param data character preselected data.frame if provided and exist in environment.
#' @param target character vector preselected target column.
#' @param quant numeric quantile to calculate. Default=0.95.
#' @param group character vector preselected column(s) to group by.
#' @param count character vector preselected column to count unique values in.
#' @param decimals numeric number of decimals. Negative does not round.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help head str
#' @importFrom graphics title
#'
#' @seealso \code{link{quantile}}, \code{link{min}}, \code{link{max}}, \code{link{mean}}, \code{link{sd}}

calculateStatistics_gui <- function(data = NULL, target = NULL, quant = 0.95,
                                    group = NULL, count = NULL, decimals = 4,
                                    env = parent.frame(), savegui = NULL,
                                    debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gSkipClear <- FALSE

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate summary statistics"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblRows <- "rows"
  strFrmOptions <- "Options"
  strLblTarget <- "Select target column:"
  strLblGroupBy <- "Group by column(s):"
  strLblCount <- "Count unique values in column:"
  strDrpColumn <- "<Select Columns>"
  strLblQuantile <- "Calculate quantile:"
  strLblDecimals <- "Round to decimals (-1 for no rounding):"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset."
  strMsgTitleError <- "Error"

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

    strtmp <- dtStrings["strFrmDataset"]$value
    strFrmDataset <- ifelse(is.na(strtmp), strFrmDataset, strtmp)

    strtmp <- dtStrings["strLblDataset"]$value
    strLblDataset <- ifelse(is.na(strtmp), strLblDataset, strtmp)

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strtmp <- dtStrings["strLblRows"]$value
    strLblRows <- ifelse(is.na(strtmp), strLblRows, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblTarget"]$value
    strLblTarget <- ifelse(is.na(strtmp), strLblTarget, strtmp)

    strtmp <- dtStrings["strLblGroupBy"]$value
    strLblGroupBy <- ifelse(is.na(strtmp), strLblGroupBy, strtmp)

    strtmp <- dtStrings["strLblCount"]$value
    strLblCount <- ifelse(is.na(strtmp), strLblCount, strtmp)

    strtmp <- dtStrings["strDrpColumn"]$value
    strDrpColumn <- ifelse(is.na(strtmp), strDrpColumn, strtmp)

    strtmp <- dtStrings["strLblQuantile"]$value
    strLblQuantile <- ifelse(is.na(strtmp), strLblQuantile, strtmp)

    strtmp <- dtStrings["strLblDecimals"]$value
    strLblDecimals <- ifelse(is.na(strtmp), strLblDecimals, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strMsgDataset"]$value
    strMsgDataset <- ifelse(is.na(strtmp), strMsgDataset, strtmp)

    strtmp <- dtStrings["strMsgTitleDataset"]$value
    strMsgTitleDataset <- ifelse(is.na(strtmp), strMsgTitleDataset, strtmp)

    strtmp <- dtStrings["strMsgCheck"]$value
    strMsgCheck <- ifelse(is.na(strtmp), strMsgCheck, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)
  }

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(title = strWinTitle, visible = FALSE)

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

  savegui_chk <- gcheckbox(text = strChkGui, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  data_frm <- gframe(
    text = strFrmDataset,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  # Dataset -------------------------------------------------------------------

  glabel(text = strLblDataset, container = data_frm)

  data_rows_lbl <- glabel(
    text = paste(" 0", strLblRows),
    container = data_frm
  )

  dfs <- c(strDrpDataset, listObjects(env = env, obj.class = "data.frame"))

  data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = data_frm,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(data_drp, handler = function(h, ...) {
    .updateWidgets()
  })

  # OPTIONS ###################################################################

  option_frm <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # METHOD --------------------------------------------------------------------

  # Target --------------------------------------------------------------------

  glabel(text = strLblTarget, container = option_frm)

  target_drp <- gcombobox(
    items = strDrpColumn,
    container = option_frm, ellipsize = "none"
  )

  # Group ---------------------------------------------------------------------

  glabel(text = strLblGroupBy, container = option_frm)

  group_drp <- gcombobox(
    items = strDrpColumn,
    container = option_frm, ellipsize = "none"
  )

  group_edt <- gedit(
    text = ifelse(is.null(group), "", paste(group, collapse = ",")),
    container = option_frm
  )

  addHandlerChanged(group_drp, handler = function(h, ...) {
    val_column <- svalue(group_drp)
    val_value <- svalue(group_edt)

    if (!is.null(val_column)) {
      if (val_column != strDrpColumn) {

        # Add new value to selected.
        if (nchar(val_value) == 0) {
          svalue(group_edt) <- val_column
        } else {
          svalue(group_edt) <- paste(val_value, val_column, sep = ",")
        }
      }
    }
  })

  # Count ---------------------------------------------------------------------

  glabel(text = strLblCount, container = option_frm)

  count_drp <- gcombobox(
    items = strDrpColumn,
    container = option_frm, ellipsize = "none"
  )

  # Quantile ------------------------------------------------------------------

  glabel(text = strLblQuantile, container = option_frm)

  quant_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = quant,
    container = option_frm
  )

  # Decimals ------------------------------------------------------------------

  glabel(text = strLblDecimals, container = option_frm)

  decimals_spb <- gspinbutton(
    from = -1, to = 10,
    by = 1, value = decimals,
    container = option_frm
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_obj <- .gDataName
    val_name <- svalue(save_edt)
    val_target <- svalue(target_drp)
    val_group <- svalue(group_edt)
    val_count <- svalue(count_drp)
    val_quant <- svalue(quant_spb)
    val_decimals <- svalue(decimals_spb)

    if (val_group == strDrpColumn) {
      val_group <- NULL
    }

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_name")
      print(val_name)
      print("val_target")
      print(val_target)
      print("val_group")
      print(val_group)
      print("val_count")
      print(val_count)
      print("val_quant")
      print(val_quant)
      print("val_decimals")
      print(val_decimals)
    }

    # Check if data.
    if (!is.null(val_data)) {
      if (!nchar(val_target) > 0 || val_target == strDrpColumn) {
        val_target <- NULL
      } else {
        # val_target <- unlist(strsplit(val_target, split = ","))
        val_target # Currently only implemented single target column.
      }

      if (!nchar(val_group) > 0 || val_group == strDrpColumn) {
        val_group <- NULL
      } else {
        val_group <- unlist(strsplit(val_group, split = ","))
      }

      if (!nchar(val_count) > 0 || val_count == strDrpColumn) {
        val_count <- NULL
      } else {
        # val_count <- unlist(strsplit(val_count, split = ","))
        val_count # Currently only implemented single count column.
      }

      # Check if suitable.
      requiredCol <- c(val_target, val_group, val_count)
      requiredCol <- requiredCol[requiredCol != strDrpColumn]
      ok <- checkDataset(
        name = val_obj, reqcol = requiredCol,
        env = env, parent = w, debug = debug
      )

      if (ok) {
        if (debug) {
          print("Sent Values:")
          print("val_target")
          print(val_target)
          print("val_group")
          print(val_group)
          print("val_count")
          print(val_count)
          print("val_quant")
          print(val_quant)
          print("val_decimals")
          print(val_decimals)
        }

        # Change button.
        blockHandlers(calculate_btn)
        svalue(calculate_btn) <- strBtnProcessing
        unblockHandlers(calculate_btn)
        enabled(calculate_btn) <- FALSE

        datanew <- calculateStatistics(
          data = val_data,
          target = val_target,
          group = val_group,
          count = val_count,
          quant = val_quant,
          decimals = val_decimals,
          debug = debug
        )

        # Create key-value pairs to log.
        keys <- list("data", "target", "group", "count", "quant")

        values <- list(val_obj, val_target, val_group, val_count, val_quant)

        # Update audit trail.
        datanew <- auditTrail(
          obj = datanew, key = keys, value = values,
          label = fnc, arguments = FALSE,
          package = "strvalidator"
        )

        # Save data.
        saveObject(name = val_name, object = datanew, parent = w, env = env)

        if (debug) {
          print(str(datanew))
          print(head(datanew))
          print(paste("EXIT:", fnc))
        }

        # Only save settings if started without parameters.
        if (all(is.null(data), is.null(target), is.null(group))) {
          .saveSettings()
        }

        # Close GUI.
        dispose(w)
      }
    } else {
      gmessage(
        msg = strMsgDataset,
        title = strMsgTitleDataset,
        icon = "error",
        parent = w
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .updateWidgets <- function() {
    val_obj <- gWidgets2::svalue(data_drp)

    if (val_obj != strDrpDataset) {
      # Load or change components.

      # get dataset.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      gWidgets2::svalue(data_rows_lbl) <- paste(nrow(.gData), strLblRows)

      # Update dropdown menues.
      target_columns <- unique(c(strDrpColumn, names(.gData)))
      target_drp[] <- target_columns
      group_drp[] <- target_columns
      count_drp[] <- target_columns

      # Select default value, try to find matching column name.
      hit <- which(target_columns %in% target)
      index <- ifelse(is.null(target) | gWidgets2::is_empty(hit), 1, hit)
      gWidgets2::svalue(target_drp, index = TRUE) <- index

      # Select default value.
      gWidgets2::svalue(group_drp, index = TRUE) <- 1

      # Select default value, try to find matching column name.
      hit <- which(target_columns %in% count)
      index <- ifelse(is.null(target) | gWidgets2::is_empty(hit), 1, hit)
      gWidgets2::svalue(count_drp, index = TRUE) <- index

      # Suggest a name for the result.
      gWidgets2::svalue(save_edt) <- paste(val_obj, "_stats", sep = "")
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      gWidgets2::svalue(data_drp, index = TRUE) <- 1
      gWidgets2::svalue(data_rows_lbl) <- paste(" 0", strLblRows)
      gWidgets2::svalue(save_edt) <- ""

      # Update dropdown menues.
      target_drp[] <- strDrpColumn
      group_drp[] <- strDrpColumn
      count_drp[] <- strDrpColumn

      # Select default value.
      gWidgets2::svalue(target_drp, index = TRUE) <- 1
      gWidgets2::svalue(group_drp, index = TRUE) <- 1
      gWidgets2::svalue(count_drp, index = TRUE) <- 1
    }

    # Change button.
    gWidgets2::blockHandlers(calculate_btn)
    gWidgets2::svalue(calculate_btn) <- strBtnCalculate
    gWidgets2::unblockHandlers(calculate_btn)
    gWidgets2::enabled(calculate_btn) <- TRUE
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
      if (exists(".strvalidator_calculateStatistics_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateStatistics_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateStatistics_gui_group", envir = env, inherits = FALSE)) {
        svalue(group_edt) <- get(".strvalidator_calculateStatistics_gui_group", envir = env)
      }
      if (exists(".strvalidator_calculateStatistics_gui_quant", envir = env, inherits = FALSE)) {
        svalue(quant_spb) <- get(".strvalidator_calculateStatistics_gui_quant", envir = env)
      }
      if (exists(".strvalidator_calculateStatistics_gui_decimals", envir = env, inherits = FALSE)) {
        svalue(decimals_spb) <- get(".strvalidator_calculateStatistics_gui_decimals", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateStatistics_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateStatistics_gui_group", value = svalue(group_edt), envir = env)
      assign(x = ".strvalidator_calculateStatistics_gui_quant", value = svalue(quant_spb), envir = env)
      assign(x = ".strvalidator_calculateStatistics_gui_decimals", value = svalue(decimals_spb), envir = env)

      if (debug) {
        print("Settings saved!")
      }
    } else if (.gSkipClear) {
      # Do nothing when started with parameters.
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateStatistics_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStatistics_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateStatistics_gui_group", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStatistics_gui_group", envir = env)
      }
      if (exists(".strvalidator_calculateStatistics_gui_quant", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStatistics_gui_quant", envir = env)
      }

      if (debug) {
        print("Settings cleared!")
      }
    }
  }

  # END GUI ###################################################################

  # Update dropdown if data is provided.
  i_drp <- which(dfs %in% data)
  svalue(data_drp, index = TRUE) <- ifelse(length(i_drp) == 0, 1, i_drp)
  .updateWidgets() # Needed to update tcltk widgets.

  # Only load settings if started without parameters.
  if (all(is.null(data), is.null(target), is.null(group))) {

    # Load GUI settings.
    .loadSavedSettings()
  } else {

    # Save settings disabled when starting with parameters.
    enabled(savegui_chk) <- FALSE
    .gSkipClear <- TRUE
  }

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
