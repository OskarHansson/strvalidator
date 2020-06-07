################################################################################
# CHANGE LOG (last 20 changes)
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

calculateStatistics_gui <- function(data = NULL, target = NULL, quant = 0.95, group = NULL,
                                    env = parent.frame(), savegui = NULL,
                                    debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL

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
  strDrpColumn <- "<Select Columns>"
  strLblQuantile <- "Calculate quantile"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset."
  strMsgTitleError <- "Error"

  # Get strings from language file.
  # dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  # if (!is.null(dtStrings)) {
  # Get language strings, use default if not found.

  # }

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

  gv <- ggroup(
    horizontal = FALSE,
    spacing = 8,
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
    spacing = 5,
    container = gv
  )

  # Dataset -------------------------------------------------------------------

  glabel(text = strLblDataset, container = data_frm)

  dfs <- c(strDrpDataset, listObjects(env = env, obj.class = "data.frame"))

  data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = data_frm,
    ellipsize = "none"
  )
  data_rows_lbl <- glabel(
    text = paste(" 0", strLblRows),
    container = data_frm
  )

  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)

    if (val_obj != strDrpDataset) {
      # Load or change components.

      # get dataset.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      svalue(data_rows_lbl) <- paste(nrow(.gData), strLblRows)

      # Update dropdown menues.
      target_columns <- unique(c(strDrpColumn, names(.gData)))
      target_drp[, ] <- target_columns
      group_drp[, ] <- unique(c(strDrpColumn, names(.gData)))

      # Select default value.
      svalue(target_drp, index = TRUE) <- ifelse(is.null(target), 1,
                                                 which(target_columns %in% target))
      svalue(group_drp, index = TRUE) <- 1

      # Suggest a name for the result.
      svalue(save_edt) <- paste(val_obj, "_stats", sep = "")
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_drp, index = TRUE) <- 1
      svalue(data_rows_lbl) <- paste(" 0", strLblRows)
      svalue(save_edt) <- ""

      # Update dropdown menues.
      target_drp[, ] <- strDrpColumn
      group_drp[, ] <- strDrpColumn

      # Select default value.
      svalue(target_drp, index = TRUE) <- 1
      svalue(group_drp, index = TRUE) <- 1
    }

    # Change button.
    blockHandlers(calculate_btn)
    svalue(calculate_btn) <- strBtnCalculate
    unblockHandlers(calculate_btn)
    enabled(calculate_btn) <- TRUE
  })

  # OPTIONS ###################################################################

  option_frm <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 10,
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

  # Quantile ------------------------------------------------------------------

  glabel(text = strLblQuantile, container = option_frm)

  quant_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = quant,
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
    val_quant <- svalue(quant_spb)

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
      print("val_quant")
      print(val_quant)
    }

    # Check if data.
    if (!is.null(val_data)) {
      if (!nchar(val_target) > 0) {
        val_target <- NULL
      } else {
        val_target <- unlist(strsplit(val_target, split = ","))
      }

      if (!nchar(val_group) > 0) {
        val_group <- NULL
      } else {
        val_group <- unlist(strsplit(val_group, split = ","))
      }

      # Check if suitable.
      requiredCol <- c(val_target, val_group)
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
          print("val_quant")
          print(val_quant)
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
          quant = val_quant,
          debug = debug
        )

        # Create key-value pairs to log.
        keys <- list("data", "target", "group", "quant")

        values <- list(val_obj, val_target, val_group, val_quant)

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
      if (exists(".strvalidator_calculateStatistics_gui_target", envir = env, inherits = FALSE)) {
        svalue(target_edt) <- get(".strvalidator_calculateStatistics_gui_target", envir = env)
      }
      if (exists(".strvalidator_calculateStatistics_gui_group", envir = env, inherits = FALSE)) {
        svalue(group_edt) <- get(".strvalidator_calculateStatistics_gui_group", envir = env)
      }
      if (exists(".strvalidator_calculateStatistics_gui_quant", envir = env, inherits = FALSE)) {
        svalue(quant_spb) <- get(".strvalidator_calculateStatistics_gui_quant", envir = env)
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
      assign(x = ".strvalidator_calculateStatistics_gui_target", value = svalue(target_edt), envir = env)
      assign(x = ".strvalidator_calculateStatistics_gui_group", value = svalue(group_edt), envir = env)
      assign(x = ".strvalidator_calculateStatistics_gui_quant", value = svalue(quant_spb), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateStatistics_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStatistics_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateStatistics_gui_target", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStatistics_gui_target", envir = env)
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

    if (debug) {
      print("Settings saved!")
    }
  }

  # END GUI ###################################################################

  # Update dropdown if data is provided.
  i_drp <- which(dfs %in% data)
  svalue(data_drp, index = TRUE) <- ifelse(length(i_drp) == 0, 1, i_drp)

  # Only load settings if started without parameters.
  if (all(is.null(data), is.null(target), is.null(group))) {

    # Load GUI settings.
    .loadSavedSettings()
  } else {

    # Save settings disabled when starting with parameters.    
    enabled(savegui_chk) <- FALSE

  }

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}