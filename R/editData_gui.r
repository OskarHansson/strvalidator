################################################################################
# CHANGE LOG (last 20 changes)
# 08.03.2020: Added language support.
# 18.03.2019: Fixed freeze when opened with a NA containing dataset in view mode (tcltk).
# 14.03.2019: Fixed R-Check note.
# 20.02.2019: Fixed drop-down menu should default to <Select data frame> (tcltk).
# 19.02.2019: Expand table and text field under tcltk.
# 10.02.2019: Try version dependent fix.
# 27.01.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 26.01.2019: Fixed table not updated after selecting from drop-down (tcltk)
# 07.08.2017: Added audit trail.
# 17.07.2017: Fixed "Error in if (nchar(text) > 0) set_value(text) : argument is of length zero"
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow drop-down with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 07.07.2017: Replaced gWidgets:: with gWidgets2::
# 24.06.2016: 'Save as' textbox expandable.
# 14.04.2016: Limit number of rows now FALSE by default + tooltip.
# 06.01.2016: Fixed attributes window bug. Error when close using X.
# 26.10.2015: Fixed attributes window bug.
# 04.10.2015: Added options to limit number of rows, and show attributes.

#' @title Edit or View Data Frames
#'
#' @description
#' GUI to edit and view data frames.
#'
#' @details Select a data frame from the drop-down to view or edit a dataset.
#' It is possible to save as a new dataframe. To enable sorting by clicking the
#' column headers the view mode must be used (i.e. edit = FALSE). There is an
#' option to limit the number of rows shown that can be used to preview large
#' datasets that may otherwise cause performance problems. Attributes of the
#' dataset can be views in a separate window.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param data data.frame for instant viewing.
#' @param name character string with the name of the provided dataset.
#' @param edit logical TRUE to enable edit (uses \code{\link{gdf}}), FALSE to
#' view and enable sorting by clicking a column header (uses \code{\link{gtable}}).
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help write.table
#'
#' @return TRUE
#'
#' @seealso \code{\link{trim_gui}}, \code{\link{cropData_gui}}, \code{\link{combine_gui}}

editData_gui <- function(env = parent.frame(), savegui = NULL, data = NULL,
                         name = NULL, edit = TRUE, debug = FALSE, parent = NULL) {
  .gData <- data
  .gDataName <- name
  .hideMsg <- FALSE

  # gedit cannot handle zero length 'text'.
  if (length(.gDataName) == 0) {
    .gDataName <- ""
  }

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitleEdit <- "Edit or view dataset"
  strWinTitleView <- "View dataset"
  strWinTitleAttributes <- "Attributes"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples,"
  strLblColumns <- "columns,"
  strLblRows <- "rows"
  strFrmOptions <- "Options"
  strChkAttributes <- "Show attributes (separate window)"
  strChkLimit <- "Limit number of rows to:"
  strTipLimit <- "NB! Sorting will only be performed on the loaded data."
  strBtnCopy <- "Copy"
  strTipCopy <- "Copy to clipboard (NB! large datasets might get truncated)."
  strBtnCopying <- "Copying..."
  strBtnExport <- "Export"
  strTipExport <- "Opens the export dialog."
  strBtnSave <- "Save as"
  strTipSave <- "Save as new dataset in this project."
  strBtnSaving <- "Saving..."
  strFrmSave <- "Copy|Export|Save"
  strLblNoData <- "There is no data"
  strMsgSave <- "A name must be provided."
  strMsgTitleError <- "Error"
  strLblTcltk <- "The tcltk gui toolkit does not handle NA values in tables.\nNA values will be replaced with empty strings.\nIf you edit the table, NA values will be permanently replaced."
  strChkShow <- "Don't show this message again."
  strMsgTitleWarning <- "Warning"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.null(dtStrings)) {
    # Get language strings, use default if not found.

    strtmp <- dtStrings["strWinTitleEdit"]$value
    strWinTitleEdit <- ifelse(is.na(strtmp), strWinTitleEdit, strtmp)

    strtmp <- dtStrings["strWinTitleView"]$value
    strWinTitleView <- ifelse(is.na(strtmp), strWinTitleView, strtmp)

    strtmp <- dtStrings["strWinTitleAttributes"]$value
    strWinTitleAttributes <- ifelse(is.na(strtmp), strWinTitleAttributes, strtmp)

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

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strLblColumns"]$value
    strLblColumns <- ifelse(is.na(strtmp), strLblColumns, strtmp)

    strtmp <- dtStrings["strLblRows"]$value
    strLblRows <- ifelse(is.na(strtmp), strLblRows, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkAttributes"]$value
    strChkAttributes <- ifelse(is.na(strtmp), strChkAttributes, strtmp)

    strtmp <- dtStrings["strChkLimit"]$value
    strChkLimit <- ifelse(is.na(strtmp), strChkLimit, strtmp)

    strtmp <- dtStrings["strTipLimit"]$value
    strTipLimit <- ifelse(is.na(strtmp), strTipLimit, strtmp)

    strtmp <- dtStrings["strBtnCopy"]$value
    strBtnCopy <- ifelse(is.na(strtmp), strBtnCopy, strtmp)

    strtmp <- dtStrings["strTipCopy"]$value
    strTipCopy <- ifelse(is.na(strtmp), strTipCopy, strtmp)

    strtmp <- dtStrings["strBtnCopying"]$value
    strBtnCopying <- ifelse(is.na(strtmp), strBtnCopying, strtmp)

    strtmp <- dtStrings["strBtnExport"]$value
    strBtnExport <- ifelse(is.na(strtmp), strBtnExport, strtmp)

    strtmp <- dtStrings["strTipExport"]$value
    strTipExport <- ifelse(is.na(strtmp), strTipExport, strtmp)

    strtmp <- dtStrings["strBtnSave"]$value
    strBtnSave <- ifelse(is.na(strtmp), strBtnSave, strtmp)

    strtmp <- dtStrings["strTipSave"]$value
    strTipSave <- ifelse(is.na(strtmp), strTipSave, strtmp)

    strtmp <- dtStrings["strBtnSaving"]$value
    strBtnSaving <- ifelse(is.na(strtmp), strBtnSaving, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblNoData"]$value
    strLblNoData <- ifelse(is.na(strtmp), strLblNoData, strtmp)

    strtmp <- dtStrings["strMsgSave"]$value
    strMsgSave <- ifelse(is.na(strtmp), strMsgSave, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)

    strtmp <- dtStrings["strLblTcltk"]$value
    strLblTcltk <- ifelse(is.na(strtmp), strLblTcltk, strtmp)

    strtmp <- dtStrings["strChkShow"]$value
    strChkShow <- ifelse(is.na(strtmp), strChkShow, strtmp)

    strtmp <- dtStrings["strMsgTitleWarning"]$value
    strMsgTitleWarning <- ifelse(is.na(strtmp), strMsgTitleWarning, strtmp)
  }

  # WINDOW ####################################################################

  if (edit) {
    guiTitle <- strWinTitleEdit
  } else {
    guiTitle <- strWinTitleView
  }

  # Create windows.
  w <- gwindow(title = guiTitle, visible = FALSE)
  w_attributes <- gwindow(title = strWinTitleAttributes, visible = FALSE)
  attr_text <- gtext("", container = w_attributes)

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

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  g0 <- glayout(container = f0, spacing = 1)

  g0[1, 1] <- glabel(text = strLblDataset, container = g0)

  g0[1, 2] <- dataset_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )

  if (!is.null(.gDataName) && nchar(.gDataName) > 0) {
    svalue(dataset_drp) <- .gDataName
  }

  g0_samples_lbl <- glabel(text = paste(" 0", strLblSamples), container = g0)
  g0_columns_lbl <- glabel(text = paste(" 0", strLblColumns), container = g0)
  g0_rows_lbl <- glabel(text = paste(" 0", strLblRows), container = g0)

  g0[1, 3] <- g0_samples_lbl
  g0[1, 4] <- g0_columns_lbl
  g0[1, 5] <- g0_rows_lbl

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    message("Dataset ", val_obj, " selected.")

    # Check if suitable.
    ok <- checkDataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      # Refresh info and load table.
      .refreshInfo()
      .refreshTbl()
    } else {

      # Clear info.
      .refreshInfo(clear = TRUE)
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  g1 <- glayout(container = f1, spacing = 1)

  g1[1, 1] <- f1_show_attr_chk <- gcheckbox(
    text = strChkAttributes,
    checked = FALSE, container = g1
  )


  g1[2, 1] <- f1_limit_chk <- gcheckbox(
    text = strChkLimit,
    checked = FALSE, container = g1
  )
  tooltip(f1_limit_chk) <- strTipLimit

  g1[2, 2] <- f1_max_edt <- gedit(text = 100, width = 8, container = g1)

  addHandlerChanged(f1_show_attr_chk, handler = function(h, ...) {
    if (svalue(f1_show_attr_chk)) {
      .showAttributes()
    } else {
      if (isExtant(w_attributes)) {
        visible(w_attributes) <- FALSE
      }
    }
  })

  addHandlerChanged(f1_limit_chk, handler = function(h, ...) {
    .refreshTbl()
  })


  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strFrmSave,
    horizontal = TRUE, spacing = 5, container = gv
  )

  copy_btn <- gbutton(text = strBtnCopy, container = f2)
  tooltip(copy_btn) <- strTipCopy

  export_btn <- gbutton(text = strBtnExport, container = f2)
  tooltip(export_btn) <- strTipExport

  save_btn <- gbutton(text = strBtnSave, container = f2)
  tooltip(save_btn) <- strTipSave

  save_txt <- gedit(text = .gDataName, container = f2, expand = TRUE, fill = TRUE)

  addHandlerClicked(copy_btn, handler = function(h, ...) {
    val_tbl <- data_tbl[]

    # Change button.
    blockHandlers(copy_btn)
    svalue(copy_btn) <- strBtnCopying
    unblockHandlers(copy_btn)
    enabled(copy_btn) <- FALSE

    # Copy data.
    write.table(val_tbl, "clipboard",
      sep = "\t", row.names = FALSE
    )

    # Change button.
    blockHandlers(copy_btn)
    svalue(copy_btn) <- strBtnCopy
    unblockHandlers(copy_btn)
    enabled(copy_btn) <- TRUE
  })

  addHandlerClicked(save_btn, handler = function(h, ...) {
    val_name <- svalue(save_txt)
    datanew <- data_tbl[]

    if (debug) {
      print("names(datanew)")
      print(names(datanew))
    }

    if (!is.na(val_name) && !is.null(val_name)) {

      # Copy and add attributes (retain names).
      names(.gData) <- names(datanew)
      attributes(datanew) <- attributes(.gData)

      # Change button.
      blockHandlers(save_btn)
      svalue(save_btn) <- strBtnSaving
      unblockHandlers(save_btn)
      enabled(save_btn) <- FALSE

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, label = fnc,
        arguments = FALSE, package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env, debug = debug)

      # Change button.
      blockHandlers(save_btn)
      svalue(save_btn) <- strBtnSave
      unblockHandlers(save_btn)
      enabled(save_btn) <- TRUE
    } else {
      gmessage(
        msg = strMsgSave,
        title = strMsgTitleError,
        icon = "error"
      )
    }
  })

  addHandlerChanged(export_btn, handler = function(h, ...) {

    # Open GUI.
    export_gui(env = env, savegui = savegui, debug = debug, parent = parent)
  })

  # FRAME 3 ###################################################################

  f3 <- gvbox(container = gv, expand = TRUE, fill = TRUE)

  # Add dummy table.
  data_tbl <- gWidgets2::gtable(
    items = data.frame(Data = strLblNoData),
    container = f3, expand = TRUE
  )

  # INTERNAL FUNCTIONS ########################################################

  .showAttributes <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Get options.
    val_attr <- svalue(f1_show_attr_chk)

    if (val_attr & !is.null(.gData)) {
      if (!isExtant(w_attributes)) {

        # Re-create window.
        w_attributes <<- gwindow(
          title = strWinTitleAttributes,
          visible = FALSE
        )
        attr_text <<- gtext("", container = w_attributes)
      }

      # Get list of attributes.
      attributeList <- attributes(.gData)

      # Remove common non-strvalidator attributes (too much to show).
      attributeList$names <- NULL
      attributeList$row.names <- NULL
      attributeList$class <- NULL

      # Empty text fiels.
      svalue(attr_text) <- ""

      # Get names of attributes.
      attrNames <- names(attributeList)

      # Loop over list of attributes and att to text object.
      for (a in seq(along = attrNames)) {

        # Insert text for current attribute.
        insert(attr_text, paste(attrNames[a], attributeList[a], sep = ": "))
      }

      # Show window.
      visible(w_attributes) <- TRUE
    }
  }

  .refreshInfo <- function(clear = FALSE) {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    if (!clear) {
      # Update info.
      if ("Sample.Name" %in% names(.gData)) {
        samples <- length(unique(.gData$Sample.Name))
      } else if ("Sample.File.Name" %in% names(.gData)) {
        samples <- length(unique(.gData$Sample.File.Name))
      } else if (any(grepl("SAMPLE", names(.gData), ignore.case = TRUE))) {
        # Get (first) column name containing "Sample".
        sampleCol <- names(.gData)[grep("SAMPLE", names(.gData), ignore.case = TRUE)[1]]
        # Grab sample names.
        samples <- length(unique(.gData[, sampleCol]))
      } else {
        samples <- "<NA>"
      }
      svalue(g0_samples_lbl) <- paste(" ", samples, strLblSamples)
      svalue(g0_columns_lbl) <- paste(" ", ncol(.gData), strLblColumns)
      svalue(g0_rows_lbl) <- paste(" ", nrow(.gData), strLblRows)
    } else {
      svalue(g0_samples_lbl) <- paste(" <NA>", strLblSamples)
      svalue(g0_columns_lbl) <- paste(" <NA>", strLblColumns)
      svalue(g0_rows_lbl) <- paste(" <NA>", strLblRows)
    }
  }

  .refreshTbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Get options.
    val_limit <- svalue(f1_limit_chk)
    val_max <- as.numeric(svalue(f1_max_edt))
    val_attr <- svalue(f1_show_attr_chk)

    if (!is.null(.gData)) {
      if (val_attr) {
        .showAttributes()
      }

      # Update "save as" with current dataset name.
      svalue(save_txt) <- paste(.gDataName, "_edit", sep = "")

      # Check which toolkit we are using.
      if (gtoolkit() == "tcltk") {
        # tcltk gtable and gdf does not like NA values.

        # Check for NA if tcltk is used.
        if (any(is.na(.gData))) {
          .gData[is.na(.gData)] <<- ""
          message("tcltk compatibility: NA values replaced with empty string.")

          if (!.hideMsg) {
            d <- gbasicdialog(
              title = strMsgTitleWarning, parent = w,
              handler = function(h, ...) {
                .hideMsg <<- svalue(show_msg_chk)
              }
            )
            g <- ggroup(container = d, horizontal = FALSE)
            glabel(text = strLblTcltk, container = g)
            show_msg_chk <- gcheckbox(text = strChkShow, container = g)
            visible(w) <- TRUE # Main window must be visible to show message.
            visible(d) <- TRUE # Show message window.
          }
        }
      }

      # Replace data with limited or full dataset.
      if (val_limit) {
        data_tbl[] <<- head(.gData, val_max)
        message("Showing ", val_max, " rows.")
      } else {
        data_tbl[] <<- .gData
        message("Showing all data.")
      }
    } else {

      # Update with place holder.
      data_tbl[] <<- data.frame(Data = strLblNoData)
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
      if (exists(".strvalidator_editData_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_editData_gui_savegui", envir = env)
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
      if (exists(".strvalidator_editData_gui_attr", envir = env, inherits = FALSE)) {
        svalue(f1_show_attr_chk) <- get(".strvalidator_editData_gui_attr", envir = env)
      }
      if (exists(".strvalidator_editData_gui_limit", envir = env, inherits = FALSE)) {
        svalue(f1_limit_chk) <- get(".strvalidator_editData_gui_limit", envir = env)
      }
      if (exists(".strvalidator_editData_gui_maxrow", envir = env, inherits = FALSE)) {
        svalue(f1_max_edt) <- get(".strvalidator_editData_gui_maxrow", envir = env)
      }
      if (exists(".strvalidator_editData_gui_hide", envir = env, inherits = FALSE)) {
        .hideMsg <<- get(".strvalidator_editData_gui_hide", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_editData_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_editData_gui_attr", value = svalue(f1_show_attr_chk), envir = env)
      assign(x = ".strvalidator_editData_gui_limit", value = svalue(f1_limit_chk), envir = env)
      assign(x = ".strvalidator_editData_gui_maxrow", value = svalue(f1_max_edt), envir = env)
      assign(x = ".strvalidator_editData_gui_hide", value = .hideMsg, envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_editData_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_editData_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_editData_gui_attr", envir = env, inherits = FALSE)) {
        remove(".strvalidator_editData_gui_attr", envir = env)
      }
      if (exists(".strvalidator_editData_gui_limit", envir = env, inherits = FALSE)) {
        remove(".strvalidator_editData_gui_limit", envir = env)
      }
      if (exists(".strvalidator_editData_gui_maxrow", envir = env, inherits = FALSE)) {
        remove(".strvalidator_editData_gui_maxrow", envir = env)
      }
      if (exists(".strvalidator_editData_gui_hide", envir = env, inherits = FALSE)) {
        remove(".strvalidator_editData_gui_hide", envir = env)
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

  # Populate table.
  .refreshInfo()
  .refreshTbl()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
