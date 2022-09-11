# NB! Can't handle Sample.Names as factors?
################################################################################
# CHANGE LOG (last 20 changes)
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 03.05.2020: Added language support.
# 01.03.2019: Fixed expansion of widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 07.07.2017: Replaced gWidgets:: with gWidgets2::
# 24.06.2016: 'Save as' textbox expandable.
# 09.01.2016: Added attributes to result.
# 29.08.2015: Added importFrom.
# 23.05.2015: Re-named internal variable 'new' (R function) to 'new_val'.
# 11.05.2015: Accepts (the first) column name containing the string 'Sample'
# as alternative to colum name 'Sample.Name'. All made case in-sensitive.
# 04.05.2015: Implemented 'checkDataset'.
# 07.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 14.01.2014: Removed requirement for column 'Sample.Name'.

#' @title Trim Data
#'
#' @description
#' GUI wrapper for the \code{\link{trim}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{trim}} function by providing a graphical
#' user interface to it.
#'
#' @param env environment in which to search for data frames and save result.
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
#' @seealso \code{\link{trim}}
#'


trim_gui <- function(env = parent.frame(), savegui = NULL,
                     debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- data.frame(Sample.Name = "NA")
  .gDataName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Trim dataset"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblColumns <- "columns"
  strLblRows <- "rows"
  strFrmSamples <- "Samples"
  strLblSelectedSamples <- "Selected samples (separate by pipe |):"
  strEdtMsg <- "Doubleklick or drag sample names to list"
  strRadKeep <- "Keep"
  strRadRemove <- "Remove"
  strFrmColumns <- "Columns"
  strLblSelectedColumns <- "Selected columns (separate by pipe |):"
  strFrmOptions <- "Options"
  strChkColumns <- "Remove empty columns"
  strChkNA <- "Remove NA columns"
  strChkWord <- "Add word boundaries"
  strChkIgnore <- "Ignore case"
  strLblReplace <- "Replace missing values with:"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strMsgName <- "A file name must be provided!"
  strBtnTrim <- "Trim dataset"
  strBtnProcessing <- "Processing..."
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

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strLblColumns"]$value
    strLblColumns <- ifelse(is.na(strtmp), strLblColumns, strtmp)

    strtmp <- dtStrings["strLblRows"]$value
    strLblRows <- ifelse(is.na(strtmp), strLblRows, strtmp)

    strtmp <- dtStrings["strFrmSamples"]$value
    strFrmSamples <- ifelse(is.na(strtmp), strFrmSamples, strtmp)

    strtmp <- dtStrings["strLblSelectedSamples"]$value
    strLblSelectedSamples <- ifelse(is.na(strtmp), strLblSelectedSamples, strtmp)

    strtmp <- dtStrings["strEdtMsg"]$value
    strEdtMsg <- ifelse(is.na(strtmp), strEdtMsg, strtmp)

    strtmp <- dtStrings["strRadKeep"]$value
    strRadKeep <- ifelse(is.na(strtmp), strRadKeep, strtmp)

    strtmp <- dtStrings["strRadRemove"]$value
    strRadRemove <- ifelse(is.na(strtmp), strRadRemove, strtmp)

    strtmp <- dtStrings["strFrmColumns"]$value
    strFrmColumns <- ifelse(is.na(strtmp), strFrmColumns, strtmp)

    strtmp <- dtStrings["strLblSelectedColumns"]$value
    strLblSelectedColumns <- ifelse(is.na(strtmp), strLblSelectedColumns, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkColumns"]$value
    strChkColumns <- ifelse(is.na(strtmp), strChkColumns, strtmp)

    strtmp <- dtStrings["strChkNA"]$value
    strChkNA <- ifelse(is.na(strtmp), strChkNA, strtmp)

    strtmp <- dtStrings["strChkWord"]$value
    strChkWord <- ifelse(is.na(strtmp), strChkWord, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strLblReplace"]$value
    strLblReplace <- ifelse(is.na(strtmp), strLblReplace, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strMsgName"]$value
    strMsgName <- ifelse(is.na(strtmp), strMsgName, strtmp)

    strtmp <- dtStrings["strBtnTrim"]$value
    strBtnTrim <- ifelse(is.na(strtmp), strBtnTrim, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

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

  # Vertical sub group.
  g0 <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = gv,
    expand = FALSE,
    fill = TRUE
  )

  # Horizontal sub group.
  g1 <- ggroup(
    horizontal = TRUE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = gv,
    expand = TRUE,
    fill = TRUE
  )

  # Vertical sub group.
  g2 <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = gv,
    expand = FALSE,
    fill = TRUE
  )


  # DATASET ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = TRUE,
    spacing = 1,
    container = g0
  )

  glabel(text = strLblDataset, container = f0)

  info_txt <- paste(" 0 ", strLblSamples,
    ", 0 ", strLblColumns,
    ", 0 ", strLblRows,
    sep = ""
  )

  info_lbl <- glabel(text = info_txt, container = f0)

  dataset_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- NULL
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      # Load or change components.
      .refresh_samples_tbl()
      .refresh_columns_tbl()
      # Info.
      if ("SAMPLE.NAME" %in% toupper(names(.gData))) {
        samples <- length(unique(.gData$Sample.Name))
        svalue(info_lbl) <- paste(
          " ", samples, " ", strLblSamples,
          ", ", ncol(.gData), strLblColumns,
          ", ", nrow(.gData), strLblRows,
          sep = ""
        )
      } else if ("SAMPLE.FILE.NAME" %in% toupper(names(.gData))) {
        samples <- length(unique(.gData$Sample.File.Name))
        svalue(info_lbl) <- paste(
          " ", samples, " ", strLblSamples,
          ", ", ncol(.gData), strLblColumns,
          ", ", nrow(.gData), strLblRows,
          sep = ""
        )
      } else if (any(grepl("SAMPLE", names(.gData), ignore.case = TRUE))) {
        # Get (first) column name containing "Sample".
        sampleCol <- names(.gData)[grep("Sample", names(.gData), ignore.case = TRUE)[1]]
        samples <- length(unique(.gData[sampleCol]))
        svalue(info_lbl) <- paste(
          " ", samples, " ", strLblSamples,
          ", ", ncol(.gData), strLblColumns,
          ", ", nrow(.gData), strLblRows,
          sep = ""
        )
      } else {
        svalue(info_lbl) <- paste(
          " <NA> ", strLblSamples,
          ", ", ncol(.gData), strLblColumns,
          ", ", nrow(.gData), strLblRows,
          sep = ""
        )
      }

      # Result name.
      svalue(save_edt) <- paste(val_obj, "_trim", sep = "")
    } else {

      # Reset components.
      .gData <<- data.frame(Sample.Name = "NA")
      .gDataName <<- NULL
      svalue(sample_edt) <- ""
      svalue(column_edt) <- ""
      .refresh_samples_tbl()
      .refresh_columns_tbl()
      svalue(info_lbl) <- paste(" ", "<NA> ", strLblSamples, ", ",
        "<NA> ", strLblColumns, ", ",
        "<NA> ", strLblRows,
        sep = ""
      )
      svalue(save_edt) <- ""
    }
  })

  # SAMPLES ###################################################################

  if (debug) {
    print("SAMPLES")
  }

  sample_f <- gframe(strFrmSamples,
    horizontal = FALSE, container = g1,
    expand = TRUE, fill = TRUE
  )


  sample_opt <- gradio(
    items = c(strRadKeep, strRadRemove),
    selected = 1,
    horizontal = FALSE,
    container = sample_f
  )

  sample_lbl <- glabel(
    text = strLblSelectedSamples,
    container = sample_f,
    anchor = c(-1, 0)
  )

  sample_edt <- gedit(
    initial.msg = strEdtMsg,
    width = 40,
    container = sample_f
  )

  if ("SAMPLE.NAME" %in% toupper(names(.gData))) {
    df_items <- data.frame(
      Samples = unique(.gData$Sample.Name),
      stringsAsFactors = FALSE
    )
  } else if ("SAMPLE.FILE.NAME" %in% toupper(names(.gData))) {
    df_items <- data.frame(
      Samples = unique(.gData$Sample.File.Name),
      stringsAsFactors = FALSE
    )
  } else if (any(grepl("SAMPLE", names(.gData), ignore.case = TRUE))) {
    # Get (first) column name containing "Sample".
    sampleCol <- names(.gData)[grep("Sample", names(.gData), ignore.case = TRUE)[1]]
    df_items <- data.frame(
      Samples = unique(.gData[sampleCol]),
      stringsAsFactors = FALSE
    )
  }
  sample_tbl <- gWidgets2::gtable(items = df_items, container = sample_f, expand = TRUE)

  # Set initial size (only height is important here).
  size(sample_tbl) <- c(100, 200)

  addDropTarget(sample_edt, handler = function(h, ...) {
    # Get values.
    drp_val <- h$dropdata
    sample_val <- svalue(h$obj)

    # Add new value to selected.
    new_val <- ifelse(nchar(sample_val) > 0, paste(sample_val, drp_val, sep = "|"), drp_val)

    # Update text box.
    svalue(h$obj) <- new_val

    # Update sample name table.
    tmp_tbl <- sample_tbl[, ] # Get all values.
    tmp_tbl <- tmp_tbl[tmp_tbl != drp_val] # Remove value added to selected.
    sample_tbl[, ] <- tmp_tbl # Update table.
  })


  # COLUMNS ###################################################################

  if (debug) {
    print("COLUMNS")
  }

  column_f <- gframe(strFrmColumns,
    horizontal = FALSE,
    container = g1,
    expand = TRUE,
    fill = TRUE
  )

  column_opt <- gradio(
    items = c(strRadKeep, strRadRemove),
    selected = 1,
    horizontal = FALSE,
    container = column_f
  )

  column_lbl <- glabel(
    text = strLblSelectedColumns,
    container = column_f,
    anchor = c(-1, 0)
  )

  column_edt <- gedit(
    initial.msg = strEdtMsg,
    width = 40,
    container = column_f
  )


  column_tbl <- gWidgets2::gtable(
    items = names(.gData),
    container = column_f,
    expand = TRUE
  )

  addDropTarget(column_edt, handler = function(h, ...) {
    # Get values.
    drp_val <- h$dropdata
    column_val <- svalue(h$obj)

    # Add new value to selected.
    new_val <- ifelse(nchar(column_val) > 0,
      paste(column_val, drp_val, sep = "|"),
      drp_val
    )

    # Update text box.
    svalue(h$obj) <- new_val

    # Update column name table.
    tmp_tbl <- column_tbl[, ] # Get all values.
    tmp_tbl <- tmp_tbl[tmp_tbl != drp_val] # Remove value added to selected.
    column_tbl[, ] <- tmp_tbl # Update table.
  })

  # OPTIONS ###################################################################

  option_f <- gframe(strFrmOptions,
    horizontal = FALSE,
    container = g1,
    expand = TRUE,
    fill = TRUE
  )

  empty_chk <- gcheckbox(
    text = strChkColumns,
    checked = TRUE,
    container = option_f
  )

  na_chk <- gcheckbox(
    text = strChkNA,
    checked = TRUE,
    container = option_f
  )

  word_chk <- gcheckbox(
    text = strChkWord,
    checked = FALSE,
    container = option_f
  )

  case_chk <- gcheckbox(
    text = strChkIgnore,
    checked = TRUE,
    container = option_f
  )

  glabel(
    text = strLblReplace,
    container = option_f
  )
  na_edt <- gedit(
    text = "NA",
    container = option_f
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = g2)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  trim_btn <- gbutton(text = strBtnTrim, container = g2)

  addHandlerChanged(trim_btn, handler = function(h, ...) {

    # Get new dataset name.
    val_name <- svalue(save_edt)

    if (nchar(val_name) > 0) {

      # Get values.
      val_data <- .gData
      val_data_name <- .gDataName
      sample_val <- svalue(sample_edt)
      column_val <- svalue(column_edt)
      word_val <- svalue(word_chk)
      case_val <- svalue(case_chk)
      sample_opt_val <- if (svalue(sample_opt, index = TRUE) == 1) {
        FALSE
      } else {
        TRUE
      }
      column_opt_val <- if (svalue(column_opt, index = TRUE) == 1) {
        FALSE
      } else {
        TRUE
      }
      na_val <- svalue(na_chk)
      empty_val <- svalue(empty_chk)
      na_edt_val <- svalue(na_edt)

      # NA can't be string.
      if (na_edt_val == "NA") {
        na_edt_val <- NA
      }

      # Empty string -> NULL.
      if (sample_val == "") {
        sample_val <- NULL
      }

      # Empty string -> NULL.
      if (column_val == "") {
        column_val <- NULL
      }

      if (debug) {
        print("val_data")
        print(names(val_data))
        print("sample_val")
        print(sample_val)
        print("column_val")
        print(column_val)
        print("word_val")
        print(word_val)
        print("case_val")
        print(case_val)
        print("sample_opt_val")
        print(sample_opt_val)
        print("column_opt_val")
        print(column_opt_val)
        print("na_val")
        print(na_val)
        print("empty_val")
        print(empty_val)
        print("na_edt_val")
        print(na_edt_val)
      }

      # Change button.
      blockHandlers(trim_btn)
      svalue(trim_btn) <- strBtnProcessing
      unblockHandlers(trim_btn)
      enabled(trim_btn) <- FALSE

      datanew <- trim(
        data = val_data, samples = sample_val, columns = column_val,
        word = word_val, ignore.case = case_val, invert.s = sample_opt_val, invert.c = column_opt_val,
        rm.na.col = na_val, rm.empty.col = empty_val, missing = na_edt_val, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "samples", "columns", "word", "ignore.case",
        "invert.s", "invert.c", "rm.na.col", "rm.empty.col",
        "missing"
      )

      values <- list(
        val_data_name, sample_val, column_val, word_val, case_val,
        sample_opt_val, column_opt_val, na_val, empty_val,
        na_edt_val
      )

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strMsgName,
        title = strMsgTitleError,
        icon = "error",
        parent = w
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .refresh_samples_tbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    if (any(grepl("SAMPLE", names(.gData), ignore.case = TRUE))) {

      # Refresh widget by removing it and...
      delete(sample_f, sample_tbl)

      if ("SAMPLE.NAME" %in% toupper(names(.gData))) {
        df_items <- data.frame(
          Samples = unique(.gData$Sample.Name),
          stringsAsFactors = FALSE
        )
      } else if ("SAMPLE.FILE.NAME" %in% toupper(names(.gData))) {
        df_items <- data.frame(
          Samples = unique(.gData$Sample.File.Name),
          stringsAsFactors = FALSE
        )
      } else if (any(grepl("SAMPLE", names(.gData), ignore.case = TRUE))) {
        # Get (first) column name containing "Sample".
        sampleCol <- names(.gData)[grep("Sample", names(.gData), ignore.case = TRUE)[1]]
        df_items <- data.frame(
          Samples = unique(.gData[sampleCol]),
          stringsAsFactors = FALSE
        )
      }

      # ...creating a new table.
      sample_tbl <<- gWidgets2::gtable(items = df_items, container = sample_f, expand = TRUE)

      addDropSource(sample_tbl, handler = function(h, ...) svalue(h$obj))

      addHandlerDoubleclick(sample_tbl, handler = function(h, ...) {

        # Get values.
        tbl_val <- svalue(h$obj)
        sample_val <- svalue(sample_edt)

        # Add new value to selected.
        new_val <- ifelse(nchar(sample_val) > 0,
          paste(sample_val, tbl_val, sep = "|"),
          tbl_val
        )

        # Update text box.
        svalue(sample_edt) <- new_val


        # Update sample name table.
        tmp_tbl <- sample_tbl[, ] # Get all values.
        tmp_tbl <- tmp_tbl[tmp_tbl != tbl_val] # Remove value added to selected.
        sample_tbl[, ] <- tmp_tbl # Update table.
      })
    }
  }

  .refresh_columns_tbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Refresh widget by removing it and...
    delete(column_f, column_tbl)

    # ...creating a new table.
    column_tbl <<- gWidgets2::gtable(
      items = names(.gData),
      container = column_f,
      expand = TRUE
    )

    addDropSource(column_tbl, handler = function(h, ...) svalue(h$obj))

    addHandlerDoubleclick(column_tbl, handler = function(h, ...) {

      # Get values.
      tbl_val <- svalue(h$obj)
      column_val <- svalue(column_edt)

      # Add new value to selected.
      new_val <- ifelse(nchar(column_val) > 0,
        paste(column_val, tbl_val, sep = "|"),
        tbl_val
      )

      # Update text box.
      svalue(column_edt) <- new_val

      # Update column name table.
      tmp_tbl <- column_tbl[, ] # Get all values.
      tmp_tbl <- tmp_tbl[tmp_tbl != tbl_val] # Remove value added to selected.
      column_tbl[, ] <- tmp_tbl # Update table.
    })
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
      if (exists(".strvalidator_trim_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_trim_gui_savegui", envir = env)
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
      if (exists(".strvalidator_trim_gui_sample_option", envir = env, inherits = FALSE)) {
        svalue(sample_opt) <- get(".strvalidator_trim_gui_sample_option", envir = env)
      }
      if (exists(".strvalidator_trim_gui_column_option", envir = env, inherits = FALSE)) {
        svalue(column_opt) <- get(".strvalidator_trim_gui_column_option", envir = env)
      }
      if (exists(".strvalidator_trim_gui_remove_empty", envir = env, inherits = FALSE)) {
        svalue(empty_chk) <- get(".strvalidator_trim_gui_remove_empty", envir = env)
      }
      if (exists(".strvalidator_trim_gui_remove_na", envir = env, inherits = FALSE)) {
        svalue(na_chk) <- get(".strvalidator_trim_gui_remove_na", envir = env)
      }
      if (exists(".strvalidator_trim_gui_add_word", envir = env, inherits = FALSE)) {
        svalue(word_chk) <- get(".strvalidator_trim_gui_add_word", envir = env)
      }
      if (exists(".strvalidator_trim_gui_ignore_case", envir = env, inherits = FALSE)) {
        svalue(case_chk) <- get(".strvalidator_trim_gui_ignore_case", envir = env)
      }
      if (exists(".strvalidator_trim_gui_replace_na", envir = env, inherits = FALSE)) {
        svalue(na_edt) <- get(".strvalidator_trim_gui_replace_na", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_trim_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_trim_gui_sample_option", value = svalue(sample_opt), envir = env)
      assign(x = ".strvalidator_trim_gui_column_option", value = svalue(column_opt), envir = env)
      assign(x = ".strvalidator_trim_gui_remove_empty", value = svalue(empty_chk), envir = env)
      assign(x = ".strvalidator_trim_gui_remove_na", value = svalue(na_chk), envir = env)
      assign(x = ".strvalidator_trim_gui_add_word", value = svalue(word_chk), envir = env)
      assign(x = ".strvalidator_trim_gui_ignore_case", value = svalue(case_chk), envir = env)
      assign(x = ".strvalidator_trim_gui_replace_na", value = svalue(na_edt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_trim_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_trim_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_trim_gui_sample_option", envir = env, inherits = FALSE)) {
        remove(".strvalidator_trim_gui_sample_option", envir = env)
      }
      if (exists(".strvalidator_trim_gui_column_option", envir = env, inherits = FALSE)) {
        remove(".strvalidator_trim_gui_column_option", envir = env)
      }
      if (exists(".strvalidator_trim_gui_remove_empty", envir = env, inherits = FALSE)) {
        remove(".strvalidator_trim_gui_remove_empty", envir = env)
      }
      if (exists(".strvalidator_trim_gui_remove_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_trim_gui_remove_na", envir = env)
      }
      if (exists(".strvalidator_trim_gui_add_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_trim_gui_add_word", envir = env)
      }
      if (exists(".strvalidator_trim_gui_ignore_case", envir = env, inherits = FALSE)) {
        remove(".strvalidator_trim_gui_ignore_case", envir = env)
      }
      if (exists(".strvalidator_trim_gui_replace_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_trim_gui_replace_na", envir = env)
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
