################################################################################
# CHANGE LOG (last 20 changes)
# 03.05.2020: Added language support.
# 02.03.2019: Fixed expansion of widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 07.07.2017: Replaced gWidgets:: with gWidgets2::
# 24.06.2016: 'Save as' textbox expandable.
# 06.01.2016: Added attributes to result.
# 29.08.2015: Added importFrom.
# 07.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 02.12.2013: Fixed 'Option' frame not visible.
# 20.11.2013: Specified package for function 'gtable' -> 'gWidgets::gtable'
# 06.08.2013: Added rows and columns to info.
# 18.07.2013: Check before overwrite object.
# 16.07.2013: Added save GUI settings.

#' @title Slim Data Frames
#'
#' @description
#' GUI wrapper for the \code{\link{slim}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{slim}} function by providing a graphical
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
#' @seealso \code{\link{slim}}

slim_gui <- function(env = parent.frame(), savegui = NULL,
                     debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- data.frame(No.Data = NA)
  .gDataName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Slim dataset"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblColumns <- "columns"
  strLblRows <- "rows"
  strFrmOptions <- "Options"
  strChkKeep <- "Keep rows in fixed columns even if no data in stacked columns"
  strLblKeep <- "(i.e. keep one row per marker for each sample even if no peak)"
  strTipColumns <- "Tip: Manually edit the columns to fix and stack (e.g. stack='Allele' will stack 'Allele.1', 'Allele.2'...)"
  strFrmFix <- "Fix"
  strLblFix <- "Columns to keep fixed (separate by pipe |):"
  strEdtMsg <- "Doubleklick or drag column names to list"
  strFrmStack <- "Stack"
  strLblStack <- "Columns to stack (separate by pipe |):"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strMsgName <- "A file name must be provided!"
  strBtnSlim <- "Slim dataset"
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

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkKeep"]$value
    strChkKeep <- ifelse(is.na(strtmp), strChkKeep, strtmp)

    strtmp <- dtStrings["strLblKeep"]$value
    strLblKeep <- ifelse(is.na(strtmp), strLblKeep, strtmp)

    strtmp <- dtStrings["strTipColumns"]$value
    strTipColumns <- ifelse(is.na(strtmp), strTipColumns, strtmp)

    strtmp <- dtStrings["strFrmFix"]$value
    strFrmFix <- ifelse(is.na(strtmp), strFrmFix, strtmp)

    strtmp <- dtStrings["strLblFix"]$value
    strLblFix <- ifelse(is.na(strtmp), strLblFix, strtmp)

    strtmp <- dtStrings["strEdtMsg"]$value
    strEdtMsg <- ifelse(is.na(strtmp), strEdtMsg, strtmp)

    strtmp <- dtStrings["strFrmStack"]$value
    strFrmStack <- ifelse(is.na(strtmp), strFrmStack, strtmp)

    strtmp <- dtStrings["strLblStack"]$value
    strLblStack <- ifelse(is.na(strtmp), strLblStack, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strMsgName"]$value
    strMsgName <- ifelse(is.na(strtmp), strMsgName, strtmp)

    strtmp <- dtStrings["strBtnSlim"]$value
    strBtnSlim <- ifelse(is.na(strtmp), strBtnSlim, strtmp)

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
    horizontal = FALSE,
    spacing = 5,
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
    spacing = 2,
    use.scrollwindow = FALSE,
    container = gv,
    expand = FALSE,
    fill = TRUE
  )

  # Horizontal sub group.
  g1 <- ggroup(
    horizontal = TRUE,
    spacing = 2,
    use.scrollwindow = FALSE,
    container = gv,
    expand = TRUE,
    fill = TRUE
  )

  # Vertical sub group.
  g2 <- ggroup(
    horizontal = FALSE,
    spacing = 2,
    use.scrollwindow = FALSE,
    container = gv,
    expand = FALSE,
    fill = TRUE
  )


  # DATASET ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 2,
    container = g0
  )

  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1, 1] <- glabel(text = strLblDataset, container = f0g0)

  f0g0[1, 2] <- dataset_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none"
  )

  f0g0[1, 3] <- f0_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f0g0
  )
  f0g0[1, 4] <- f0_columns_lbl <- glabel(
    text = paste(" 0", strLblColumns),
    container = f0g0
  )
  f0g0[1, 5] <- f0_rows_lbl <- glabel(
    text = paste(" 0", strLblRows),
    container = f0g0
  )


  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      .refresh_fix_tbl()
      .refresh_stack_tbl()

      samples <- length(unique(.gData$Sample.Name))
      # Info.
      if ("Sample.Name" %in% names(.gData)) {
        samples <- length(unique(.gData$Sample.Name))
        svalue(f0_samples_lbl) <- paste(" ", samples, " ",
          strLblSamples, ", ",
          sep = ""
        )
      } else {
        svalue(f0_samples_lbl) <- paste(" ", "<NA> ",
          strLblSamples, ", ",
          sep = ""
        )
      }
      svalue(f0_columns_lbl) <- paste(" ", ncol(.gData), " ",
        strLblColumns, ", ",
        sep = ""
      )
      svalue(f0_rows_lbl) <- paste(" ", nrow(.gData), " ",
        strLblRows, ", ",
        sep = ""
      )
      # Result name.
      svalue(save_edt) <- paste(val_obj, "_slim", sep = "")

      # Guess column names to keep fixed.
      svalue(fix_edt) <- colNames(
        data = .gData, slim = TRUE,
        numbered = TRUE, concatenate = "|"
      )

      # Guess column names to stack.
      svalue(stack_edt) <- colNames(
        data = .gData, slim = FALSE,
        numbered = TRUE, concatenate = "|"
      )

      # Reset button.
      svalue(slim_btn) <- strBtnSlim
      enabled(slim_btn) <- TRUE
    } else {

      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(fix_edt) <- ""
      svalue(stack_edt) <- ""
      .refresh_fix_tbl()
      .refresh_stack_tbl()
      svalue(f0_samples_lbl) <- paste(" ", "<NA> ",
        strLblSamples, ", ",
        sep = ""
      )
      svalue(f0_columns_lbl) <- paste(" ", "<NA> ",
        strLblColumns, ", ",
        sep = ""
      )
      svalue(f0_rows_lbl) <- paste(" ", "<NA> ",
        strLblRows, ", ",
        sep = ""
      )
      svalue(save_edt) <- ""
    }
  })

  # FIX #######################################################################

  if (debug) {
    print("COLUMNS")
    print(names(.gData))
  }

  fix_f <- gframe(
    text = strFrmFix,
    horizontal = FALSE, container = g1,
    expand = TRUE, fill = TRUE
  )

  fix_lbl <- glabel(
    text = strLblFix,
    container = fix_f,
    anchor = c(-1, 0)
  )

  fix_edt <- gedit(
    initial.msg = strEdtMsg,
    width = 40,
    container = fix_f
  )
  tooltip(fix_edt) <- strTipColumns

  fix_tbl <- gWidgets2::gtable(
    items = names(.gData),
    container = fix_f,
    expand = TRUE
  )

  # Set initial size (only height is important here).
  size(fix_tbl) <- c(100, 200)

  addDropTarget(fix_edt, handler = function(h, ...) {
    if (debug) {
      print("SAMPLES:DROPTARGET")
    }

    # Get values.
    drp_val <- h$dropdata
    fix_val <- svalue(h$obj)

    # Add new value to selected.
    new <- ifelse(nchar(fix_val) > 0,
      paste(fix_val, drp_val, sep = "|"),
      drp_val
    )

    # Update text box.
    svalue(h$obj) <- new

    # Update sample name table.
    tmp_tbl <- fix_tbl[, ] # Get all values.
    print(tmp_tbl)
    tmp_tbl <- tmp_tbl[tmp_tbl != drp_val] # Remove value added to selected.
    fix_tbl[, ] <- tmp_tbl # Update table.
  })

  # STACK #####################################################################

  if (debug) {
    print("STACK")
  }

  stack_f <- gframe(
    text = strFrmStack,
    horizontal = FALSE, container = g1,
    expand = TRUE, fill = TRUE
  )

  stack_lbl <- glabel(
    text = strLblStack,
    container = stack_f,
    anchor = c(-1, 0)
  )

  stack_edt <- gedit(
    initial.msg = strEdtMsg,
    width = 40,
    container = stack_f
  )
  tooltip(stack_edt) <- strTipColumns

  stack_tbl <- gWidgets2::gtable(
    items = names(.gData),
    container = stack_f,
    expand = TRUE
  )

  addDropTarget(stack_edt, handler = function(h, ...) {
    # Get values.
    drp_val <- h$dropdata
    stack_val <- svalue(h$obj)

    # Add new value to selected.
    new <- ifelse(nchar(stack_val) > 0,
      paste(stack_val, drp_val, sep = "|"),
      drp_val
    )

    # Update text box.
    svalue(h$obj) <- new

    # Update column name table.
    tmp_tbl <- stack_tbl[, ] # Get all values.
    print(tmp_tbl)
    tmp_tbl <- tmp_tbl[tmp_tbl != drp_val] # Remove value added to selected.
    stack_tbl[, ] <- tmp_tbl # Update table.
  })

  # OPTIONS ###################################################################

  option_frm <- gframe(text = strFrmOptions, horizontal = FALSE, container = g0)

  keep_chk <- gcheckbox(text = strChkKeep, checked = TRUE, container = option_frm)

  glabel(text = strLblKeep, container = option_frm, anchor = c(-1, 0))

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = g2)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  slim_btn <- gbutton(text = strBtnSlim, container = g2)

  addHandlerClicked(slim_btn, handler = function(h, ...) {

    # Get new dataset name.
    val_name <- svalue(save_edt)
    val_data <- .gData
    val_data_name <- .gDataName

    if (nchar(val_name) > 0) {

      # Get values.
      fix_val <- svalue(fix_edt)
      stack_val <- svalue(stack_edt)
      keep_val <- svalue(keep_chk)

      # Slim require a vector of strings.
      fix_val <- unlist(strsplit(fix_val, "|", fixed = TRUE))
      stack_val <- unlist(strsplit(stack_val, "|", fixed = TRUE))

      if (debug) {
        print("val_data")
        print(names(val_data))
        print("fix_val")
        print(fix_val)
        print("stack_val")
        print(stack_val)
        print("keep_val")
        print(keep_val)
      }

      # Change button.
      blockHandlers(slim_btn)
      svalue(slim_btn) <- strBtnProcessing
      unblockHandlers(slim_btn)
      enabled(slim_btn) <- FALSE

      datanew <- slim(
        data = val_data, fix = fix_val, stack = stack_val,
        keep.na = keep_val, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "fix", "stack", "keep.na")

      values <- list(val_data_name, fix_val, stack_val, keep_val)

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(paste("EXIT:", fnc))
      }

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

  .refresh_fix_tbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Refresh widget by removing it and...
    delete(fix_f, fix_tbl)

    # ...creating a new table.
    fix_tbl <<- gWidgets2::gtable(
      items = names(.gData),
      container = fix_f,
      expand = TRUE
    )

    addDropSource(fix_tbl, handler = function(h, ...) svalue(h$obj))

    addHandlerDoubleclick(fix_tbl, handler = function(h, ...) {

      # Get values.
      tbl_val <- svalue(h$obj)
      fix_val <- svalue(fix_edt)

      # Add new value to selected.
      new <- ifelse(nchar(fix_val) > 0,
        paste(fix_val, tbl_val, sep = "|"),
        tbl_val
      )

      # Update text box.
      svalue(fix_edt) <- new

      # Update sample name table.
      tmp_tbl <- fix_tbl[, ] # Get all values.
      tmp_tbl <- tmp_tbl[tmp_tbl != tbl_val] # Remove value added to selected.
      fix_tbl[, ] <- tmp_tbl # Update table.
    })
  }

  .refresh_stack_tbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Refresh widget by removing it and...
    delete(stack_f, stack_tbl)

    # ...creating a new table.
    stack_tbl <<- gWidgets2::gtable(
      items = names(.gData),
      container = stack_f,
      expand = TRUE
    )

    addDropSource(stack_tbl, handler = function(h, ...) svalue(h$obj))

    addHandlerDoubleclick(stack_tbl, handler = function(h, ...) {

      # Get values.
      tbl_val <- svalue(h$obj)
      stack_val <- svalue(stack_edt)

      # Add new value to selected.
      new <- ifelse(nchar(stack_val) > 0,
        paste(stack_val, tbl_val, sep = "|"),
        tbl_val
      )

      # Update text box.
      svalue(stack_edt) <- new

      # Update column name table.
      tmp_tbl <- stack_tbl[, ] # Get all values.
      tmp_tbl <- tmp_tbl[tmp_tbl != tbl_val] # Remove value added to selected.
      stack_tbl[, ] <- tmp_tbl # Update table.
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
      if (exists(".strvalidator_slim_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_slim_gui_savegui", envir = env)
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
      if (exists(".strvalidator_slim_gui_title", envir = env, inherits = FALSE)) {
        svalue(keep_chk) <- get(".strvalidator_slim_gui_title", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_slim_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_slim_gui_title", value = svalue(keep_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_slim_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_slim_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_slim_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_slim_gui_title", envir = env)
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
} # End of GUI
