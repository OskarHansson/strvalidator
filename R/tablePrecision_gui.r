################################################################################
# CHANGE LOG (last 20 changes)
# 03.05.2020: Added language support.
# 02.05.2019: Further adjustments to tables in gui and handlers.
# 26.03.2019: Further adjustments to tables in gui and handlers.
# 24.03.2019: Improved tables with set initial height.
# 23.03.2019: Fixed save field not expanded (tcltk)
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 23.07.2018: Made 'Save as' textbox expandable.
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 07.07.2017: Replaced gWidgets:: with gWidgets2::
# 29.08.2015: Added importFrom.
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 06.02.2014: Fixed button locks when error.
# 06.02.2014: Changed name calculatePrecision_gui -> tablePrecision_gui

#' @title Table Precision
#'
#' @description
#' GUI wrapper for the \code{\link{tablePrecision}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{tablePrecision}} function by providing
#' a graphical user interface.
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
#' @importFrom utils help head str
#' @importFrom graphics title
#'
#' @seealso \code{\link{tablePrecision}}, \code{\link{checkSubset}}

tablePrecision_gui <- function(env = parent.frame(), savegui = NULL,
                               debug = FALSE, parent = NULL) {

  # Global variables.
  # NB! Omitting 'stringsAsFactors = FALSE' creates really strange behaviour.
  .gData <- data.frame(Please.select.a.dataset = NA, stringsAsFactors = FALSE)
  .gRef <- NULL
  .gDataName <- NULL
  .gRefName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate summary statistics for precision"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strRadReference <- "Filter by reference dataset"
  strRadBins <- "Filter by kit bins"
  strRadNone <- "Do not filter"
  strLblReference <- "Reference dataset:"
  strChkIgnore <- "Ignore case"
  strBtnCheck <- "Check subsetting"
  strLblKit <- "Kit:"
  strChkExclude <- "Exclude virtual bins"
  strFrmKey <- "Create key from columns"
  strEdtMessage <- "Doubleklick or drag column names to list"
  strFrmTarget <- "Calculate precision for target columns"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A dataset and a reference dataset have to be selected."
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset."
  strMsgTitleDataset <- "Datasets not selected"
  strWinTitleCheck <- "Check subsetting"
  strMsgTitleError <- "Error"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.na(dtStrings)) {
    # Get language strings, use default if not found.
    strTmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)

    strTmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)

    strTmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strTmp <- dtStrings["strFrmDataset"]$value
    strFrmDataset <- ifelse(is.na(strtmp), strFrmDataset, strtmp)

    strTmp <- dtStrings["strLblDataset"]$value
    strLblDataset <- ifelse(is.na(strtmp), strLblDataset, strtmp)

    strTmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strTmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strTmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strTmp <- dtStrings["strRadReference"]$value
    strRadReference <- ifelse(is.na(strtmp), strRadReference, strtmp)

    strTmp <- dtStrings["strRadBins"]$value
    strRadBins <- ifelse(is.na(strtmp), strRadBins, strtmp)

    strTmp <- dtStrings["strRadNone"]$value
    strRadNone <- ifelse(is.na(strtmp), strRadNone, strtmp)

    strTmp <- dtStrings["strLblReference"]$value
    strLblReference <- ifelse(is.na(strtmp), strLblReference, strtmp)

    strTmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strTmp <- dtStrings["strBtnCheck"]$value
    strBtnCheck <- ifelse(is.na(strtmp), strBtnCheck, strtmp)

    strTmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

    strTmp <- dtStrings["strChkExclude"]$value
    strChkExclude <- ifelse(is.na(strtmp), strChkExclude, strtmp)

    strTmp <- dtStrings["strFrmKey"]$value
    strFrmKey <- ifelse(is.na(strtmp), strFrmKey, strtmp)

    strTmp <- dtStrings["strEdtMessage"]$value
    strEdtMessage <- ifelse(is.na(strtmp), strEdtMessage, strtmp)

    strTmp <- dtStrings["strFrmTarget"]$value
    strFrmTarget <- ifelse(is.na(strtmp), strFrmTarget, strtmp)

    strTmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strTmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strTmp <- dtStrings["strBtnCalculate"]$value
    strBtnCalculate <- ifelse(is.na(strtmp), strBtnCalculate, strtmp)

    strTmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strTmp <- dtStrings["strMsgDataset"]$value
    strMsgDataset <- ifelse(is.na(strtmp), strMsgDataset, strtmp)

    strTmp <- dtStrings["strMsgCheck"]$value
    strMsgCheck <- ifelse(is.na(strtmp), strMsgCheck, strtmp)

    strTmp <- dtStrings["strMsgTitleDataset"]$value
    strMsgTitleDataset <- ifelse(is.na(strtmp), strMsgTitleDataset, strtmp)

    strTmp <- dtStrings["strWinTitleCheck"]$value
    strWinTitleCheck <- ifelse(is.na(strtmp), strWinTitleCheck, strtmp)

    strTmp <- dtStrings["strMsgTitleError"]$value
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

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = TRUE,
    spacing = 2,
    container = gv
  )

  g0 <- glayout(container = f0, spacing = 1)

  # Dataset -------------------------------------------------------------------

  g0[1, 1] <- glabel(text = strLblDataset, container = g0)

  dfs <- c(strDrpDataset, listObjects(env = env, obj.class = "data.frame"))

  g0[1, 2] <- g0_data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )
  g0[1, 3] <- g0_data_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = g0
  )

  addHandlerChanged(g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_data_drp)

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

      .refresh_key_tbl()
      .refresh_target_tbl()

      svalue(f1_key_txt) <- ""
      svalue(f1_target_txt) <- ""

      svalue(g0_data_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strLblSamples
      )
      svalue(save_edt) <- paste(val_obj, "_precision_stat", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(f2g2_kit_drp, index = TRUE) <- kitIndex

      # Enable buttons.
      enabled(calculate_btn) <- TRUE
    } else {

      # Reset components.
      .gData <<- data.frame(Please.select.a.dataset = "NA")
      .gDataName <<- NULL
      svalue(g0_data_drp, index = TRUE) <- 1
      svalue(g0_data_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
      .refresh_key_tbl()
      .refresh_target_tbl()
      svalue(f1_key_txt) <- ""
      svalue(f1_target_txt) <- ""
    }
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 2,
    container = gv
  )

  f2_filter_opt <- gradio(
    items = c(strRadReference, strRadBins, strRadNone),
    selected = 3,
    horizontal = FALSE,
    container = f2
  )

  addHandlerChanged(f2_filter_opt, handler = function(h, ...) {
    .updateGui()
  })


  # Reference -----------------------------------------------------------------

  f2g1 <- glayout(container = f2, spacing = 1)
  enabled(f2g1) <- FALSE

  f2g1[1, 1] <- glabel(text = strLblReference, container = f2g1)

  # NB! dfs defined in previous section.
  f2g1[2, 1] <- f2g1_ref_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = f2g1,
    ellipsize = "none"
  )

  f2g1[2, 2] <- f2g1_ref_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = f2g1
  )

  f2g1[2, 3] <- f2g1_ignore_chk <- gcheckbox(
    text = strChkIgnore,
    checked = TRUE, container = f2g1
  )


  addHandlerChanged(f2g1_ref_drp, handler = function(h, ...) {
    val_obj <- svalue(f2g1_ref_drp)

    if (exists(val_obj, envir = env, inherits = FALSE)) {
      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj

      # Check if suitable.
      requiredCol <- c("Sample.Name", "Marker", "Allele")
      ok <- checkDataset(
        name = val_obj, reqcol = requiredCol, slimcol = "Allele",
        env = env, parent = w, debug = debug
      )

      if (ok) {

        # Load or change components.
        svalue(f2g1_ref_samples_lbl) <- paste(
          length(unique(.gRef$Sample.Name)),
          strLblSamples
        )
      } else {

        # Reset components.
        .gRef <<- NULL
        svalue(f2g1_ref_drp, index = TRUE) <- 1
        svalue(f2g1_ref_samples_lbl) <- paste(" 0", strLblSamples)
      }
    } else {

      # Reset components.
      svalue(f2g1_ref_samples_lbl) <- ""
      .gRef <<- NULL
    }
  })

  # CHECK ---------------------------------------------------------------------

  f2g1[3, 1] <- f2g1_check_btn <- gbutton(
    text = strBtnCheck,
    container = f2g1
  )

  addHandlerChanged(f2g1_check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- .gDataName
    val_name_ref <- .gRefName
    val_ignore <- svalue(f2g1_ignore_chk)
    val_word <- FALSE

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = strWinTitleCheck,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- checkSubset(
        data = val_data,
        ref = val_ref,
        console = FALSE,
        ignore.case = val_ignore,
        word = val_word
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
        wrap = FALSE, container = chksubset_w
      )

      visible(chksubset_w) <- TRUE
    } else {
      gmessage(
        msg = strMsgCheck,
        title = strMsgTitleError,
        icon = "error"
      )
    }
  })

  # Kit -------------------------------------------------------------------

  f2g2 <- ggroup(horizontal = TRUE, container = f2)
  enabled(f2g2) <- FALSE

  glabel(text = strLblKit, container = f2g2)

  f2g2_kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = f2g2,
    ellipsize = "none"
  )

  f2g2_virtual_chk <- gcheckbox(
    text = strChkExclude,
    checked = TRUE,
    container = f2g2
  )

  # FRAME 1 ###################################################################

  # KEY -----------------------------------------------------------------------

  f1_key_f <- gframe(
    text = strFrmKey,
    horizontal = FALSE,
    container = gv,
    expand = TRUE,
    fill = TRUE
  )

  f1_key_txt <- gedit(
    initial.msg = strEdtMessage,
    container = f1_key_f
  )

  # Populate table.
  f1_key_tbl <- gWidgets2::gtable(
    items = names(.gData),
    container = f1_key_f,
    expand = TRUE
  )
  # Set initial table size.
  size(f1_key_tbl) <- list(height = 100, width = 350, column.widths = 350)

  # Add click handler to table widget.
  addHandlerDoubleclick(f1_key_tbl, handler = function(h, ...) {

    # Get values.
    tbl_val <- svalue(h$obj, index = FALSE)
    key_val <- svalue(f1_key_txt)

    # Check that the action was on a list element.
    if (!identical(tbl_val, character(0)) && length(tbl_val) > 0) {

      # Add new value to selected.
      new <- ifelse(nchar(key_val) > 0,
        paste(key_val, tbl_val, sep = ","),
        tbl_val
      )

      # Update text box.
      svalue(f1_key_txt) <- new

      # Update column name table.
      tmp_tbl <- f1_key_tbl[] # Get all values.

      # Remove value added to selected and Update table.
      f1_key_tbl[] <- tmp_tbl[tmp_tbl != tbl_val]
    }
  })

  # Add a drop source to the table.
  addDropSource(f1_key_tbl, handler = function(h, ...) svalue(h$obj))

  # Add the edit widget as a drop target.
  addDropTarget(f1_key_txt, handler = function(h, ...) {

    # Get values.
    drp_val <- h$dropdata
    key_val <- svalue(h$obj)

    # Check that the action was on a list element.
    if (!identical(drp_val, character(0)) && length(drp_val) > 0 && nchar(drp_val) > 0) {

      # Add new value to selected.
      new <- ifelse(nchar(key_val) > 0,
        paste(key_val, drp_val, sep = ","),
        drp_val
      )

      # Update text box.
      svalue(f1_key_txt) <- new

      # Update column name table.
      tmp_tbl <- f1_key_tbl[] # Get all values.

      # Remove value added to selected and update table.
      f1_key_tbl[] <- tmp_tbl[tmp_tbl != drp_val]
    }
  })

  # TARGET --------------------------------------------------------------------

  f1_target_f <- gframe(
    text = strFrmTarget,
    horizontal = FALSE,
    container = gv,
    expand = TRUE,
    fill = TRUE
  )

  f1_target_txt <- gedit(
    initial.msg = strEdtMessage,
    container = f1_target_f
  )

  # Populate table.
  f1_target_tbl <- gWidgets2::gtable(
    items = names(.gData),
    container = f1_target_f,
    expand = TRUE
  )
  # Set initial table size.
  size(f1_target_tbl) <- list(height = 100, width = 350, column.widths = 350)

  # Add click handler to table widget.
  addHandlerDoubleclick(f1_target_tbl, handler = function(h, ...) {

    # Get values.
    tbl_val <- svalue(h$obj)
    target_val <- svalue(f1_target_txt)

    # Check that the action was on a list element.
    if (!identical(tbl_val, character(0)) && length(tbl_val) > 0) {

      # Add new value to selected.
      new <- ifelse(nchar(target_val) > 0,
        paste(target_val, tbl_val, sep = ","),
        tbl_val
      )

      # Update text box.
      svalue(f1_target_txt) <- new

      # Update sample name table.
      tmp_tbl <- f1_target_tbl[] # Get all values.

      # Remove value added to selected and update table.
      f1_target_tbl[] <- tmp_tbl[tmp_tbl != tbl_val]
    }
  })

  # Add a drop source to the table.
  addDropSource(f1_target_tbl, handler = function(h, ...) svalue(h$obj))

  # Add the edit widget as a drop target.
  addDropTarget(f1_target_txt, handler = function(h, ...) {
    # Get values.
    drp_val <- h$dropdata
    target_val <- svalue(h$obj)

    # Check that the action was on a list element.
    if (!identical(drp_val, character(0)) && length(drp_val) > 0 && nchar(drp_val) > 0) {

      # Add new value to selected.
      new <- ifelse(nchar(target_val) > 0,
        paste(target_val, drp_val, sep = ","),
        drp_val
      )

      # Update text box.
      svalue(h$obj) <- new

      # Update column name table.
      tmp_tbl <- f1_target_tbl[] # Get all values.

      # Remove value added to selected and update table.
      f1_target_tbl[] <- tmp_tbl[tmp_tbl != drp_val]
    }
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_filter <- svalue(f2_filter_opt, index = TRUE)
    val_ignore <- svalue(f2g1_ignore_chk)
    val_key <- svalue(f1_key_txt)
    val_target <- svalue(f1_target_txt)
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- .gDataName
    val_name_ref <- .gRefName
    val_name <- svalue(save_edt)
    val_kit <- svalue(f2g2_kit_drp)
    val_exclude <- svalue(f2g2_virtual_chk)

    if (val_filter == 3) {
      # Data should not be filtered. Set ref to NA (NULL gives error message.)
      val_ref <- NA
    } else if (val_filter == 2) {
      # Filter by kit bins.

      # Get markers, bins and flag for virtual bins.
      val_ref <- getKit(kit = val_kit, what = "VIRTUAL")

      if (val_exclude) {
        # Remove virtual bins.
        val_ref <- val_ref[val_ref$Virtual == 0, ]
      }
    }

    if (debug) {
      print("Read Values:")
      print("val_filter")
      print(val_filter)
      print("val_ignore")
      print(val_ignore)
      print("val_target")
      print(val_target)
      print("val_key")
      print(val_key)
      print("val_name")
      print(val_name)
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
    }

    if (!is.null(val_data) & !is.null(val_ref)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      # Filter dataset.
      if (val_filter != 3) {
        val_data <- filterProfile(
          data = val_data, ref = val_ref,
          add.missing.loci = FALSE, keep.na = FALSE,
          ignore.case = val_ignore, debug = debug
        )
      }

      # Replace whitespace and split by comma.
      val_key <- gsub("\\s", "", val_key)
      val_key <- strsplit(val_key, ",")
      val_key <- unlist(val_key)

      # Replace whitespace and split by comma.
      val_target <- gsub("\\s", "", val_target)
      val_target <- strsplit(val_target, ",")
      val_target <- unlist(val_target)

      if (debug) {
        print("Sent Values:")
        print("val_target")
        print(val_target)
        print("val_key")
        print(val_key)
        print("val_data")
        print(head(val_data))
      }

      # Calculate precision.
      datanew <- tablePrecision(
        data = val_data,
        key = val_key,
        target = val_target,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "filter", "ref", "ignore",
        "kit", "exclude", "key", "target"
      )

      values <- list(
        val_name_data, val_filter, val_name_ref, val_ignore,
        val_kit, val_exclude, val_key, val_target
      )

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
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
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

  .updateGui <- function() {

    # Get radio button selection.
    val_opt <- svalue(f2_filter_opt, index = TRUE)

    if (val_opt == 1) {
      enabled(f2g1) <- TRUE
      enabled(f2g2) <- FALSE
    } else if (val_opt == 2) {
      enabled(f2g1) <- FALSE
      enabled(f2g2) <- TRUE
    } else {
      enabled(f2g1) <- FALSE
      enabled(f2g2) <- FALSE
    }
  }

  .refresh_target_tbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Populate table.
    f1_target_tbl[] <<- names(.gData)
  }

  .refresh_key_tbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Populate table.
    f1_key_tbl[] <<- names(.gData)
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
      if (exists(".strvalidator_tablePrecision_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_tablePrecision_gui_savegui", envir = env)
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
      if (exists(".strvalidator_tablePrecision_gui_filter", envir = env, inherits = FALSE)) {
        svalue(f2_filter_opt) <- get(".strvalidator_tablePrecision_gui_filter", envir = env)
      }
      if (exists(".strvalidator_tablePrecision_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f2g1_ignore_chk) <- get(".strvalidator_tablePrecision_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_tablePrecision_gui_exclude", envir = env, inherits = FALSE)) {
        svalue(f2g2_virtual_chk) <- get(".strvalidator_tablePrecision_gui_exclude", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_tablePrecision_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_tablePrecision_gui_filter", value = svalue(f2_filter_opt), envir = env)
      assign(x = ".strvalidator_tablePrecision_gui_ignore", value = svalue(f2g1_ignore_chk), envir = env)
      assign(x = ".strvalidator_tablePrecision_gui_exclude", value = svalue(f2g2_virtual_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_tablePrecision_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_tablePrecision_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_tablePrecision_gui_filter", envir = env, inherits = FALSE)) {
        remove(".strvalidator_tablePrecision_gui_filter", envir = env)
      }
      if (exists(".strvalidator_tablePrecision_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_tablePrecision_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_tablePrecision_gui_exclude", envir = env, inherits = FALSE)) {
        remove(".strvalidator_tablePrecision_gui_exclude", envir = env)
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
