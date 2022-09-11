################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 04.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 09.07.2018: Fixed blank drop-down menues after selecting a dataset.
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 16.06.2016: 'Save as' textbox expandable.
# 22.12.2015: First version.

#' @title Calculate Ratio
#'
#' @description
#' GUI wrapper for the \code{\link{calculateRatio}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateRatio}} function
#' by providing a graphical user interface.
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
#' @seealso \code{link{calculateRatio}}, \code{link{checkSubset}}

calculateRatio_gui <- function(env = parent.frame(), savegui = NULL,
                               debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
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
  strWinTitle <- "Calculate ratio"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefDataset <- "Reference dataset:"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strFrmOptions <- "Options"
  strLblPre <- "Pre-processing:"
  strChkOL <- "Remove off-ladder alleles"
  strLblMethod <- "Calculate marker ratio:"
  strLblNumerator <- "Select numerator markers:"
  strDrpMarker <- "<Select Marker>"
  strLblDenominator <- "Select denominator markers:"
  strLblGroupBy <- "Group by column:"
  strDrpColumn <- "<Select Columns>"
  strLblMatching <- "Reference sample name matching:"
  strChkIgnore <- "Ignore case"
  strChkWord <- "Add word boundaries"
  strChkExact <- "Exact matching"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A sample dataset and a reference dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset."
  strWinTitleCheck <- "Check subsetting"
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

    strtmp <- dtStrings["strLblRefDataset"]$value
    strLblRefDataset <- ifelse(is.na(strtmp), strLblRefDataset, strtmp)

    strtmp <- dtStrings["strLblRef"]$value
    strLblRef <- ifelse(is.na(strtmp), strLblRef, strtmp)

    strtmp <- dtStrings["strBtnCheck"]$value
    strBtnCheck <- ifelse(is.na(strtmp), strBtnCheck, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblPre"]$value
    strLblPre <- ifelse(is.na(strtmp), strLblPre, strtmp)

    strtmp <- dtStrings["strChkOL"]$value
    strChkOL <- ifelse(is.na(strtmp), strChkOL, strtmp)

    strtmp <- dtStrings["strLblMethod"]$value
    strLblMethod <- ifelse(is.na(strtmp), strLblMethod, strtmp)

    strtmp <- dtStrings["strLblNumerator"]$value
    strLblNumerator <- ifelse(is.na(strtmp), strLblNumerator, strtmp)

    strtmp <- dtStrings["strDrpMarker"]$value
    strDrpMarker <- ifelse(is.na(strtmp), strDrpMarker, strtmp)

    strtmp <- dtStrings["strLblDenominator"]$value
    strLblDenominator <- ifelse(is.na(strtmp), strLblDenominator, strtmp)

    strtmp <- dtStrings["strLblGroupBy"]$value
    strLblGroupBy <- ifelse(is.na(strtmp), strLblGroupBy, strtmp)

    strtmp <- dtStrings["strDrpColumn"]$value
    strDrpColumn <- ifelse(is.na(strtmp), strDrpColumn, strtmp)

    strtmp <- dtStrings["strLblMatching"]$value
    strLblMatching <- ifelse(is.na(strtmp), strLblMatching, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strChkWord"]$value
    strChkWord <- ifelse(is.na(strtmp), strChkWord, strtmp)

    strtmp <- dtStrings["strChkExact"]$value
    strChkExact <- ifelse(is.na(strtmp), strChkExact, strtmp)

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

    strtmp <- dtStrings["strWinTitleCheck"]$value
    strWinTitleCheck <- ifelse(is.na(strtmp), strWinTitleCheck, strtmp)

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

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # Dataset -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

  data_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = g0
  )

  dfs <- c(strDrpDataset, listObjects(env = env, obj.class = "data.frame"))

  data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Height",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # get dataset.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      svalue(data_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strLblSamples
      )
      # Update dropdown menues.
      f1_numerator_drp[, ] <- unique(c(strDrpMarker, .gData$Marker))
      f1_denominator_drp[, ] <- unique(c(strDrpMarker, .gData$Marker))
      f1_group_drp[, ] <- unique(c(strDrpColumn, names(.gData)))

      # Select default value.
      svalue(f1_numerator_drp, index = TRUE) <- 1
      svalue(f1_denominator_drp, index = TRUE) <- 1
      svalue(f1_group_drp, index = TRUE) <- 1

      # Suggest a name for the result.
      svalue(save_edt) <- paste(val_obj, "_ratio", sep = "")
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_drp, index = TRUE) <- 1
      svalue(data_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""

      # Update dropdown menues.
      f1_numerator_drp[, ] <- strDrpMarker
      f1_denominator_drp[, ] <- strDrpMarker
      f1_group_drp[, ] <- strDrpColumn

      # Select default value.
      svalue(f1_numerator_drp, index = TRUE) <- 1
      svalue(f1_denominator_drp, index = TRUE) <- 1
      svalue(f1_group_drp, index = TRUE) <- 1
    }
  })

  # Reference -----------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblRefDataset, container = g1)

  ref_samples_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = g1
  )

  # NB! dfs defined in previous section.
  ref_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(ref_drp, handler = function(h, ...) {
    val_obj <- svalue(ref_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj
      svalue(ref_samples_lbl) <- paste(
        length(unique(.gRef$Sample.Name)),
        strLblRef
      )
    } else {

      # Reset components.
      .gRef <<- NULL
      .gRefName <<- NULL
      svalue(ref_drp, index = TRUE) <- 1
      svalue(ref_samples_lbl) <- paste(" 0", strLblRef)
    }
  })

  # CHECK #####################################################################

  check_btn <- gbutton(text = strBtnCheck, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)

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

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # PRE-PROCESSING ------------------------------------------------------------

  glabel(text = strLblPre, anchor = c(-1, 0), container = f1)

  f1_ol_chk <- gcheckbox(
    text = strChkOL, checked = TRUE,
    container = f1
  )

  # METHOD --------------------------------------------------------------------

  glabel(text = strLblMethod, anchor = c(-1, 0), container = f1)

  f1g1 <- glayout(container = f1)

  f1g1[1, 1] <- glabel(text = strLblNumerator, container = f1g1)
  f1g1[1, 2] <- f1_numerator_drp <- gcombobox(
    items = strDrpMarker,
    container = f1g1, ellipsize = "none"
  )
  f1g1[2, 1:2] <- f1_numerator_edt <- gedit(text = "", container = f1g1)

  f1g1[3, 1] <- glabel(text = strLblDenominator, container = f1g1)
  f1g1[3, 2] <- f1_denominator_drp <- gcombobox(
    items = strDrpMarker,
    container = f1g1, ellipsize = "none"
  )
  f1g1[4, 1:2] <- f1_denominator_edt <- gedit(text = "", container = f1g1)

  f1g1[5, 1] <- glabel(text = strLblGroupBy, container = f1g1)
  f1g1[5, 2] <- f1_group_drp <- gcombobox(
    items = strDrpColumn,
    container = f1g1, ellipsize = "none"
  )

  addHandlerChanged(f1_numerator_drp, handler = function(h, ...) {
    val_marker <- svalue(f1_numerator_drp)
    val_value <- svalue(f1_numerator_edt)

    if (!is.null(val_marker)) {
      if (val_marker != strDrpMarker) {

        # Add new value to selected.
        if (nchar(val_value) == 0) {
          svalue(f1_numerator_edt) <- val_marker
        } else {
          svalue(f1_numerator_edt) <- paste(val_value, val_marker, sep = ",")
        }
      }
    }
  })

  addHandlerChanged(f1_denominator_drp, handler = function(h, ...) {
    val_marker <- svalue(f1_denominator_drp)
    val_value <- svalue(f1_denominator_edt)

    if (!is.null(val_marker)) {
      if (val_marker != strDrpMarker) {

        # Add new value to selected.
        if (nchar(val_value) == 0) {
          svalue(f1_denominator_edt) <- val_marker
        } else {
          svalue(f1_denominator_edt) <- paste(val_value, val_marker, sep = ",")
        }
      }
    }
  })

  # MATCHING ------------------------------------------------------------------

  glabel(text = strLblMatching, anchor = c(-1, 0), container = f1)

  f1_ignore_chk <- gcheckbox(
    text = strChkIgnore, checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = strChkWord, checked = FALSE,
    container = f1
  )

  f1_exact_chk <- gcheckbox(
    text = strChkExact, checked = FALSE,
    container = f1
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
    val_name_data <- .gDataName
    val_ref <- .gRef
    val_name_ref <- .gRefName
    val_ol <- svalue(f1_ol_chk)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    val_name <- svalue(save_edt)
    val_numerator <- svalue(f1_numerator_edt)
    val_denominator <- svalue(f1_denominator_edt)
    val_group <- svalue(f1_group_drp)

    if (val_group == strDrpColumn) {
      val_group <- NULL
    }

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
      print("val_ol")
      print(val_ol)
      print("val_ignore")
      print(val_ignore)
      print("val_word")
      print(val_word)
      print("val_exact")
      print(val_exact)
      print("val_name")
      print(val_name)
    }

    # Check if data.
    if (!is.null(.gData)) {
      if (!nchar(val_numerator) > 0) {
        val_numerator <- NULL
      } else {
        val_numerator <- unlist(strsplit(val_numerator, split = ","))
      }

      if (!nchar(val_denominator) > 0) {
        val_denominator <- NULL
      } else {
        val_denominator <- unlist(strsplit(val_denominator, split = ","))
      }

      if (debug) {
        print("Sent Values:")
        print("val_numerator")
        print(val_numerator)
        print("val_denominator")
        print(val_denominator)
        print("val_group")
        print(val_group)
      }

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strBtnProcessing
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateRatio(
        data = val_data, ref = val_ref,
        numerator = val_numerator,
        denominator = val_denominator,
        group = val_group, ol.rm = val_ol,
        ignore.case = val_ignore, word = val_word,
        exact = val_exact, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "numerator",
        "denominator", "group", "ol", "ignore.case", "word",
        "exact"
      )

      values <- list(
        val_name_data, val_name_ref, val_numerator,
        val_denominator, val_group, val_ol, val_ignore, val_word,
        val_exact
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
        print(head(datanew))
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
      if (exists(".strvalidator_calculateRatio_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateRatio_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateRatio_gui_numerator", envir = env, inherits = FALSE)) {
        svalue(f1_numerator_edt) <- get(".strvalidator_calculateRatio_gui_numerator", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_denominator", envir = env, inherits = FALSE)) {
        svalue(f1_denominator_edt) <- get(".strvalidator_calculateRatio_gui_denominator", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_ol", envir = env, inherits = FALSE)) {
        svalue(f1_ol_chk) <- get(".strvalidator_calculateRatio_gui_ol", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculateRatio_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_calculateRatio_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_calculateRatio_gui_exact", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateRatio_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_numerator", value = svalue(f1_numerator_edt), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_denominator", value = svalue(f1_denominator_edt), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_exact", value = svalue(f1_exact_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateRatio_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_numerator", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_numerator", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_denominator", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_denominator", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_exact", envir = env)
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
