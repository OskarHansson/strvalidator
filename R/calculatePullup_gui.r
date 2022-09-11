################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 04.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 10.05.2016: Added new option 'limit' to remove high ratios from the result.
# 10.05.2016: Added attributes to result.
# 10.05.2016: 'Save as' textbox expandable.
# 28.08.2015: Added importFrom.
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 13.12.2014: Added kit dropdown and kit attribute to result.
# 04.12.2014: First version.

#' @title Calculate Spectral Pull-up
#'
#' @description
#' GUI wrapper for the \code{\link{calculatePullup}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculatePullup}} function by
#' providing a graphical user interface.
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
#' @seealso \code{\link{calculatePullup}}, \code{\link{checkSubset}}
#'


calculatePullup_gui <- function(env = parent.frame(), savegui = NULL,
                                debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gRef <- NULL
  .gRefName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Calculate spectral pull-up"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDefault <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefDataset <- "Reference dataset:"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strFrmOptions <- "Options"
  strLblPre <- "Pre-processing:"
  strChkOL <- "Remove off-ladder peaks"
  strLblMethod <- "Pull-up settings:"
  strLblRange <- "Pull-up analysis range (data points) around known alleles:"
  strLblBlock <- "Blocking range (data points) around known alleles:"
  strLblDiscard <- "Discard pull-ups with ratio >"
  strLblMatching <- "Reference sample name matching:"
  strChkIgnore <- "Ignore case"
  strChkWord <- "Add word boundaries"
  strLblPost <- "Post-processing:"
  strChkDiscard <- "Discard alleles with no pull-up from the result table"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strLblKit <- "Kit attribute:"
  strBtnCalculate <- "Calculate"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A sample dataset and a reference dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
  strMsgCheck <- "Data frame is NULL!\n\nMake sure to select a sample dataset."
  strWinTitleCheck <- "Check subsetting"
  strMsgTitleError <- "Error"
  strMsgNA <- "'NA' in 'Dye' column. \nUse add dye function to fix."
  strMsgTitleNA <- "NA detected!"

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

    strtmp <- dtStrings["strDrpDefault"]$value
    strDrpDefault <- ifelse(is.na(strtmp), strDrpDefault, strtmp)

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

    strtmp <- dtStrings["strLblRange"]$value
    strLblRange <- ifelse(is.na(strtmp), strLblRange, strtmp)

    strtmp <- dtStrings["strLblBlock"]$value
    strLblBlock <- ifelse(is.na(strtmp), strLblBlock, strtmp)

    strtmp <- dtStrings["strLblDiscard"]$value
    strLblDiscard <- ifelse(is.na(strtmp), strLblDiscard, strtmp)

    strtmp <- dtStrings["strLblMatching"]$value
    strLblMatching <- ifelse(is.na(strtmp), strLblMatching, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strChkWord"]$value
    strChkWord <- ifelse(is.na(strtmp), strChkWord, strtmp)

    strtmp <- dtStrings["strLblPost"]$value
    strLblPost <- ifelse(is.na(strtmp), strLblPost, strtmp)

    strtmp <- dtStrings["strChkDiscard"]$value
    strChkDiscard <- ifelse(is.na(strtmp), strChkDiscard, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

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

    strtmp <- dtStrings["strMsgNA"]$value
    strMsgNA <- ifelse(is.na(strtmp), strMsgNA, strtmp)

    strtmp <- dtStrings["strMsgTitleNA"]$value
    strMsgTitleNA <- ifelse(is.na(strtmp), strMsgTitleNA, strtmp)
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

  dfs <- c(strDrpDefault, listObjects(env = env, obj.class = "data.frame"))

  g0_data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Allele", "Marker", "Dye", "Height", "Size", "Data.Point")
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

      # Suggest a name for result.
      svalue(save_edt) <- paste(val_obj, "_pullup", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(g0_data_drp, index = TRUE) <- 1
      svalue(data_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
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
    text = strChkOL,
    checked = FALSE,
    container = f1
  )

  # METHOD --------------------------------------------------------------------

  glabel(text = strLblMethod, anchor = c(-1, 0), container = f1)

  f1g1 <- glayout(container = f1, anchor = c(-1, 0), spacing = 1)

  f1g1[1, 1] <- glabel(text = strLblRange, anchor = c(-1, 0), container = f1g1)
  f1g1[1, 2] <- f1_pullup_spb <- gspinbutton(from = 0, to = 1000, by = 10, value = 6, container = f1g1)

  f1g1[2, 1] <- glabel(text = strLblBlock, anchor = c(-1, 0), container = f1g1)
  f1g1[2, 2] <- f1_block_spb <- gspinbutton(from = 0, to = 1000, by = 10, value = 70, container = f1g1)

  f1g1[3, 1] <- glabel(text = strLblDiscard, anchor = c(-1, 0), container = f1g1)
  f1g1[3, 2] <- f1_limit_spb <- gspinbutton(from = 0, to = 10, by = 0.1, value = 1, container = f1g1)

  # MATCHING ------------------------------------------------------------------

  glabel(text = strLblMatching, anchor = c(-1, 0), container = f1)

  f1_ignore_chk <- gcheckbox(
    text = strChkIgnore,
    checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = strChkWord,
    checked = FALSE,
    container = f1
  )

  # POST-PROCESSING -----------------------------------------------------------

  glabel(text = strLblPost, anchor = c(-1, 0), container = f1)

  f1_discard_chk <- gcheckbox(
    text = strChkDiscard,
    checked = FALSE,
    container = f1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  glabel(text = strLblKit, container = save_frame)

  kit_drp <- gcombobox(
    items = getKit(), container = save_frame,
    ellipsize = "none"
  )

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- .gDataName
    val_name_ref <- .gRefName
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_ol <- svalue(f1_ol_chk)
    val_pullup <- svalue(f1_pullup_spb)
    val_block <- svalue(f1_block_spb)
    val_limit <- svalue(f1_limit_spb)
    val_discard <- svalue(f1_discard_chk)
    val_name <- svalue(save_edt)
    val_kit <- svalue(kit_drp)

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
      print("val_ignore")
      print(val_ignore)
      print("val_word")
      print(val_word)
      print("val_ol")
      print(val_ol)
      print("val_pullup")
      print(val_pullup)
      print("val_block")
      print(val_block)
      print("val_limit")
      print(val_limit)
      print("val_name")
      print(val_name)
    }

    # Check if data.
    if (!is.null(.gData) & !is.null(.gRef)) {

      # Check for NA's in dye column.
      if (!any(is.na(.gData$Dye))) {

        # Change button.
        blockHandlers(calculate_btn)
        svalue(calculate_btn) <- strBtnProcessing
        unblockHandlers(calculate_btn)
        enabled(calculate_btn) <- FALSE

        datanew <- calculatePullup(
          data = val_data,
          ref = val_ref,
          pullup.range = val_pullup,
          block.range = val_block,
          ol.rm = val_ol,
          ignore.case = val_ignore,
          word = val_word,
          discard = val_discard,
          limit = val_limit,
          debug = debug
        )

        # Add attributes to result.
        attr(datanew, which = "kit") <- val_kit

        # Create key-value pairs to log.
        keys <- list(
          "data", "ref", "pullup.range", "block.range", "ol.rm",
          "ignore.case", "word", "discard", "limit"
        )

        values <- list(
          val_name_data, val_name_ref, val_pullup, val_block, val_ol,
          val_ignore, val_word, val_discard, val_limit
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
          msg = strMsgNA,
          title = strMsgTitleNA,
          icon = "error",
          parent = w
        )
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
      if (exists(".strvalidator_calculatePullup_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculatePullup_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculatePullup_gui_window", envir = env, inherits = FALSE)) {
        svalue(f1_pullup_spb) <- get(".strvalidator_calculatePullup_gui_window", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_block", envir = env, inherits = FALSE)) {
        svalue(f1_block_spb) <- get(".strvalidator_calculatePullup_gui_block", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_limit", envir = env, inherits = FALSE)) {
        svalue(f1_limit_spb) <- get(".strvalidator_calculatePullup_gui_limit", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_ol", envir = env, inherits = FALSE)) {
        svalue(f1_ol_chk) <- get(".strvalidator_calculatePullup_gui_ol", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculatePullup_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_calculatePullup_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_discard", envir = env, inherits = FALSE)) {
        svalue(f1_discard_chk) <- get(".strvalidator_calculatePullup_gui_discard", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculatePullup_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculatePullup_gui_window", value = svalue(f1_pullup_spb), envir = env)
      assign(x = ".strvalidator_calculatePullup_gui_block", value = svalue(f1_block_spb), envir = env)
      assign(x = ".strvalidator_calculatePullup_gui_limit", value = svalue(f1_limit_spb), envir = env)
      assign(x = ".strvalidator_calculatePullup_gui_ol", value = svalue(f1_ol_chk), envir = env)
      assign(x = ".strvalidator_calculatePullup_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculatePullup_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_calculatePullup_gui_discard", value = svalue(f1_discard_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculatePullup_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePullup_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_window", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePullup_gui_window", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_block", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePullup_gui_block", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_limit", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePullup_gui_limit", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_ol", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePullup_gui_ol", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePullup_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePullup_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculatePullup_gui_discard", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculatePullup_gui_discard", envir = env)
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
