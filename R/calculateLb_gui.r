################################################################################
# CHANGE LOG (last 20 changes)
# 03.03.2020: Added language support.
# 20.03.2019: Added new option to calculate balance (Issue:#14).
# 15.02.2019: Expand text fields in tcltk by setting fill = TRUE.
# 11.02.2019: Minor adjustments to tcltk gui.
# 11.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.08.2016: Implemented new calculateHeight, removed calculateHeterozygous.
# 28.06.2016: Added option to remove quality sensor.
# 02.12.2016: Fixed options save bug.
# 30.12.2015: First version.

#' @title Calculate Locus Balance
#'
#' @description
#' GUI wrapper for the \code{\link{calculateLb}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateLb}} function
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
#' @seealso \code{link{calculateLb}}, \code{link{checkSubset}}
#'

calculateLb_gui <- function(env = parent.frame(), savegui = NULL,
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
  strWinTitle <- "Calculate locus balance"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblRefDataset <- "Reference dataset:"
  strTipRef <- "If provided, known alleles will be extracted from data"
  strLblRef <- "references"
  strBtnCheck <- "Check subsetting"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strLblPre <- "Pre-processing:"
  strChkOL <- "Remove off-ladder alleles"
  strChkSex <- "Remove sex markers"
  strChkSensors <- "Remove quality sensors"
  strLblMissing <- "Replace missing data with peak height:"
  strLblMethod <- "Calculate locus balance:"
  strRadProp <- "Proportional"
  strRadNorm <- "Normalised"
  strRadCent <- "Centred Quantities"
  strRadRatio <- "Peak Ratio"
  strChkDye <- "Calculate Lb by dye channel"
  strLblMatching <- "Reference sample name matching:"
  strChkIgnore <- "Ignore case"
  strChkWord <- "Add word boundaries"
  strChkExact <- "Exact matching"
  strLblPost <- "Post-processing:"
  strChkAverage <- "Calculate average peak height"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
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
  if (!is.na(dtStrings)) {
    # Get language strings, use default if not found.

    strTmp <- dtStrings["strWinTitle"]$Value
    strWinTitle <- ifelse(is.na(strTmp), strWinTitle, strTmp)

    strTmp <- dtStrings["strChkGui"]$Value
    strChkGui <- ifelse(is.na(strTmp), strChkGui, strTmp)

    strTmp <- dtStrings["strBtnHelp"]$Value
    strBtnHelp <- ifelse(is.na(strTmp), strBtnHelp, strTmp)

    strTmp <- dtStrings["strFrmDataset"]$Value
    strFrmDataset <- ifelse(is.na(strTmp), strFrmDataset, strTmp)

    strTmp <- dtStrings["strLblDataset"]$Value
    strLblDataset <- ifelse(is.na(strTmp), strLblDataset, strTmp)

    strTmp <- dtStrings["strDrpDataset"]$Value
    strDrpDataset <- ifelse(is.na(strTmp), strDrpDataset, strTmp)

    strTmp <- dtStrings["strLblSamples"]$Value
    strLblSamples <- ifelse(is.na(strTmp), strLblSamples, strTmp)

    strTmp <- dtStrings["strLblRefDataset"]$Value
    strLblRefDataset <- ifelse(is.na(strTmp), strLblRefDataset, strTmp)

    strTmp <- dtStrings["strTipRef"]$Value
    strTipRef <- ifelse(is.na(strTmp), strTipRef, strTmp)

    strTmp <- dtStrings["strLblRef"]$Value
    strLblRef <- ifelse(is.na(strTmp), strLblRef, strTmp)

    strTmp <- dtStrings["strBtnCheck"]$Value
    strBtnCheck <- ifelse(is.na(strTmp), strBtnCheck, strTmp)

    strTmp <- dtStrings["strLblKit"]$Value
    strLblKit <- ifelse(is.na(strTmp), strLblKit, strTmp)

    strTmp <- dtStrings["strFrmOptions"]$Value
    strFrmOptions <- ifelse(is.na(strTmp), strFrmOptions, strTmp)

    strTmp <- dtStrings["strLblPre"]$Value
    strLblPre <- ifelse(is.na(strTmp), strLblPre, strTmp)

    strTmp <- dtStrings["strChkOL"]$Value
    strChkOL <- ifelse(is.na(strTmp), strChkOL, strTmp)

    strTmp <- dtStrings["strChkSex"]$Value
    strChkSex <- ifelse(is.na(strTmp), strChkSex, strTmp)

    strTmp <- dtStrings["strChkSensors"]$Value
    strChkSensors <- ifelse(is.na(strTmp), strChkSensors, strTmp)

    strTmp <- dtStrings["strLblMissing"]$Value
    strLblMissing <- ifelse(is.na(strTmp), strLblMissing, strTmp)

    strTmp <- dtStrings["strLblMethod"]$Value
    strLblMethod <- ifelse(is.na(strTmp), strLblMethod, strTmp)

    strTmp <- dtStrings["strRadProp"]$Value
    strRadProp <- ifelse(is.na(strTmp), strRadProp, strTmp)

    strTmp <- dtStrings["strRadNorm"]$Value
    strRadNorm <- ifelse(is.na(strTmp), strRadNorm, strTmp)

    strTmp <- dtStrings["strRadCent"]$Value
    strRadCent <- ifelse(is.na(strTmp), strRadCent, strTmp)

    strTmp <- dtStrings["strRadRatio"]$Value
    strRadRatio <- ifelse(is.na(strTmp), strRadRatio, strTmp)

    strTmp <- dtStrings["strChkDye"]$Value
    strChkDye <- ifelse(is.na(strTmp), strChkDye, strTmp)

    strTmp <- dtStrings["strLblMatching"]$Value
    strLblMatching <- ifelse(is.na(strTmp), strLblMatching, strTmp)

    strTmp <- dtStrings["strChkIgnore"]$Value
    strChkIgnore <- ifelse(is.na(strTmp), strChkIgnore, strTmp)

    strTmp <- dtStrings["strChkWord"]$Value
    strChkWord <- ifelse(is.na(strTmp), strChkWord, strTmp)

    strTmp <- dtStrings["strChkExact"]$Value
    strChkExact <- ifelse(is.na(strTmp), strChkExact, strTmp)

    strTmp <- dtStrings["strLblPost"]$Value
    strLblPost <- ifelse(is.na(strTmp), strLblPost, strTmp)

    strTmp <- dtStrings["strChkAverage"]$Value
    strChkAverage <- ifelse(is.na(strTmp), strChkAverage, strTmp)

    strTmp <- dtStrings["strFrmSave"]$Value
    strFrmSave <- ifelse(is.na(strTmp), strFrmSave, strTmp)

    strTmp <- dtStrings["strLblSave"]$Value
    strLblSave <- ifelse(is.na(strTmp), strLblSave, strTmp)

    strTmp <- dtStrings["strBtnCalculate"]$Value
    strBtnCalculate <- ifelse(is.na(strTmp), strBtnCalculate, strTmp)

    strTmp <- dtStrings["strBtnProcessing"]$Value
    strBtnProcessing <- ifelse(is.na(strTmp), strBtnProcessing, strTmp)

    strTmp <- dtStrings["strMsgDataset"]$Value
    strMsgDataset <- ifelse(is.na(strTmp), strMsgDataset, strTmp)

    strTmp <- dtStrings["strMsgTitleDataset"]$Value
    strMsgTitleDataset <- ifelse(is.na(strTmp), strMsgTitleDataset, strTmp)

    strTmp <- dtStrings["strMsgCheck"]$Value
    strMsgCheck <- ifelse(is.na(strTmp), strMsgCheck, strTmp)

    strTmp <- dtStrings["strWinTitleCheck"]$Value
    strWinTitleCheck <- ifelse(is.na(strTmp), strWinTitleCheck, strTmp)

    strTmp <- dtStrings["strMsgTitleError"]$Value
    strMsgTitleError <- ifelse(is.na(strTmp), strMsgTitleError, strTmp)

    strTmp <- dtStrings["strMsgNA"]$Value
    strMsgNA <- ifelse(is.na(strTmp), strMsgNA, strTmp)

    strTmp <- dtStrings["strMsgTitleNA"]$Value
    strMsgTitleNA <- ifelse(is.na(strTmp), strMsgTitleNA, strTmp)
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
    horizontal = TRUE,
    spacing = 5,
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
    requiredCol <- c("Sample.Name", "Marker", "Height")
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
      svalue(g0_data_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strLblSamples
      )

      # Suggest a name for the result.
      svalue(save_edt) <- paste(val_obj, "_lb", sep = "")

      # Detect kit.
      kitIndex <- detectKit(data = .gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(g0_data_drp, index = TRUE) <- 1
      svalue(g0_data_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""
    }
  })

  # Reference -----------------------------------------------------------------

  g0[2, 1] <- glabel(text = strLblRefDataset, container = g0)

  # NB! dfs defined in previous section.
  g0[2, 2] <- g0_ref_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )
  tooltip(g0_ref_drp) <- strTipRef

  g0[2, 3] <- g0_ref_samples_lbl <- glabel(
    text = paste(" 0", strLblRef),
    container = g0
  )

  addHandlerChanged(g0_ref_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_ref_drp)

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
      svalue(g0_ref_samples_lbl) <- paste(
        length(unique(.gRef$Sample.Name)),
        strLblRef
      )
      # Enable checkbox to calculate H.
      enabled(f1_h_chk) <- TRUE
    } else {

      # Reset components.
      .gRef <<- NULL
      .gRefName <<- NULL
      svalue(g0_ref_drp, index = TRUE) <- 1
      svalue(g0_ref_samples_lbl) <- paste(" 0", strLblRef)

      # Disable checkbox to calculate H.
      enabled(f1_h_chk) <- FALSE
    }
  })

  # CHECK ---------------------------------------------------------------------

  g0[3, 2] <- g0_check_btn <- gbutton(text = strBtnCheck, container = g0)

  addHandlerChanged(g0_check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)

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

  # Kit -----------------------------------------------------------------------

  g0[4, 1] <- glabel(text = strLblKit, container = g0)

  g0[4, 2] <- kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = g0,
    ellipsize = "none"
  )


  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  #----------------------------------------------------------------------------
  glabel(text = strLblPre, anchor = c(-1, 0), container = f1)

  f1_ol_chk <- gcheckbox(
    text = strChkOL, checked = TRUE,
    container = f1
  )

  f1_sex_chk <- gcheckbox(
    text = strChkSex, checked = FALSE,
    container = f1
  )

  f1_qs_chk <- gcheckbox(
    text = strChkSensors, checked = TRUE,
    container = f1
  )

  #----------------------------------------------------------------------------
  glabel(
    text = strLblMissing, anchor = c(-1, 0),
    container = f1
  )
  f1_na_edt <- gedit(expand = TRUE, container = f1)

  #----------------------------------------------------------------------------
  glabel(text = strLblMethod, anchor = c(-1, 0), container = f1)

  f1_options_lb <- c(
    strRadProp,
    strRadNorm,
    strRadCent,
    strRadRatio
  )

  f1_lb_opt <- gradio(
    items = f1_options_lb,
    selected = 1,
    horizontal = TRUE,
    container = f1
  )

  f1_dye_chk <- gcheckbox(
    text = strChkDye, checked = FALSE,
    container = f1
  )

  #----------------------------------------------------------------------------
  glabel(
    text = strLblMatching, anchor = c(-1, 0),
    container = f1
  )

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

  #----------------------------------------------------------------------------
  glabel(text = strLblPost, anchor = c(-1, 0), container = f1)

  f1_h_chk <- gcheckbox(
    text = strChkAverage, checked = TRUE,
    container = f1
  )

  # Disable checkbox to calculate H.
  enabled(f1_h_chk) <- FALSE

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strBtnCalculate, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_option <- svalue(f1_lb_opt, index = TRUE)
    val_dye <- svalue(f1_dye_chk)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    val_data <- .gData
    val_data_name <- .gDataName
    val_ref <- .gRef
    val_ref_name <- .gRefName
    val_name <- svalue(save_edt)
    val_kit <- svalue(kit_drp)
    val_ol <- svalue(f1_ol_chk)
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)
    val_na <- as.numeric(svalue(f1_na_edt))
    val_h_enabled <- enabled(f1_h_chk)
    val_h <- svalue(f1_h_chk)
    val_exclude <- NULL # Argument to calculateHeight.

    if (debug) {
      print("Read Values:")
      print("val_option")
      print(val_option)
      print("val_dye")
      print(val_dye)
      print("val_ol")
      print(val_ol)
      print("val_sex")
      print(val_sex)
      print("val_qs")
      print(val_qs)
      print("val_na")
      print(val_na)
      print("val_ignore")
      print(val_ignore)
      print("val_word")
      print(val_word)
      print("val_exact")
      print(val_exact)
      print("val_name")
      print(val_name)
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
      print("val_exclude")
      print(val_exclude)
    }

    # Check if data.
    if (!is.null(val_data)) {

      # Check for NA's in dye column.
      if (!any(is.na(val_data$Dye))) {
        if (val_option == 1) {
          val_option <- "prop"
        } else if (val_option == 2) {
          val_option <- "norm"
        } else if (val_option == 3) {
          val_option <- "cent"
        } else if (val_option == 4) {
          val_option <- "peak"
        } else {
          stop("val_option =", val_option, "not implemented!")
        }

        if (is.na(val_na)) {
          val_na <- NULL
        }

        if (!val_h_enabled) {
          val_h <- FALSE
        }


        # Check if off-ladder peaks should be removed.
        if (val_ol) {
          val_exclude <- "OL"
        }

        if (debug) {
          print("Sent Values:")
          print("val_option")
          print(val_option)
          print("val_na")
          print(val_na)
          print("val_h")
          print(val_h)
          print("val_ignore")
          print(val_ignore)
        }

        # Change button.
        blockHandlers(calculate_btn)
        svalue(calculate_btn) <- strBtnProcessing
        unblockHandlers(calculate_btn)
        enabled(calculate_btn) <- FALSE

        datanew <- calculateLb(
          data = val_data,
          ref = val_ref,
          option = val_option,
          by.dye = val_dye,
          ol.rm = val_ol,
          sex.rm = val_sex,
          qs.rm = val_qs,
          na = val_na,
          kit = val_kit,
          ignore.case = val_ignore,
          word = val_word,
          exact = val_exact,
          debug = debug
        )

        # Add attributes to result.
        attr(datanew, which = "kit") <- val_kit

        # Create key-value pairs to log.
        keys <- list(
          "data", "ref", "option", "by.dye",
          "ol.rm", "sex.rm", "qs.rm", "na", "kit", "ignore.case",
          "word", "exact", "calculate.h"
        )

        values <- list(
          val_data_name, val_ref_name, val_option, val_dye,
          val_ol, val_sex, val_qs, val_na, val_kit, val_ignore,
          val_word, val_exact, val_h
        )

        # Update audit trail.
        datanew <- auditTrail(
          obj = datanew, key = keys, value = values,
          label = fnc, arguments = FALSE,
          package = "strvalidator"
        )

        # Calculate and add average peak height.
        if (val_h) {

          # Calculate average peak height.
          dfH <- calculateHeight(
            data = val_data, ref = val_ref,
            na.replace = 0, add = FALSE,
            exclude = val_exclude, sex.rm = val_sex,
            qs.rm = val_qs, kit = val_kit,
            ignore.case = val_ignore, exact = FALSE,
            debug = debug
          )

          message("Average peak height calculated.")

          # Add average peak height to dataset.
          datanew <- addData(
            data = datanew, new.data = dfH,
            by.col = "Sample.Name", then.by.col = NULL,
            exact = TRUE, ignore.case = val_ignore,
            debug = debug
          )

          message("Average peak height added to result.")
        }

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
      if (exists(".strvalidator_calculateLb_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateLb_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateLb_gui_option", envir = env, inherits = FALSE)) {
        svalue(f1_lb_opt) <- get(".strvalidator_calculateLb_gui_option", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_dye", envir = env, inherits = FALSE)) {
        svalue(f1_dye_chk) <- get(".strvalidator_calculateLb_gui_dye", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_ol", envir = env, inherits = FALSE)) {
        svalue(f1_ol_chk) <- get(".strvalidator_calculateLb_gui_ol", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateLb_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_calculateLb_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_na", envir = env, inherits = FALSE)) {
        svalue(f1_na_edt) <- get(".strvalidator_calculateLb_gui_na", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculateLb_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_calculateLb_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_calculateLb_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_h", envir = env, inherits = FALSE)) {
        svalue(f1_h_chk) <- get(".strvalidator_calculateLb_gui_h", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateLb_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_option", value = svalue(f1_lb_opt), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_dye", value = svalue(f1_dye_chk), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_ol", value = svalue(f1_ol_chk), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_qs", value = svalue(f1_qs_chk), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_na", value = svalue(f1_na_edt), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_calculateLb_gui_h", value = svalue(f1_h_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateLb_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_option", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_option", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_dye", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_dye", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_ol", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_ol", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_na", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateLb_gui_h", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateLb_gui_h", envir = env)
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
