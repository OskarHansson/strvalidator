################################################################################
# CHANGE LOG (last 20 changes)
# 09.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 08.03.2020: Added language support.
# 02.03.2019: Tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 10.07.2018: Fixed error replacing NA's.
# 10.07.2018: Fixed blank drop-down menues after selecting a dataset.
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow drop-down with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 06.01.2017: Added attributes to result.
# 06.01.2017: New options "containing", "not containing" and fixed list ref.
# 02.05.2016: 'Save as' textbox expandable.
# 08.10.2015: Option to remove NA (earlier NA was automatically removed).
# 08.10.2015: Fixed Info not updated when selecting a column.
# 28.08.2015: Added importFrom.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 07.05.2014: Implemented 'checkDataset'.
# 07.05.2014: Fixed 'Target Value' drop not updated.

#' @title Crop Or Replace
#'
#' @description
#' GUI simplifying cropping and replacing values in data frames.
#'
#' @details Select a data frame from the drop-down and a target column.
#' To remove rows with 'NA' check the appropriate box.
#' Select to discard or replace values and additional options.
#' Click button to 'Apply' changes.
#' Multiple actions can be performed on one dataset before saving as
#' a new dataframe.
#' NB! Check that data type is correct before click apply to avoid strange behavior.
#' If data type is numeric any string will become a numeric 'NA'.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help head str tail
#'
#' @return TRUE
#'
#' @seealso \code{\link{trim_gui}}, \code{\link{editData_gui}}, \code{\link{combine_gui}}


cropData_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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
  strWinTitle <- "Crop or replace data"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples,"
  strLblColumns <- "columns,"
  strLblRows <- "rows"
  strFrmColumn <- "Column"
  strLblTargetCol <- "Target column:"
  strDrpColumn <- "<Select column>"
  strLblInfo <- "Info:"
  strLblMin <- "Min:"
  strLblMax <- "Max:"
  strChkNa <- "Ignore NA in info"
  strFrmOptions <- "Options"
  strChkRemNA <- "Remove NA"
  strLblAction <- "Action:"
  strRadDiscard <- "Discard values"
  strRadReplace <- "Replace values"
  strDrpAbove <- "above"
  strDrpAboveEqual <- "above or equal to"
  strDrpBelow <- "below"
  strDrpBelowEqual <- "below or equal to"
  strDrpEqual <- "equal to"
  strDrpNotEqual <- "not equal to"
  strDrpNA <- "is NA"
  strDrpNotNA <- "is not NA"
  strDrpContain <- "containing"
  strDrpNotContain <- "not containing"
  strDrpTarget <- "<Target value>"
  strLblWith <- "with"
  strLblTarget <- "Target column contain data of type:"
  strRadNumeric <- "Numeric"
  strRadCharacter <- "Character"
  strBtnApply <- "Apply"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSave <- "Save"
  strBtnSaved <- "Saved!"
  strMsgDrop <- "Do not make subsets of a drop-out dataset before modelling!\n1) Make a subset from the original data.\n2) Run calculate dropout on that subset.\n3) Model drop-out from the new drop-out dataset."
  strMsgTitleDrop <- "Warning!"
  strMsgDataset <- "A dataset must be selected."
  strMsgTitleDataset <- "Dataset not selected"
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

    strtmp <- dtStrings["strFrmColumn"]$value
    strFrmColumn <- ifelse(is.na(strtmp), strFrmColumn, strtmp)

    strtmp <- dtStrings["strLblTargetCol"]$value
    strLblTargetCol <- ifelse(is.na(strtmp), strLblTargetCol, strtmp)

    strtmp <- dtStrings["strDrpColumn"]$value
    strDrpColumn <- ifelse(is.na(strtmp), strDrpColumn, strtmp)

    strtmp <- dtStrings["strLblInfo"]$value
    strLblInfo <- ifelse(is.na(strtmp), strLblInfo, strtmp)

    strtmp <- dtStrings["strLblMin"]$value
    strLblMin <- ifelse(is.na(strtmp), strLblMin, strtmp)

    strtmp <- dtStrings["strLblMax"]$value
    strLblMax <- ifelse(is.na(strtmp), strLblMax, strtmp)

    strtmp <- dtStrings["strChkNa"]$value
    strChkNa <- ifelse(is.na(strtmp), strChkNa, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkRemNA"]$value
    strChkRemNA <- ifelse(is.na(strtmp), strChkRemNA, strtmp)

    strtmp <- dtStrings["strLblAction"]$value
    strLblAction <- ifelse(is.na(strtmp), strLblAction, strtmp)

    strtmp <- dtStrings["strRadDiscard"]$value
    strRadDiscard <- ifelse(is.na(strtmp), strRadDiscard, strtmp)

    strtmp <- dtStrings["strRadReplace"]$value
    strRadReplace <- ifelse(is.na(strtmp), strRadReplace, strtmp)

    strtmp <- dtStrings["strDrpAbove"]$value
    strDrpAbove <- ifelse(is.na(strtmp), strDrpAbove, strtmp)

    strtmp <- dtStrings["strDrpAboveEqual"]$value
    strDrpAboveEqual <- ifelse(is.na(strtmp), strDrpAboveEqual, strtmp)

    strtmp <- dtStrings["strDrpBelow"]$value
    strDrpBelow <- ifelse(is.na(strtmp), strDrpBelow, strtmp)

    strtmp <- dtStrings["strDrpBelowEqual"]$value
    strDrpBelowEqual <- ifelse(is.na(strtmp), strDrpBelowEqual, strtmp)

    strtmp <- dtStrings["strDrpEqual"]$value
    strDrpEqual <- ifelse(is.na(strtmp), strDrpEqual, strtmp)

    strtmp <- dtStrings["strDrpNotEqual"]$value
    strDrpNotEqual <- ifelse(is.na(strtmp), strDrpNotEqual, strtmp)

    strtmp <- dtStrings["strDrpNA"]$value
    strDrpNA <- ifelse(is.na(strtmp), strDrpNA, strtmp)

    strtmp <- dtStrings["strDrpNotNA"]$value
    strDrpNotNA <- ifelse(is.na(strtmp), strDrpNotNA, strtmp)

    strtmp <- dtStrings["strDrpContain"]$value
    strDrpContain <- ifelse(is.na(strtmp), strDrpContain, strtmp)

    strtmp <- dtStrings["strDrpNotContain"]$value
    strDrpNotContain <- ifelse(is.na(strtmp), strDrpNotContain, strtmp)

    strtmp <- dtStrings["strDrpTarget"]$value
    strDrpTarget <- ifelse(is.na(strtmp), strDrpTarget, strtmp)

    strtmp <- dtStrings["strLblWith"]$value
    strLblWith <- ifelse(is.na(strtmp), strLblWith, strtmp)

    strtmp <- dtStrings["strLblTarget"]$value
    strLblTarget <- ifelse(is.na(strtmp), strLblTarget, strtmp)

    strtmp <- dtStrings["strRadNumeric"]$value
    strRadNumeric <- ifelse(is.na(strtmp), strRadNumeric, strtmp)

    strtmp <- dtStrings["strRadCharacter"]$value
    strRadCharacter <- ifelse(is.na(strtmp), strRadCharacter, strtmp)

    strtmp <- dtStrings["strBtnApply"]$value
    strBtnApply <- ifelse(is.na(strtmp), strBtnApply, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnSave"]$value
    strBtnSave <- ifelse(is.na(strtmp), strBtnSave, strtmp)

    strtmp <- dtStrings["strBtnSaved"]$value
    strBtnSaved <- ifelse(is.na(strtmp), strBtnSaved, strtmp)

    strtmp <- dtStrings["strMsgDrop"]$value
    strMsgDrop <- ifelse(is.na(strtmp), strMsgDrop, strtmp)

    strtmp <- dtStrings["strMsgTitleDrop"]$value
    strMsgTitleDrop <- ifelse(is.na(strtmp), strMsgTitleDrop, strtmp)

    strtmp <- dtStrings["strMsgDataset"]$value
    strMsgDataset <- ifelse(is.na(strtmp), strMsgDataset, strtmp)

    strtmp <- dtStrings["strMsgTitleDataset"]$value
    strMsgTitleDataset <- ifelse(is.na(strtmp), strMsgTitleDataset, strtmp)

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

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # DATASET -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

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
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # SUMMARY -------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  samples_lbl <- glabel(text = paste(" <NA>", strLblSamples), container = g1)
  columns_lbl <- glabel(text = paste(" <NA>", strLblColumns), container = g1)
  rows_lbl <- glabel(text = paste(" <NA>", strLblRows), container = g1)

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    ok <- checkDataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      # Check for dropout dataset and warn.
      if ("Dropout" %in% names(.gData)) {
        gmessage(
          msg = strMsgDrop,
          title = strMsgTitleDrop,
          icon = "warning", parent = w
        )
      }

      # Refresh column drop-down menu.
      .refresh_column_drp()

      # Update info prior to action.
      samples <- length(unique(.gData$Sample.Name))
      svalue(samples_lbl) <- paste(" ", samples, strLblSamples)
      svalue(columns_lbl) <- paste(" ", ncol(.gData), strLblColumns)
      svalue(rows_lbl) <- paste(" ", nrow(.gData), strLblRows)
      .refresh_column_drp()

      # Update 'Save as'.
      svalue(save_edt) <- .gDataName
      samples <- length(unique(.gData$Sample.Name))
      svalue(f3_samples_lbl) <- paste(" ", samples, strLblSamples)
      svalue(f3_columns_lbl) <- paste(" ", ncol(.gData), strLblColumns)
      svalue(f3_rows_lbl) <- paste(" ", nrow(.gData), strLblRows)

      # Update info:
      svalue(f1_min_lbl) <- strLblMin
      svalue(f1_max_lbl) <- strLblMax
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(samples_lbl) <- paste(" <NA>", strLblSamples)
      svalue(columns_lbl) <- paste(" <NA>", strLblColumns)
      svalue(rows_lbl) <- paste(" <NA>", strLblRows)

      # Update info:
      svalue(f1_min_lbl) <- " Min:"
      svalue(f1_max_lbl) <- " Max:"

      # Update 'Save as'.
      svalue(save_edt) <- ""
      svalue(f3_samples_lbl) <- paste(" <NA>", strLblSamples)
      svalue(f3_columns_lbl) <- paste(" <NA>", strLblColumns)
      svalue(f3_rows_lbl) <- paste(" <NA>", strLblRows)
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmColumn,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strLblTargetCol, container = f1)

  f1_column_drp <- gcombobox(
    items = strDrpColumn,
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  glabel(text = strLblInfo, container = f1)
  f1_min_lbl <- glabel(text = strLblMin, container = f1)
  f1_max_lbl <- glabel(text = strLblMax, container = f1)
  f1_na_chk <- gcheckbox(text = strChkNa, checked = TRUE, container = f1)

  addHandlerChanged(f1_na_chk, handler = function(h, ...) {
    .refresh_info()
  })

  addHandlerChanged(f1_column_drp, handler = function(h, ...) {
    val_column <- svalue(f1_column_drp)

    # Detect data type.
    if (!is.null(.gData) & !is.null(val_column)) {
      # Check that a (existing) column is selected.
      if (val_column %in% names(.gData)) {
        if (is.numeric(.gData[, val_column])) {
          svalue(f2_type_opt, index = TRUE) <- 1
        } else if (is.character(.gData[, val_column])) {
          svalue(f2_type_opt, index = TRUE) <- 2
        } else {
          if (debug) {
            print("Selected column is not 'numeric' and not 'character'")
          }
        }
        # Update target value etc.
        .refresh_info()
        # Update target value etc.
        .refresh_options()
      }
    }
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f2_na_rm_chk <- gcheckbox(text = strChkRemNA, checked = FALSE, container = f2)

  glabel(text = strLblAction, visible = FALSE, anchor = c(-1, -1), container = f2)

  f2g1 <- glayout(container = f2, spacing = 1)

  f2g1[1, 1] <- f2g1_task_opt <- gradio(
    items = c(strRadDiscard, strRadReplace),
    selected = 1,
    container = f2g1
  )

  f2_items <- c(
    strDrpAbove, strDrpAboveEqual,
    strDrpBelow, strDrpBelowEqual,
    strDrpEqual, strDrpNotEqual,
    strDrpNA, strDrpNotNA,
    strDrpContain, strDrpNotContain
  )

  f2g1[1, 2] <- f2g1_operator_drp <- gcombobox(
    items = f2_items, container = f2g1,
    ellipsize = "none"
  )

  f2g1[1, 3] <- f2g1_target_cbo <- gcombobox(
    items = strDrpTarget,
    selected = 1,
    editable = TRUE,
    container = f2g1,
    ellipsize = "none"
  )

  f2g1[1, 4] <- f2g1_new_lbl <- glabel(
    text = strLblWith,
    visible = FALSE,
    anchor = c(0, -1),
    container = f2g1
  )

  f2g1[1, 5] <- f2g1_new_edt <- gedit(
    visible = FALSE,
    expand = TRUE,
    fill = TRUE,
    container = f2g1
  )

  glabel(
    text = strLblTarget,
    visible = FALSE, anchor = c(-1, -1), container = f2
  )

  f2_type_opt <- gradio(
    items = c(strRadNumeric, strRadCharacter),
    horizontal = FALSE,
    selected = 1,
    container = f2
  )

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f2g1_task_opt, handler = function(h, ...) {
    .refresh_options()
  })

  addHandlerChanged(f2g1_operator_drp, handler = function(h, ...) {
    .refresh_options()
  })

  # BUTTON ####################################################################

  apply_btn <- gbutton(text = strBtnApply, container = gv)


  addHandlerClicked(apply_btn, handler = function(h, ...) {
    val_column <- svalue(f1_column_drp)
    val_task <- svalue(f2g1_task_opt, index = TRUE)
    val_operator <- svalue(f2g1_operator_drp, index = TRUE)
    val_target <- svalue(f2g1_target_cbo)
    val_new <- svalue(f2g1_new_edt)
    val_type <- svalue(f2_type_opt, index = TRUE)
    val_na_rm <- svalue(f2_na_rm_chk)

    # If second round, get name from save box.
    if (svalue(dataset_drp, index = TRUE) == 1) {
      .gDataName <<- svalue(save_edt)
    }

    # Check data type and convert.
    if (val_type == 1) { # Numeric.
      val_target <- as.numeric(val_target)
      val_new <- as.numeric(val_new)
      if (!is.numeric(.gData[, val_column])) {
        .gData[, val_column] <<- as.numeric(.gData[, val_column])
        message("Target column not numeric. Data converted!")
      }
    } else if (val_type == 2) { # Character.
      val_target <- as.character(val_target) # Not needed, edit box always character.
      val_new <- as.character(val_new) # Not needed, edit box always character.
      if (!is.character(.gData[, val_column])) {
        .gData[, val_column] <<- as.character(.gData[, val_column])
        message("Target column not character. Data converted!")
      }
    } else {
      warning(paste("Unsupported data type selected!", val_type))
    }

    if (debug) {
      print("val_column")
      print(val_column)
      print("val_task")
      print(val_task)
      print("val_operator")
      print(val_operator)
      print("val_target")
      print(val_target)
      print("val_new")
      print(val_new)
      print("val_type")
      print(val_type)
    }

    # Change button.
    blockHandlers(apply_btn)
    svalue(apply_btn) <- strBtnProcessing
    unblockHandlers(apply_btn)
    enabled(apply_btn) <- FALSE

    blockHandlers(save_btn)
    svalue(save_btn) <- strBtnSave
    unblockHandlers(save_btn)

    if (!is.null(.gData) && !is.null(.gData)) {
      if (debug) {
        print("Before action: .gData dim, str, head, tail:")
        print(dim(.gData))
        print(str(.gData))
        print(head(.gData))
        print(tail(.gData))
      }

      if (val_na_rm) {
        # Remove all rows with NA in target column.
        .gData <<- .gData[!is.na(.gData[[val_column]]), ]
      }

      if (val_operator == 1) { # above

        if (val_task == 1) { # crop
          .gData <<- .gData[is.na(.gData[[val_column]]) | !.gData[val_column] > val_target, ]
        } else { # replace
          .gData[val_column][.gData[val_column] > val_target] <<- val_new
        }
      } else if (val_operator == 2) { # above or equal to

        if (val_task == 1) { # crop
          .gData <<- .gData[is.na(.gData[[val_column]]) | !.gData[val_column] >= val_target, ]
        } else { # replace
          .gData[val_column][.gData[val_column] >= val_target] <<- val_new
        }
      } else if (val_operator == 3) { # below

        if (val_task == 1) { # crop
          .gData <<- .gData[is.na(.gData[[val_column]]) | !.gData[val_column] < val_target, ]
        } else { # replace
          .gData[val_column][.gData[val_column] < val_target] <<- val_new
        }
      } else if (val_operator == 4) { # below or equal to

        if (val_task == 1) { # crop
          .gData <<- .gData[is.na(.gData[[val_column]]) | !.gData[val_column] <= val_target, ]
        } else { # replace
          .gData[val_column][.gData[val_column] <= val_target] <<- val_new
        }
      } else if (val_operator == 5) { # equal to

        if (val_task == 1) { # crop
          .gData <<- .gData[is.na(.gData[[val_column]]) | !.gData[val_column] == val_target, ]
        } else { # replace
          .gData[val_column][.gData[val_column] == val_target] <<- val_new
        }
      } else if (val_operator == 6) { # not equal to

        if (val_task == 1) { # crop
          .gData <<- .gData[is.na(.gData[[val_column]]) | !.gData[val_column] != val_target, ]
        } else { # replace
          .gData[val_column][.gData[val_column] != val_target] <<- val_new
        }
      } else if (val_operator == 7) { # is NA

        if (val_task == 1) { # crop
          .gData <<- .gData[!is.na(.gData[[val_column]]), ]
        } else { # replace
          .gData[val_column][is.na(.gData[val_column])] <<- val_new
        }
      } else if (val_operator == 8) { # is not NA

        if (val_task == 1) { # crop
          .gData <<- .gData[is.na(.gData[[val_column]]), ]
        } else { # replace
          .gData[val_column][!is.na(.gData[val_column])] <<- val_new
        }
      } else if (val_operator == 9) { # containing

        if (val_task == 1) { # crop
          sel <- is.na(.gData[[val_column]]) | !grepl(val_target, .gData[[val_column]], fixed = TRUE)
          .gData <<- .gData[sel, ]
        } else { # replace
          sel <- is.na(.gData[[val_column]]) | !grepl(val_target, .gData[[val_column]], fixed = TRUE)
          .gData[val_column][sel, ] <<- val_new
        }
      } else if (val_operator == 10) { # not containing

        if (val_task == 1) { # crop
          sel <- is.na(.gData[[val_column]]) | grepl(val_target, .gData[[val_column]], fixed = TRUE)
          .gData <<- .gData[sel, ]
        } else { # replace
          sel <- is.na(.gData[[val_column]]) | grepl(val_target, .gData[[val_column]], fixed = TRUE)
          .gData[val_column][sel, ] <<- val_new
        }
      }

      # Create key-value pairs to log.
      keys <- list(
        "data", "column", "na", "task", "operator",
        "target", "value", "type"
      )

      values <- list(
        .gDataName, val_column, val_na_rm, val_task, val_operator,
        val_target, val_new, val_type
      )

      # Update audit trail.
      .gData <- auditTrail(
        obj = .gData, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Update the current dataset to perform possible additional tasks.
      .gData <<- .gData

      if (debug) {
        print("After action: .gData dim, str, head, tail:")
        print(dim(.gData))
        print(str(.gData))
        print(head(.gData))
        print(tail(.gData))
      }
    } else {
      gmessage(
        msg = strMsgDataset,
        title = strMsgTitleError,
        icon = "error"
      )
    }

    # Change button.
    blockHandlers(apply_btn)
    svalue(apply_btn) <- strBtnApply
    unblockHandlers(apply_btn)
    enabled(apply_btn) <- TRUE

    # Update 'Save as'.
    .refresh_info()
    currentName <- svalue(save_edt)
    if (nchar(currentName) == 0) {
      svalue(save_edt) <- paste(.gDataName, val_target, sep = "_")
    } else {
      svalue(save_edt) <- paste(currentName, val_target, sep = "_")
    }
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  save_btn <- gbutton(text = strBtnSave, container = save_frame)

  f3_samples_lbl <- glabel(text = paste(" <NA>", strLblSamples), container = save_frame)
  f3_columns_lbl <- glabel(text = paste(" <NA>", strLblColumns), container = save_frame)
  f3_rows_lbl <- glabel(text = paste(" <NA>", strLblRows), container = save_frame)

  addHandlerChanged(save_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)
    datanew <- .gData

    if (debug) {
      print("Save pressed!")
      print(val_name)
      print("datanew dim, str, head, tail:")
      print(dim(datanew))
      print(str(datanew))
      print(head(datanew))
      print(tail(datanew))
    }

    # Save data.
    saveObject(name = val_name, object = datanew, parent = w, env = env)
    blockHandlers(save_btn)
    svalue(save_btn) <- strBtnSaved
    unblockHandlers(save_btn)
  })

  # INTERNAL FUNCTIONS ########################################################

  .refresh_column_drp <- function() {
    if (debug) {
      print("Refresh column drop-down")
    }

    # Get data frames in global workspace.
    dfs <- names(.gData)

    if (!is.null(dfs)) {
      blockHandler(f1_column_drp)

      # Populate drop list.
      f1_column_drp[] <- c(strDrpColumn, dfs)

      # Select default value.
      svalue(f1_column_drp, index = TRUE) <- 1

      unblockHandler(f1_column_drp)
    }

    if (debug) {
      print("Column drop-down refreshed!")
    }
  }

  .refresh_info <- function() {
    if (debug) {
      print("Refresh info")
    }

    val_col <- svalue(f1_column_drp)
    val_na <- svalue(f1_na_chk)

    if (length(val_col) != 0) {
      if (val_col %in% names(.gData)) {
        # Update info:
        if (is.factor(.gData[, val_col])) {
          svalue(f1_min_lbl) <- paste(
            strLblMin,
            min(as.character(.gData[, val_col]),
              na.rm = val_na
            )
          )
          svalue(f1_max_lbl) <- paste(
            strLblMax,
            max(as.character(.gData[, val_col]),
              na.rm = val_na
            )
          )
        } else {
          svalue(f1_min_lbl) <- paste(
            strLblMin,
            min(.gData[, val_col], na.rm = val_na)
          )
          svalue(f1_max_lbl) <- paste(
            strLblMax,
            max(.gData[, val_col], na.rm = val_na)
          )
        }

        # Update 'Save As'
        samples <- length(unique(.gData$Sample.Name))
        svalue(f3_samples_lbl) <- paste(" ", samples, strLblSamples)
        svalue(f3_columns_lbl) <- paste(" ", ncol(.gData), strLblColumns)
        svalue(f3_rows_lbl) <- paste(" ", nrow(.gData), strLblRows)
      } else {
        # Update info:
        svalue(f1_min_lbl) <- strLblMin
        svalue(f1_max_lbl) <- strLblMax

        svalue(f3_samples_lbl) <- paste(" <NA>", strLblSamples)
        svalue(f3_columns_lbl) <- paste(" <NA>", strLblColumns)
        svalue(f3_rows_lbl) <- paste(" <NA>", strLblRows)
      }
    }

    if (debug) {
      print("Info refreshed!")
    }
  }

  .refresh_options <- function() {
    if (debug) {
      print("Refresh options")
    }

    val_col <- svalue(f2g1_task_opt, index = TRUE)
    val_drp <- svalue(f2g1_operator_drp, index = TRUE)

    # Update target combo with column content.
    if (!is.null(.gData)) {
      selectedColumn <- svalue(f1_column_drp)
      if (length(selectedColumn) != 0) {
        if (selectedColumn %in% names(.gData)) {
          f2g1_target_cbo[, ] <- unique(.gData[selectedColumn])

          # Select default value.
          svalue(f2g1_target_cbo, index = TRUE) <- 1
        }
      }
    }

    # Crop.
    if (val_col == 1) {
      enabled(f2g1_new_lbl) <- FALSE
      enabled(f2g1_new_edt) <- FALSE

      # NA
      if (val_drp == 7 || val_drp == 8) {
        svalue(f2g1_target_cbo) <- NA
        enabled(f2g1_target_cbo) <- FALSE
        svalue(f2g1_new_edt) <- NA
      } else {
        enabled(f2g1_target_cbo) <- TRUE
      }
    } else {
      enabled(f2g1_new_lbl) <- TRUE
      enabled(f2g1_new_edt) <- TRUE

      # NA
      if (val_drp == 7 || val_drp == 8) {
        svalue(f2g1_target_cbo) <- NA
        enabled(f2g1_target_cbo) <- FALSE
        svalue(f2g1_new_edt) <- NA
      } else {
        enabled(f2g1_target_cbo) <- TRUE
      }
    }

    if (debug) {
      print("Options refreshed!")
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
      if (exists(".strvalidator_cropData_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_cropData_gui_savegui", envir = env)
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
      if (exists(".strvalidator_cropData_gui_na", envir = env, inherits = FALSE)) {
        svalue(f1_na_chk) <- get(".strvalidator_cropData_gui_na", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_na_rm", envir = env, inherits = FALSE)) {
        svalue(f2_na_rm_chk) <- get(".strvalidator_cropData_gui_na_rm", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_task", envir = env, inherits = FALSE)) {
        svalue(f2g1_task_opt) <- get(".strvalidator_cropData_gui_task", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_operator", envir = env, inherits = FALSE)) {
        svalue(f2g1_operator_drp) <- get(".strvalidator_cropData_gui_operator", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_new", envir = env, inherits = FALSE)) {
        svalue(f2g1_new_edt) <- get(".strvalidator_cropData_gui_new", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_cropData_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_cropData_gui_na", value = svalue(f1_na_chk), envir = env)
      assign(x = ".strvalidator_cropData_gui_na_rm", value = svalue(f2_na_rm_chk), envir = env)
      assign(x = ".strvalidator_cropData_gui_task", value = svalue(f2g1_task_opt), envir = env)
      assign(x = ".strvalidator_cropData_gui_operator", value = svalue(f2g1_operator_drp), envir = env)
      assign(x = ".strvalidator_cropData_gui_new", value = svalue(f2g1_new_edt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_cropData_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_cropData_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_cropData_gui_na", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_na_rm", envir = env, inherits = FALSE)) {
        remove(".strvalidator_cropData_gui_na_rm", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_task", envir = env, inherits = FALSE)) {
        remove(".strvalidator_cropData_gui_task", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_operator", envir = env, inherits = FALSE)) {
        remove(".strvalidator_cropData_gui_operator", envir = env)
      }
      if (exists(".strvalidator_cropData_gui_new", envir = env, inherits = FALSE)) {
        remove(".strvalidator_cropData_gui_new", envir = env)
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
