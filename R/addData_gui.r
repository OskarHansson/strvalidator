################################################################################
# CHANGE LOG (last 20 changes)
# 10.09.2022: Prepended .strvalidator_ to saved settings.
# 01.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 03.03.2020: Fixed reference to function name.
# 23.02.2020: Added language support.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 19.07.2018: Minor changes to some labels (clarity).
# 11.07.2018: 'Save as' textbox expandable.
# 10.07.2018: Fixed blank drop-down menues after selecting a dataset.
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 30.11.2015: Added attributes to result.
# 30.11.2015: Added new option for columns to add.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 06.05.2014: Implemented 'checkDataset'.

#' @title Add Data
#'
#' @description
#' GUI wrapper for \code{\link{addData}}.
#'
#' @details
#' Simplifies the use of the \code{\link{addData}} function by providing a graphical
#' user interface to it.
#' @param env environment in which to search for data frames.
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
#' @seealso  \code{\link{addData}}

addData_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gDataDest <- NULL
  .gDataDestName <- NULL
  .gDataDestColumns <- NULL
  .gDataSource <- NULL
  .gDataSourceColumns <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Add data"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDestination <- "Select destination dataset:"
  strDrpDefault <- "<Select column>"
  strLblSamples <- "samples"
  strLblSource <- "Select source dataset:"
  strFrmOptions <- "Options"
  strChkExact <- "Exact key matching"
  strChkIgnore <- "Ignore case in marker name"
  strLblKey1 <- "Select primary key column:"
  strLblKey2 <- "Select secondary key column (optional):"
  strLblColumns <- "Select columns to add to the new dataset:"
  strEdtMsg <- "Default is all columns"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnAdd <- "Add"
  strBtnAddActive <- "Processing..."
  strMsgDataset <- "A destination and source dataset must be selected."
  strMsgTitleDataset <- "Datasets not selected"

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

    strtmp <- dtStrings["strLblDestination"]$value
    strLblDestination <- ifelse(is.na(strtmp), strLblDestination, strtmp)

    strtmp <- dtStrings["strDrpDefault"]$value
    strDrpDefault <- ifelse(is.na(strtmp), strDrpDefault, strtmp)

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strLblSource"]$value
    strLblSource <- ifelse(is.na(strtmp), strLblSource, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkExact"]$value
    strChkExact <- ifelse(is.na(strtmp), strChkExact, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strLblKey1"]$value
    strLblKey1 <- ifelse(is.na(strtmp), strLblKey1, strtmp)

    strtmp <- dtStrings["strLblKey2"]$value
    strLblKey2 <- ifelse(is.na(strtmp), strLblKey2, strtmp)

    strtmp <- dtStrings["strLblColumns"]$value
    strLblColumns <- ifelse(is.na(strtmp), strLblColumns, strtmp)

    strtmp <- dtStrings["strEdtMsg"]$value
    strEdtMsg <- ifelse(is.na(strtmp), strEdtMsg, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnAdd"]$value
    strBtnAdd <- ifelse(is.na(strtmp), strBtnAdd, strtmp)

    strtmp <- dtStrings["strBtnAddActive"]$value
    strBtnAddActive <- ifelse(is.na(strtmp), strBtnAddActive, strtmp)

    strtmp <- dtStrings["strMsgDataset"]$value
    strMsgDataset <- ifelse(is.na(strtmp), strMsgDataset, strtmp)

    strtmp <- dtStrings["strMsgTitle"]$value
    strMsgTitle <- ifelse(is.na(strtmp), strMsgTitle, strtmp)
  }

  # ---------------------------------------------------------------------------

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
    container = gv,
    expand = FALSE,
    fill = "x"
  )

  # Datasets ------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDestination, container = g0)

  g0_samples_lbl <- glabel(text = paste(" 0", strLblSamples), container = g0)

  dataset_drp <- gcombobox(
    items = c(
      strDrpDefault,
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

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    ok <- checkDataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gDataDest <<- get(val_obj, envir = env)
      .gDataDestName <<- val_obj
      .gDataDestColumns <<- names(.gDataDest)

      samples <- length(unique(.gDataDest$Sample.Name))
      svalue(g0_samples_lbl) <- paste(" ", samples, strLblSamples)
      svalue(save_edt) <- paste(.gDataDestName, "_new", sep = "")

      # Update dropdown menues.
      f1_key_drp[] <- c(
        strDrpDefault,
        intersect(.gDataDestColumns, .gDataSourceColumns)
      )
      f1_key2_drp[] <- c(
        strDrpDefault,
        intersect(.gDataDestColumns, .gDataSourceColumns)
      )

      # Select default value.
      svalue(f1_key_drp, index = TRUE) <- 1
      svalue(f1_key2_drp, index = TRUE) <- 1
    } else {

      # Reset components.
      .gDataDest <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strLblSamples)
      svalue(save_edt) <- ""

      # Update dropdown menues.
      f1_key_drp[] <- strDrpDefault
      f1_key2_drp[] <- strDrpDefault

      # Select default value.
      svalue(f1_key_drp, index = TRUE) <- 1
      svalue(f1_key2_drp, index = TRUE) <- 1
    }
  })

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblSource, container = g1)

  g1_ref_lbl <- glabel(text = paste(" 0", strLblSamples), container = g1)

  refset_drp <- gcombobox(
    items = c(
      strDrpDefault,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(refset_drp, handler = function(h, ...) {
    val_obj <- svalue(refset_drp)

    # Check if suitable.
    ok <- checkDataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gDataSource <<- get(val_obj, envir = env)
      .gDataSourceColumns <<- names(.gDataSource)
      ref <- length(unique(.gDataSource$Sample.Name))
      svalue(g1_ref_lbl) <- paste(" ", ref, strLblSamples)

      # Update dropdown menues.
      f1_key_drp[] <- c(
        strDrpDefault,
        intersect(.gDataDestColumns, .gDataSourceColumns)
      )

      f1_key2_drp[] <- c(
        strDrpDefault,
        intersect(.gDataDestColumns, .gDataSourceColumns)
      )

      f1_col_drp[] <- c(strDrpDefault, .gDataSourceColumns)

      # Select default value.
      svalue(f1_key_drp, index = TRUE) <- 1
      svalue(f1_key2_drp, index = TRUE) <- 1
      svalue(f1_col_drp, index = TRUE) <- 1
    } else {
      .gDataSource <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(g1_ref_lbl) <- paste(" 0", strLblSamples)

      # Update dropdown menues.
      f1_key_drp[] <- strDrpDefault
      f1_key2_drp[] <- strDrpDefault
      f1_col_drp[] <- strDrpDefault

      # Select default value.
      svalue(f1_key_drp, index = TRUE) <- 1
      svalue(f1_key2_drp, index = TRUE) <- 1
      svalue(f1_col_drp, index = TRUE) <- 1
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1_exact_chk <- gcheckbox(
    text = strChkExact,
    checked = TRUE, container = f1
  )

  f1_ignore_chk <- gcheckbox(
    text = strChkIgnore,
    checked = TRUE, container = f1
  )

  enabled(f1_ignore_chk) <- !svalue(f1_exact_chk)

  glabel(text = strLblKey1, container = f1, anchor = c(-1, 0))
  f1_key_drp <- gcombobox(
    items = strDrpDefault,
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  glabel(
    text = strLblKey2, container = f1,
    anchor = c(-1, 0)
  )
  f1_key2_drp <- gcombobox(
    items = strDrpDefault,
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  glabel(
    text = strLblColumns, container = f1,
    anchor = c(-1, 0)
  )
  f1_col_drp <- gcombobox(
    items = strDrpDefault,
    selected = 1,
    editable = FALSE,
    container = f1,
    ellipsize = "none"
  )

  f1_col_edt <- gedit(
    text = "", initial.msg = strEdtMsg,
    container = f1
  )

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f1_exact_chk, handler = function(h, ...) {
    val_obj <- svalue(f1_exact_chk)

    if (val_obj) {
      enabled(f1_ignore_chk) <- FALSE
    } else {
      enabled(f1_ignore_chk) <- TRUE
    }
  })

  addHandlerChanged(f1_col_drp, handler = function(h, ...) {
    val_drp <- svalue(f1_col_drp)
    val_edt <- svalue(f1_col_edt)

    if (!is.null(val_drp) && val_drp != strDrpDefault) {
      if (nchar(val_edt) == 0) {
        svalue(f1_col_edt) <- val_drp
      } else {
        svalue(f1_col_edt) <- paste(val_edt, val_drp, sep = ",")
      }
    }
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################


  add_btn <- gbutton(text = strBtnAdd, container = gv)

  addHandlerClicked(add_btn, handler = function(h, ...) {

    # Get values.
    val_destination <- svalue(dataset_drp)
    val_source <- svalue(refset_drp)
    val_exact <- svalue(f1_exact_chk)
    val_key <- svalue(f1_key_drp)
    val_key2 <- svalue(f1_key2_drp)
    val_what <- svalue(f1_col_edt)
    val_ignore <- svalue(f1_ignore_chk)
    val_name <- svalue(save_edt)

    # Check if default.
    if (val_key == strDrpDefault) {
      val_key <- NULL
    }
    if (val_key2 == strDrpDefault) {
      val_key2 <- NULL
    }

    # Prepare columns to add.
    if (nchar(val_what) == 0) {

      # Default is all columns.
      val_what <- NULL
    } else {

      # Create vector of column names to add.
      delimeters <- ",|, | |;|; |:|: "
      val_what <- strsplit(x = val_what, split = delimeters, fixed = FALSE)
      val_what <- unlist(val_what)
    }

    if (debug) {
      print("val_exact")
      print(val_exact)
      print("val_key")
      print(val_key)
      print("val_key2")
      print(val_key2)
      print("val_what")
      print(val_what)
      print("val_name")
      print(val_name)
      print("val_ignore")
      print(val_ignore)
    }

    # Check dataset and first key (second key is optional)
    if (!is.null(.gDataDest) & !is.null(.gDataSource) & !is.null(val_key)) {

      # Change button.
      blockHandlers(add_btn)
      svalue(add_btn) <- strBtnAddActive
      unblockHandlers(add_btn)
      enabled(add_btn) <- FALSE

      datanew <- addData(
        data = .gDataDest,
        new.data = .gDataSource,
        exact = val_exact,
        by.col = val_key,
        then.by.col = val_key2,
        what = val_what,
        ignore.case = val_ignore
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "new.data", "exact", "by.col", "then.by.col",
        "what", "ignore.case"
      )

      values <- list(
        val_destination, val_source, val_exact, val_key, val_key2,
        val_what, val_ignore
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
        print(datanew)
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
      if (exists(".strvalidator_addData_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_addData_gui_savegui", envir = env)
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
      if (exists(".strvalidator_addData_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_addData_gui_exact", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_addData_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_addData_gui_exact", value = svalue(f1_exact_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_addData_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addData_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_addData_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addData_gui_exact", envir = env)
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
