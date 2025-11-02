################################################################################
# CHANGE LOG (last 20 changes)
# 16.09.2022: Filename as suggested name for dataset for single file import.
# 10.09.2022: Compacted the gui. Removed destroy workaround.
# 16.03.2020: Added language support.
# 17.03.2019: Fixed widgets not enabled.
# 01.03.2019: Rearranged widgets and changed visibility for more intuitive options.
# 22.02.2019: Compressed tcltk gui.
# 15.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 17.07.2017: Added label "Selected for import".
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed expanded 'gexpandgroup'.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 11.07.2017: Changed part of layout and Implemented last directory as default.
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.12.2015: Removed "0" from the default 'na.strings'.
# 04.12.2015: Implemented new parameter 'na.strings'.
# 05.10.2015: Added attributes.
# 29.08.2015: Added importFrom.
# 23.05.2015: Added new options available in 'import'.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.

#' @title Import Data
#'
#' @description
#' GUI wrapper for the \code{\link{import}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{import}} function by providing a graphical
#' user interface to it.
#'
#' @param env environment into which the object will be saved.
#' Default is the current environment.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help
#' @importFrom tools file_path_sans_ext
#'
#' @seealso \code{\link{import}}


import_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Define variables.
  .default_file_dir <- NA
  .default_file_name <- NA
  .default_dir <- NA

  .selectedFile <- NULL
  .selectedFolder <- NULL
  .batchImport <- TRUE

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Import from files"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strBtnFile <- "Select file"
  strTipFile <- "Import of a single file into one dataset."
  strMsgFile <- "Select a file..."
  strBtnFolder <- "Select folder"
  strTipFolder <- "Batch import of multiple files into one dataset."
  strMsgFolder <- "Select a folder..."
  strLblSelected <- "Selected for import:"
  strFrmOptions <- "Options"
  strExpMultiple <- "Multiple files options"
  strChkIgnore <- "Ignore case"
  strLblPrefix <- "Prefix:"
  strLblSuffix <- "Suffix:"
  strLblExtension <- "Extension:"
  strChkName <- "Save file name"
  strChkTime <- "Save file time stamp"
  strLblDelimiter <- "Delimiter:"
  strDrpTab <- "TAB"
  strDrpSpace <- "SPACE"
  strDrpComma <- "COMMA"
  strDrpSemicolon <- "SEMICOLON"
  strLblNA <- "NA strings (separated by comma):"
  strChkTrim <- "Auto trim samples"
  strLblTrim <- "Trim samples containing the word (separate by pipe |):"
  strChkInvert <- "Invert (remove matching samples"
  strChkSlim <- "Auto slim repeated columns"
  strChkKeep <- "Keep all fixed (keep a row even if no data)"
  strFrmSave <- "Save as"
  strLblSave <- "Name for dataset:"
  strMsgSave <- "Name for new dataset"
  strBtnImport <- "Import"
  strBtnProcessing <- "Processing..."
  strMsgName <- "A name for the dataset must be provided."
  strMsgDataset <- "Dataset empty!\nCheck your file filter."
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

    strtmp <- dtStrings["strBtnFile"]$value
    strBtnFile <- ifelse(is.na(strtmp), strBtnFile, strtmp)

    strtmp <- dtStrings["strTipFile"]$value
    strTipFile <- ifelse(is.na(strtmp), strTipFile, strtmp)

    strtmp <- dtStrings["strMsgFile"]$value
    strMsgFile <- ifelse(is.na(strtmp), strMsgFile, strtmp)

    strtmp <- dtStrings["strBtnFolder"]$value
    strBtnFolder <- ifelse(is.na(strtmp), strBtnFolder, strtmp)

    strtmp <- dtStrings["strTipFolder"]$value
    strTipFolder <- ifelse(is.na(strtmp), strTipFolder, strtmp)

    strtmp <- dtStrings["strMsgFolder"]$value
    strMsgFolder <- ifelse(is.na(strtmp), strMsgFolder, strtmp)

    strtmp <- dtStrings["strLblSelected"]$value
    strLblSelected <- ifelse(is.na(strtmp), strLblSelected, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strExpMultiple"]$value
    strExpMultiple <- ifelse(is.na(strtmp), strExpMultiple, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strLblPrefix"]$value
    strLblPrefix <- ifelse(is.na(strtmp), strLblPrefix, strtmp)

    strtmp <- dtStrings["strLblSuffix"]$value
    strLblSuffix <- ifelse(is.na(strtmp), strLblSuffix, strtmp)

    strtmp <- dtStrings["strLblExtension"]$value
    strLblExtension <- ifelse(is.na(strtmp), strLblExtension, strtmp)

    strtmp <- dtStrings["strChkName"]$value
    strChkName <- ifelse(is.na(strtmp), strChkName, strtmp)

    strtmp <- dtStrings["strChkTime"]$value
    strChkTime <- ifelse(is.na(strtmp), strChkTime, strtmp)

    strtmp <- dtStrings["strLblDelimiter"]$value
    strLblDelimiter <- ifelse(is.na(strtmp), strLblDelimiter, strtmp)

    strtmp <- dtStrings["strDrpTab"]$value
    strDrpTab <- ifelse(is.na(strtmp), strDrpTab, strtmp)

    strtmp <- dtStrings["strDrpSpace"]$value
    strDrpSpace <- ifelse(is.na(strtmp), strDrpSpace, strtmp)

    strtmp <- dtStrings["strDrpComma"]$value
    strDrpComma <- ifelse(is.na(strtmp), strDrpComma, strtmp)

    strtmp <- dtStrings["strDrpSemicolon"]$value
    strDrpSemicolon <- ifelse(is.na(strtmp), strDrpSemicolon, strtmp)

    strtmp <- dtStrings["strLblNA"]$value
    strLblNA <- ifelse(is.na(strtmp), strLblNA, strtmp)

    strtmp <- dtStrings["strChkTrim"]$value
    strChkTrim <- ifelse(is.na(strtmp), strChkTrim, strtmp)

    strtmp <- dtStrings["strLblTrim"]$value
    strLblTrim <- ifelse(is.na(strtmp), strLblTrim, strtmp)

    strtmp <- dtStrings["strChkInvert"]$value
    strChkInvert <- ifelse(is.na(strtmp), strChkInvert, strtmp)

    strtmp <- dtStrings["strChkSlim"]$value
    strChkSlim <- ifelse(is.na(strtmp), strChkSlim, strtmp)

    strtmp <- dtStrings["strChkKeep"]$value
    strChkKeep <- ifelse(is.na(strtmp), strChkKeep, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strMsgSave"]$value
    strMsgSave <- ifelse(is.na(strtmp), strMsgSave, strtmp)

    strtmp <- dtStrings["strBtnImport"]$value
    strBtnImport <- ifelse(is.na(strtmp), strBtnImport, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strMsgName"]$value
    strMsgName <- ifelse(is.na(strtmp), strMsgName, strtmp)

    strtmp <- dtStrings["strMsgDataset"]$value
    strMsgDataset <- ifelse(is.na(strtmp), strMsgDataset, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)
  }

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(
    title = strWinTitle,
    visible = FALSE
  )

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
  gh <- ggroup(container = gv, expand = FALSE, fill = TRUE)

  savegui_chk <- gcheckbox(text = strChkGui, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # GUI #######################################################################

  # Button for single file import.
  select_file <- gbutton(text = strBtnFile, container = gv)
  tooltip(select_file) <- strTipFile

  addHandlerClicked(select_file, handler = function(h, ...) {
    # Set batch import flag.
    .batchImport <<- FALSE

    # Check if last location is available.
    if (!is.na(.default_file_dir) && !is.na(.default_file_name)) {
      m_file_dir <- .default_file_dir
      message("Last used file directory: ", m_file_dir)
      m_filename <- .default_file_name
      message("Last used file name: ", m_filename)
    } else {
      m_file_dir <- getwd()
      m_filename <- NULL
      message("Could not access last used import directory.")
      message("Using current working directory: ", getwd())
    }

    # Open file selector.
    .selectedFile <<- gfile(
      text = strMsgFile, type = "open",
      initial.filename = m_filename,
      initial.dir = m_file_dir
    )

    # Save last used filename and directory.
    if (length(.selectedFile) > 0) {
      .default_file_name <<- basename(.selectedFile)
      .default_file_dir <<- dirname(.selectedFile)
    }

    # Update current selection.
    svalue(selected_lbl) <- paste0(c(strLblSelected, .selectedFile))
    # Suggest filename as name for dataset.
    svalue(import_edt) <- file_path_sans_ext(basename(.selectedFile))
  })

  # Button for multiple files import.
  select_folder <- gbutton(text = strBtnFolder, container = gv)
  tooltip(select_folder) <- strTipFolder

  addHandlerClicked(select_folder, handler = function(h, ...) {
    # Set import from folder flag.
    .batchImport <<- TRUE

    # Check if last location is available.
    if (!is.na(.default_dir)) {
      m_dir <- .default_dir
      message("Last used batch directory: ", m_dir)
    } else {
      m_dir <- getwd()
      message("Could not access last used import directory.")
      message("Using current working directory: ", getwd())
    }

    # Open folder selector.
    .selectedFolder <<- gfile(
      text = strMsgFolder, type = "selectdir",
      initial.dir = m_dir
    )

    # Save last used directory.
    if (length(.selectedFolder) > 0) {
      .default_dir <<- .selectedFolder
    }

    # Update current selection.
    svalue(selected_lbl) <- paste0(c(strLblSelected, .selectedFolder))
  })

  # Selected status label.
  selected_lbl <- glabel(text = strLblSelected, container = gv, anchor = c(-1, 0))

  # OPTIONS -------------------------------------------------------------------

  opt_frm <- gframe(
    text = strFrmOptions, pos = 0, horizontal = FALSE,
    spacing = 1, container = gv
  )

  # MULTIPLE FILES OPTIONS ----------------------------------------------------

  multi_frm <- gexpandgroup(
    text = strExpMultiple,
    horizontal = FALSE, container = opt_frm
  )

  # Start collapsed.
  visible(multi_frm) <- FALSE

  multi_case_chk <- gcheckbox(
    text = strChkIgnore, checked = TRUE,
    container = multi_frm
  )

  glabel(text = strLblPrefix, container = multi_frm, anchor = c(-1, 0))
  multi_pre_edt <- gedit(container = multi_frm, expand = TRUE, fill = TRUE)

  glabel(text = strLblSuffix, container = multi_frm, anchor = c(-1, 0))
  multi_suf_edt <- gedit(container = multi_frm, expand = TRUE, fill = TRUE)

  glabel(text = strLblExtension, container = multi_frm, anchor = c(-1, 0))
  multi_ext_edt <- gedit(
    text = "txt", container = multi_frm,
    expand = TRUE, fill = TRUE
  )

  # INFO ----------------------------------------------------------------------

  opt_file_chk <- gcheckbox(
    text = strChkName, checked = TRUE,
    container = opt_frm
  )

  opt_time_chk <- gcheckbox(
    text = strChkTime, checked = TRUE,
    container = opt_frm
  )

  # DELIMITER -----------------------------------------------------------------

  glabel(text = strLblDelimiter, container = opt_frm, anchor = c(-1, 0))
  opt_sep_drp <- gcombobox(
    items = c(strDrpTab, strDrpSpace, strDrpComma, strDrpSemicolon),
    selected = 1, editable = FALSE, container = opt_frm,
    ellipsize = "none"
  )

  glabel(
    text = strLblNA,
    container = opt_frm, anchor = c(-1, 0)
  )
  opt_na_edt <- gedit(text = "NA,,", container = opt_frm)

  # TRIM ----------------------------------------------------------------------

  opt_trim_chk <- gcheckbox(
    text = strChkTrim, checked = FALSE,
    container = opt_frm
  )

  addHandlerChanged(opt_trim_chk, handler = function(h, ...) {
    .updateGui()
  })

  trim_grp <- ggroup(horizontal = FALSE, container = opt_frm)
  enabled(trim_grp) <- FALSE

  glabel(
    text = strLblTrim,
    container = trim_grp, anchor = c(-1, 0)
  )

  trim_samples_edt <- gedit(
    text = "pos|neg|ladder",
    width = 25, container = trim_grp, expand = TRUE
  )

  trim_invert_chk <- gcheckbox(
    text = strChkInvert, checked = TRUE,
    container = trim_grp
  )

  # SLIM ----------------------------------------------------------------------

  opt_slim_chk <- gcheckbox(
    text = strChkSlim,
    checked = FALSE, container = opt_frm
  )

  addHandlerChanged(opt_slim_chk, handler = function(h, ...) {
    .updateGui()
  })

  slim_grp <- ggroup(horizontal = FALSE, container = opt_frm)

  enabled(slim_grp) <- FALSE

  slim_fix_chk <- gcheckbox(
    text = strChkKeep,
    checked = TRUE, container = slim_grp
  )

  # SAVE --------------------------------------------------------------------

  save_frm <- gframe(
    text = strFrmSave, pos = 0,
    horizontal = FALSE, container = gv
  )

  glabel(text = strLblSave, container = save_frm, anchor = c(-1, 0))
  import_edt <- gedit(
    initial.msg = strMsgSave,
    container = save_frm, expand = TRUE, fill = TRUE
  )

  # IMPORT --------------------------------------------------------------------

  import_btn <- gbutton(text = strBtnImport, container = gv)


  addHandlerClicked(import_btn, handler = function(h, ...) {
    # Get values.
    file_val <- .selectedFile
    folder_val <- .selectedFolder
    ignore_val <- svalue(multi_case_chk)
    prefix_val <- svalue(multi_pre_edt)
    suffix_val <- svalue(multi_suf_edt)
    extension_val <- svalue(multi_ext_edt)
    folder_opt_val <- .batchImport
    val_name <- svalue(import_edt)
    get_file_val <- svalue(opt_file_chk)
    get_time_val <- svalue(opt_time_chk)
    del_val <- svalue(opt_sep_drp, index = TRUE)
    na_val <- svalue(opt_na_edt)
    trim_val <- svalue(opt_trim_chk)
    trim_what_val <- svalue(trim_samples_edt)
    trim_invert_val <- svalue(trim_invert_chk)
    slim_val <- svalue(opt_slim_chk)
    slim_fix_val <- svalue(slim_fix_chk)

    # Assign a delimiter character.
    val_delimiter <- NA
    if (del_val == 1) {
      val_delimiter <- "\t"
    } else if (del_val == 2) {
      val_delimiter <- " "
    } else if (del_val == 3) {
      val_delimiter <- ","
    } else if (del_val == 4) {
      val_delimiter <- ";"
    }

    # Convert to character vector.
    val_na <- unlist(strsplit(na_val, ","))

    # Initiate variable.
    ok <- TRUE

    # Check that a name has been provided for the new data object.
    if (nchar(val_name) == 0) {
      gmessage(
        msg = strMsgName,
        title = strMsgTitleError, icon = "error", parent = w
      )

      ok <- FALSE
    }

    # Check if ok to import data to 'env'.
    if (ok) {
      # Set arguments.
      if (folder_opt_val) {
        file_val <- NA
      } else {
        folder_val <- NA
      }

      if (!nchar(prefix_val) > 0) {
        prefix_val <- NA
      }

      if (!nchar(suffix_val) > 0) {
        suffix_val <- NA
      }

      if (debug) {
        print("val_name")
        print(val_name)
        print("ignore_val")
        print(ignore_val)
        print("prefix_val")
        print(prefix_val)
        print("suffix_val")
        print(suffix_val)
        print("folder_opt_val")
        print(folder_opt_val)
        print("file_val")
        print(file_val)
        print("folder_val")
        print(folder_val)
        print("get_file_val")
        print(get_file_val)
        print("get_time_val")
        print(get_time_val)
        print("del_val")
        print(del_val)
        print("na_val")
        print(na_val)
        print("val_na")
        print(val_na)
        print("trim_val")
        print(trim_val)
        print("trim_what_val")
        print(trim_what_val)
        print("trim_invert_val")
        print(trim_invert_val)
        print("slim_val")
        print(slim_val)
        print("slim_fix_val")
        print(slim_fix_val)
      }

      # Change button.
      blockHandlers(import_btn)
      svalue(import_btn) <- strBtnProcessing
      unblockHandlers(import_btn)
      enabled(import_btn) <- FALSE

      # Call function.
      datanew <- import(
        folder = folder_opt_val,
        extension = extension_val,
        suffix = suffix_val,
        prefix = prefix_val,
        import.file = file_val,
        folder.name = folder_val,
        file.name = get_file_val,
        time.stamp = get_time_val,
        separator = val_delimiter,
        ignore.case = ignore_val,
        auto.trim = trim_val,
        trim.samples = trim_what_val,
        trim.invert = trim_invert_val,
        auto.slim = slim_val,
        slim.na = slim_fix_val,
        na.strings = val_na,
        debug = debug
      )

      if (length(datanew) == 0) {
        # Show warning.
        gmessage(
          msg = strMsgDataset,
          title = strMsgTitleError,
          icon = "error",
          parent = w
        )

        # Change button.
        blockHandlers(import_btn)
        svalue(import_btn) <- strBtnImport
        unblockHandlers(import_btn)
        enabled(import_btn) <- TRUE
      } else {
        # Create key-value pairs to log.
        keys <- list(
          "folder", "extension", "suffix", "prefix",
          "import.file", "folder.name", "file.name", "time.stamp",
          "ignore.case", "auto.trim", "trim.samples", "trim.invert",
          "auto.slim", "slim.na", "separator", "na.strings"
        )

        values <- list(
          folder_opt_val, extension_val, suffix_val, prefix_val,
          file_val, folder_val, get_file_val, get_time_val,
          ignore_val, trim_val, trim_what_val, trim_invert_val,
          slim_val, slim_fix_val, val_delimiter, val_na
        )

        # Update audit trail.
        datanew <- auditTrail(
          obj = datanew, key = keys, value = values,
          label = fnc, arguments = FALSE,
          package = "strvalidator"
        )

        # Save data.
        saveObject(
          name = val_name, object = datanew, parent = w,
          env = env, debug = debug
        )

        # Close GUI.
        .saveSettings()
        dispose(w)
      }
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {
    # Get values.
    val_trim <- svalue(opt_trim_chk)
    val_slim <- svalue(opt_slim_chk)
    folder_opt_val <- .batchImport


    if (val_trim) {
      enabled(trim_grp) <- TRUE
    } else {
      enabled(trim_grp) <- FALSE
    }

    if (val_slim) {
      enabled(slim_grp) <- TRUE
    } else {
      enabled(slim_grp) <- FALSE
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
      if (exists(".strvalidator_import_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_import_gui_savegui", envir = env)
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
      if (exists(".strvalidator_import_gui_last_dir", envir = env, inherits = FALSE)) {
        .default_dir <<- get(".strvalidator_import_gui_last_dir", envir = env)
      }
      if (exists(".strvalidator_import_gui_last_file_dir", envir = env, inherits = FALSE)) {
        .default_file_dir <<- get(".strvalidator_import_gui_last_file_dir", envir = env)
      }
      if (exists(".strvalidator_import_gui_last_file_name", envir = env, inherits = FALSE)) {
        .default_file_name <<- get(".strvalidator_import_gui_last_file_name", envir = env)
      }
      if (exists(".strvalidator_import_gui_file", envir = env, inherits = FALSE)) {
        svalue(opt_file_chk) <- get(".strvalidator_import_gui_file", envir = env)
      }
      if (exists(".strvalidator_import_gui_time", envir = env, inherits = FALSE)) {
        svalue(opt_time_chk) <- get(".strvalidator_import_gui_time", envir = env)
      }
      if (exists(".strvalidator_import_gui_sep", envir = env, inherits = FALSE)) {
        svalue(opt_sep_drp) <- get(".strvalidator_import_gui_sep", envir = env)
      }
      if (exists(".strvalidator_import_gui_na", envir = env, inherits = FALSE)) {
        svalue(opt_na_edt) <- get(".strvalidator_import_gui_na", envir = env)
      }
      if (exists(".strvalidator_import_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(multi_case_chk) <- get(".strvalidator_import_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_import_gui_prefix", envir = env, inherits = FALSE)) {
        svalue(multi_pre_edt) <- get(".strvalidator_import_gui_prefix", envir = env)
      }
      if (exists(".strvalidator_import_gui_suffix", envir = env, inherits = FALSE)) {
        svalue(multi_suf_edt) <- get(".strvalidator_import_gui_suffix", envir = env)
      }
      if (exists(".strvalidator_import_gui_extension", envir = env, inherits = FALSE)) {
        svalue(multi_ext_edt) <- get(".strvalidator_import_gui_extension", envir = env)
      }
      if (exists(".strvalidator_import_gui_trim", envir = env, inherits = FALSE)) {
        svalue(opt_trim_chk) <- get(".strvalidator_import_gui_trim", envir = env)
      }
      if (exists(".strvalidator_import_gui_trim_samples", envir = env, inherits = FALSE)) {
        svalue(trim_samples_edt) <- get(".strvalidator_import_gui_trim_samples", envir = env)
      }
      if (exists(".strvalidator_import_gui_trim_invert", envir = env, inherits = FALSE)) {
        svalue(trim_invert_chk) <- get(".strvalidator_import_gui_trim_invert", envir = env)
      }
      if (exists(".strvalidator_import_gui_slim", envir = env, inherits = FALSE)) {
        svalue(opt_slim_chk) <- get(".strvalidator_import_gui_slim", envir = env)
      }
      if (exists(".strvalidator_import_gui_slim_fix", envir = env, inherits = FALSE)) {
        svalue(slim_fix_chk) <- get(".strvalidator_import_gui_slim_fix", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_import_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_import_gui_last_dir", value = .default_dir, envir = env)
      assign(x = ".strvalidator_import_gui_last_file_dir", value = .default_file_dir, envir = env)
      assign(x = ".strvalidator_import_gui_last_file_name", value = .default_file_name, envir = env)
      assign(x = ".strvalidator_import_gui_file", value = svalue(opt_file_chk), envir = env)
      assign(x = ".strvalidator_import_gui_time", value = svalue(opt_time_chk), envir = env)
      assign(x = ".strvalidator_import_gui_sep", value = svalue(opt_sep_drp), envir = env)
      assign(x = ".strvalidator_import_gui_na", value = svalue(opt_na_edt), envir = env)
      assign(x = ".strvalidator_import_gui_ignore", value = svalue(multi_case_chk), envir = env)
      assign(x = ".strvalidator_import_gui_prefix", value = svalue(multi_pre_edt), envir = env)
      assign(x = ".strvalidator_import_gui_suffix", value = svalue(multi_suf_edt), envir = env)
      assign(x = ".strvalidator_import_gui_extension", value = svalue(multi_ext_edt), envir = env)
      assign(x = ".strvalidator_import_gui_trim", value = svalue(opt_trim_chk), envir = env)
      assign(x = ".strvalidator_import_gui_trim_samples", value = svalue(trim_samples_edt), envir = env)
      assign(x = ".strvalidator_import_gui_trim_invert", value = svalue(trim_invert_chk), envir = env)
      assign(x = ".strvalidator_import_gui_slim", value = svalue(opt_slim_chk), envir = env)
      assign(x = ".strvalidator_import_gui_slim_fix", value = svalue(slim_fix_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_import_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_import_gui_last_dir", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_last_dir", envir = env)
      }
      if (exists(".strvalidator_import_gui_last_file_dir", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_last_file_dir", envir = env)
      }
      if (exists(".strvalidator_import_gui_last_file_name", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_last_file_name", envir = env)
      }
      if (exists(".strvalidator_import_gui_file", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_file", envir = env)
      }
      if (exists(".strvalidator_import_gui_time", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_time", envir = env)
      }
      if (exists(".strvalidator_import_gui_sep", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_sep", envir = env)
      }
      if (exists(".strvalidator_import_gui_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_na", envir = env)
      }
      if (exists(".strvalidator_import_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_import_gui_prefix", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_prefix", envir = env)
      }
      if (exists(".strvalidator_import_gui_suffix", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_suffix", envir = env)
      }
      if (exists(".strvalidator_import_gui_extension", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_extension", envir = env)
      }
      if (exists(".strvalidator_import_gui_trim", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_trim", envir = env)
      }
      if (exists(".strvalidator_import_gui_trim_samples", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_trim_samples", envir = env)
      }
      if (exists(".strvalidator_import_gui_trim_invert", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_trim_invert", envir = env)
      }
      if (exists(".strvalidator_import_gui_slim", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_slim", envir = env)
      }
      if (exists(".strvalidator_import_gui_slim_fix", envir = env, inherits = FALSE)) {
        remove(".strvalidator_import_gui_slim_fix", envir = env)
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
