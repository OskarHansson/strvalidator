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

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Import from files",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_BTN_FILE            = "Select file",
    STR_TIP_FILE            = "Import of a single file into one dataset.",
    STR_MSG_FILE            = "Select a file...",
    STR_BTN_FOLDER          = "Select folder",
    STR_TIP_FOLDER          = "Batch import of multiple files into one dataset.",
    STR_MSG_FOLDER          = "Select a folder...",
    STR_LBL_SELECTED        = "Selected for import:",
    STR_FRM_OPTIONS         = "Options",
    STR_EXP_MULTIPLE        = "Multiple files options",
    STR_CHK_IGNORE          = "Ignore case",
    STR_LBL_PREFIX          = "Prefix:",
    STR_LBL_SUFFIX          = "Suffix:",
    STR_LBL_EXTENSION       = "Extension:",
    STR_CHK_NAME            = "Save file name",
    STR_CHK_TIME            = "Save file time stamp",
    STR_LBL_DELIMITER       = "Delimiter:",
    STR_DRP_TAB             = "TAB",
    STR_DRP_SPACE           = "SPACE",
    STR_DRP_COMMA           = "COMMA",
    STR_DRP_SEMICOLON       = "SEMICOLON",
    STR_LBL_NA              = "NA strings (separated by comma):",
    STR_CHK_TRIM            = "Auto trim samples",
    STR_LBL_TRIM            = "Trim samples containing the word (separate by pipe |):",
    STR_CHK_INVERT          = "Invert (remove matching samples",
    STR_CHK_SLIM            = "Auto slim repeated columns",
    STR_CHK_KEEP            = "Keep all fixed (keep a row even if no data)",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for dataset:",
    STR_MSG_SAVE            = "Name for new dataset",
    STR_BTN_IMPORT          = "Import",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_NAME            = "A name for the dataset must be provided.",
    STR_MSG_DATASET         = "Dataset empty!\nCheck your file filter.",
    STR_MSG_TITLE_ERROR     = "Error"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(
    title = strings$STR_WIN_TITLE,
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

  savegui_chk <- gcheckbox(text = strings$STR_CHK_GUI, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # GUI #######################################################################

  # Button for single file import.
  select_file <- gbutton(text = strings$STR_BTN_FILE, container = gv)
  tooltip(select_file) <- strings$STR_TIP_FILE

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
      text = strings$STR_MSG_FILE, type = "open",
      initial.filename = m_filename,
      initial.dir = m_file_dir
    )

    # Save last used filename and directory.
    if (length(.selectedFile) > 0) {
      .default_file_name <<- basename(.selectedFile)
      .default_file_dir <<- dirname(.selectedFile)
    }

    # Update current selection.
    svalue(selected_lbl) <- paste0(c(strings$STR_LBL_SELECTED, .selectedFile))
    # Suggest filename as name for dataset.
    svalue(import_edt) <- file_path_sans_ext(basename(.selectedFile))
  })

  # Button for multiple files import.
  select_folder <- gbutton(text = strings$STR_BTN_FOLDER, container = gv)
  tooltip(select_folder) <- strings$STR_TIP_FOLDER

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
      text = strings$STR_MSG_FOLDER, type = "selectdir",
      initial.dir = m_dir
    )

    # Save last used directory.
    if (length(.selectedFolder) > 0) {
      .default_dir <<- .selectedFolder
    }

    # Update current selection.
    svalue(selected_lbl) <- paste0(c(strings$STR_LBL_SELECTED, .selectedFolder))
  })

  # Selected status label.
  selected_lbl <- glabel(text = strings$STR_LBL_SELECTED, container = gv, anchor = c(-1, 0))

  # OPTIONS -------------------------------------------------------------------

  opt_frm <- gframe(
    text = strings$STR_FRM_OPTIONS, pos = 0, horizontal = FALSE,
    spacing = 1, container = gv
  )

  # MULTIPLE FILES OPTIONS ----------------------------------------------------

  multi_frm <- gexpandgroup(
    text = strings$STR_EXP_MULTIPLE,
    horizontal = FALSE, container = opt_frm
  )

  # Start collapsed.
  visible(multi_frm) <- FALSE

  multi_case_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE, checked = TRUE,
    container = multi_frm
  )

  glabel(text = strings$STR_LBL_PREFIX, container = multi_frm, anchor = c(-1, 0))
  multi_pre_edt <- gedit(container = multi_frm, expand = TRUE, fill = TRUE)

  glabel(text = strings$STR_LBL_SUFFIX, container = multi_frm, anchor = c(-1, 0))
  multi_suf_edt <- gedit(container = multi_frm, expand = TRUE, fill = TRUE)

  glabel(text = strings$STR_LBL_EXTENSION, container = multi_frm, anchor = c(-1, 0))
  multi_ext_edt <- gedit(
    text = "txt", container = multi_frm,
    expand = TRUE, fill = TRUE
  )

  # INFO ----------------------------------------------------------------------

  opt_file_chk <- gcheckbox(
    text = strings$STR_CHK_NAME, checked = TRUE,
    container = opt_frm
  )

  opt_time_chk <- gcheckbox(
    text = strings$STR_CHK_TIME, checked = TRUE,
    container = opt_frm
  )

  # DELIMITER -----------------------------------------------------------------

  glabel(text = strings$STR_LBL_DELIMITER, container = opt_frm, anchor = c(-1, 0))
  opt_sep_drp <- gcombobox(
    items = c(strings$STR_DRP_TAB, strings$STR_DRP_SPACE, strings$STR_DRP_COMMA, strings$STR_DRP_SEMICOLON),
    selected = 1, editable = FALSE, container = opt_frm,
    ellipsize = "none"
  )

  glabel(
    text = strings$STR_LBL_NA,
    container = opt_frm, anchor = c(-1, 0)
  )
  opt_na_edt <- gedit(text = "NA,,", container = opt_frm)

  # TRIM ----------------------------------------------------------------------

  opt_trim_chk <- gcheckbox(
    text = strings$STR_CHK_TRIM, checked = FALSE,
    container = opt_frm
  )

  addHandlerChanged(opt_trim_chk, handler = function(h, ...) {
    .updateGui()
  })

  trim_grp <- ggroup(horizontal = FALSE, container = opt_frm)
  enabled(trim_grp) <- FALSE

  glabel(
    text = strings$STR_LBL_TRIM,
    container = trim_grp, anchor = c(-1, 0)
  )

  trim_samples_edt <- gedit(
    text = "pos|neg|ladder",
    width = 25, container = trim_grp, expand = TRUE
  )

  trim_invert_chk <- gcheckbox(
    text = strings$STR_CHK_INVERT, checked = TRUE,
    container = trim_grp
  )

  # SLIM ----------------------------------------------------------------------

  opt_slim_chk <- gcheckbox(
    text = strings$STR_CHK_SLIM,
    checked = TRUE, container = opt_frm
  )

  addHandlerChanged(opt_slim_chk, handler = function(h, ...) {
    .updateGui()
  })

  slim_grp <- ggroup(horizontal = FALSE, container = opt_frm)

  enabled(slim_grp) <- FALSE

  slim_fix_chk <- gcheckbox(
    text = strings$STR_CHK_KEEP,
    checked = TRUE, container = slim_grp
  )

  # SAVE --------------------------------------------------------------------

  save_frm <- gframe(
    text = strings$STR_FRM_SAVE, pos = 0,
    horizontal = FALSE, container = gv
  )

  glabel(text = strings$STR_LBL_SAVE, container = save_frm, anchor = c(-1, 0))
  import_edt <- gedit(
    initial.msg = strings$STR_MSG_SAVE,
    container = save_frm, expand = TRUE, fill = TRUE
  )

  # IMPORT --------------------------------------------------------------------

  import_btn <- gbutton(text = strings$STR_BTN_IMPORT, container = gv)


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
        msg = strings$STR_MSG_NAME,
        title = strings$STR_MSG_TITLE_ERROR, icon = "error", parent = w
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
      svalue(import_btn) <- strings$STR_BTN_PROCESSING
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
          msg = strings$STR_MSG_DATASET,
          title = strings$STR_MSG_TITLE_ERROR,
          icon = "error",
          parent = w
        )

        # Change button.
        blockHandlers(import_btn)
        svalue(import_btn) <- strings$STR_BTN_IMPORT
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
        datanew <- audit_trail(
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
