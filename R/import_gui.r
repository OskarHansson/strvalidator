################################################################################
# CHANGE LOG (last 20 changes)
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
# 20.01.2014: Remove redundant "overwrite?" message dialog.
# 13.01.2014: Handle empty dataframe by stay in gui and show message.
# 10.12.2013: Updated with new parameter names in function 'import'.
# 12.11.2013: Pass debug to function.

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
#'
#' @seealso \code{\link{import}}


import_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Define variables.
  .default_file_dir <- NA
  .default_file_name <- NA
  .default_dir <- NA

  .selectedFile <- NULL
  .selectedFolder <- NULL
  .batchImport <- TRUE

  .selectedLabel <- "Selected for import: "

  # Main window.
  w <- gwindow(
    title = "Import from files",
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

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("import_gui", help_type = "html"))
  })

  # GUI #######################################################################

  # Button for single file import.
  select_file <- gbutton(text = "Select file", container = gv)
  tooltip(select_file) <- "Import of a single file into one dataset."

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
      text = "Select a file...", type = "open",
      initial.filename = m_filename,
      initial.dir = m_file_dir
    )

    # Save last used filename and directory.
    if (length(.selectedFile) > 0) {
      .default_file_name <<- basename(.selectedFile)
      .default_file_dir <<- dirname(.selectedFile)
    }

    # Update current selection.
    svalue(selected_lbl) <- paste0(c(.selectedLabel, .selectedFile))
  })

  # Button for multiple files import.
  select_folder <- gbutton(text = "Select folder", container = gv)
  tooltip(select_folder) <- "Batch import of multiple files into one dataset."

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
      text = "Select a folder...", type = "selectdir",
      initial.dir = m_dir
    )

    # Save last used directory.
    if (length(.selectedFolder) > 0) {
      .default_dir <<- .selectedFolder
    }

    # Update current selection.
    svalue(selected_lbl) <- paste0(c(.selectedLabel, .selectedFolder))
  })

  # Selected status label.
  selected_lbl <- glabel(text = .selectedLabel, container = gv, anchor = c(-1, 0))

  # OPTIONS -------------------------------------------------------------------

  opt_frm <- gframe(text = "Options", pos = 0, horizontal = FALSE, container = gv)

  opt_file_chk <- gcheckbox(
    text = "Save file name", checked = TRUE,
    container = opt_frm
  )

  opt_time_chk <- gcheckbox(
    text = "Save file time stamp", checked = TRUE,
    container = opt_frm
  )

  glabel(text = "Delimiter:", container = opt_frm, anchor = c(-1, 0))
  opt_sep_drp <- gcombobox(
    items = c("TAB", "SPACE", "COMMA", "SEMICOLON"),
    selected = 1, editable = FALSE, container = opt_frm,
    ellipsize = "none"
  )

  glabel(
    text = "NA strings (separated by comma):",
    container = opt_frm, anchor = c(-1, 0)
  )
  opt_na_edt <- gedit(text = "NA,,", container = opt_frm)

  opt_trim_chk <- gcheckbox(
    text = "Auto trim samples", checked = FALSE,
    container = opt_frm
  )

  opt_slim_chk <- gcheckbox(
    text = "Auto slim repeated columns",
    checked = FALSE, container = opt_frm
  )

  addHandlerChanged(opt_trim_chk, handler = function(h, ...) {
    .refresh()
  })

  addHandlerChanged(opt_slim_chk, handler = function(h, ...) {
    .refresh()
  })


  # MULTIPLE FILES OPTIONS ----------------------------------------------------

  multi_frm <- gexpandgroup(
    text = "Multiple files options",
    horizontal = FALSE, container = gv
  )

  # Start collapsed.
  visible(multi_frm) <- FALSE

  multi_case_chk <- gcheckbox(
    text = "Ignore case", checked = TRUE,
    container = multi_frm
  )

  glabel(text = "Prefix:", container = multi_frm, anchor = c(-1, 0))
  multi_pre_edt <- gedit(
    initial.msg = "", width = 25,
    container = multi_frm, expand = TRUE
  )

  glabel(text = "Suffix:", container = multi_frm, anchor = c(-1, 0))
  multi_suf_edt <- gedit(
    initial.msg = "", width = 25,
    container = multi_frm, expand = TRUE
  )

  glabel(text = "Extension:", container = multi_frm, anchor = c(-1, 0))
  multi_ext_edt <- gedit(
    text = "txt", width = 25,
    container = multi_frm, expand = TRUE
  )

  # TRIM ----------------------------------------------------------------------

  trim_frm <- gexpandgroup(
    text = "Trim options",
    horizontal = FALSE, container = gv
  )

  visible(trim_frm) <- FALSE

  glabel(
    text = "Trim samples containing the word (separate by pipe |):",
    container = trim_frm, anchor = c(-1, 0)
  )

  trim_samples_edt <- gedit(
    text = "pos|neg|ladder",
    width = 25, container = trim_frm, expand = TRUE
  )

  trim_invert_chk <- gcheckbox(
    text = "Invert (remove matching samples)", checked = TRUE,
    container = trim_frm
  )

  # SLIM ----------------------------------------------------------------------

  slim_frm <- gexpandgroup(
    text = "Slim options",
    horizontal = FALSE, container = gv
  )

  visible(slim_frm) <- FALSE

  slim_fix_chk <- gcheckbox(
    text = "Keep all fixed (keep a row even if no data)",
    checked = TRUE, container = slim_frm
  )

  # SAVE --------------------------------------------------------------------

  save_frm <- gframe(
    text = "Save options", pos = 0,
    horizontal = FALSE, container = gv
  )

  glabel(text = "Name:", container = save_frm, anchor = c(-1, 0))
  import_edt <- gedit(
    initial.msg = "Name for new dataset",
    width = 25, container = save_frm, expand = TRUE
  )

  # IMPORT --------------------------------------------------------------------

  import_btn <- gbutton(text = "Import", container = gv)


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
      gmessage("A name for the dataset must be provided.",
        title = "Error", icon = "error", parent = w
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
      svalue(import_btn) <- "Processing..."
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
          msg = "Dataset empty!\nCheck your file filter.",
          title = "Error",
          icon = "error",
          parent = w
        )

        # Change button.
        blockHandlers(import_btn)
        svalue(import_btn) <- "Import"
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
          label = "import_gui", arguments = FALSE,
          package = "strvalidator"
        )

        # Save data.
        saveObject(name = val_name, object = datanew, parent = w, env = env)

        # Close GUI.
        .saveSettings()
        dispose(w)
      }
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .refresh <- function() {

    # Get values.
    val_trim <- svalue(opt_trim_chk)
    val_slim <- svalue(opt_slim_chk)
    folder_opt_val <- .batchImport


    if (val_trim) {
      enabled(trim_samples_edt) <- TRUE
      enabled(trim_invert_chk) <- TRUE
    } else {
      enabled(trim_samples_edt) <- FALSE
      enabled(trim_invert_chk) <- FALSE
    }

    if (val_slim) {
      enabled(slim_fix_chk) <- TRUE
    } else {
      enabled(slim_fix_chk) <- FALSE
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
  .refresh()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
