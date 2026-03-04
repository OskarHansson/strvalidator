#' @title Read GeneMarker Kit Definition
#'
#' @description
#' Launch a GUI to import kit definitions from GeneMarker XML files.
#'
#' @details
#' The user selects a GeneMarker kit definition XML file using a file picker.
#' Unique panel names are extracted and shown in a drop-down menu. After selecting
#' a panel, kit information for the selected panel is imported and either returned,
#' saved in the current environment, or passed to a callback function.
#'
#' @param env environment in which to save the imported data.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical; if `TRUE`, print debug information to the console.
#' @param parent optional GUI widget to receive focus when finished.
#' @param callback optional function to receive the imported kit data.
#'
#' @return A data frame containing kit information if run interactively.
#' Invisibly returns `NULL` when using a callback or after saving to the environment.
#'
#' @export
#'
#' @importFrom gWidgets2 gwindow ggroup gfilebrowse gcombobox addHandlerChanged
#'   svalue gmessage gbutton enabled visible gcheckbox addSpring focus
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom magrittr %>%
#' @importFrom utils help
#' @importFrom graphics par
#' @importFrom tools file_path_sans_ext
#' @import gWidgets2tcltk

read_gene_marker_kit_gui <- function(env = parent.frame(),
                                     savegui = NULL, debug = FALSE,
                                     parent = NULL, callback = NULL) {
  # Load the language file for this specific GUI scope
  lng_strings <- get_strings(gui = get_gui_scope())

  # Define default strings
  default_strings <- list(
    STR_WIN_TITLE = "GeneMarker Kit Import",
    STR_CHK_GUI = "Save GUI settings",
    STR_BTN_HELP = "Help",
    STR_BTN_SELECT_FILE = "Select panel",
    STR_BTN_TRANSFORM = "Import Kit",
    STR_MSG_FILE_NOT_EXIST = "The selected file does not exist.",
    STR_MSG_READ_ERROR = "Failed to read XML file:",
    STR_MSG_NO_PANELS = "No panel names found in the selected XML file.",
    STR_MSG_SELECT_FILE = "You must select an XML file first.",
    STR_MSG_SELECT_PANEL = "You must select the panel to extract from the dropdown menu",
    STR_ERROR_OCCURRED = "An error occurred:"
  )

  # Update default strings with language-specific values
  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # Ensure the correct GUI toolkit is set
  options(guiToolkit = "tcltk")

  # Create main window
  window <- gwindow(title = strings$STR_WIN_TITLE, visible = FALSE)

  addHandlerUnrealize(window, handler = function(h, ...) {
    .save_settings()
    if (!is.null(parent)) {
      focus(parent)
    }
    FALSE
  })

  # Create main group
  group <- ggroup(horizontal = FALSE, container = window)

  header_group <- ggroup(container = group, expand = FALSE, fill = "both")
  savegui_chk <- gcheckbox(text = strings$STR_CHK_GUI, checked = FALSE, container = header_group)
  addSpring(header_group)
  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = header_group)
  addHandlerChanged(help_btn, handler = function(h, ...) {
    print(help("read_gene_marker_kit_gui", help_type = "html"))
  })

  # Button to select XML file
  file_button <- gfilebrowse(text = strings$STR_BTN_SELECT_FILE, container = group)

  # Dropdown for panel selection
  panel_dropdown <- gcombobox("", container = group)
  enabled(panel_dropdown) <- FALSE

  # Update dropdown items based on selected XML file
  addHandlerChanged(file_button, handler = function(h, ...) {
    file_path <- svalue(file_button)

    if (!file.exists(file_path)) {
      gmessage(strings$STR_MSG_FILE_NOT_EXIST, parent = window)
      return()
    }

    # Attempt to read the XML file safely.
    xml_doc <- tryCatch(
      {
        xml2::read_xml(file_path)
      },
      error = function(e) {
        gmessage(paste(strings$STR_MSG_READ_ERROR, e$message), parent = window)
        return(NULL)
      }
    )

    # If reading the XML file failed, exit the handler.
    if (is.null(xml_doc)) {
      return()
    }

    # Extract the panel names.
    panel_names <- xml2::xml_find_all(
      xml_doc, "//Panel/PanelName"
    ) %>% xml_text()

    if (length(panel_names) == 0) {
      gmessage(strings$STR_MSG_NO_PANELS, parent = window)
      return()
    }

    # Populate the dropdown menu.
    panel_dropdown[] <- c("All Panels", panel_names)
    enabled(panel_dropdown) <- TRUE
    svalue(panel_dropdown) <- "All Panels"
  })

  # Button to execute transformation
  action_button <- gbutton(
    text = strings$STR_BTN_TRANSFORM,
    container = group, handler = function(h, ...) {
      selected_file <- svalue(file_button)
      selected_panel <- svalue(panel_dropdown)

      if (selected_file == "") {
        gmessage(strings$STR_MSG_SELECT_FILE, parent = window)
        return()
      }

      if (is.null(selected_panel) || selected_panel == "" || is.na(selected_panel)) {
        gmessage(strings$STR_MSG_SELECT_PANEL, parent = window)
        return()
      }

      # Disable buttons
      enabled(file_button) <- FALSE
      enabled(action_button) <- FALSE

      # Use tryCatch to handle errors and ensure buttons are re-enabled.
      tryCatch({
        panel_data <- read_gene_marker_kit(selected_file, selected_panel)

        # Use callback or return directly
        if (!is.null(callback)) {
          if (debug) message("Return the data to the callback")
          callback(panel_data) # Pass data to the callback
          dispose(window) # Close the GUI
          return(invisible(NULL)) # No return in callback mode
        } else {
          # Use input filename as name for datasest.
          val_name <- tools::file_path_sans_ext(basename(selected_file))
          # Save kitinfo to the environment.
          save_object(
            name = val_name, object = panel_data,
            parent = window, env = env
          )
          if (debug) message("Save the data in environment")
          dispose(window) # Close the GUI
          return(invisible(NULL))
        }
      }, error = function(e) {
        gmessage(paste(strings$STR_ERROR_OCCURRED, e$message), parent = window)
      }, finally = {
        # Re-enable buttons after function execution
        enabled(file_button) <- TRUE
        enabled(action_button) <- TRUE
      })
    }
  )

  .load_settings <- function() {
    if (!is.null(savegui)) {
      svalue(savegui_chk) <- savegui
      enabled(savegui_chk) <- FALSE
    } else if (exists(".strvalidator_read_gene_marker_kit_gui_savegui", envir = env, inherits = FALSE)) {
      svalue(savegui_chk) <- get(".strvalidator_read_gene_marker_kit_gui_savegui", envir = env)
    }

    if (isTRUE(svalue(savegui_chk))) {
      if (exists(".strvalidator_read_gene_marker_kit_gui_file", envir = env, inherits = FALSE)) {
        saved_file <- get(".strvalidator_read_gene_marker_kit_gui_file", envir = env)
        if (is.character(saved_file) && length(saved_file) == 1 && nzchar(saved_file) && file.exists(saved_file)) {
          svalue(file_button) <- saved_file
        }
      }
    }
  }

  .save_settings <- function() {
    if (isTRUE(svalue(savegui_chk))) {
      assign(".strvalidator_read_gene_marker_kit_gui_savegui", svalue(savegui_chk), envir = env)
      assign(".strvalidator_read_gene_marker_kit_gui_file", svalue(file_button), envir = env)
    } else {
      for (nm in c(".strvalidator_read_gene_marker_kit_gui_savegui", ".strvalidator_read_gene_marker_kit_gui_file")) {
        if (exists(nm, envir = env, inherits = FALSE)) {
          remove(list = nm, envir = env)
        }
      }
    }
  }

  # Show the main window
  .load_settings()
  visible(window) <- TRUE
}
