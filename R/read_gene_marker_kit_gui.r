################################################################################
# CHANGE LOG (last 20 changes)
# 17.08.2024: New function to import kit from GeneMarker files.

#' @title Read GeneMarker Kit Definition
#'
#' @description
#' Read GeneMarker kit definition file.
#'
#' @details Select the kit definition XML-file using the file picker.
#' The unique Panel Names will appear in the drop-down menu.
#' Select the panel to import.
#' Kit information for the selected panel will be extracted.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in
#' the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return data.frame
#'
#' @export
#'
#' @importFrom gWidgets2 gwindow ggroup gfilebrowse gcombobox addHandlerChanged
#' svalue gmessage gbutton enabled visible
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom magrittr %>%
#' @importFrom utils help
#' @importFrom graphics par
#' @import gWidgets2tcltk

read_gene_marker_kit_gui <- function(env = parent.frame(),
                                     savegui = NULL, debug = FALSE,
                                     parent = NULL, callback = NULL) {
  # Load the language file for this specific GUI scope
  lng_strings <- getStrings(gui = "read_gene_marker_kit_gui")

  # Define default strings
  default_strings <- list(
    strWinTitle = "GeneMarker Kit Import",
    strBtnSelectFile = "Select panel",
    strBtnTransform = "Import Kit",
    strMsgFileNotExist = "The selected file does not exist.",
    strMsgReadError = "Failed to read XML file:",
    strMsgNoPanelNames = "No panel names found in the selected XML file.",
    strMsgSelectFileFirst = "You must select an XML file first.",
    strMsgSelectPanel = "You must select the panel to extract from the
    dropdown menu",
    strErrorOccurred = "An error occurred:"
  )

  # Update default strings with language-specific values
  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # Ensure the correct GUI toolkit is set
  options(guiToolkit = "tcltk")

  # Create main window
  window <- gwindow(title = strings$strWinTitle, visible = FALSE)

  # Create main group
  group <- ggroup(horizontal = FALSE, container = window)

  # Button to select XML file
  file_button <- gfilebrowse(text = strings$strBtnSelectFile, container = group)

  # Dropdown for panel selection
  panel_dropdown <- gcombobox("", container = group)
  enabled(panel_dropdown) <- FALSE

  # Update dropdown items based on selected XML file
  addHandlerChanged(file_button, handler = function(h, ...) {
    file_path <- svalue(file_button)

    if (!file.exists(file_path)) {
      gmessage(strings$strMsgFileNotExist, parent = window)
      return()
    }

    # Attempt to read the XML file safely.
    xml_doc <- tryCatch(
      {
        xml2::read_xml(file_path)
      },
      error = function(e) {
        gmessage(paste(strings$strMsgReadError, e$message), parent = window)
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
      gmessage(strings$strMsgNoPanelNames, parent = window)
      return()
    }

    # Populate the dropdown menu.
    panel_dropdown[] <- c("All Panels", panel_names)
    enabled(panel_dropdown) <- TRUE
    svalue(panel_dropdown) <- "All Panels"
  })

  # Button to execute transformation
  action_button <- gbutton(
    text = strings$strBtnTransform,
    container = group, handler = function(h, ...) {
      selected_file <- svalue(file_button)
      selected_panel <- svalue(panel_dropdown)

      if (selected_file == "") {
        gmessage(strings$strMsgSelectFileFirst, parent = window)
        return()
      }

      if (is.null(selected_panel) || selected_panel == "" || is.na(selected_panel)) {
        gmessage(strings$strMsgSelectPanel, parent = window)
        return()
      }

      # Disable buttons
      enabled(file_button) <- FALSE
      enabled(action_button) <- FALSE

      # Use tryCatch to handle errors and ensure buttons are re-enabled.
      tryCatch({
        message("Debug: before read_gene_marker_kit")
        panel_data <- read_gene_marker_kit(selected_file, selected_panel)
        message("Debug: after read_gene_marker_kit")

        # Use callback or return directly
        if (!is.null(callback)) {
          if (debug) message("Return the data to the callback")
          callback(panel_data) # Pass data to the callback
          dispose(window) # Close the GUI
          return(invisible(NULL)) # No return in callback mode
        } else {
          if (debug) message("Return the data synchronously")
          dispose(window) # Close the GUI before returning data
          return(panel_data) # Return the data synchronously if no callback
        }
      }, error = function(e) {
        gmessage(paste(strings$strErrorOccurred, e$message), parent = window)
      }, finally = {
        # Re-enable buttons after function execution
        enabled(file_button) <- TRUE
        enabled(action_button) <- TRUE
      })
    }
  )

  # Show the main window
  visible(window) <- TRUE
}
