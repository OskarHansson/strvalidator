#' @title Read GeneMapper Kit Definition (GUI)
#'
#' @description
#' Import GeneMapper kit definition files through a graphical user interface.
#'
#' @details
#' Select the kit bins and panels file using the file picker.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return data.frame
#'
#' @export
#'
#' @importFrom gWidgets2 gwindow ggroup gfilebrowse gcombobox addHandlerChanged svalue gmessage gbutton enabled visible
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom utils read.table
#'
readGeneMapperKit_gui <- function(env = globalenv(), savegui = TRUE, debug = FALSE, parent = NULL, callback = NULL) {
  if (debug) message("Initializing GUI elements...")

  # Create GUI elements
  window <- gwindow("GeneMapper Kit Import", parent = parent)
  group <- ggroup(cont = window, horizontal = FALSE, expand = TRUE)

  # File selection for bins and panels file
  bins_file_browser <- gfilebrowse(text = "Select GeneMapper Bins File", type = "open", cont = group)
  panels_file_browser <- gfilebrowse(text = "Select GeneMapper Panels File", type = "open", cont = group)

  # Panel selection dropdown (initially empty but visible and inactive)
  panel_combo <- gcombobox(c(""), cont = group)
  enabled(panel_combo) <- FALSE

  # Confirm button (visible but inactive)
  confirm_button <- gbutton("Import Kit", cont = group)
  enabled(confirm_button) <- FALSE

  kit_info <- NULL # Store kit_info locally

  # Function to handle file selection and processing
  process_files <- function() {
    bins_file_path <- svalue(bins_file_browser)
    panels_file_path <- svalue(panels_file_browser)

    if (debug) {
      message("Bins file selected: ", bins_file_path)
      message("Panels file selected: ", panels_file_path)
    }

    if (file.exists(bins_file_path) && file.exists(panels_file_path)) {
      # Read and combine the bins and panels files into kit_info
      kit_info <<- try(readGeneMapperKit(bin.files = bins_file_path, panel.files = panels_file_path, debug = debug), silent = TRUE)

      # Handle errors
      if (inherits(kit_info, "try-error") || is.null(kit_info) || nrow(kit_info) == 0) {
        gmessage("Failed to read GeneMapper Kit files or no data found.", parent = window)
        enabled(panel_combo) <- FALSE
        enabled(confirm_button) <- FALSE
        kit_info <<- NULL
        return(NULL)
      }

      if (!"Panel" %in% colnames(kit_info)) {
        gmessage("The selected files do not contain Panel data.", parent = window)
        enabled(panel_combo) <- FALSE
        enabled(confirm_button) <- FALSE
        kit_info <<- NULL
        return(NULL)
      }

      panels <- unique(kit_info$Panel)
      if (length(panels) == 0) {
        gmessage("No panels found in the selected files.", parent = window)
        enabled(panel_combo) <- FALSE
        enabled(confirm_button) <- FALSE
        kit_info <<- NULL
        return(NULL)
      }

      # Populate panel selection and enable UI
      panel_combo[] <- c("All Panels", panels)
      enabled(panel_combo) <- TRUE
      enabled(confirm_button) <- TRUE
      if (debug) message("Panels found: ", paste(panels, collapse = ", "))

      return(kit_info) # Return the processed kit_info
    } else {
      # Disable elements until files are selected
      enabled(panel_combo) <- FALSE
      enabled(confirm_button) <- FALSE
      kit_info <<- NULL
      return(NULL)
    }
  }

  # File selection handlers
  addHandlerChanged(bins_file_browser, handler = function(h, ...) {
    kit_info <- process_files()
  })
  addHandlerChanged(panels_file_browser, handler = function(h, ...) {
    kit_info <- process_files()
  })

  # Confirm button handler
  addHandlerChanged(confirm_button, handler = function(h, ...) {
    if (is.null(kit_info)) {
      gmessage("Please select both the bins and panels files.", parent = window)
      return(NULL)
    }

    selected_panel <- svalue(panel_combo)
    panel_data <- if (selected_panel == "All Panels" || selected_panel == "") kit_info else kit_info[kit_info$Panel == selected_panel, ]

    if (nrow(panel_data) == 0) {
      gmessage("No data found for the selected panel.", parent = window)
      return(NULL)
    }

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
  })

  # Show GUI
  visible(window) <- TRUE
  focus(window)

  if (debug) message("GUI ready for interaction.")

  return(invisible(NULL)) # Nothing to return initially; wait for the user to click "Confirm"
}
