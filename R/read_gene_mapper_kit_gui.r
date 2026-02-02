#' @title Read GeneMapper Kit Definition (GUI)
#'
#' @description
#' Launch a graphical user interface (GUI) for importing GeneMapper kit
#' definition files (\code{.bins} and \code{.panels}) and previewing panel data.
#'
#' @details
#' This interactive interface allows users to browse and select GeneMapper
#' kit files from disk, automatically read them via
#' \code{\link{read_gene_mapper_kit}}, and optionally filter or return data
#' for a specific panel. The GUI validates file selection and provides feedback
#' if any file is invalid or contains no data.
#'
#' When both valid bins and panels files are selected, the available panels
#' are listed in a dropdown for inspection. The "Import Kit" button becomes
#' enabled once valid files are loaded. Data are returned either directly or
#' via a callback function, depending on the context.
#'
#' @param env Environment in which GUI settings are stored (default: 
#' \code{globalenv()}).
#' @param savegui Logical indicating if GUI settings should be saved between
#' sessions.
#' @param debug Logical indicating whether to print detailed diagnostic 
#' messages.
#' @param parent Optional parent window to assign focus after closing.
#' @param callback Optional function to receive the imported data instead of
#' returning it directly. The callback will be called as \code{callback(data)}.
#'
#' @return
#' If \code{callback} is \code{NULL}, returns a \code{data.frame} containing
#' the parsed GeneMapper kit definition. Otherwise, returns 
#' \code{invisible(NULL)} after passing the data to the callback function.
#'
#' @seealso
#' \code{\link{read_gene_mapper_kit}} for reading kits programmatically,  
#' \code{\link{combine_bins_and_panels}} for merging bins and panels data.
#'
#' @examples
#' \dontrun{
#' # Launch the GeneMapper Kit Import GUI
#' read_gene_mapper_kit_gui(debug = TRUE)
#'
#' # Launch with callback
#' read_gene_mapper_kit_gui(callback = function(data) {
#'   print(head(data))
#' })
#' }
#'
#' @importFrom gWidgets2 gwindow ggroup gfilebrowse gcombobox gbutton
#'   gmessage addHandlerChanged svalue enabled visible focus dispose
#' @export

read_gene_mapper_kit_gui <- function(env = globalenv(), savegui = TRUE,
                                     debug = FALSE, parent = NULL,
                                     callback = NULL) {
  if (debug) message("Initializing GUI elements...")

  # Create GUI elements
  window <- gwindow("GeneMapper Kit Import", parent = parent)
  group <- ggroup(container = window, horizontal = FALSE, expand = TRUE)

  # File selection for bins file
  bins_frame <- gframe(
    text = "GeneMapper bins file",
    horizontal = FALSE,
    container = group
  )
  
  bins_file_browser <- gfilebrowse(
    text = "Select GeneMapper Bins File",
    type = "open", container = bins_frame
  )
    
  # File selection for panels file
  panels_frame <- gframe(
    text = "GeneMapper panels file",
    horizontal = FALSE,
    container = group
  )

  panels_file_browser <- gfilebrowse(
    text = "Select GeneMapper Panels File",
    type = "open", container = panels_frame
  )
  
  # Panel selection dropdown (initially empty but visible and inactive)
  panel_combo <- gcombobox(c(""), container = group)
  enabled(panel_combo) <- FALSE

  # Confirm button (visible but inactive)
  confirm_button <- gbutton("Import Kit", container = group)
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
      kit_info <<- try(read_gene_mapper_kit(
        bin_files = bins_file_path,
        panel_files = panels_file_path,
        debug = debug
      ), silent = TRUE)

      # Handle errors
      if (inherits(kit_info, "try-error") || is.null(kit_info) || nrow(kit_info) == 0) {
        gmessage("Failed to read GeneMapper Kit files or no data found.",
          parent = window
        )
        enabled(panel_combo) <- FALSE
        enabled(confirm_button) <- FALSE
        kit_info <<- NULL
        return(NULL)
      }

      if (!"Panel" %in% colnames(kit_info)) {
        gmessage("The selected files do not contain Panel data.",
          parent = window
        )
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
      svalue(panel_combo) <- "All Panels"
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
    if (selected_panel == "All Panels") {
      panel_data <- kit_info
    } else {
      panel_data <- kit_info[kit_info$Panel == selected_panel, ]
    }

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
