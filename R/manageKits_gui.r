################################################################################
# CHANGE LOG (last 20 changes)
# 15.09.2024: Re-worked function replaces makeKit_gui.

#' @title Manage Kits
#'
#' @description
#' Manage kits, import new kits, or edit the kit file through a graphical user
#'  interface.
#'
#' @details
#' This function provides a graphical user interface (GUI) for managing kits,
#' including the ability to import new kits, edit the short and full names of
#' existing kits, or remove kits. The gender marker of each kit is auto-detected
#' but can be manually adjusted. Note that the short name for each kit must be
#' unique.
#'
#' @param env Environment in which to search for data frames.
#' @param savegui Logical indicating if GUI settings should be saved in the
#' environment (Currently not in use).
#' @param debug Logical indicating whether to print debug information.
#' @param parent Widget to get focus when finished.
#'
#' @return TRUE if the operation completes successfully.
#'
#' @export
#'
#' @importFrom gWidgets2 gwindow ggroup gframe gradio gbutton gedit glabel
#' addSpring visible svalue size focus gmessage
#' @import gWidgets2tcltk
#' @importFrom utils write.table read.delim help

# Core GUI function
manageKits_gui <- function(env = parent.frame(), savegui = NULL,
                           debug = FALSE, parent = NULL) {
  # Define the file path using the provided snippet
  separator <- .Platform$file.sep # Platform-dependent path separator
  package_path <- path.package("strvalidator", quiet = FALSE)
  sub_folder <- "extdata"
  file_name <- "kit.txt"
  file_path <- paste(package_path, sub_folder, file_name, sep = separator)

  # Internal variables
  kit_info <- NULL # Variable to hold kit information data frame

  # Existing kit info
  existing_kit_info <- NULL
  if (file.exists(file_path)) {
    existing_kit_info <- read.delim(
      file = file_path,
      header = TRUE,
      sep = "\t",
      quote = "\"",
      dec = ".",
      fill = TRUE,
      stringsAsFactors = FALSE
    )

    if (debug) {
      print("Existing kit file loaded in background.")
    }
  }

  # Get language-specific strings
  dt_strings <- getStrings(gui = "manageKits_gui")

  # Default strings for GUI labels and messages
  default_strings <- list(
    str_win_title = "Manage kits",
    str_btn_help = "Help",
    str_frm_action = "Action",
    str_rad_edit = "Edit kit file (overwrite)",
    str_rad_add = "Add new kits (append)",
    str_frm_file = "Path to current kit file",
    str_frm_new = "Import kit information from",
    str_btn_gene_mapperid = "Gene MapperID-X",
    str_btn_gene_marker = "Gene Marker",
    str_frm_save = "Save options",
    str_btn_save = "Save to kit definition file",
    str_btn_saving = "Saving...",
    str_btn_view = "View Kit File",
    str_msg_file_not_found = "The kit file was not found",
    str_msg_title_file_not_found = "File not found",
    str_msg_duplicate1 = "A kit with short name",
    str_msg_duplicate2 = "already exists! Short name must be unique!",
    str_msg_title_duplicate = "Duplicate short name",
    str_msg_missing = "A short name must be provided for all new kits",
    str_msg_title_missing = "Missing short name",
    str_lbl_sex = "List sex markers (separate by comma)",
    str_lbl_sensors = "List quality sensors (separate by comma)",
    str_lbl_remove = "Remove",
    str_lbl_panel = "Panel",
    str_lbl_short_name = "Short name",
    str_lbl_full_name = "Full name",
    str_lbl_none = "<none>",
    str_msg_updated = "Kit definition file updated!",
    str_msg_no_kit_info_returned = "No kit information was returned.",
    str_msg_no_kit_info_loaded = "No kit information loaded.",
    str_msg_no_kit_info_to_save = "No kit information to save.",
    str_msg_title_error = "Error",
    str_msg_error_reading_file = "Error reading file:",
    str_msg_error_writing_file = "Error writing to file:"
  )

  # Update strings with language-specific values
  strings <- update_strings_with_language_file(
    default_strings,
    dt_strings$value
  )

  # ----------------------------------------------------------------------------

  # Main window
  main_window <- gwindow(title = strings$str_win_title, visible = FALSE)

  # Runs when window is closed
  addHandlerUnrealize(main_window, handler = function(h, ...) {
    # Focus on parent window
    if (!is.null(parent)) {
      focus(parent)
    }

    # Destroy window
    return(FALSE)
  })

  # Main group container for the GUI components
  main_group <- ggroup(
    horizontal = FALSE, spacing = 1,
    use.scrollwindow = FALSE, container = main_window,
    expand = TRUE
  )

  # Help Button
  help_group <- ggroup(container = main_group, expand = FALSE, fill = "both")
  addSpring(help_group)
  help_btn <- gbutton(text = strings$str_btn_help, container = help_group)

  addHandlerClicked(help_btn, handler = function(h, ...) {
    print(help("manageKits_gui", help_type = "html"))
  })

  # ACTION FRAME ###############################################################

  # Action Selection (Edit or Add Kits)
  action_selection_frame <- gframe(
    text = strings$str_frm_action,
    horizontal = FALSE, spacing = 1,
    container = main_group
  )
  option_radio <- gradio(
    items = c(strings$str_rad_add, strings$str_rad_edit),
    selected = 1, container = action_selection_frame
  )

  addHandlerChanged(option_radio, handler = function(h, ...) {
    # Get status of add_kit flag
    add_kit <- svalue(option_radio, index = TRUE) == 1

    if (add_kit) {
      # Empty kit info
      populate_gui_with_kits(NULL)

      # Enable these widgets
      enabled(import_kit_info_frame) <- TRUE # New kit import options
      enabled(save_file_btn) <- TRUE # Save kit definition file button

      # Disable these widgets
      enabled(file_path_frame) <- FALSE # Load kit file options
    } else {
      # Enable these widgets
      enabled(import_kit_info_frame) <- FALSE # New kit import options
      enabled(save_file_btn) <- TRUE # Save kit definition file button

      # Disable these widgets
      # Load kit file options are disabled since loading happens automatically
      enabled(file_path_frame) <- FALSE

      # Load current kit info
      populate_gui_with_kits(existing_kit_info, add_kit = FALSE)
    }
  })

  # PATH FRAME #################################################################

  # Kit File Path
  file_path_frame <- gframe(
    text = strings$str_frm_file, horizontal = TRUE,
    spacing = 1, container = main_group
  )
  gedit(
    text = file_path, container = file_path_frame,
    expand = TRUE, fill = TRUE
  )
  enabled(file_path_frame) <- FALSE # Disabled since loading is automatic

  # VIEW KIT FILE ##############################################################

  # Button to view the current kit file
  view_btn <- gbutton(strings$str_btn_view, container = main_group)

  addHandlerClicked(view_btn, handler = function(h, ...) {
    if (!is.null(existing_kit_info)) {
      # Display the kit information in a datatable
      dt <- DT::datatable(existing_kit_info,
        rownames = FALSE, filter = "top",
        extensions = "Buttons", options = list(
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print")
        )
      )
      print(dt)
    } else {
      gmessage(strings$str_msg_no_kit_info_loaded, parent = main_window)
    }
  })

  # IMPORT FRAME ###############################################################

  # Import Kit Information Section
  import_kit_info_frame <- gframe(
    text = strings$str_frm_new,
    horizontal = FALSE, spacing = 1,
    container = main_group
  )
  gene_mapperid_btn <- gbutton(
    text = strings$str_btn_gene_mapperid,
    container = import_kit_info_frame
  )
  gene_marker_btn <- gbutton(
    text = strings$str_btn_gene_marker,
    container = import_kit_info_frame
  )

  # Handler for importing kits
  addHandlerClicked(gene_mapperid_btn, handler = function(h, ...) {
    read_gene_mapper_kit_gui(
      env = env,
      savegui = savegui,
      debug = debug,
      parent = main_window,
      callback = function(imported_kit_info) {
        if (!is.null(imported_kit_info) && nrow(imported_kit_info) > 0) {
          # Assign to the outer scope variable kit_info
          kit_info <<- imported_kit_info
          # Populate GUI with new kit information
          populate_gui_with_kits(imported_kit_info, add_kit = TRUE)
        } else {
          gmessage(str_msg_no_kit_info_returned, parent = main_window)
        }
      }
    )
  })

  # Handler for importing kits
  addHandlerClicked(gene_marker_btn, handler = function(h, ...) {
    read_gene_marker_kit_gui(
      env = env,
      savegui = savegui,
      debug = debug,
      parent = main_window,
      callback = function(imported_kit_info) {
        if (!is.null(imported_kit_info) && nrow(imported_kit_info) > 0) {
          # Assign to the outer scope variable kit_info
          kit_info <<- imported_kit_info
          # Populate GUI with new kit information
          populate_gui_with_kits(imported_kit_info)
        } else {
          gmessage(str_msg_no_kit_info_returned, parent = main_window)
        }
      }
    )
  })

  # INFO FRAME #################################################################

  # Frame to display kit information for editing
  kit_info_layout_frame <- ggroup(
    horizontal = FALSE, use.scrollwindow = TRUE,
    expand = TRUE, fill = TRUE,
    container = main_group
  )

  # Layout to display the kit information.
  kit_info_layout <- glayout(
    container = kit_info_layout_frame, spacing = 1,
    expand = TRUE, fill = "both", homogeneous = TRUE
  )

  # SAVE FRAME #################################################################

  # Save options (Append, Overwrite)
  save_options_frame <- gframe(
    text = strings$str_frm_save, horizontal = FALSE,
    expand = FALSE, spacing = 1,
    container = main_group
  )
  save_file_btn <- gbutton(
    text = strings$str_btn_save, expand = TRUE,
    container = save_options_frame
  )

  addHandlerClicked(save_file_btn, handler = function(h, ...) {
    if (debug) {
      print("save_file_btn clicked")
    }

    if (is.null(kit_info)) {
      gmessage(strings$str_msg_no_kit_info_to_save, parent = main_window)
      return()
    }

    # Disable and update button text
    enabled(save_file_btn) <- FALSE
    svalue(save_file_btn) <- strings$str_btn_saving

    # Get status of add_kit flag
    add_kit <- svalue(option_radio, index = TRUE) == 1

    # Read changes from gui
    read_from_gui(kit_info = kit_info, kit_append = add_kit)

    if (add_kit) {
      # Append to kit file

      tryCatch(
        {
          write.table(
            rbind(existing_kit_info, kit_info),
            file = file_path,
            row.names = FALSE,
            col.names = TRUE,
            sep = "\t"
          )

          # Update existing kit.
          existing_kit_info <<- rbind(existing_kit_info, kit_info)

          if (debug) {
            print("Append:")
            print(head(kit_info))
            print(tail(kit_info))
            print(head(existing_kit_info))
            print(tail(existing_kit_info))
          }

          gmessage(strings$str_msg_updated, parent = main_window)
        },
        error = function(e) {
          gmessage(
            msg = paste(strings$str_msg_error_writing_file, e$message),
            title = strings$str_msg_title_error,
            icon = "error",
            parent = main_window
          )
        }
      )
    } else {
      # Overwrite kit file

      tryCatch(
        {
          write.table(
            kit_info,
            file = file_path,
            row.names = FALSE,
            col.names = TRUE,
            sep = "\t"
          )

          # Update existing_kit_info
          existing_kit_info <<- kit_info

          if (debug) {
            print("Overwrite:")
            print(head(kit_info))
            print(tail(kit_info))
            print(head(existing_kit_info))
            print(tail(existing_kit_info))
          }

          gmessage(strings$str_msg_updated, parent = main_window)
        },
        error = function(e) {
          gmessage(
            msg = paste(strings$str_msg_error_writing_file, e$message),
            title = strings$str_msg_title_error,
            icon = "error",
            parent = main_window
          )
        }
      )
    }

    # Load current kit info
    populate_gui_with_kits(existing_kit_info, add_kit = FALSE)

    # Enter edit mode after saving
    svalue(option_radio, index = TRUE) <- 2

    # Enable and update button text
    enabled(save_file_btn) <- TRUE
    svalue(save_file_btn) <- strings$str_btn_save
  })

  # INTERNAL FUNCTIONS #########################################################

  # Helper function to clean and process marker inputs
  clean_marker_input <- function(marker_input) {
    if (debug) {
      print("clean_marker_input")
      print(marker_input)
    }

    # Remove spaces after commas (e.g., ", " becomes ",")
    cleaned_input <- gsub(",\\s+", ",", marker_input)

    # Remove spaces before commas (e.g., " ," or "  ," becomes ",")
    cleaned_input <- gsub("\\s+,", ",", cleaned_input)

    # Remove any multiple consecutive commas
    cleaned_input <- gsub(",{2,}", ",", cleaned_input)

    # Trim leading and trailing whitespace
    cleaned_input <- trimws(cleaned_input)

    if (debug) {
      print(cleaned_input)
    }

    return(cleaned_input)
  }

  #-----------------------------------------------------------------------------

  # Function to add kit information
  read_from_gui <- function(kit_info, kit_append) {
    # Proceed only if kit info exists
    if (is.null(kit_info)) {
      return()
    }

    # Initialize vectors to store kit information
    remove_kit <- logical()
    short_name <- character()
    full_name <- character()
    sex_markers <- character()
    qs_markers <- character()

    # Extract unique panels from the kit info
    panels <- unique(kit_info$Panel)

    # Iterate through each panel to gather information
    for (p in seq_along(panels)) {
      remove_kit[p] <- svalue(kit_info_layout[1 + p, 1])
      short_name[p] <- svalue(kit_info_layout[1 + p, 3])
      full_name[p] <- svalue(kit_info_layout[1 + p, 4])

      # Process and clean sex marker and quality sensor inputs
      sex_markers[p] <- clean_marker_input(svalue(kit_info_layout[1 + p, 5]))
      qs_markers[p] <- clean_marker_input(svalue(kit_info_layout[1 + p, 6]))
    }

    # Debug information
    if (debug) {
      cat("Short.Name:", short_name, "\n")
      cat("Full.Name:", full_name, "\n")
      cat("Sex.Markers:", sex_markers, "\n")
      cat("QS.Markers:", qs_markers, "\n")
    }

    # Validate short names and check for duplicates
    if (!any(short_name[!remove_kit] == "")) {
      if (kit_append) { # Append new kits
        existing_short_names <- short_name[!remove_kit] %in% getKit()
      } else { # Overwrite or save as new data frame
        existing_short_names <- duplicated(toupper(short_name[!remove_kit]))
      }

      if (!any(existing_short_names)) {
        # Process each panel to update kit information
        for (p in seq_along(panels)) {
          update_kit_info(
            panels[p], short_name[p], full_name[p],
            sex_markers[p], qs_markers[p]
          )
        }

        # Remove kits marked for deletion
        if (any(remove_kit)) {
          kit_info <<- kit_info[!kit_info$Panel %in% panels[remove_kit], ]
        }

        # Debug information after processing
        if (debug) {
          cat("Processed kit info:\n")
          print(head(kit_info))
          print(tail(kit_info))
        }
      } else {
        gmessage(
          msg = paste(
            strings$str_msg_duplicate1,
            short_name[existing_short_names][1],
            strings$str_msg_duplicate2
          ),
          title = strings$str_msg_title_duplicate,
          icon = "error",
          parent = main_window
        )
      }
    } else {
      gmessage(
        msg = strings$str_msg_missing,
        title = strings$str_msg_title_missing,
        icon = "error",
        parent = main_window
      )
    }
  }

  #-----------------------------------------------------------------------------

  # Helper function to update kit information for a given panel
  update_kit_info <- function(panel, short_name, full_name,
                              sex_markers,
                              qs_markers) {
    if (is.null(kit_info$Short.Name)) {
      kit_info$Short.Name <<- NA
    }
    kit_info$Short.Name[kit_info$Panel == panel] <<- short_name

    if (is.null(kit_info$Full.Name)) {
      kit_info$Full.Name <<- NA
    }
    kit_info$Full.Name[kit_info$Panel == panel] <<- full_name

    # Update sex markers
    update_marker_flag(panel, sex_markers, "Sex.Marker")

    # Update quality sensors
    update_marker_flag(panel, qs_markers, "Quality.Sensor")
  }

  # Helper function to update marker flags (sex marker or quality sensor)
  update_marker_flag <- function(panel, markers, flag_name) {
    if (is.null(kit_info[[flag_name]])) {
      kit_info[[flag_name]] <<- NA
    }
    current_markers <- unlist(strsplit(markers, split = ",", fixed = TRUE))
    current_markers <- current_markers[current_markers != strings$str_lbl_none]
    sel_panel <- kit_info$Panel == panel
    sel_marker <- kit_info$Marker %in% current_markers
    kit_info[[flag_name]][sel_panel] <<- FALSE
    kit_info[[flag_name]][sel_panel & sel_marker] <<- TRUE

    # Check for misspelled markers
    if (!all(current_markers %in% kit_info$Marker[sel_panel])) {
      stop(paste(
        "Given", flag_name, ":",
        paste(current_markers[!current_markers %in% kit_info$Marker[sel_panel]], collapse = ","), "not found in", panel
      ))
    }
  }

  #-----------------------------------------------------------------------------

  # Function to pad labels with spaces
  pad_label <- function(label, max_length) {
    pad_length <- max(0, 2 * max_length - nchar(label) + 2) # Add extra spaces
    paste0(label, strrep(" ", pad_length))
  }

  # Internal function to populate the GUI with kits
  populate_gui_with_kits <- function(kit_info_input, add_kit = NULL) {
    if (debug) {
      message("Populating GUI with kits...")
      print(head(kit_info_input))
    }

    # Determine if we're adding kits or editing existing ones
    if (is.null(add_kit)) {
      add_kit <- svalue(option_radio, index = TRUE) == 1
    }

    # Update outer kit_info
    kit_info <<- kit_info_input

    # Clear the current layout if it exists
    if (!is.null(kit_info_layout)) {
      delete(obj = kit_info_layout_frame, child = kit_info_layout)
      if (debug) {
        message("Current layout cleared.")
      }
    }

    # Return early if no kit information is provided
    if (is.null(kit_info_input)) {
      if (debug) {
        message("No kit information provided.")
      }
      return(NULL)
    }

    # Get panels
    panels <- unique(kit_info_input$Panel)
    if (debug) {
      message("Panels: ", paste(panels, collapse = " "))
    }

    # Ensure required columns exist in kit_info
    if (!"Short.Name" %in% names(kit_info_input)) {
      if (debug) {
        message("Adding missing Short.Name column")
      }
      # Add an empty string column
      kit_info_input$Short.Name <- rep("", nrow(kit_info_input))
    }

    if (!"Full.Name" %in% names(kit_info_input)) {
      if (debug) {
        message("Adding missing Full.Name column")
      }
      # Add an empty string column
      kit_info_input$Full.Name <- rep("", nrow(kit_info_input))
    }

    if (!"Sex.Marker" %in% names(kit_info_input)) {
      if (debug) {
        message("Adding missing Sex.Marker column")
      }
      # Add a FALSE bolean column
      kit_info_input$Sex.Marker <- rep(FALSE, nrow(kit_info_input))
    }

    if (!"Quality.Sensor" %in% names(kit_info_input)) {
      if (debug) {
        message("Adding missing Quality.Sensor column")
      }
      # Add a FALSE bolean column
      kit_info_input$Quality.Sensor <- rep(FALSE, nrow(kit_info_input))
    }

    # Return early if no panels found
    if (length(panels) == 0) {
      gmessage("No panels found in the selected kit.", parent = main_window)
      return(NULL)
    }

    # Create a new layout for displaying the kit information
    kit_info_layout <<- glayout(
      container = kit_info_layout_frame,
      spacing = 1,
      expand = TRUE,
      fill = "both"
    )
    if (debug) {
      message("New kit layout created")
    }

    # Calculate width based on maximum string length
    max_full_name_length <- max(nchar(kit_info_input$Full.Name), na.rm = TRUE)
    str_lbl_full_name_padded <- pad_label(
      strings$str_lbl_full_name,
      max_full_name_length
    )

    # Add titles to the layout
    kit_info_layout[1, 1] <- glabel(
      text = strings$str_lbl_remove,
      container = kit_info_layout
    )
    kit_info_layout[1, 2] <- glabel(
      text = strings$str_lbl_panel,
      container = kit_info_layout
    )
    kit_info_layout[1, 3] <- glabel(
      text = strings$str_lbl_short_name,
      container = kit_info_layout
    )
    kit_info_layout[1, 4] <- glabel(
      text = str_lbl_full_name_padded,
      expand = TRUE, fill = "both",
      container = kit_info_layout
    )
    kit_info_layout[1, 5] <- glabel(
      text = strings$str_lbl_sex,
      expand = TRUE, fill = "both",
      container = kit_info_layout
    )
    kit_info_layout[1, 6] <- glabel(
      text = strings$str_lbl_sensors,
      expand = TRUE, fill = "both",
      container = kit_info_layout
    )

    # Loop over each panel and add the kit information to the layout
    for (p in seq_along(panels)) {
      markers <- unique(kit_info_input$Marker[kit_info_input$Panel == panels[p]])

      # Define short and full names, or use default values
      # Handling short_name
      short_name_values <- kit_info_input$Short.Name[kit_info_input$Panel == panels[p]]
      short_name_value <- unique(short_name_values[!is.na(short_name_values) & short_name_values != ""])
      if (length(short_name_value) == 0) {
        short_name <- ""
      } else {
        short_name <- short_name_value[1]
      }

      # Handling full_name
      full_name_values <- kit_info_input$Full.Name[kit_info_input$Panel == panels[p]]
      full_name_value <- unique(full_name_values[!is.na(full_name_values) & full_name_values != ""])
      if (length(full_name_value) == 0) {
        full_name <- as.character(panels[p])
      } else {
        full_name <- full_name_value[1]
      }

      # Handling sex markers
      if (add_kit) {
        # For new kits, attempt to auto-detect sex markers
        sex_markers <- grep("AM|Y", markers, ignore.case = TRUE, value = TRUE)
      } else {
        # For existing kits, use stored sex markers
        sex_markers <- unique(kit_info_input$Marker[kit_info_input$Panel == panels[p] & kit_info_input$Sex.Marker])
      }

      sex_markers <- ifelse(
        length(sex_markers) == 0,
        strings$str_lbl_none,
        paste(sex_markers, collapse = ",")
      )

      # Handling quality sensors
      # Detect quality sensors or retrieve them from existing kits
      if (add_kit) {
        # For new kits, attempt to auto-detect quality sensors
        quality_sensors <- grep("QS", markers, ignore.case = TRUE, value = TRUE)
      } else {
        # For existing kits, use stored quality sensors
        quality_sensors <- unique(kit_info_input$Marker[kit_info_input$Panel == panels[p] & kit_info_input$Quality.Sensor])
      }

      quality_sensors <- ifelse(
        length(quality_sensors) == 0,
        strings$str_lbl_none,
        paste(quality_sensors, collapse = ",")
      )

      # Add components to the layout
      kit_info_layout[p + 1, 1] <- gcheckbox(
        text = "", checked = FALSE,
        expand = FALSE,
        container = kit_info_layout
      )
      kit_info_layout[p + 1, 2] <- glabel(
        text = panels[p], expand = FALSE,
        container = kit_info_layout
      )
      kit_info_layout[p + 1, 3] <- gedit(
        text = short_name, expand = FALSE,
        container = kit_info_layout
      )
      kit_info_layout[p + 1, 4] <- gedit(
        text = full_name, expand = TRUE,
        fill = "both",
        container = kit_info_layout
      )
      kit_info_layout[p + 1, 5] <- gedit(
        text = sex_markers, expand = TRUE,
        fill = "both",
        container = kit_info_layout
      )
      kit_info_layout[p + 1, 6] <- gedit(
        text = quality_sensors, expand = TRUE,
        fill = "both",
        container = kit_info_layout
      )
    }
  }

  # GUI READY ##################################################################

  # Show the main window
  visible(main_window) <- TRUE
  focus(main_window)

  if (debug) message("GUI ready for interaction.")
}
