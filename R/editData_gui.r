#' @title Edit or View Data Frames
#'
#' @description
#' GUI to edit and view data frames.
#'
#' @details Select a data frame from the drop-down to view or edit a dataset.
#' It is possible to save as a new dataframe. To enable sorting by clicking the
#' column headers the view mode must be used (i.e. edit = FALSE). There is an
#' option to limit the number of rows shown that can be used to preview large
#' datasets that may otherwise cause performance problems. Attributes of the
#' dataset can be views in a separate window.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param data data.frame for instant viewing.
#' @param name character string with the name of the provided dataset.
#' @param edit logical TRUE to enable edit (uses \code{\link[gWidgets2:gdf]{gdf}}, FALSE to
#' view and enable sorting by clicking a column header (uses \code{\link[gtable:gtable]{gtable}}).
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help write.table
#' @importFrom DT datatable
#'
#' @return TRUE
#'
#' @seealso \code{\link{trim_gui}}, \code{\link{cropData_gui}}, \code{\link{combine_gui}}

edit_data_gui <- function(env = parent.frame(), savegui = NULL, data = NULL,
                         name = NULL, edit = TRUE, debug = FALSE, parent = NULL) {
  .gData <- data
  .gDataName <- name
  .gDataTypes <- NULL        # list of original column classes
  .gDataDisplay <- NULL      # last display table we showed (character)
  .hideMsg <- FALSE
  
  if (!is.null(.gData)){
    .gDataTypes <- lapply(.gData, class)    
  } 

  # gedit cannot handle zero length 'text'.
  if (length(.gDataName) == 0) {
    .gDataName <- ""
  }

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # ---- Strings --------------------------------------------------------------
  # Load the language file for this specific GUI scope
  lng_strings <- get_strings(gui = get_gui_scope())
  
  # Define default strings
  default_strings <- list(
    STR_WIN_TITLE_EDIT      = "Edit or view dataset",
    STR_WIN_TITLE_VIEW      = "View dataset",
    STR_WIN_TITLE_ATTRIBUTES = "Attributes",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples,",
    STR_LBL_COLUMNS         = "columns,",
    STR_LBL_ROWS            = "rows",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_ATTRIBUTES      = "Show attributes (separate window)",
    STR_CHK_LIMIT           = "Limit number of rows to:",
    STR_TIP_LIMIT           = "NB! Sorting will only be performed on the loaded data.",
    STR_BTN_VIEW            = "View",
    STR_TIP_VIEW            = "View as interactive table in Posit Viewer tab.",
    STR_BTN_COPY            = "Copy",
    STR_TIP_COPY            = "Copy to clipboard (NB! large datasets might get truncated).",
    STR_BTN_COPYING         = "Copying...",
    STR_BTN_EXPORT          = "Export",
    STR_TIP_EXPORT          = "Opens the export dialog.",
    STR_BTN_SAVE            = "Save as",
    STR_TIP_SAVE            = "Save as new dataset in this project.",
    STR_BTN_SAVING          = "Saving...",
    STR_FRM_SAVE            = "View|Copy|Export|Save",
    STR_LBL_NO_DATA         = "There is no data",
    STR_MSG_SAVE            = "A name must be provided.",
    STR_MSG_TITLE_ERROR     = "Error",
    STR_LBL_TCLTK           = paste(
      "The tcltk gui toolkit does not handle NA values in tables.",
      "NA values will be replaced with empty strings.",
      "If you edit the table, NA values will be permanently replaced.",
      sep = "\n"
    ),
    STR_CHK_SHOW            = "Don't show this message again.",
    STR_MSG_TITLE_WARNING   = "Warning"
  )

  # Update default strings with language-specific values
  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # WINDOW ####################################################################

  if (edit) {
    guiTitle <- strings$STR_WIN_TITLE_EDIT
  } else {
    guiTitle <- strings$STR_WIN_TITLE_VIEW
  }

  # Create windows.
  w <- gwindow(title = guiTitle, visible = FALSE)
  w_attributes <- gwindow(title = strings$STR_WIN_TITLE_ATTRIBUTES, visible = FALSE)
  attr_text <- gtext("", container = w_attributes)

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

  savegui_chk <- gcheckbox(text = strings$STR_CHK_GUI, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  # Show info about selected dataset.
  g0_samples_lbl <- glabel(text = paste(" 0", strings$STR_LBL_SAMPLES), container = g0)
  g0_columns_lbl <- glabel(text = paste(" 0", strings$STR_LBL_COLUMNS), container = g0)
  g0_rows_lbl <- glabel(text = paste(" 0", strings$STR_LBL_ROWS), container = g0)

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      list_objects(
        env = env,
        obj_class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  if (!is.null(.gDataName) && nchar(.gDataName) > 0) {
    svalue(dataset_drp) <- .gDataName
  }

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    message("Dataset ", val_obj, " selected.")

    # Check if suitable.
    ok <- check_dataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      .gDataTypes <<- lapply(.gData, class)
      
      # Refresh info and load table.
      .refreshInfo()
      .refreshTbl()
    } else {
      # Clear info.
      .refreshInfo(clear = TRUE)
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  g1 <- glayout(container = f1, spacing = 1)

  g1[1, 1] <- f1_show_attr_chk <- gcheckbox(
    text = strings$STR_CHK_ATTRIBUTES,
    checked = FALSE, container = g1
  )


  g1[2, 1] <- f1_limit_chk <- gcheckbox(
    text = strings$STR_CHK_LIMIT,
    checked = FALSE, container = g1
  )
  tooltip(f1_limit_chk) <- strings$STR_TIP_LIMIT

  g1[2, 2] <- f1_max_edt <- gedit(text = 100, width = 8, container = g1)

  addHandlerChanged(f1_show_attr_chk, handler = function(h, ...) {
    if (svalue(f1_show_attr_chk)) {
      .showAttributes()
    } else {
      if (isExtant(w_attributes)) {
        visible(w_attributes) <- FALSE
      }
    }
  })

  addHandlerChanged(f1_limit_chk, handler = function(h, ...) {
    .refreshTbl()
  })


  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strings$STR_FRM_SAVE,
    horizontal = TRUE, spacing = 5, container = gv
  )

  view_btn <- gbutton(text = strings$STR_BTN_VIEW, container = f2)
  tooltip(view_btn) <- strings$STR_TIP_VIEW

  copy_btn <- gbutton(text = strings$STR_BTN_COPY, container = f2)
  tooltip(copy_btn) <- strings$STR_TIP_COPY

  export_btn <- gbutton(text = strings$STR_BTN_EXPORT, container = f2)
  tooltip(export_btn) <- strings$STR_TIP_EXPORT

  save_btn <- gbutton(text = strings$STR_BTN_SAVE, container = f2)
  tooltip(save_btn) <- strings$STR_TIP_SAVE

  save_txt <- gedit(text = .gDataName, container = f2, expand = TRUE, fill = TRUE)

  addHandlerClicked(view_btn, handler = function(h, ...) {
    val_tbl <- data_tbl[]

    # Disable button.
    enabled(view_btn) <- FALSE

    # Convert to DT and show in viewer.
    dt <- DT::datatable(val_tbl,
      rownames = FALSE,
      filter = "top", extensions = "Buttons",
      options = list(dom = "Blfrtip", buttons = c("copy", "csv", "excel", "pdf", "print"))
    )
    print(dt)

    # Enable button.
    enabled(view_btn) <- TRUE
  })

  addHandlerClicked(copy_btn, handler = function(h, ...) {
    val_tbl <- data_tbl[]

    # Change button.
    blockHandlers(copy_btn)
    svalue(copy_btn) <- strings$STR_BTN_COPYING
    unblockHandlers(copy_btn)
    enabled(copy_btn) <- FALSE

    # Copy data.
    write.table(val_tbl, "clipboard",
      sep = "\t", row.names = FALSE
    )

    # Change button.
    blockHandlers(copy_btn)
    svalue(copy_btn) <- strings$STR_BTN_COPY
    unblockHandlers(copy_btn)
    enabled(copy_btn) <- TRUE
  })

  addHandlerClicked(save_btn, handler = function(h, ...) {
    val_name <- svalue(save_txt)
    datanew <- data_tbl[]

    if (debug) {
      print("names(datanew)")
      print(names(datanew))
    }
    
    # If we showed a character display table (tcltk), coerce back to original types
    if (gtoolkit() == "tcltk" && !is.null(.gDataDisplay) && !is.null(.gDataTypes)) {
      datanew <- as.data.frame(datanew, stringsAsFactors = FALSE)
      
      for (nm in names(datanew)) {
        cls <- .gDataTypes[[nm]]
        if (is.null(cls)) next
        
        x <- datanew[[nm]]
        
        # Convert "" back to NA before type conversion
        x_chr <- as.character(x)
        x_chr[x_chr == ""] <- NA
        x <- x_chr
        
        # Use the first class as the target (common pattern)
        target <- cls[1]
        
        datanew[[nm]] <- switch(
          target,
          "numeric"  = suppressWarnings(as.numeric(x)),
          "double"   = suppressWarnings(as.numeric(x)),
          "integer"  = suppressWarnings(as.integer(x)),
          "logical"  = suppressWarnings(as.logical(x)),
          "factor"   = factor(x),
          "Date"     = suppressWarnings(as.Date(x)),
          "POSIXct"  = suppressWarnings(as.POSIXct(x)),
          "character"= as.character(x),
          # default fallback
          x
        )
      }
    }
    

    if (!is.na(val_name) && !is.null(val_name)) {
      # Copy and add attributes (retain names).
      names(.gData) <- names(datanew)
      attributes(datanew) <- attributes(.gData)

      # Change button.
      blockHandlers(save_btn)
      svalue(save_btn) <- strings$STR_BTN_SAVING
      unblockHandlers(save_btn)
      enabled(save_btn) <- FALSE

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, label = fnc,
        arguments = FALSE, package = "strvalidator"
      )

      # Save data.
      save_object(name = val_name, object = datanew, parent = w, env = env, debug = debug)

      # Change button.
      blockHandlers(save_btn)
      svalue(save_btn) <- strings$STR_BTN_SAVE
      unblockHandlers(save_btn)
      enabled(save_btn) <- TRUE
    } else {
      gmessage(
        msg = strings$STR_MSG_SAVE,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    }
  })

  addHandlerChanged(export_btn, handler = function(h, ...) {
    # Open GUI.
    export_gui(env = env, savegui = savegui, debug = debug, parent = parent)
  })

  # FRAME 3 ###################################################################

  f3 <- gvbox(container = gv, expand = TRUE, fill = TRUE)

  # Add dummy table.
  data_tbl <- gWidgets2::gtable(
    items = data.frame(Data = strings$STR_LBL_NO_DATA),
    container = f3, expand = TRUE
  )

  # INTERNAL FUNCTIONS ########################################################

  .showAttributes <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Get options.
    val_attr <- svalue(f1_show_attr_chk)

    if (val_attr & !is.null(.gData)) {
      if (!isExtant(w_attributes)) {
        # Re-create window.
        w_attributes <<- gwindow(
          title = strings$STR_WIN_TITLE_ATTRIBUTES,
          visible = FALSE
        )
        attr_text <<- gtext("", container = w_attributes)
      }

      # Get list of attributes.
      attributeList <- attributes(.gData)

      # Remove common non-strvalidator attributes (too much to show).
      attributeList$names <- NULL
      attributeList$row.names <- NULL
      attributeList$class <- NULL

      # Empty text fiels.
      svalue(attr_text) <- ""

      # Get names of attributes.
      attrNames <- names(attributeList)

      # Loop over list of attributes and att to text object.
      for (a in seq(along = attrNames)) {
        # Insert text for current attribute.
        insert(attr_text, paste(attrNames[a], attributeList[a], sep = ": "))
      }

      # Show window.
      visible(w_attributes) <- TRUE
    }
  }

  .refreshInfo <- function(clear = FALSE) {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    if (!clear && !is.null(.gData)) {
      # Update info.
      if ("Sample.Name" %in% names(.gData)) {
        samples <- length(unique(.gData$Sample.Name))
      } else if ("Sample.File.Name" %in% names(.gData)) {
        samples <- length(unique(.gData$Sample.File.Name))
      } else if (any(grepl("SAMPLE", names(.gData), ignore.case = TRUE))) {
        # Get (first) column name containing "Sample".
        sampleCol <- names(.gData)[grep("SAMPLE", names(.gData), ignore.case = TRUE)[1]]
        # Grab sample names.
        samples <- length(unique(.gData[, sampleCol]))
      } else {
        samples <- "<NA>"
      }
      svalue(g0_samples_lbl) <- paste(" ", samples, strings$STR_LBL_SAMPLES)
      svalue(g0_columns_lbl) <- paste(" ", ncol(.gData), strings$STR_LBL_COLUMNS)
      svalue(g0_rows_lbl) <- paste(" ", nrow(.gData), strings$STR_LBL_ROWS)
    } else {
      svalue(g0_samples_lbl) <- paste(" <NA>", strings$STR_LBL_SAMPLES)
      svalue(g0_columns_lbl) <- paste(" <NA>", strings$STR_LBL_COLUMNS)
      svalue(g0_rows_lbl) <- paste(" <NA>", strings$STR_LBL_ROWS)
    }
  }

  .refreshTbl <- function() {
    if (debug) {
      print(paste("IN:", match.call()[[1]]))
    }

    # Get options.
    val_limit <- svalue(f1_limit_chk)
    val_max <- as.numeric(svalue(f1_max_edt))
    val_attr <- svalue(f1_show_attr_chk)

    if (!is.null(.gData)) {
      if (val_attr) {
        .showAttributes()
      }

      # Update "save as" with current dataset name.
      svalue(save_txt) <- paste(.gDataName, "_edit", sep = "")

      # Build what we actually show in the table
      data_to_show <- .gData
      .gDataDisplay <<- NULL

      if (gtoolkit() == "tcltk") {
        # tcltk struggles with NA in gtable/gdf; show a character view instead
        if (any(is.na(data_to_show))) {
          
          # Make a pure display copy (character) so "" assignment is always valid
          data_to_show <- as.data.frame(lapply(data_to_show, function(x) {
            # keep factors readable
            if (is.factor(x)) x <- as.character(x)
            # convert everything to character for safe NA -> ""
            as.character(x)
          }), stringsAsFactors = FALSE)
          
          data_to_show[is.na(data_to_show)] <- ""
          
          .gDataDisplay <<- data_to_show  # remember what was shown
          message("tcltk compatibility: showing NA as empty strings (display only).")
          
          # Keep your warning dialog as-is (optional)
          if (!.hideMsg) {
            d <- gbasicdialog(
              title = strings$STR_MSG_TITLE_WARNING, parent = w,
              handler = function(h, ...) {
                .hideMsg <<- svalue(show_msg_chk)
              }
            )
            g <- ggroup(container = d, horizontal = FALSE)
            glabel(text = strings$STR_LBL_TCLTK, container = g)
            show_msg_chk <- gcheckbox(text = strings$STR_CHK_SHOW, container = g)
            visible(w) <- TRUE # Main window must be visible to show message.
            visible(d) <- TRUE # Show message window.
          }
        }
      }
      

      # Replace data with limited or full dataset.
      if (val_limit) {
        data_tbl[] <<- head(data_to_show, val_max)
        message("Showing ", val_max, " rows.")
      } else {
        data_tbl[] <<- data_to_show
        message("Showing all data.")
      }
      
    } else {
      # Update with place holder.
      data_tbl[] <<- data.frame(Data = strings$STR_LBL_NO_DATA)
    }
  }

  settings_prefix <- ".strvalidator_editData_gui_"
  settings_widgets <- list(
    attr = f1_show_attr_chk,
    limit = f1_limit_chk,
    maxrow = f1_max_edt
  )

  settings_key <- function(name) {
    paste0(settings_prefix, name)
  }

  get_saved_setting <- function(name) {
    key <- settings_key(name)
    if (exists(key, envir = env, inherits = FALSE)) {
      return(get(key, envir = env))
    }
    NULL
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
      saved_savegui <- get_saved_setting("savegui")
      if (!is.null(saved_savegui)) {
        svalue(savegui_chk) <- saved_savegui
      }
      if (debug) {
        print("Save GUI status loaded!")
      }
    }
    if (debug) {
      print(svalue(savegui_chk))
    }

    # Then load settings if true.
    if (isTRUE(svalue(savegui_chk))) {
      for (name in names(settings_widgets)) {
        value <- get_saved_setting(name)
        if (!is.null(value)) {
          svalue(settings_widgets[[name]]) <- value
        }
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (isTRUE(svalue(savegui_chk))) {
      assign(x = settings_key("savegui"), value = svalue(savegui_chk), envir = env)
      for (name in names(settings_widgets)) {
        assign(x = settings_key(name), value = svalue(settings_widgets[[name]]), envir = env)
      }
    } else { # or remove all saved values if false.
      for (name in c("savegui", names(settings_widgets))) {
        key <- settings_key(name)
        if (exists(key, envir = env, inherits = FALSE)) {
          remove(key, envir = env)
        }
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

  # Populate table.
  .refreshInfo()
  .refreshTbl()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
