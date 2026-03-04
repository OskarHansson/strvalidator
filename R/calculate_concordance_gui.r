#' @title Calculate Concordance
#'
#' @description
#' GUI wrapper for the \code{\link{calculate_concordance}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculate_concordance}} function by
#' providing a graphical user interface.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @seealso \code{\link{calculate_concordance}}
#' @aliases calculateConcordance_gui
#' @importFrom utils help head str
#' @export

calculate_concordance_gui <- function(env = parent.frame(), savegui = NULL,
                                     debug = FALSE, parent = NULL) {
  # Global variables.
  .g_data <- NULL

  # Get this functions name from call.
  fnc <- get_gui_scope()
  
  if (debug) {
    print(paste("IN:", fnc))
  }

  # Language ------------------------------------------------------------------

  # Load the language file for this specific GUI scope
  lng_strings <- get_strings(gui = fnc)
  
  # Define default strings
  default_strings <- list(
    STR_WIN_TITLE = "Calculate concordance",
    STR_CHK_GUI = "Save GUI settings",
    STR_BTN_HELP = "Help",
    STR_FRM_DATASET = "Dataset and kit",
    STR_LBL_DATASET = "Sample dataset:",
    STR_DRP_DEFAULT = "<Select dataset>",
    STR_LBL_SAMPLES = "samples",
    STR_LBL_KIT = "Kit:",
    STR_BTN_ADD = "Add",
    STR_FRM_OPTIONS = "Options",
    STR_LBL_DELIMITER = "Delimiter for alleles in genotype:",
    STR_LBL_NO_SAMPLE = "String for missing samples:",
    STR_LBL_NO_MARKER = "String for missing markers:",
    STR_CHK_INCLUDE = "Include missing samples in result.",
    STR_TIP_INCLUDE = "Samples not in all datasets will always be included in the result.",
    STR_FRM_SELECTED = "Selected datasets",
    STR_LBL_SELECTED = "Name for datasets to analyse (separated by comma):",
    STR_LBL_USED_KITS = "Name for analysis kit (separated by comma):",
    STR_FRM_SAVE = "Save as",
    STR_LBL_SAVE = "Name for discordance table:",
    STR_LBL_SAVE2 = "Name for concordance table:",
    STR_BTN_CALCULATE = "Calculate",
    STR_BTN_PROCESSING = "Processing...",
    STR_MSG_ADD_MESSAGE = "Data frame is NULL!\n\nMake sure to select a dataset",
    STR_MSG_TITLE_ERROR = "Error",
    STR_MSG_DATASET = "A dataset must be selected.",
    STR_MSG_TITLE_DATASET = "Datasets not selected"
  )
  
  # Update default strings with language-specific values
  strings <- update_strings_with_language_file(default_strings, lng_strings$value)
  
  # WINDOW ####################################################################
  
  # Main window.
  w <- gwindow(title = strings$STR_WIN_TITLE, visible = FALSE)
  
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
    text = strings$STR_FRM_DATASET, horizontal = FALSE,
    spacing = 1, container = gv
  )
  
  # Samples -------------------------------------------------------------------
  
  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  
  glabel(text = strings$STR_LBL_DATASET, container = f0g0)
  
  f0_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0g0
  )
  
  f0_list <- c(strings$STR_DRP_DEFAULT, list_objects(env = env, obj_class = "data.frame"))
  
  dataset_drp <- gcombobox(
    items = f0_list, selected = 1,
    editable = FALSE, container = f0g0,
    ellipsize = "none", expand = TRUE, fill = "x"
  )
  
  # Sizing --------------------------------------------------------------------
  
  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  
  glabel(text = strings$STR_LBL_KIT, container = f0g1)
  
  kit_drp <- gcombobox(
    items = get_kit(), selected = 1,
    editable = FALSE, container = f0g1,
    ellipsize = "none", expand = TRUE,
    fill = "x"
  )
  
  f0_add_btn <- gbutton(text = strings$STR_BTN_ADD, container = f0)
  
  # HANDLERS ------------------------------------------------------------------
  
  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)
    
    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )
    
    if (ok) {
      # Load or change components.
      
      # Get data.
      .g_data <<- get(val_obj, envir = env)
      
      svalue(f0_samples_lbl) <- paste(
        length(unique(.g_data$Sample.Name)),
        strings$STR_LBL_SAMPLES
      )
      
      # Detect kit.
      kitIndex <- detect_kit(.g_data, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .g_data <<- NULL
      svalue(f4_save1_edt) <- ""
      svalue(f4_save2_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste("0", strings$STR_LBL_SAMPLES)
    }
  })
  
  addHandlerChanged(f0_add_btn, handler = function(h, ...) {
    # Get values.
    val_obj <- svalue(dataset_drp)
    val_dataset <- svalue(f3_dataset_edt)
    val_kit <- svalue(f3_kit_edt)
    val_new_kit <- svalue(kit_drp)
    
    if (!is.null(.g_data)) {
      # Add new value to selected.
      new <- ifelse(nchar(val_dataset) > 0,
                    paste(val_dataset, val_obj, sep = ","),
                    val_obj
      )
      
      # Update text box.
      svalue(f3_dataset_edt) <- new
      
      # Add new value to selected.
      new <- ifelse(nchar(val_kit) > 0,
                    paste(val_kit, val_new_kit, sep = ","),
                    val_new_kit
      )
      
      # Update text box.
      svalue(f3_kit_edt) <- new
    } else {
      gmessage(
        msg = strings$STR_MSG_ADD_MESSAGE,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    }
  })
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(text = strings$STR_FRM_OPTIONS, horizontal = FALSE, spacing = 1, container = gv)
  
  f1g0 <- glayout(container = f1, expand = TRUE, fill = "both", spacing = 1)
  
  f1g0[1, 1] <- glabel(
    text = strings$STR_LBL_DELIMITER,
    anchor = c(-1, 0), container = f1g0
  )
  f1g0[1, 2] <- f1_delimeter_edt <- gedit(
    text = ",",
    width = 15, container = f1g0
  )
  
  f1g0[2, 1] <- glabel(
    text = strings$STR_LBL_NO_SAMPLE,
    anchor = c(-1, 0), container = f1g0
  )
  f1g0[2, 2] <- f1_no_sample_edt <- gedit(
    text = "NO SAMPLE",
    width = 15, container = f1g0
  )
  
  f1g0[3, 1] <- glabel(
    text = strings$STR_LBL_NO_MARKER,
    anchor = c(-1, 0), container = f1g0
  )
  f1g0[3, 2] <- f1_no_marker_edt <- gedit(
    text = "NO MARKER",
    width = 15, container = f1g0
  )
  
  f1_all_chk <- gcheckbox(
    text = strings$STR_CHK_INCLUDE,
    checked = FALSE, container = f1
  )
  tooltip(f1_all_chk) <- strings$STR_TIP_INCLUDE
  
  # FRAME 3 ###################################################################
  
  f3 <- gframe(
    text = strings$STR_FRM_SELECTED,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )
  
  glabel(
    text = strings$STR_LBL_SELECTED,
    anchor = c(-1, 0), container = f3
  )
  
  f3_dataset_edt <- gedit(container = f3)
  
  glabel(
    text = strings$STR_LBL_USED_KITS,
    anchor = c(-1, 0), container = f3
  )
  
  f3_kit_edt <- gedit(container = f3)
  
  # FRAME 4 ###################################################################
  
  f4 <- gframe(
    text = strings$STR_FRM_SAVE,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )
  
  glabel(text = strings$STR_LBL_SAVE, anchor = c(-1, 0), container = f4)
  
  f4_save1_edt <- gedit(text = "table_discordance", container = f4)
  
  glabel(text = strings$STR_LBL_SAVE2, anchor = c(-1, 0), container = f4)
  
  f4_save2_edt <- gedit(text = "table_concordance", container = f4)
  
  # BUTTON ####################################################################
  
  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)
  
  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    val_datasets <- svalue(f3_dataset_edt)
    val_kits <- svalue(f3_kit_edt)
    val_name1 <- svalue(f4_save1_edt)
    val_name2 <- svalue(f4_save2_edt)
    val_del <- svalue(f1_delimeter_edt)
    val_nosample <- svalue(f1_no_sample_edt)
    val_nomarker <- svalue(f1_no_marker_edt)
    val_all <- svalue(f1_all_chk)
    val_list <- list()

    if (debug) {
      print("Read Values:")
      print("val_datasets")
      print(val_datasets)
      print("val_kits")
      print(val_kits)
      print("val_name1")
      print(val_name1)
      print("val_name2")
      print(val_name2)
      print("val_delimeter")
      print(val_del)
      print("val_nosample")
      print(val_nosample)
      print("val_nomarker")
      print(val_nomarker)
      print("val_all")
      print(val_all)
    }

    # Check if data.
    if (!val_datasets == "") {
      # Create list of datasets.
      val_datasets <- unlist(strsplit(val_datasets, ","))
      for (d in seq(along = val_datasets)) {
        # Get data and store in list.
        val_list[[d]] <- get(val_datasets[d], envir = env)
      }

      # Create vector of kit names.
      val_kits <- unlist(strsplit(val_kits, ","))

      if (debug) {
        print("Sent Values:")
        print("val_list")
        print(str(val_list))
        print("val_kits")
        print(val_kits)
        print("val_del")
        print(val_del)
        print("val_nosample")
        print(val_nosample)
        print("val_nomarker")
        print(val_nomarker)
        print("val_all")
        print(val_all)
      }

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculate_concordance(
        data = val_list,
        kit_name = val_kits,
        no_sample = val_nosample,
        no_marker = val_nomarker,
        delimiter = val_del,
        list_all = val_all,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "kit.name", "no.sample", "no.marker",
        "delimeter", "list.all"
      )

      values <- list(
        val_datasets, val_kits, val_nosample, val_nomarker,
        val_del, val_all
      )

      # Update audit trail.
      datanew[[1]] <- audit_trail(
        obj = datanew[[1]], key = keys, value = values,
        label = fnc,
        arguments = FALSE, package = "strvalidator"
      )

      datanew[[2]] <- audit_trail(
        obj = datanew[[2]], key = keys, value = values,
        label = fnc,
        arguments = FALSE, package = "strvalidator"
      )

      # Save data.
      save_object(name = val_name1, object = datanew[[1]], parent = w, env = env)
      save_object(name = val_name2, object = datanew[[2]], parent = w, env = env)

      if (debug) {
        print(str(datanew))
        print(head(datanew))
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strings$STR_MSG_DATASET,
        title = strings$STR_MSG_TITLE_DATASET,
        icon = "error",
        parent = w
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  settings_prefix <- ".strvalidator_calculate_concordance_gui_"
  settings_widgets <- list(
    delimeter = f1_delimeter_edt,
    sample = f1_no_sample_edt,
    marker = f1_no_marker_edt,
    all = f1_all_chk
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

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}

