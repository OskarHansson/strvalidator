################################################################################
# CHANGE LOG (last 20 changes)
# 16.11.2025: Changed 'checkSubset' to 'check_subset' and parameter 'ignore.case' to 'ignore_case'.
# 03.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 31.08.2022: Compacted the gui. Fixed Replacement table not expanding.
# 12.08.2022: Changed logical (-> NA in tcltk) to strings in replace-false-stutter df + check for NAs.
# 07.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 28.04.2016: 'Save as' textbox expandable.
# 26.10.2015: Added attributes.
# 28.08.2015: Added importFrom.
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 05.01.2015: Added kit dropdown and kit attribute to result.
# 07.10.2014: Added 'focus', added 'parent' parameter.
# 03.08.2014: Added detection of kit and add attribute to result.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 26.07.2013: Changed parameter 'fixed' to 'word' for 'checkSubset' function.

#' @title Calculate Stutter
#'
#' @description
#' GUI wrapper for the \code{\link{calculateStutter}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateStutter}} function by providing
#' a graphical user interface to it.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help head
#' @importFrom graphics title
#'
#' @return TRUE
#'
#' @seealso \code{\link{calculateStutter}}, \code{\link{check_subset}}

calculateStutter_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gRef <- NULL
  .gDataName <- NULL
  .gRefName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Calculate stutter ratio",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Datasets",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_REF_DATASET     = "Reference dataset:",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_RANGE           = "Calculate stutter ratio within the following analysis range:",
    STR_LBL_BACK            = "back stutters to",
    STR_LBL_FORWARD         = "forward stutters.",
    STR_LBL_NB              = "NB! Additive effects outside the analysis range cannot be controlled.\nA narrow range like 0 to 1 can be greately affected by neighbouring -1 stutters.",
    STR_LBL_INTERFERENCE    = "Level of interference within the given range:",
    STR_RAD_NONE            = "no overlap between stutters and alleles",
    STR_RAD_STUTTER         = "stutter-stutter overlap allowed",
    STR_RAD_ALLELE          = "stutter-allele overlap allowed",
    STR_LBL_REPLACE         = "Replace 'false' stutters:",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A sample dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected",
    STR_MSG_CHECK           = "Data frame is NULL!\n\nMake sure to select a sample dataset.",
    STR_WIN_TITLE_CHECK     = "Check subsetting",
    STR_MSG_TITLE_ERROR     = "Error"
  )

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

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = w,
    expand = FALSE
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
    container = gv,
    expand = FALSE,
    fill = "x"
  )

  # Datasets ------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g0)

  f0_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0g0
  )

  f0_dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(f0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(f0_dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(f0_samples_lbl) <- paste("", samples, strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- paste(val_obj, "_stutter", sep = "")
      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0_dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_REF_DATASET, container = f0g1)

  f0_ref_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = f0g1
  )

  f0_refset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(f0_refset_drp, handler = function(h, ...) {
    val_obj <- svalue(f0_refset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj
      refs <- length(unique(.gRef$Sample.Name))
      svalue(f0_ref_lbl) <- paste("", refs, strings$STR_LBL_REF)
    } else {
      # Reset components.
      .gRef <<- NULL
      .gRefName <<- NULL
      svalue(f0_refset_drp, index = TRUE) <- 1
      svalue(f0_ref_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # Kit -----------------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_KIT, container = f0g2)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f0g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # CHECK #####################################################################

  check_btn <- gbutton(text = strings$STR_BTN_CHECK, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = strings$STR_WIN_TITLE_CHECK,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- check_subset(
        data = val_data,
        ref = val_ref,
        console = FALSE,
        ignore_case = TRUE,
        word = FALSE
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
        wrap = FALSE, container = chksubset_w
      )

      visible(chksubset_w) <- TRUE
    } else {
      gmessage(
        msg = strings$STR_MSG_CHECK,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    }
  })

  # OPTIONS ###################################################################

  f1 <- gframe(strings$STR_FRM_OPTIONS,
    spacing = 1,
    horizontal = FALSE, container = gv,
    expand = TRUE, fill = TRUE
  )

  glabel(text = strings$STR_LBL_RANGE, container = f1, anchor = c(-1, 0))

  f1g1 <- ggroup(
    horizontal = TRUE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = f1
  )

  f1g1_range_b_spb <- gspinbutton(
    from = 0, to = 3, by = 1,
    value = 1, digits = 0,
    container = f1g1
  )

  glabel(text = strings$STR_LBL_BACK, container = f1g1, anchor = c(-1, 0))

  f1g1_range_f_spb <- gspinbutton(
    from = 0, to = 3, by = 1,
    value = 1, digits = 0,
    container = f1g1
  )

  glabel(text = strings$STR_LBL_FORWARD, container = f1g1, anchor = c(-1, 0))

  glabel(text = strings$STR_LBL_NB, container = f1, anchor = c(-1, 0))

  # INTERFERENCE --------------------------------------------------------------

  glabel(text = strings$STR_LBL_INTERFERENCE, container = f1, anchor = c(-1, 0))

  options <- c(strings$STR_RAD_NONE, strings$STR_RAD_STUTTER, strings$STR_RAD_ALLELE)

  interference_opt <- gradio(
    items = options, selected = 1,
    horizontal = FALSE, container = f1
  )

  # FALSE STUTTERS ------------------------------------------------------------

  glabel(text = strings$STR_LBL_REPLACE, container = f1, anchor = c(-1, 0))

  # Create default data frame.
  replace_val <- c(-1.9, -1.8, -1.7, -0.9, -0.8, -0.7, 0.9, 0.8, 0.7)
  by_val <- c(-1.3, -1.2, -1.1, -0.3, -0.2, -0.1, 0.3, 0.2, 0.1)
  default <- data.frame(
    False.Stutter = replace_val,
    True.Stutter = by_val,
    Replace = rep("TRUE", length(replace_val)),
    stringsAsFactors = FALSE
  )

  # gdf generates an error when gui is closed.
  # (rsession.exe:20768): Gtk-CRITICAL **: gtk_tree_view_unref_tree_helper: assertion `node != NULL' failed
  # Issue reported here: https://github.com/jverzani/gWidgets2/issues/11
  default_gdf <- gdf(items = default, container = f1, expand = TRUE, fill = TRUE)

  # Set initial minimum size.
  size(default_gdf) <- c(100, 100)

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    val_back <- svalue(f1g1_range_b_spb)
    val_forward <- svalue(f1g1_range_f_spb)
    val_interference <- svalue(interference_opt, index = TRUE) - 1 # NB! range [0-2]
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- .gDataName
    val_name_ref <- .gRefName
    val_name <- svalue(save_edt)
    val_replace_df <- default_gdf[]
    val_chk <- as.logical(val_replace_df$Replace) # Convert strings to logicals.
    val_replace <- val_replace_df$False.Stutter
    val_by <- val_replace_df$True.Stutter
    val_kit <- svalue(kit_drp)

    # Check for NAs.
    num_na <- sum(is.na(val_chk))
    if (num_na > 0) {
      val_chk[is.na(val_chk)] <- FALSE
      message("Replaced ", num_na, " non TRUE/FALSE strings in column Replace by FALSE")
    }

    # Get selected values.
    val_replace <- val_replace[val_chk]
    val_by <- val_by[val_chk]

    if (length(val_replace) == 0) {
      val_replace <- NULL
    }
    if (length(val_by) == 0) {
      val_by <- NULL
    }

    if (!is.null(val_data) & !is.null(val_ref)) {
      if (debug) {
        print("val_data")
        print(head(val_data))
        print("val_ref")
        print(head(val_ref))
        print("val_back")
        print(val_back)
        print("val_forward")
        print(val_forward)
        print("val_interference")
        print(val_interference)
        print("val_replace")
        print(val_replace)
        print("val_by")
        print(val_by)
      }

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      # Calculate stutter.
      datanew <- calculateStutter(
        data = val_data, ref = val_ref,
        back = val_back, forward = val_forward,
        interference = val_interference,
        replace.val = val_replace,
        by.val = val_by,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "back", "forward",
        "interference", "replace.val", "by.val"
      )

      values <- list(
        val_name_data, val_name_ref, val_back, val_forward,
        val_interference, val_replace, val_by
      )

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print("datanew")
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
      if (exists(".strvalidator_calculateStutter_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateStutter_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateStutter_gui_back", envir = env, inherits = FALSE)) {
        svalue(f1g1_range_b_spb) <- get(".strvalidator_calculateStutter_gui_back", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_forward", envir = env, inherits = FALSE)) {
        svalue(f1g1_range_f_spb) <- get(".strvalidator_calculateStutter_gui_forward", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_interference", envir = env, inherits = FALSE)) {
        svalue(interference_opt) <- get(".strvalidator_calculateStutter_gui_interference", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_replace", envir = env, inherits = FALSE)) {
        default_gdf[, ] <- get(".strvalidator_calculateStutter_gui_replace", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateStutter_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateStutter_gui_back", value = svalue(f1g1_range_b_spb), envir = env)
      assign(x = ".strvalidator_calculateStutter_gui_forward", value = svalue(f1g1_range_f_spb), envir = env)
      assign(x = ".strvalidator_calculateStutter_gui_interference", value = svalue(interference_opt), envir = env)
      assign(x = ".strvalidator_calculateStutter_gui_replace", value = default_gdf[], envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateStutter_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_back", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_back", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_forward", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_forward", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_interference", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_interference", envir = env)
      }
      if (exists(".strvalidator_calculateStutter_gui_replace", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateStutter_gui_replace", envir = env)
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
