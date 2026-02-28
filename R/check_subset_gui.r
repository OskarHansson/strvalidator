#' @title Check Subset
#'
#' @description
#' GUI wrapper for the \code{\link{check_subset}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{check_subset}} function by providing
#' a graphical user interface to it.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help
#' @importFrom graphics title
#'
#' @return TRUE
#'
#' @seealso \code{\link{check_subset}}
check_subset_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gRef <- NULL
  
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
    STR_WIN_TITLE = "Check subsetting",
    STR_CHK_GUI = "Save GUI settings",
    STR_BTN_HELP = "Help",
    STR_FRM_DATASET = "Datasets",
    STR_LBL_DATASET = "Sample dataset:",
    STR_DRP_DATASET = "<Select dataset>",
    STR_LBL_SAMPLES = "samples",
    STR_LBL_REF_DATASET = "Reference dataset:",
    STR_LBL_REF = "references",
    STR_LBL_MANUAL = "Or type a reference name:",
    STR_FRM_OPTIONS = "Options",
    STR_LBL_MATCHING = "Reference sample name matching:",
    STR_CHK_IGNORE = "Ignore case ('A' will match 'A', 'B-a.2', and 'A2')",
    STR_CHK_WORD = "Add word boundaries ('A' will match 'A', 'B-A.2', and 'A 2' but not 'A2')",
    STR_CHK_EXACT = "Exact matching ('A' will match 'A' but not 'B-A.2', 'A 2', or 'A2')",
    STR_CHK_REVERSE = "Reversed matching (data -> reference)",
    STR_FRM_SAVE = "Save as",
    STR_LBL_SAVE = "Name for result:",
    STR_BTN_CALCULATE = "Subset",
    STR_MSG_CHECK = "Data frame is NULL!\n\nMake sure to select a sample dataset and a reference dataset, or type a reference name",
    STR_WIN_TITLE_CHECK = "Check subsetting",
    STR_MSG_TITLE_ERROR = "Error"
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
  
  # Vertical main group.
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
  
  # DATASET ###################################################################
  
  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )
  
  # SAMPLE --------------------------------------------------------------------
  
  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  
  glabel(text = strings$STR_LBL_DATASET, container = f0g1)
  
  dataset_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0g1
  )
  
  dataset_drp <- gcombobox(
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
  
  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)
    
    # Check if suitable.
    requiredCol <- c("Sample.Name")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )
    
    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      samples <- length(unique(.gData$Sample.Name))
      svalue(dataset_samples_lbl) <- paste("", samples, strings$STR_LBL_SAMPLES)
    } else {
      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      svalue(dataset_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
    }
  })
  
  # REFERENCE -----------------------------------------------------------------
  
  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  
  glabel(text = strings$STR_LBL_REF_DATASET, container = f0g2)
  
  dataset_ref_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = f0g2
  )
  
  dataset_ref_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )
  
  # MANUAL --------------------------------------------------------------------
  
  f0g3 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  
  glabel(text = strings$STR_LBL_MANUAL, container = f0g3)
  
  dataset_ref_edt <- gedit(container = f0g3, expand = TRUE, fill = "x")
  
  addHandlerChanged(dataset_ref_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_ref_drp)
    
    # Check if suitable.
    requiredCol <- c("Sample.Name")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )
    
    if (ok) {
      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      refs <- length(unique(.gRef$Sample.Name))
      svalue(dataset_ref_lbl) <- paste("", refs, strings$STR_LBL_REF)
    } else {
      # Reset components.
      .gRef <<- data.frame(No.Data = NA)
      svalue(dataset_ref_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })
  
  # FRAME 1 ###################################################################
  
  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )
  
  glabel(text = strings$STR_LBL_MATCHING, anchor = c(-1, 0), container = f1)
  
  f1_ignore_case_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE,
    checked = TRUE,
    container = f1
  )
  
  f1_word_chk <- gcheckbox(
    text = strings$STR_CHK_WORD,
    checked = FALSE,
    container = f1
  )
  
  f1_exact_chk <- gcheckbox(
    text = strings$STR_CHK_EXACT,
    checked = FALSE,
    container = f1
  )
  
  f1_reverse_chk <- gcheckbox(
    text = strings$STR_CHK_REVERSE,
    checked = FALSE,
    container = f1
  )
  
  # BUTTON ####################################################################
  
  check_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)
  
  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ref_name <- svalue(dataset_ref_edt)
    val_ignore <- svalue(f1_ignore_case_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    val_reverse <- svalue(f1_reverse_chk)
    
    if (is.null(.gRef)) {
      # If no reference dataset use given name.
      val_ref <- val_ref_name
    }
    
    # Check that data is available.
    if (!is.null(val_data) && !is.null(val_ref)) {
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
        ignore_case = val_ignore,
        word = val_word,
        exact = val_exact,
        reverse = val_reverse
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
      if (exists(".strvalidator_check_subset_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_check_subset_gui_savegui", envir = env)
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
      if (exists(".strvalidator_check_subset_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_case_chk) <- get(".strvalidator_check_subset_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_check_subset_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_check_subset_gui_word", envir = env)
      }
      if (exists(".strvalidator_check_subset_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_check_subset_gui_exact", envir = env)
      }
      if (exists(".strvalidator_check_subset_gui_reverse", envir = env, inherits = FALSE)) {
        svalue(f1_reverse_chk) <- get(".strvalidator_check_subset_gui_reverse", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }
  
  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_check_subset_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_check_subset_gui_ignore", value = svalue(f1_ignore_case_chk), envir = env)
      assign(x = ".strvalidator_check_subset_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_check_subset_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_check_subset_gui_reverse", value = svalue(f1_reverse_chk), envir = env)
    } else { # or remove all saved values if false.
      
      if (exists(".strvalidator_check_subset_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_check_subset_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_check_subset_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_check_subset_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_check_subset_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_check_subset_gui_word", envir = env)
      }
      if (exists(".strvalidator_check_subset_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_check_subset_gui_exact", envir = env)
      }
      if (exists(".strvalidator_check_subset_gui_reverse", envir = env, inherits = FALSE)) {
        remove(".strvalidator_check_subset_gui_reverse", envir = env)
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
} # End of GUI

################################################################################
#' @rdname check_subset_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [check_subset_gui()] instead.
################################################################################

checkSubset_gui <- function(env = parent.frame(),
                            savegui = NULL, 
                            debug = FALSE,
                            parent = NULL,
                            ...) {
  
  .Deprecated("check_subset_gui", package = "strvalidator")
  
  # Remap arguments
  check_subset_gui(
    env = env,
    savegui = savegui,
    debug = debug,
    parent = parent,
    ...
  )
}


