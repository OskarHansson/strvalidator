################################################################################
# CHANGE LOG (last 20 changes)
# 16.11.2025: Changed 'checkSubset' to 'check_subset' and parameter 'ignore.case' to 'ignore_case'.
# 02.09.2022: Compacted the gui.
# 03.03.2020: Fixed reference to function name.
# 01.03.2020: Added language support.
# 10.01.2020: Changed "LDT" to "AT" following the book.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 17.07.2018: 'Save as' textbox expandable.
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.08.2016: Implemented new calculateHeight, removed calculateHeterozygous.
# 29.06.2016: Added option to remove sex markers and quality sensor.
# 16.06.2016: 'Save as' textbox expandable.
# 05.10.2015: Added more attributes to result.
# 04.10.2015: Added automatic calculation of average peak height 'H'.
# 28.08.2015: Added importFrom
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 13.12.2014: Added kit dropdown and kit attribute to result.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.

#' @title Calculate Dropout Events
#'
#' @description
#' GUI wrapper for the \code{\link{calculateDropout}} function.
#'
#' @details Scores dropouts for a dataset.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help head
#' @importFrom graphics title
#'
#' @seealso \code{\link{calculateDropout}}, \code{\link{check_subset}}

calculateDropout_gui <- function(env = parent.frame(), savegui = NULL,
                                 debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gRef <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Calculate dropout",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset and kit",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_REF_DATASET     = "Reference dataset:",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_IGNORE          = "Ignore case",
    STR_CHK_SEX             = "Remove sex markers",
    STR_CHK_SENSORS         = "Remove quality sensors",
    STR_CHK_AVERAGE         = "Calculate average peak height",
    STR_LBL_AT              = "Analytical threshold (AT):",
    STR_LBL_METHODS         = "Select one or more dropout scoring methods:",
    STR_CHK_METHOD1         = "Score dropout relative to the low molecular weight allele",
    STR_CHK_METHOD2         = "Score dropout relative to the high molecular weight allele",
    STR_CHK_METHOD_X        = "Score dropout relative to a random allele",
    STR_CHK_METHOD_L        = "Score dropout per locus",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A sample dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected",
    STR_WIN_TITLE_CHECK     = "Check subsetting",
    STR_MSG_CHECK           = "Data frame is NULL!\n\nMake sure to select a sample dataset and a reference dataset.",
    STR_MSG_TITLE_ERROR     = "Error"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # ---------------------------------------------------------------------------

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
    # expand = TRUE,
    # fill = "x"
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "x")

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

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  g0_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = g0
  )
  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )


  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gData <<- get(val_obj, envir = env)
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, strings$STR_LBL_SAMPLES)
      svalue(f1g1_at_edt) <- min(as.numeric(.gData$Height), na.rm = TRUE)

      # Suggest a name for result.
      svalue(f2_save_edt) <- paste(val_obj, "_dropout", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(f1g1_at_edt) <- ""
      svalue(f2_save_edt) <- ""
    }
  })

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_REF_DATASET, container = g1)

  ref_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = g1
  )

  refset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(refset_drp, handler = function(h, ...) {
    val_obj <- svalue(refset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gRef <<- get(val_obj, envir = env)
      ref <- length(unique(.gRef$Sample.Name))
      svalue(ref_lbl) <- paste("", ref, strings$STR_LBL_REF)
    } else {
      # Reset components.
      .gRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(ref_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # Kit -----------------------------------------------------------------------

  g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_KIT, container = g2)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = g2,
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
    val_ignore <- svalue(f1_ignore_case_chk)

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
        ignore_case = val_ignore,
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

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1_ignore_case_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE, checked = TRUE,
    container = f1
  )

  f1_sex_chk <- gcheckbox(
    text = strings$STR_CHK_SEX, checked = FALSE,
    container = f1
  )

  f1_qs_chk <- gcheckbox(
    text = strings$STR_CHK_SENSORS, checked = TRUE,
    container = f1
  )

  f1_h_chk <- gcheckbox(
    text = strings$STR_CHK_AVERAGE, checked = TRUE,
    container = f1
  )

  f1g1 <- glayout(container = f1)

  f1g1[1, 1] <- glabel(
    text = strings$STR_LBL_AT,
    container = f1g1, anchor = c(-1, 0)
  )

  f1g1[1, 2] <- f1g1_at_edt <- gedit(text = "", width = 6, container = f1g1)

  glabel(
    text = strings$STR_LBL_METHODS,
    container = f1, anchor = c(-1, 0)
  )

  f1_score1_chk <- gcheckbox(
    text = strings$STR_CHK_METHOD1,
    checked = TRUE, container = f1
  )

  f1_score2_chk <- gcheckbox(
    text = strings$STR_CHK_METHOD2,
    checked = TRUE, container = f1
  )

  f1_scorex_chk <- gcheckbox(
    text = strings$STR_CHK_METHOD_X,
    checked = TRUE, container = f1
  )

  f1_scorel_chk <- gcheckbox(
    text = strings$STR_CHK_METHOD_L,
    checked = TRUE, container = f1
  )

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strings$STR_FRM_SAVE,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strings$STR_LBL_SAVE, container = f2)

  f2_save_edt <- gedit(expand = TRUE, fill = "x", container = f2)

  # BUTTON ####################################################################


  dropout_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(dropout_btn, handler = function(h, ...) {
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- svalue(dataset_drp)
    val_name_ref <- svalue(refset_drp)
    val_ignore_case <- svalue(f1_ignore_case_chk)
    val_h <- svalue(f1_h_chk)
    val_threshold <- as.numeric(svalue(f1g1_at_edt))
    val_name <- svalue(f2_save_edt)
    val_kit <- svalue(kit_drp)
    val_method <- vector()
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)

    # Get methods:
    if (svalue(f1_score1_chk)) {
      val_method <- c(val_method, "1")
    }
    if (svalue(f1_score2_chk)) {
      val_method <- c(val_method, "2")
    }
    if (svalue(f1_scorex_chk)) {
      val_method <- c(val_method, "X")
    }
    if (svalue(f1_scorel_chk)) {
      val_method <- c(val_method, "L")
    }

    if (debug) {
      print("GUI options:")
      print("val_ignore_case")
      print(val_ignore_case)
      print("val_threshold")
      print(val_threshold)
      print("val_name")
      print(val_name)
      print("val_method")
      print(val_method)
    }

    # No threshold is represented by NULL (not needed).
    if (length(val_threshold) == 0) {
      val_threshold <- NULL
    }

    if (debug) {
      print("Function arguments:")
      print("val_ignore_case")
      print(val_ignore_case)
      print("val_threshold")
      print(val_threshold)
      print("val_name")
      print(val_name)
      print("val_sex")
      print(val_sex)
      print("val_qs")
      print(val_qs)
    }

    if (!is.null(val_data) & !is.null(val_ref)) {
      # Change button.
      blockHandlers(dropout_btn)
      svalue(dropout_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(dropout_btn)
      enabled(dropout_btn) <- FALSE

      datanew <- calculateDropout(
        data = val_data,
        ref = val_ref,
        threshold = val_threshold,
        method = val_method,
        ignore.case = val_ignore_case,
        sex.rm = val_sex,
        qs.rm = val_qs,
        kit = val_kit,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "threshold", "method",
        "ignore.case", "sex.rm", "qs.rm", "kit", "calculate.h"
      )

      values <- list(
        val_name_data, val_name_ref, val_threshold, val_method,
        val_ignore_case, val_sex, val_qs, val_kit, val_h
      )

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Calculate and add average peak height.
      if (val_h) {
        message("User selected to add average peak height...")

        # Calculate average peak height.
        dfH <- calculateHeight(
          data = val_data, ref = val_ref, na.replace = 0,
          add = FALSE, exclude = "OL", sex.rm = val_sex,
          qs.rm = val_qs, kit = val_kit,
          ignore.case = val_ignore_case, exact = FALSE,
          debug = debug
        )

        message("Average peak height calculated.")

        # Add average peak height to dataset.
        datanew <- addData(
          data = datanew, new.data = dfH,
          by.col = "Sample.Name", then.by.col = NULL,
          exact = TRUE, ignore.case = val_ignore_case,
          debug = debug
        )

        message("Average peak height added to result.")
      }

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
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
      if (exists(".strvalidator_calculateDropout_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateDropout_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateDropout_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_case_chk) <- get(".strvalidator_calculateDropout_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateDropout_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_calculateDropout_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_h", envir = env, inherits = FALSE)) {
        svalue(f1_h_chk) <- get(".strvalidator_calculateDropout_gui_h", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score1", envir = env, inherits = FALSE)) {
        svalue(f1_score1_chk) <- get(".strvalidator_calculateDropout_gui_score1", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score2", envir = env, inherits = FALSE)) {
        svalue(f1_score2_chk) <- get(".strvalidator_calculateDropout_gui_score2", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorex", envir = env, inherits = FALSE)) {
        svalue(f1_scorex_chk) <- get(".strvalidator_calculateDropout_gui_scorex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorel", envir = env, inherits = FALSE)) {
        svalue(f1_scorel_chk) <- get(".strvalidator_calculateDropout_gui_scorel", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateDropout_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_ignore", value = svalue(f1_ignore_case_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_qs", value = svalue(f1_qs_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_h", value = svalue(f1_h_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_score1", value = svalue(f1_score1_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_score2", value = svalue(f1_score2_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_scorex", value = svalue(f1_scorex_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_scorel", value = svalue(f1_scorel_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateDropout_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_h", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_h", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score1", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_score1", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score2", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_score2", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_scorex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorel", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_scorel", envir = env)
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
