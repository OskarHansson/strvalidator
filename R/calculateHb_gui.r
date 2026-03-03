################################################################################
# CHANGE LOG (last 20 changes)
# 02.12.2025: Fixed option exact not propagated to check_subsetting.
# 03.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 03.03.2020: Fixed reference to function name.
# 01.03.2020: Added language support.
# 15.02.2019: Expand text fields in tcltk by setting fill = TRUE.
# 11.02.2019: Minor adjustments to tcltk gui.
# 11.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 29.08.2016: First version.

#' @title Calculate Heterozygote Balance
#'
#' @description
#' GUI wrapper for the \code{\link{calculateHb}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateHb}} function
#' by providing a graphical user interface.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help head str
#' @importFrom graphics title
#'
#' @seealso \code{link{calculateHb}}, \code{link{check_subset}}

calculateHb_gui <- function(env = parent.frame(), savegui = NULL,
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
    STR_WIN_TITLE           = "Calculate heterozygote balance",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Datasets",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_REF_DATASET     = "Reference dataset:",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_PRE             = "Pre-processing:",
    STR_CHK_SEX             = "Remove sex markers",
    STR_CHK_SENSORS         = "Remove quality sensors",
    STR_LBL_HB              = "Define Hb as:",
    STR_ITEM_HM_WO_LMW      = "High molecular weight / low molecular weight",
    STR_ITEM_LM_WO_HMW      = "Low molecular weight / high molecular weight",
    STR_ITEM_SMLO_LRG       = "Smaller peak / larger peak",
    STR_LBL_MATCHING        = "Sample name matching:",
    STR_CHK_IGNORE          = "Ignore case",
    STR_CHK_WORD            = "Add word boundaries",
    STR_CHK_EXACT           = "Exact matching",
    STR_LBL_POST            = "Post-processing:",
    STR_CHK_AVERAGE         = "Calculate average peak height",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_LBL_KIT             = "Kit attribute:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A sample dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected",
    STR_MSG_CHECK           = "Data frame is NULL!\n\nMake sure to select a sample dataset and a reference dataset.",
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

  gv <- ggroup(
    horizontal = FALSE, spacing = 1, use.scrollwindow = FALSE,
    container = w, expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(
    text = strings$STR_CHK_GUI,
    checked = FALSE, container = gh
  )

  addSpring(gh)

  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET, horizontal = FALSE,
    spacing = 1, container = gv, expand = FALSE, fill = "x"
  )

  # Dataset -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  g0_data_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = g0
  )

  dfs <- c(
    strings$STR_DRP_DEFAULT,
    listObjects(env = env, obj.class = "data.frame")
  )

  g0_data_drp <- gcombobox(
    items = dfs, selected = 1,
    editable = FALSE, container = g0,
    ellipsize = "none", expand = TRUE, fill = "x"
  )

  addHandlerChanged(g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Dye", "Height")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Height",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # get dataset.
      .gData <<- get(val_obj, envir = env)
      svalue(g0_data_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strings$STR_LBL_SAMPLES
      )

      # Suggest a name for the result.
      svalue(f4_save_edt) <- paste(val_obj, "_hb", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(f4_kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(g0_data_drp, index = TRUE) <- 1
      svalue(g0_data_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(f4_save_edt) <- ""
    }
  })

  # Reference -----------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_REF_DATASET, container = g1)

  g0_ref_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = g1
  )

  # NB! dfs defined in previous section.
  g0_ref_drp <- gcombobox(
    items = dfs, selected = 1,
    editable = FALSE, container = g1,
    ellipsize = "none", expand = TRUE, fill = "x"
  )

  addHandlerChanged(g0_ref_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_ref_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gRef <<- get(val_obj, envir = env)
      svalue(g0_ref_samples_lbl) <- paste(
        length(unique(.gRef$Sample.Name)),
        strings$STR_LBL_REF
      )
    } else {
      # Reset components.
      .gRef <<- NULL
      svalue(g0_ref_drp, index = TRUE) <- 1
      svalue(g0_ref_samples_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # CHECK #####################################################################

  g0_check_btn <- gbutton(text = strings$STR_BTN_CHECK, container = gv)

  addHandlerChanged(g0_check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    
    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = strings$STR_WIN_TITLE_CHECK,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- check_subset(
        data = val_data, ref = val_ref,
        console = FALSE, ignore_case = val_ignore,
        word = val_word, exact = val_exact
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300,
        font.attr = NULL, wrap = FALSE, container = chksubset_w
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
    text = strings$STR_FRM_OPTIONS, horizontal = FALSE,
    spacing = 1, container = gv
  )

  glabel(text = strings$STR_LBL_PRE, anchor = c(-1, 0), container = f1)

  f1_sex_chk <- gcheckbox(
    text = strings$STR_CHK_SEX, checked = TRUE,
    container = f1
  )

  f1_qs_chk <- gcheckbox(
    text = strings$STR_CHK_SENSORS, checked = TRUE,
    container = f1
  )

  glabel(text = strings$STR_LBL_HB, anchor = c(-1, 0), container = f1)

  f1_methods <- c(
    strings$STR_ITEM_HM_WO_LMW,
    strings$STR_ITEM_LM_WO_HMW,
    strings$STR_ITEM_SMLO_LRG
  )

  f1_method_drp <- gcombobox(
    items = f1_methods, selected = 1,
    expand = FALSE, container = f1, ellipsize = "none"
  )

  glabel(text = strings$STR_LBL_MATCHING, anchor = c(-1, 0), container = f1)

  f1_ignore_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE, checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = strings$STR_CHK_WORD, checked = FALSE,
    container = f1
  )

  f1_exact_chk <- gcheckbox(
    text = strings$STR_CHK_EXACT, checked = FALSE,
    container = f1
  )

  glabel(text = strings$STR_LBL_POST, anchor = c(-1, 0), container = f1)

  f1_h_chk <- gcheckbox(
    text = strings$STR_CHK_AVERAGE,
    checked = TRUE, container = f1
  )


  # FRAME 4 ###################################################################

  f4 <- gframe(
    text = strings$STR_FRM_SAVE, horizontal = TRUE,
    spacing = 1, container = gv
  )

  glabel(text = strings$STR_LBL_SAVE, container = f4)

  f4_save_edt <- gedit(text = "", container = f4, expand = TRUE, fill = TRUE)

  glabel(text = strings$STR_LBL_KIT, container = f4)

  f4_kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f4, ellipsize = "none"
  )


  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- svalue(g0_data_drp)
    val_name_ref <- svalue(g0_ref_drp)
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)
    val_hb <- svalue(f1_method_drp, index = TRUE)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    val_h <- svalue(f1_h_chk)
    val_name <- svalue(f4_save_edt)
    val_kit <- svalue(f4_kit_drp)

    # Check if data.
    if (!is.null(val_data) & !is.null(val_ref)) {
      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateHb(
        data = val_data, ref = val_ref, hb = val_hb,
        kit = val_kit, sex.rm = val_sex, qs.rm = val_qs,
        ignore.case = val_ignore, exact = val_exact,
        word = val_word, debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "sex.rm", "qs.rm", "hb",
        "kit", "ignore.case", "word", "exact", "calculate.h"
      )

      values <- list(
        val_name_data, val_name_ref, val_sex, val_qs, val_hb,
        val_kit, val_ignore, val_word, val_exact, val_h
      )

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Calculate and add average peak height.
      if (val_h) {
        message("Calculating average peak height...")

        # Calculate average peak height.
        dfH <- calculateHeight(
          data = val_data, ref = val_ref, na.replace = 0,
          add = FALSE, exclude = "OL", sex.rm = val_sex,
          qs.rm = val_qs, kit = val_kit,
          ignore.case = val_ignore, exact = val_exact,
          debug = debug
        )


        message("Adding average peak height to result...")

        # Add average peak height to dataset.
        datanew <- addData(
          data = datanew, new.data = dfH,
          by.col = "Sample.Name", then.by.col = NULL,
          exact = TRUE, ignore.case = val_ignore,
          debug = debug
        )
      }

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

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
      if (exists(".strvalidator_calculateHb_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateHb_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateHb_gui_method", envir = env, inherits = FALSE)) {
        svalue(f1_method_drp) <- get(".strvalidator_calculateHb_gui_method", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculateHb_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_calculateHb_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_calculateHb_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateHb_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_calculateHb_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_h", envir = env, inherits = FALSE)) {
        svalue(f1_h_chk) <- get(".strvalidator_calculateHb_gui_h", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateHb_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateHb_gui_method", value = svalue(f1_method_drp), envir = env)
      assign(x = ".strvalidator_calculateHb_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculateHb_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_calculateHb_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_calculateHb_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_calculateHb_gui_qs", value = svalue(f1_qs_chk), envir = env)
      assign(x = ".strvalidator_calculateHb_gui_h", value = svalue(f1_h_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateHb_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHb_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_method", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHb_gui_method", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHb_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHb_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHb_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHb_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHb_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateHb_gui_h", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHb_gui_h", envir = env)
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
