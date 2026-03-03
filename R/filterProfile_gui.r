################################################################################
# CHANGE LOG (last 20 changes)
# 16.11.2025: Changed 'checkSubset' to 'check_subset' and parameter 'ignore.case' to 'ignore_case'.
# 02.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 14.03.2020: Added language support and minor gui adjustments.
# 02.03.2019: Tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 17.09.2016: Updated to pass 'kit' option. Dropdown always active.
# 07.09.2016: Updated to include new filterProfile options.
# 28.04.2016: 'Save as' textbox expandable.
# 15.12.2015: Added attributes to result.
# 15.12.2015: Added option 'exact' sample name matching.
# 29.08.2015: Added importFrom.
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 09.04.2015: Added option 'invert' to filter peaks NOT in reference.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 12.01.2014: Replaced 'subset' with native code.

#' @title Filter Profile
#'
#' @description
#' GUI wrapper for the \code{\link{filterProfile}} function.
#'
#' @details Simplifies the use of the \code{\link{filterProfile}} function
#' by providing a graphical user interface to it.
#' All data not matching/matching the reference will be discarded.
#' Useful for filtering stutters and artifacts from raw typing data or
#' to identify drop-ins.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help str
#' @importFrom graphics title
#'
#' @return TRUE
#'
#' @seealso \code{\link{filterProfile}}, \code{\link{check_subset}}

filterProfile_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  .gData <- NULL
  .gDataName <- NULL
  .gRef <- NULL
  .gRefName <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Filter profile",
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
    STR_CHK_VIRTUAL         = "Exclude virtual bins",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_PRE             = "Pre-processing:",
    STR_CHK_SEX             = "Remove sex markers",
    STR_TIP_SEX             = "Removes sex markers defined in the selected kit.",
    STR_CHK_SENSORS         = "Remove quality sensors",
    STR_TIP_SENSORS         = "Removes quality sensors defined in the selected kit.",
    STR_LBL_METHOD          = "Filter options:",
    STR_RAD_REF             = "Filter by reference profiles",
    STR_RAD_BINS            = "Filter by kit bins (allelic ladder)",
    STR_CHK_INVERT          = "Invert (remove peaks matching)",
    STR_LBL_MATCHING        = "Reference sample name matching:",
    STR_CHK_IGNORE          = "Ignore case",
    STR_TIP_IGNORE          = "'A' will match 'A', 'B-a.2', and 'A2'",
    STR_CHK_WORD            = "Add word boundaries",
    STR_TIP_WORD            = "'A' will match 'A', 'B-A.2', and 'A 2' but not 'A2'",
    STR_CHK_EXACT           = "Exact matching",
    STR_TIP_EXACT           = "'A' will match 'A' but not 'B-A.2', 'A 2', or 'A2'",
    STR_LBL_POST            = "Post-processing:",
    STR_CHK_ADD             = "Add missing loci (markers)",
    STR_TIP_ADD             = "This option will be slower.",
    STR_CHK_KEEP            = "Keep loci/sample even if no matching allele",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_FILTER          = "Filter",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A sample dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected",
    STR_MSG_CHECK           = "Data frame is NULL!\n\nMake sure to select a sample dataset.",
    STR_WIN_TITLE_CHECK     = "Check subsetting",
    STR_MSG_TITLE_ERROR     = "Error"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # WINDOW ####################################################################

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
    expand = TRUE,
    fill = "x"
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

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  g0_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = g0
  )

  g0_dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
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

  addHandlerChanged(g0_dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- paste(.gDataName, "_filter", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(g0_dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  refset_lbl <- glabel(
    text = strings$STR_LBL_REF_DATASET,
    container = g1
  )

  g1_ref_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = g1
  )

  refset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
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
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj
      ref <- length(unique(.gRef$Sample.Name))
      svalue(g1_ref_lbl) <- paste("", ref, strings$STR_LBL_REF)
    } else {
      # Reset components.
      .gRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(g1_ref_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # Kit -------------------------------------------------------------------

  g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  kit_lbl <- glabel(text = strings$STR_LBL_KIT, container = g2)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  kit_chk <- gcheckbox(
    text = strings$STR_CHK_VIRTUAL,
    checked = TRUE,
    container = f0
  )

  # CHECK ###############################################################33####

  check_btn <- gbutton(text = strings$STR_BTN_CHECK, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_case_chk)
    val_exact <- svalue(f1_exact_chk)
    val_word <- svalue(f1_word_chk)

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
        exact = val_exact,
        word = val_word
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
    text = strings$STR_FRM_OPTIONS, horizontal = FALSE,
    spacing = 1, container = gv
  )

  # PRE-PROCESSING ------------------------------------------------------------

  glabel(text = strings$STR_LBL_PRE, anchor = c(-1, 0), container = f1)

  f1_sex_chk <- gcheckbox(
    text = strings$STR_CHK_SEX,
    checked = FALSE, container = f1
  )
  tooltip(f1_sex_chk) <- strings$STR_TIP_SEX

  f1_qs_chk <- gcheckbox(
    text = strings$STR_CHK_SENSORS,
    checked = FALSE, container = f1
  )
  tooltip(f1_qs_chk) <- strings$STR_TIP_SENSORS

  # METHOD --------------------------------------------------------------------

  glabel(text = strings$STR_LBL_METHOD, anchor = c(-1, 0), container = f1)

  f1_options <- c(strings$STR_RAD_REF, strings$STR_RAD_BINS)

  f1_filter_opt <- gradio(
    items = f1_options, selected = 1,
    horizontal = FALSE, container = f1
  )

  f1_invert_chk <- gcheckbox(
    text = strings$STR_CHK_INVERT,
    checked = FALSE, container = f1
  )

  f1_keep_na_chk <- gcheckbox(
    text = strings$STR_CHK_KEEP,
    checked = TRUE, container = f1
  )

  # MATCHING ------------------------------------------------------------------

  glabel(text = strings$STR_LBL_MATCHING, anchor = c(-1, 0), container = f1)

  f1_ignore_case_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE,
    checked = TRUE, container = f1
  )
  tooltip(f1_ignore_case_chk) <- strings$STR_TIP_IGNORE

  f1_exact_chk <- gcheckbox(
    text = strings$STR_CHK_EXACT,
    checked = FALSE, container = f1
  )
  tooltip(f1_exact_chk) <- strings$STR_TIP_EXACT

  f1_word_chk <- gcheckbox(
    text = strings$STR_CHK_WORD,
    checked = FALSE, container = f1
  )
  tooltip(f1_word_chk) <- strings$STR_TIP_WORD

  # POST-PROCESSING -----------------------------------------------------------

  glabel(text = strings$STR_LBL_POST, anchor = c(-1, 0), container = f1)

  f1_add_missing_loci_chk <- gcheckbox(
    text = strings$STR_CHK_ADD,
    checked = TRUE, container = f1
  )
  tooltip(f1_add_missing_loci_chk) <- strings$STR_TIP_ADD

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f1_filter_opt, handler = function(h, ...) {
    .refreshOptions()
  })

  addHandlerChanged(f1_exact_chk, handler = function(h, ...) {
    .refreshOptions()
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  filter_btn <- gbutton(text = strings$STR_BTN_FILTER, container = gv)

  addHandlerClicked(filter_btn, handler = function(h, ...) {
    val_data <- .gData
    val_name_data <- .gDataName
    val_ref <- .gRef
    val_name_ref <- .gRefName
    val_invert <- svalue(f1_invert_chk)
    val_add_missing_loci <- svalue(f1_add_missing_loci_chk)
    val_keep_na <- svalue(f1_keep_na_chk)
    val_ignore_case <- svalue(f1_ignore_case_chk)
    val_exact <- svalue(f1_exact_chk)
    val_word <- svalue(f1_word_chk)
    val_name <- svalue(save_edt)
    val_filter <- svalue(f1_filter_opt, index = TRUE)
    val_kit <- svalue(kit_drp)
    val_exclude <- svalue(kit_chk)
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)

    # Check if filter by kit bins.
    if (val_filter == 2) {
      # Get markers, bins and flag for virtual bins.
      val_ref <- getKit(kit = val_kit, what = "VIRTUAL")

      if (val_exclude) {
        # Remove virtual bins.
        val_ref <- val_ref[val_ref$Virtual == 0, ]
      }
    }

    if (!is.null(val_data) & !is.null(val_ref)) {
      # Change button.
      blockHandlers(filter_btn)
      svalue(filter_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(filter_btn)
      enabled(filter_btn) <- FALSE

      datanew <- filterProfile(
        data = val_data,
        ref = val_ref,
        add.missing.loci = val_add_missing_loci,
        keep.na = val_keep_na,
        ignore.case = val_ignore_case,
        exact = val_exact,
        word = val_word,
        invert = val_invert,
        sex.rm = val_sex,
        qs.rm = val_qs,
        kit = val_kit,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "add.missing.loci",
        "keep.na", "ignore.case", "exact", "word",
        "invert", "sex", "qs", "kit"
      )

      values <- list(
        val_name_data, val_name_ref, val_add_missing_loci,
        val_keep_na, val_ignore_case, val_exact, val_word,
        val_invert, val_sex, val_qs, val_kit
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
        print(str(datanew))
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

  .refreshOptions <- function() {
    val_opt <- svalue(f1_filter_opt, index = TRUE)
    val_exact <- svalue(f1_exact_chk)

    if (val_opt == 1) {
      enabled(refset_lbl) <- TRUE
      enabled(refset_drp) <- TRUE
      enabled(check_btn) <- TRUE

      enabled(kit_chk) <- FALSE
    } else if (val_opt == 2) {
      enabled(refset_lbl) <- FALSE
      enabled(refset_drp) <- FALSE
      enabled(check_btn) <- FALSE

      enabled(kit_chk) <- TRUE
    }

    # Disable 'word' if 'exact' is TRUE.
    if (val_exact) {
      enabled(f1_word_chk) <- FALSE
    } else {
      enabled(f1_word_chk) <- TRUE
    }
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
      if (exists(".strvalidator_filterProfile_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_filterProfile_gui_savegui", envir = env)
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
      if (exists(".strvalidator_filterProfile_gui_invert", envir = env, inherits = FALSE)) {
        svalue(f1_invert_chk) <- get(".strvalidator_filterProfile_gui_invert", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_add_loci", envir = env, inherits = FALSE)) {
        svalue(f1_add_missing_loci_chk) <- get(".strvalidator_filterProfile_gui_add_loci", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_keep_na", envir = env, inherits = FALSE)) {
        svalue(f1_keep_na_chk) <- get(".strvalidator_filterProfile_gui_keep_na", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_ignore_case", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_case_chk) <- get(".strvalidator_filterProfile_gui_ignore_case", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_filterProfile_gui_exact", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_filterProfile_gui_word", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_by", envir = env, inherits = FALSE)) {
        svalue(f1_filter_opt) <- get(".strvalidator_filterProfile_gui_by", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_filterProfile_gui_sex", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_filterProfile_gui_qs", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_filterProfile_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_invert", value = svalue(f1_invert_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_add_loci", value = svalue(f1_add_missing_loci_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_keep_na", value = svalue(f1_keep_na_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_ignore_case", value = svalue(f1_ignore_case_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_by", value = svalue(f1_filter_opt), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_filterProfile_gui_qs", value = svalue(f1_qs_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_filterProfile_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_invert", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_invert", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_add_loci", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_add_loci", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_keep_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_keep_na", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_ignore_case", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_ignore_case", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_exact", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_word", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_by", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_by", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_sex", envir = env)
      }
      if (exists(".strvalidator_filterProfile_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_filterProfile_gui_qs", envir = env)
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
  .refreshOptions()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
