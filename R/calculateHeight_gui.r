#' @title Calculate Peak Height
#'
#' @description
#' GUI wrapper for the \code{\link{calculateHeight}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateHeight}} function by providing a graphical
#' user interface to it.
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
#' @importFrom utils help
#'
#' @references
#' Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
#'  Journal of the Royal Statistical Society: Series C (Applied Statistics),
#'  Volume 59, Issue 5, 2010,
#'  Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
#' \doi{10.1111/j.1467-9876.2010.00722.x}
#'
#' @seealso \code{\link{calculateHeight}}

calculateHeight_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
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
    STR_WIN_TITLE           = "Calculate peak height",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Datasets",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_REF_DATASET     = "Reference dataset:",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_PRE             = "Pre-processing:",
    STR_CHK_SEX             = "Remove sex markers",
    STR_CHK_SENSORS         = "Remove quality sensors",
    STR_CHK_EXCLUDE         = "Exclude values in 'Allele' column",
    STR_CHK_EXCLUDE_INFO    = "Case sensitive values separated by comma:",
    STR_LBL_MATCHING        = "Reference sample name matching:",
    STR_CHK_IGNORE          = "Ignore case",
    STR_CHK_EXACT           = "Exact matching",
    STR_LBL_POST            = "Post-processing:",
    STR_CHK_REPLACE         = "Replace NA in the result with 0",
    STR_CHK_ADD             = "Add result to dataset",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
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

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # Samples -------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g0)

  samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = f0g0
  )

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1, editable = FALSE,
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Height")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Height",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(samples_lbl) <- paste("", samples, strings$STR_LBL_SAMPLES)

      # Suggest name for the result.
      svalue(save_edt) <- paste(val_obj, "_height", sep = "")

      # Detect kit.
      kitIndex <- detectKit(data = .gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  # References ----------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_REF_DATASET, container = f0g1)

  ref_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = f0g1
  )

  refset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1, editable = FALSE,
    container = f0g1,
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
      svalue(ref_lbl) <- paste("", ref, strings$STR_LBL_REF)
    } else {
      # Reset components.
      .gRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(ref_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # Kit -----------------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_KIT, container = f0g2)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = f0g2,
    ellipsize = "none", expand = TRUE, fill = "x"
  )

  # CHECK #####################################################################

  check_btn <- gbutton(text = strings$STR_BTN_CHECK, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_exact <- svalue(f1_exact_chk)
    val_word <- FALSE

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
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  glabel(text = strings$STR_LBL_PRE, anchor = c(-1, 0), container = f1)

  f1_sex_chk <- gcheckbox(
    text = strings$STR_CHK_SEX,
    checked = FALSE, container = f1
  )

  f1_qs_chk <- gcheckbox(
    text = strings$STR_CHK_SENSORS,
    checked = TRUE, container = f1
  )

  f1_exclude_chk <- gcheckbox(
    text = strings$STR_CHK_EXCLUDE,
    checked = TRUE, container = f1
  )

  f1_exclude_lbl <- glabel(
    text = strings$STR_CHK_EXCLUDE_INFO,
    anchor = c(-1, 0), container = f1
  )

  f1_exclude_edt <- gedit(text = "OL", container = f1)

  glabel(text = strings$STR_LBL_MATCHING, anchor = c(-1, 0), container = f1)

  f1_ignore_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE,
    checked = TRUE, container = f1
  )

  f1_exact_chk <- gcheckbox(
    text = strings$STR_CHK_EXACT,
    checked = FALSE, container = f1
  )

  glabel(text = strings$STR_LBL_POST, anchor = c(-1, 0), container = f1)

  f1_replace_chk <- gcheckbox(
    text = strings$STR_CHK_REPLACE,
    checked = TRUE, container = f1
  )

  f1_add_chk <- gcheckbox(
    text = strings$STR_CHK_ADD,
    checked = TRUE, container = f1
  )

  addHandlerChanged(f1_exclude_chk, handler = function(h, ...) {
    if (svalue(f1_exclude_chk)) {
      enabled(f1_exclude_lbl) <- TRUE
      enabled(f1_exclude_edt) <- TRUE
    } else {
      enabled(f1_exclude_lbl) <- FALSE
      enabled(f1_exclude_edt) <- FALSE
    }
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################


  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    val_data <- .gData
    val_data_name <- .gDataName
    val_ref <- .gRef
    val_ref_name <- .gRefName
    val_name <- svalue(save_edt)
    val_add <- svalue(f1_add_chk)
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)
    val_kit <- svalue(kit_drp)
    val_ignore <- svalue(f1_ignore_chk)
    val_exact <- svalue(f1_exact_chk)
    val_replace <- svalue(f1_replace_chk)
    val_exclude <- svalue(f1_exclude_chk)
    val_ex_values <- svalue(f1_exclude_edt)
    val_na <- NULL
    val_ex <- NULL

    if (val_replace) {
      val_na <- 0
    } else {
      val_na <- NULL
    }

    if (val_exclude) {
      val_ex <- unlist(strsplit(val_ex_values, ",", fixed = TRUE))
    } else {
      val_ex <- NULL
    }

    if (!is.null(val_data)) {
      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateHeight(
        data = val_data, ref = val_ref, na.replace = val_na,
        add = val_add, sex.rm = val_sex, qs.rm = val_qs,
        kit = val_kit, exclude = val_ex,
        ignore.case = val_ignore, exact = val_exact,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "na.replace", "add", "sex.rm",
        "qs.rm", "kit", "exclude", "ignore.case", "exact"
      )

      values <- list(
        val_data_name, val_ref_name, val_na, val_add, val_sex,
        val_qs, val_kit, val_ex, val_ignore, val_exact
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
        print(datanew)
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

  settings_prefix <- ".strvalidator_calculateHeight_gui_"
  settings_widgets <- list(
    sex = f1_sex_chk,
    qs = f1_qs_chk,
    ignore = f1_ignore_chk,
    exact = f1_exact_chk,
    replace = f1_replace_chk,
    add = f1_add_chk,
    exclude = f1_exclude_chk,
    exclude_edt = f1_exclude_edt
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

      # Keep dependent input state in sync with loaded checkbox value.
      exclude_enabled <- isTRUE(svalue(f1_exclude_chk))
      enabled(f1_exclude_lbl) <- exclude_enabled
      enabled(f1_exclude_edt) <- exclude_enabled

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
