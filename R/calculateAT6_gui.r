#' @title Calculate Analytical Threshold
#'
#' @description
#' GUI wrapper for the \code{\link{calculateAT6}} function.
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
#' @seealso \code{\link{calculateAT6}}, \code{\link{calculateAT}},
#'  \code{\link{calculateAT_gui}}, \code{\link{check_subset}}

calculateAT6_gui <- function(env = parent.frame(), savegui = NULL,
                             debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gRef <- NULL
  .gAm <- NULL
  .gNameData <- NULL
  .gNameRef <- NULL
  .gNameAm <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Calculate analytical threshold",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Datasets",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_DATASET_REF     = "Reference dataset:",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_LBL_AMOUNT          = "Amount dataset:",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_TEXT1           = "NB! This is an indirect method not recommended.",
    STR_LBL_TEXT2           = "See 'Help' or reference for limitations.",
    STR_CHK_IGNORE          = "Ignore case",
    STR_RAD_ITEM1           = "Linear regression",
    STR_RAD_ITEM2           = "Weighted linear regression",
    STR_LBL_LEVEL           = "Significance level:",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_WIN_CHECK           = "Check subsetting",
    STR_MSG_TITLE_ERROR     = "Error",
    STR_MSG_CHECK           = "Data frame is NULL!\n\nMake sure to select a dataset and a reference set",
    STR_MSG_DATASET         = "A dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Datasets not selected"
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

  # Datasets ------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  g0_samples_lbl <- glabel(text = paste(" 0", strings$STR_LBL_SAMPLES), container = g0)

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
      .gNameData <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, strings$STR_LBL_SAMPLES)

      # Suggest a name for result.
      svalue(f2_save_edt) <- paste(val_obj, "_at6", sep = "")
    } else {
      # Reset components.
      .gData <<- NULL
      .gNameData <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(f2_save_edt) <- ""
    }
  })

  # Refset --------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET_REF, container = g1)

  g0_ref_lbl <- glabel(text = paste(" 0", strings$STR_LBL_REF), container = g1)

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
      .gNameRef <<- val_obj
      ref <- length(unique(.gRef$Sample.Name))
      svalue(g0_ref_lbl) <- paste("", ref, strings$STR_LBL_REF)
    } else {
      # Reset components.
      .gRef <<- NULL
      .gNameRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(g0_ref_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # AMOUNT --------------------------------------------------------------------

  g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_AMOUNT, container = g2)

  am_lbl <- glabel(text = paste(" 0", strings$STR_LBL_SAMPLES), container = g2)

  amset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DEFAULT,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g2,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(amset_drp, handler = function(h, ...) {
    val_obj <- svalue(amset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Amount")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gAm <<- get(val_obj, envir = env)
      .gNameAm <<- val_obj
      am <- length(unique(.gAm$Sample.Name))
      svalue(am_lbl) <- paste("", am, strings$STR_LBL_SAMPLES)
    } else {
      # Reset components.
      .gAm <<- NULL
      .gNameAm <<- NULL
      svalue(amset_drp, index = TRUE) <- 1
      svalue(am_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
    }
  })

  # CHECK #####################################################################

  check_btn <- gbutton(text = strings$STR_BTN_CHECK, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_case_chk)

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = strings$STR_WIN_CHECK,
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

  glabel(
    text = strings$STR_LBL_TEXT1,
    anchor = c(-1, 0), container = f1
  )
  glabel(
    text = strings$STR_LBL_TEXT2,
    anchor = c(-1, 0), container = f1
  )

  f1_ignore_case_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE, checked = TRUE,
    container = f1
  )

  f1_items <- c(strings$STR_RAD_ITEM1, strings$STR_RAD_ITEM2)
  f1_weighted_opt <- gradio(items = f1_items, selected = 2, container = f1)

  glabel(text = strings$STR_LBL_LEVEL, anchor = c(-1, 0), container = f1)
  f1_alpha_spn <- gspinbutton(from = 0, to = 1, by = 0.01, value = 0.05, container = f1)

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strings$STR_FRM_SAVE,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strings$STR_LBL_SAVE, container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE, fill = TRUE)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    val_ignore_case <- svalue(f1_ignore_case_chk)
    val_weighted <- ifelse(svalue(f1_weighted_opt, index = TRUE) == 1, FALSE, TRUE)
    val_alpha <- svalue(f1_alpha_spn)
    val_name <- svalue(f2_save_edt)
    val_name_data <- .gNameData
    val_name_ref <- .gNameRef
    val_name_amount <- .gNameAm

    if (debug) {
      print("GUI options:")
      print("val_ignore_case")
      print(val_ignore_case)
      print("val_weighted")
      print(val_weighted)
      print("val_alpha")
      print(val_alpha)
      print("val_name")
      print(val_name)
    }

    if (!is.null(.gData) & !is.null(.gRef)) {
      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateAT6(
        data = .gData,
        ref = .gRef,
        amount = .gAm,
        weighted = val_weighted,
        alpha = val_alpha,
        ignore.case = val_ignore_case,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "amount",
        "weighted", "alpha", "ignore.case"
      )

      values <- list(
        val_name_data, val_name_ref, val_name_amount,
        val_weighted, val_alpha, val_ignore_case
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

  settings_prefix <- ".strvalidator_calculateAT6_gui_"
  settings_widgets <- list(
    ignore = f1_ignore_case_chk,
    weighted = f1_weighted_opt,
    alpha = f1_alpha_spn
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
