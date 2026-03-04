#' @title Calculate Spectral Pull-up
#'
#' @description
#' GUI wrapper for the \code{\link{calculatePullup}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculatePullup}} function by
#' providing a graphical user interface.
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
#' @seealso \code{\link{calculatePullup}}, \code{\link{check_subset}}
#'


calculate_pullup_gui <- function(env = parent.frame(), savegui = NULL,
                                debug = FALSE, parent = NULL) {
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
    STR_WIN_TITLE           = "Calculate spectral pull-up",
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
    STR_CHK_OL              = "Remove off-ladder peaks",
    STR_LBL_METHOD          = "Pull-up settings:",
    STR_LBL_RANGE           = "Pull-up analysis range (data points) around known alleles:",
    STR_LBL_BLOCK           = "Blocking range (data points) around known alleles:",
    STR_LBL_DISCARD         = "Discard pull-ups with ratio >",
    STR_LBL_MATCHING        = "Reference sample name matching:",
    STR_CHK_IGNORE          = "Ignore case",
    STR_CHK_WORD            = "Add word boundaries",
    STR_LBL_POST            = "Post-processing:",
    STR_CHK_DISCARD         = "Discard alleles with no pull-up from the result table",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_LBL_KIT             = "Kit attribute:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_DATASET         = "A sample dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Dataset not selected",
    STR_MSG_CHECK           = "Data frame is NULL!\n\nMake sure to select a sample dataset.",
    STR_WIN_TITLE_CHECK     = "Check subsetting",
    STR_MSG_TITLE_ERROR     = "Error",
    STR_MSG_NA              = "'NA' in 'Dye' column. \nUse add dye function to fix.",
    STR_MSG_TITLE_NA        = "NA detected!"
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

  # Dataset -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  data_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = g0
  )

  dfs <- c(strings$STR_DRP_DEFAULT, list_objects(env = env, obj_class = "data.frame"))

  g0_data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Allele", "Marker", "Dye", "Height", "Size", "Data.Point")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Height",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # get dataset.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      svalue(data_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strings$STR_LBL_SAMPLES
      )

      # Suggest a name for result.
      svalue(save_edt) <- paste(val_obj, "_pullup", sep = "")

      # Detect kit.
      kitIndex <- detect_kit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(g0_data_drp, index = TRUE) <- 1
      svalue(data_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  # Reference -----------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_REF_DATASET, container = g1)

  ref_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = g1
  )

  # NB! dfs defined in previous section.
  ref_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(ref_drp, handler = function(h, ...) {
    val_obj <- svalue(ref_drp)

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
      svalue(ref_samples_lbl) <- paste(
        length(unique(.gRef$Sample.Name)),
        strings$STR_LBL_REF
      )
    } else {
      # Reset components.
      .gRef <<- NULL
      .gRefName <<- NULL
      svalue(ref_drp, index = TRUE) <- 1
      svalue(ref_samples_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # CHECK #####################################################################

  check_btn <- gbutton(text = strings$STR_BTN_CHECK, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
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

  # PRE-PROCESSING ------------------------------------------------------------

  glabel(text = strings$STR_LBL_PRE, anchor = c(-1, 0), container = f1)

  f1_ol_chk <- gcheckbox(
    text = strings$STR_CHK_OL,
    checked = FALSE,
    container = f1
  )

  # METHOD --------------------------------------------------------------------

  glabel(text = strings$STR_LBL_METHOD, anchor = c(-1, 0), container = f1)

  f1g1 <- glayout(container = f1, anchor = c(-1, 0), spacing = 1)

  f1g1[1, 1] <- glabel(text = strings$STR_LBL_RANGE, anchor = c(-1, 0), container = f1g1)
  f1g1[1, 2] <- f1_pullup_spb <- gspinbutton(from = 0, to = 1000, by = 10, value = 6, container = f1g1)

  f1g1[2, 1] <- glabel(text = strings$STR_LBL_BLOCK, anchor = c(-1, 0), container = f1g1)
  f1g1[2, 2] <- f1_block_spb <- gspinbutton(from = 0, to = 1000, by = 10, value = 70, container = f1g1)

  f1g1[3, 1] <- glabel(text = strings$STR_LBL_DISCARD, anchor = c(-1, 0), container = f1g1)
  f1g1[3, 2] <- f1_limit_spb <- gspinbutton(from = 0, to = 10, by = 0.1, value = 1, container = f1g1)

  # MATCHING ------------------------------------------------------------------

  glabel(text = strings$STR_LBL_MATCHING, anchor = c(-1, 0), container = f1)

  f1_ignore_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE,
    checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = strings$STR_CHK_WORD,
    checked = FALSE,
    container = f1
  )

  # POST-PROCESSING -----------------------------------------------------------

  glabel(text = strings$STR_LBL_POST, anchor = c(-1, 0), container = f1)

  f1_discard_chk <- gcheckbox(
    text = strings$STR_CHK_DISCARD,
    checked = FALSE,
    container = f1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  glabel(text = strings$STR_LBL_KIT, container = save_frame)

  kit_drp <- gcombobox(
    items = get_kit(), container = save_frame,
    ellipsize = "none"
  )

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- .gDataName
    val_name_ref <- .gRefName
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_ol <- svalue(f1_ol_chk)
    val_pullup <- svalue(f1_pullup_spb)
    val_block <- svalue(f1_block_spb)
    val_limit <- svalue(f1_limit_spb)
    val_discard <- svalue(f1_discard_chk)
    val_name <- svalue(save_edt)
    val_kit <- svalue(kit_drp)

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
      print("val_ignore")
      print(val_ignore)
      print("val_word")
      print(val_word)
      print("val_ol")
      print(val_ol)
      print("val_pullup")
      print(val_pullup)
      print("val_block")
      print(val_block)
      print("val_limit")
      print(val_limit)
      print("val_name")
      print(val_name)
    }

    # Check if data.
    if (!is.null(.gData) & !is.null(.gRef)) {
      # Check for NA's in dye column.
      if (!any(is.na(.gData$Dye))) {
        # Change button.
        blockHandlers(calculate_btn)
        svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
        unblockHandlers(calculate_btn)
        enabled(calculate_btn) <- FALSE

        datanew <- calculate_pullup(
          data = val_data,
          ref = val_ref,
          pullup_range = val_pullup,
          block_range = val_block,
          ol_rm = val_ol,
          ignore_case = val_ignore,
          word = val_word,
          discard = val_discard,
          limit = val_limit,
          debug = debug
        )

        # Add attributes to result.
        attr(datanew, which = "kit") <- val_kit

        # Create key-value pairs to log.
        keys <- list(
          "data", "ref", "pullup_range", "block_range", "ol_rm",
          "ignore_case", "word", "discard", "limit"
        )

        values <- list(
          val_name_data, val_name_ref, val_pullup, val_block, val_ol,
          val_ignore, val_word, val_discard, val_limit
        )

        # Update audit trail.
        datanew <- audit_trail(
          obj = datanew, key = keys, value = values,
          label = fnc, arguments = FALSE,
          package = "strvalidator"
        )

        # Save data.
        save_object(name = val_name, object = datanew, parent = w, env = env)

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
          msg = strings$STR_MSG_NA,
          title = strings$STR_MSG_TITLE_NA,
          icon = "error",
          parent = w
        )
      }
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

  settings_prefix <- ".strvalidator_calculatePullup_gui_"
  settings_widgets <- list(
    window = f1_pullup_spb,
    block = f1_block_spb,
    limit = f1_limit_spb,
    ol = f1_ol_chk,
    ignore = f1_ignore_chk,
    word = f1_word_chk,
    discard = f1_discard_chk
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
