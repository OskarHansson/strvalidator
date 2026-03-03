################################################################################
# CHANGE LOG (last 20 changes)
# 08.09.2022: Compacted gui. Fixed narrow dropdowns. Removed destroy workaround.
# 04.03.2020: Added language support.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 09.07.2018: Fixed blank drop-down menues after selecting a dataset.
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 16.06.2016: 'Save as' textbox expandable.
# 22.12.2015: First version.

#' @title Calculate Ratio
#'
#' @description
#' GUI wrapper for the \code{\link{calculateRatio}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateRatio}} function
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
#' @seealso \code{link{calculateRatio}}, \code{link{check_subset}}

calculateRatio_gui <- function(env = parent.frame(), savegui = NULL,
                               debug = FALSE, parent = NULL) {
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
    STR_WIN_TITLE           = "Calculate ratio",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Datasets",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_REF_DATASET     = "Reference dataset:",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_PRE             = "Pre-processing:",
    STR_CHK_OL              = "Remove off-ladder alleles",
    STR_LBL_METHOD          = "Calculate marker ratio:",
    STR_LBL_NUMERATOR       = "Select numerator markers:",
    STR_DRP_MARKER          = "<Select Marker>",
    STR_LBL_DENOMINATOR     = "Select denominator markers:",
    STR_LBL_GROUP_BY        = "Group by column:",
    STR_DRP_COLUMN          = "<Select Columns>",
    STR_LBL_MATCHING        = "Reference sample name matching:",
    STR_CHK_IGNORE          = "Ignore case",
    STR_CHK_WORD            = "Add word boundaries",
    STR_CHK_EXACT           = "Exact matching",
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

  dfs <- c(strings$STR_DRP_DATASET, listObjects(env = env, obj.class = "data.frame"))

  data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
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
      # Update dropdown menues.
      f1_numerator_drp[, ] <- unique(c(strings$STR_DRP_MARKER, .gData$Marker))
      f1_denominator_drp[, ] <- unique(c(strings$STR_DRP_MARKER, .gData$Marker))
      f1_group_drp[, ] <- unique(c(strings$STR_DRP_COLUMN, names(.gData)))

      # Select default value.
      svalue(f1_numerator_drp, index = TRUE) <- 1
      svalue(f1_denominator_drp, index = TRUE) <- 1
      svalue(f1_group_drp, index = TRUE) <- 1

      # Suggest a name for the result.
      svalue(save_edt) <- paste(val_obj, "_ratio", sep = "")
    } else {
      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_drp, index = TRUE) <- 1
      svalue(data_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""

      # Update dropdown menues.
      f1_numerator_drp[, ] <- strings$STR_DRP_MARKER
      f1_denominator_drp[, ] <- strings$STR_DRP_MARKER
      f1_group_drp[, ] <- strings$STR_DRP_COLUMN

      # Select default value.
      svalue(f1_numerator_drp, index = TRUE) <- 1
      svalue(f1_denominator_drp, index = TRUE) <- 1
      svalue(f1_group_drp, index = TRUE) <- 1
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
    text = strings$STR_CHK_OL, checked = TRUE,
    container = f1
  )

  # METHOD --------------------------------------------------------------------

  glabel(text = strings$STR_LBL_METHOD, anchor = c(-1, 0), container = f1)

  f1g1 <- glayout(container = f1)

  f1g1[1, 1] <- glabel(text = strings$STR_LBL_NUMERATOR, container = f1g1)
  f1g1[1, 2] <- f1_numerator_drp <- gcombobox(
    items = strings$STR_DRP_MARKER,
    container = f1g1, ellipsize = "none"
  )
  f1g1[2, 1:2] <- f1_numerator_edt <- gedit(text = "", container = f1g1)

  f1g1[3, 1] <- glabel(text = strings$STR_LBL_DENOMINATOR, container = f1g1)
  f1g1[3, 2] <- f1_denominator_drp <- gcombobox(
    items = strings$STR_DRP_MARKER,
    container = f1g1, ellipsize = "none"
  )
  f1g1[4, 1:2] <- f1_denominator_edt <- gedit(text = "", container = f1g1)

  f1g1[5, 1] <- glabel(text = strings$STR_LBL_GROUP_BY, container = f1g1)
  f1g1[5, 2] <- f1_group_drp <- gcombobox(
    items = strings$STR_DRP_COLUMN,
    container = f1g1, ellipsize = "none"
  )

  addHandlerChanged(f1_numerator_drp, handler = function(h, ...) {
    val_marker <- svalue(f1_numerator_drp)
    val_value <- svalue(f1_numerator_edt)

    if (!is.null(val_marker)) {
      if (val_marker != strings$STR_DRP_MARKER) {
        # Add new value to selected.
        if (nchar(val_value) == 0) {
          svalue(f1_numerator_edt) <- val_marker
        } else {
          svalue(f1_numerator_edt) <- paste(val_value, val_marker, sep = ",")
        }
      }
    }
  })

  addHandlerChanged(f1_denominator_drp, handler = function(h, ...) {
    val_marker <- svalue(f1_denominator_drp)
    val_value <- svalue(f1_denominator_edt)

    if (!is.null(val_marker)) {
      if (val_marker != strings$STR_DRP_MARKER) {
        # Add new value to selected.
        if (nchar(val_value) == 0) {
          svalue(f1_denominator_edt) <- val_marker
        } else {
          svalue(f1_denominator_edt) <- paste(val_value, val_marker, sep = ",")
        }
      }
    }
  })

  # MATCHING ------------------------------------------------------------------

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

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_name_data <- .gDataName
    val_ref <- .gRef
    val_name_ref <- .gRefName
    val_ol <- svalue(f1_ol_chk)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    val_name <- svalue(save_edt)
    val_numerator <- svalue(f1_numerator_edt)
    val_denominator <- svalue(f1_denominator_edt)
    val_group <- svalue(f1_group_drp)

    if (val_group == strings$STR_DRP_COLUMN) {
      val_group <- NULL
    }

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
      print("val_ol")
      print(val_ol)
      print("val_ignore")
      print(val_ignore)
      print("val_word")
      print(val_word)
      print("val_exact")
      print(val_exact)
      print("val_name")
      print(val_name)
    }

    # Check if data.
    if (!is.null(.gData)) {
      if (!nchar(val_numerator) > 0) {
        val_numerator <- NULL
      } else {
        val_numerator <- unlist(strsplit(val_numerator, split = ","))
      }

      if (!nchar(val_denominator) > 0) {
        val_denominator <- NULL
      } else {
        val_denominator <- unlist(strsplit(val_denominator, split = ","))
      }

      if (debug) {
        print("Sent Values:")
        print("val_numerator")
        print(val_numerator)
        print("val_denominator")
        print(val_denominator)
        print("val_group")
        print(val_group)
      }

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateRatio(
        data = val_data, ref = val_ref,
        numerator = val_numerator,
        denominator = val_denominator,
        group = val_group, ol.rm = val_ol,
        ignore.case = val_ignore, word = val_word,
        exact = val_exact, debug = debug
      )

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "numerator",
        "denominator", "group", "ol", "ignore.case", "word",
        "exact"
      )

      values <- list(
        val_name_data, val_name_ref, val_numerator,
        val_denominator, val_group, val_ol, val_ignore, val_word,
        val_exact
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
      if (exists(".strvalidator_calculateRatio_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateRatio_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateRatio_gui_numerator", envir = env, inherits = FALSE)) {
        svalue(f1_numerator_edt) <- get(".strvalidator_calculateRatio_gui_numerator", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_denominator", envir = env, inherits = FALSE)) {
        svalue(f1_denominator_edt) <- get(".strvalidator_calculateRatio_gui_denominator", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_ol", envir = env, inherits = FALSE)) {
        svalue(f1_ol_chk) <- get(".strvalidator_calculateRatio_gui_ol", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculateRatio_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_word", envir = env, inherits = FALSE)) {
        svalue(f1_word_chk) <- get(".strvalidator_calculateRatio_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_calculateRatio_gui_exact", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateRatio_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_numerator", value = svalue(f1_numerator_edt), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_denominator", value = svalue(f1_denominator_edt), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_word", value = svalue(f1_word_chk), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculateRatio_gui_exact", value = svalue(f1_exact_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateRatio_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_numerator", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_numerator", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_denominator", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_denominator", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_word", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_word", envir = env)
      }
      if (exists(".strvalidator_calculateRatio_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateRatio_gui_exact", envir = env)
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
