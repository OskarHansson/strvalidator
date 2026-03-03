#' @title Calculate Locus Balance
#'
#' @description
#' GUI wrapper for the \code{\link{calculateLb}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateLb}} function
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
#' @seealso \code{link{calculateLb}}, \code{link{check_subset}}
#'

calculateLb_gui <- function(env = parent.frame(), savegui = NULL,
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
    STR_WIN_TITLE           = "Calculate locus balance",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Datasets",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_REF_DATASET     = "Reference dataset:",
    STR_TIP_REF             = "If provided, known alleles will be extracted from data",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_PRE             = "Pre-processing:",
    STR_CHK_OL              = "Remove off-ladder alleles",
    STR_CHK_SEX             = "Remove sex markers",
    STR_CHK_SENSORS         = "Remove quality sensors",
    STR_LBL_MISSING         = "Replace missing data with peak height:",
    STR_LBL_METHOD          = "Calculate locus balance:",
    STR_RAD_PROP            = "Proportional",
    STR_RAD_NORM            = "Normalised",
    STR_RAD_CENT            = "Centred Quantities",
    STR_RAD_PEAK            = "Peak Ratio",
    STR_RAD_MARKER          = "Marker Ratio",
    STR_CHK_DYE             = "Calculate Lb by dye channel",
    STR_LBL_MATCHING        = "Reference sample name matching:",
    STR_CHK_IGNORE          = "Ignore case",
    STR_CHK_WORD            = "Add word boundaries",
    STR_CHK_EXACT           = "Exact matching",
    STR_LBL_POST            = "Post-processing:",
    STR_CHK_AVERAGE         = "Calculate average peak height",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
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
    requiredCol <- c("Sample.Name", "Marker", "Height")
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

      # Suggest a name for the result.
      svalue(save_edt) <- paste(val_obj, "_lb", sep = "")

      # Detect kit.
      kitIndex <- detectKit(data = .gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(data_drp, index = TRUE) <- 1
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
  tooltip(ref_drp) <- strings$STR_TIP_REF

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
      # Enable checkbox to calculate H.
      enabled(f1_h_chk) <- TRUE
    } else {
      # Reset components.
      .gRef <<- NULL
      .gRefName <<- NULL
      svalue(ref_drp, index = TRUE) <- 1
      svalue(ref_samples_lbl) <- paste(" 0", strings$STR_LBL_REF)

      # Disable checkbox to calculate H.
      enabled(f1_h_chk) <- FALSE
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

  #----------------------------------------------------------------------------
  glabel(text = strings$STR_LBL_PRE, anchor = c(-1, 0), container = f1)

  f1_ol_chk <- gcheckbox(
    text = strings$STR_CHK_OL, checked = TRUE,
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

  #----------------------------------------------------------------------------
  glabel(
    text = strings$STR_LBL_MISSING, anchor = c(-1, 0),
    container = f1
  )
  f1_na_edt <- gedit(expand = TRUE, container = f1)

  #----------------------------------------------------------------------------
  glabel(text = strings$STR_LBL_METHOD, anchor = c(-1, 0), container = f1)

  f1_options_lb <- c(
    strings$STR_RAD_PROP,
    strings$STR_RAD_NORM,
    strings$STR_RAD_CENT,
    strings$STR_RAD_PEAK,
    strings$STR_RAD_MARKER
  )

  f1_lb_opt <- gradio(
    items = f1_options_lb,
    selected = 1,
    horizontal = TRUE,
    container = f1
  )

  f1_dye_chk <- gcheckbox(
    text = strings$STR_CHK_DYE, checked = FALSE,
    container = f1
  )

  #----------------------------------------------------------------------------
  glabel(
    text = strings$STR_LBL_MATCHING, anchor = c(-1, 0),
    container = f1
  )

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

  #----------------------------------------------------------------------------
  glabel(text = strings$STR_LBL_POST, anchor = c(-1, 0), container = f1)

  f1_h_chk <- gcheckbox(
    text = strings$STR_CHK_AVERAGE, checked = TRUE,
    container = f1
  )

  # Disable checkbox to calculate H.
  enabled(f1_h_chk) <- FALSE

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    val_option <- svalue(f1_lb_opt, index = TRUE)
    val_dye <- svalue(f1_dye_chk)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_exact <- svalue(f1_exact_chk)
    val_data <- .gData
    val_data_name <- .gDataName
    val_ref <- .gRef
    val_ref_name <- .gRefName
    val_name <- svalue(save_edt)
    val_kit <- svalue(kit_drp)
    val_ol <- svalue(f1_ol_chk)
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)
    val_na <- as.numeric(svalue(f1_na_edt))
    val_h_enabled <- enabled(f1_h_chk)
    val_h <- svalue(f1_h_chk)
    val_exclude <- NULL # Argument to calculateHeight.

    if (debug) {
      print("Read Values:")
      print("val_option")
      print(val_option)
      print("val_dye")
      print(val_dye)
      print("val_ol")
      print(val_ol)
      print("val_sex")
      print(val_sex)
      print("val_qs")
      print(val_qs)
      print("val_na")
      print(val_na)
      print("val_ignore")
      print(val_ignore)
      print("val_word")
      print(val_word)
      print("val_exact")
      print(val_exact)
      print("val_name")
      print(val_name)
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
      print("val_exclude")
      print(val_exclude)
    }

    # Check if data.
    if (!is.null(val_data)) {
      # Check for NA's in dye column.
      if (!any(is.na(val_data$Dye))) {
        if (val_option == 1) {
          val_option <- "prop"
        } else if (val_option == 2) {
          val_option <- "norm"
        } else if (val_option == 3) {
          val_option <- "cent"
        } else if (val_option == 4) {
          val_option <- "peak"
        } else if (val_option == 5) {
          val_option <- "marker"
        } else {
          stop("val_option =", val_option, "not implemented!")
        }

        if (is.na(val_na)) {
          val_na <- NULL
        }

        if (!val_h_enabled) {
          val_h <- FALSE
        }


        # Check if off-ladder peaks should be removed.
        if (val_ol) {
          val_exclude <- "OL"
        }

        if (debug) {
          print("Sent Values:")
          print("val_option")
          print(val_option)
          print("val_na")
          print(val_na)
          print("val_h")
          print(val_h)
          print("val_ignore")
          print(val_ignore)
        }

        # Change button.
        blockHandlers(calculate_btn)
        svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
        unblockHandlers(calculate_btn)
        enabled(calculate_btn) <- FALSE

        datanew <- calculateLb(
          data = val_data,
          ref = val_ref,
          option = val_option,
          by.dye = val_dye,
          ol.rm = val_ol,
          sex.rm = val_sex,
          qs.rm = val_qs,
          na = val_na,
          kit = val_kit,
          ignore.case = val_ignore,
          word = val_word,
          exact = val_exact,
          debug = debug
        )

        # Add attributes to result.
        attr(datanew, which = "kit") <- val_kit

        # Create key-value pairs to log.
        keys <- list(
          "data", "ref", "option", "by.dye",
          "ol.rm", "sex.rm", "qs.rm", "na", "kit", "ignore.case",
          "word", "exact", "calculate.h"
        )

        values <- list(
          val_data_name, val_ref_name, val_option, val_dye,
          val_ol, val_sex, val_qs, val_na, val_kit, val_ignore,
          val_word, val_exact, val_h
        )

        # Update audit trail.
        datanew <- audit_trail(
          obj = datanew, key = keys, value = values,
          label = fnc, arguments = FALSE,
          package = "strvalidator"
        )

        # Calculate and add average peak height.
        if (val_h) {
          # Calculate average peak height.
          dfH <- calculateHeight(
            data = val_data, ref = val_ref,
            na.replace = 0, add = FALSE,
            exclude = val_exclude, sex.rm = val_sex,
            qs.rm = val_qs, kit = val_kit,
            ignore.case = val_ignore, exact = FALSE,
            debug = debug
          )

          message("Average peak height calculated.")

          # Add average peak height to dataset.
          datanew <- addData(
            data = datanew, new.data = dfH,
            by.col = "Sample.Name", then.by.col = NULL,
            exact = TRUE, ignore.case = val_ignore,
            debug = debug
          )

          message("Average peak height added to result.")
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

  settings_prefix <- ".strvalidator_calculateLb_gui_"
  settings_widgets <- list(
    option = f1_lb_opt,
    dye = f1_dye_chk,
    ol = f1_ol_chk,
    sex = f1_sex_chk,
    qs = f1_qs_chk,
    na = f1_na_edt,
    ignore = f1_ignore_chk,
    word = f1_word_chk,
    exact = f1_exact_chk,
    h = f1_h_chk
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
