#' @title Calculate Mixture
#'
#' @description
#' GUI wrapper for the \code{\link{calculate_mixture}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculate_mixture}} function by
#' providing a graphical user interface.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @importFrom utils help head str
#' @importFrom graphics title
#'
#' @seealso \code{\link{calculate_mixture}}, \code{\link{checkSubset}}
#' @aliases calculateMixture_gui
#' @export

calculate_mixture_gui <- function(env = parent.frame(), savegui = NULL,
                                 debug = FALSE, parent = NULL) {
  # Global variables.
  .g_data <- NULL
  .g_ref <- NULL
  .g_name_data <- NULL
  .g_name_ref <- NULL

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
    STR_WIN_TITLE = "Calculate Mixture",
    STR_CHK_GUI = "Save GUI settings",
    STR_BTN_HELP = "Help",
    STR_FRM_DATASET = "Datasets",
    STR_LBL_DATASET = "Sample dataset:",
    STR_DRP_DEFAULT = "<Select dataset>",
    STR_LBL_SAMPLES = "samples",
    STR_LBL_REFERENCE = "Reference dataset:",
    STR_LBL_REF_WORD = "references",
    STR_BTN_CHECK = "Check subsetting",
    STR_FRM_OPTIONS = "Options",
    STR_CHK_OL = "Remove off-ladder alleles (affects number of drop-in)",
    STR_LBL_OL = "Off-ladder alleles contain",
    STR_CHK_DROP = "Include drop-out (calculate Mx anyway)",
    STR_OPT_SAMPLE = "Sample-level summaries",
    STR_OPT_MARKER = "Per marker result",
    STR_CHK_MATCH_CASE = "Match case (case-sensitive sample, marker, and allele names)",
    STR_CHK_AUTO_MINOR = "Auto swap major/minor if Mx > 0.5",
    STR_FRM_SAVE = "Save as",
    STR_LBL_SAVE = "Name for result:",
    STR_BTN_CALCULATE = "Calculate",
    STR_BTN_PROCESSING = "Processing...",
    STR_MSG_DATASET = "A sample dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET = "Dataset not selected",
    STR_MSG_CHECK = "Data frame is NULL!\n\nMake sure to select a sample dataset.",
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
    .save_settings()

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
    container = gv,
    expand = FALSE,
    fill = "x"
  )

  # Dataset -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  data_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = g0
  )

  dfs <- c(strings$STR_DRP_DEFAULT, list_objects(env = env, obj_class = "data.frame"))

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
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # get dataset.
      .g_data <<- get(val_obj, envir = env)
      .g_name_data <<- val_obj
      svalue(data_samples_lbl) <- paste(
        length(unique(.g_data$Sample.Name)),
        strings$STR_LBL_SAMPLES
      )
      svalue(save_edt) <- paste(val_obj, "_mixture", sep = "")
    } else {
      # Reset components.
      .g_data <<- NULL
      .g_name_data <<- NULL
      svalue(data_drp, index = TRUE) <- 1
      svalue(data_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(save_edt) <- ""
    }
  })

  # Reference -----------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_REFERENCE, container = g1)

  ref_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF_WORD),
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

      .g_ref <<- get(val_obj, envir = env)
      .g_name_ref <<- val_obj
      svalue(ref_samples_lbl) <- paste(
        length(unique(.g_ref$Sample.Name)),
        strings$STR_LBL_REF_WORD
      )
    } else {
      # Reset components.
      .g_ref <<- NULL
      .g_name_ref <<- NULL
      svalue(ref_drp, index = TRUE) <- 1
      svalue(ref_samples_lbl) <- paste(" 0", strings$STR_LBL_REF_WORD)
    }
  })

  # CHECK #####################################################################

  check_btn <- gbutton(text = strings$STR_BTN_CHECK, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .g_data
    val_ref <- .g_ref
    val_ignore <- svalue(f1_match_case_chk)
    # val_word <- svalue(f1_word_case_chk)
    # val_exact <- svalue(f1_exact_case_chk)

    if (!is.null(val_data) && !is.null(val_ref)) {
      chksubset_w <- gwindow(
        title = strings$STR_WIN_TITLE_CHECK,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )
      
      # # Create pattern.
      # tmp <- paste(".*", unique(.g_ref$Sample.Name), ".*",
      #   unique(.g_ref$Sample.Name), ".*",
      #   sep = ""
      # )
      # 
      # # Save as dataframe.
      # val_pattern <- data.frame(Sample.Name = tmp, stringsAsFactors = FALSE)

      # if (debug) {
      #   print("Pattern")
      #   print(val_pattern)
      # }

      chksubset_txt <- check_subset(
        data = val_data,
        ref = val_ref,
        console = FALSE,
        ignore_case = val_ignore,
        word = FALSE,
        exact = FALSE,
        reverse = TRUE,
        debug = debug
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
        wrap = FALSE, container = chksubset_w
      )

      visible(chksubset_w) <- TRUE
    } else {
      gmessage(
        msg = strings$STR_MSG_DATASET,
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

  f1_ol_chk <- gcheckbox(
    text = strings$STR_CHK_OL,
    checked = TRUE, container = f1
  )

  glabel(text = strings$STR_LBL_OL, container = f1)
  
  f1_ol_edt <- gedit(text = "OL",
                     expand = TRUE, 
                     fill = TRUE, 
                     container = f1)

  
  f1_drop_chk <- gcheckbox(
    text = strings$STR_CHK_DROP,
    checked = TRUE, container = f1
  )

  f1_match_case_chk <- gcheckbox(
    text = strings$STR_CHK_MATCH_CASE,
    checked = FALSE, container = f1
  )

  # f1_match_word_chk <- gcheckbox(
  #   text = strings$STR_CHK_MATCH_WORD,
  #   checked = FALSE, container = f1
  # )
  # 
  # f1_match_exact_chk <- gcheckbox(
  #   text = strings$STR_CHK_MATCH_EXACT,
  #   checked = FALSE, container = f1
  # )
  
  f1_auto_minor_chk <- gcheckbox(
    text = strings$STR_CHK_AUTO_MINOR,
    checked = FALSE, container = f1
  )
  
  f1_scope_opt <- gradio(
    items = c(strings$STR_OPT_SAMPLE, strings$STR_OPT_MARKER),
    horizontal = TRUE, container = f1
  )
  
  f1_at_spb <- gspinbutton(
    from = 0, to = 50000,
    by = 1, value = 0,
    container = f1
  )
  
  f1_drop_rfu_spb <- gspinbutton(
    from = 0, to = 50000,
    by = 1, value = 0,
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
    val_data <- .g_data
    val_ref <- .g_ref
    val_name_data <- .g_name_data
    val_name_ref <- .g_name_ref
    val_name <- svalue(save_edt)
    val_ol_rm <- svalue(f1_ol_chk)
    val_ol_str <- svalue(f1_ol_edt)
    val_drop <- svalue(f1_drop_chk)
    val_drop_rfu <- if (svalue(f1_drop_rfu_spb) > 0) svalue(f1_drop_rfu_spb) else NULL
    val_at <- svalue(f1_at_spb)
    val_match_case <- svalue(f1_match_case_chk)
    #val_match_word <- svalue(f1_match_word_chk)
    #val_match_exact <- svalue(f1_match_exact_chk)
    val_auto_minor <- svalue(f1_auto_minor_chk)
    val_scope <- ifelse(svalue(f1_scope_opt) == strings$STR_OPT_SAMPLE, "sample", "marker")

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
      print("val_ol_rm")
      print(val_ol_rm)
      print("val_drop")
      print(val_drop)
      print("val_name")
      print(val_name)
    }

    # Check if data.
    if (!is.null(.g_data) & !is.null(.g_ref)) {
      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculate_mixture(
        data = val_data,
        ref_profiles_df = val_ref,
        threshold = val_at,
        dropout_pseudo_rfu = val_drop_rfu,
        include_dropout = val_drop,
        match_case = val_match_case,
        ol_rm = val_ol_rm,
        ol_str = val_ol_str,
        auto_minor_from_mx = val_auto_minor,
        output_level = val_scope
      )
      
      # Create key-value pairs to log.
      keys <- list("data", "ref_profiles_df", "threshold", "dropout_pseudo_rfu",
                   "include_dropout", "match_case", "ol_rm", "ol_str",
                   "auto_minor_from_mx", "output_level")
      
      values <- list(val_name_data, val_name_ref,
                     val_at, val_drop_rfu,
                     val_drop, val_match_case,
                     val_ol_rm, val_ol_str,
                     val_auto_minor,
                     val_scope)

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
      .save_settings()
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

  settings_prefix <- ".strvalidator_calculate_mixture_gui_"
  settings_widgets <- list(
    ol = f1_ol_chk,
    dropout = f1_drop_chk,
    matchcase = f1_match_case_chk,
    autominor = f1_auto_minor_chk,
    threshold = f1_at_spb,
    droprfu = f1_drop_rfu_spb
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

  .load_saved_settings <- function() {
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

  .save_settings <- function() {
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
  .load_saved_settings()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}

