#' @title Calculate Precision
#'
#' @description
#' GUI wrapper for the \code{\link{calculate_precision}} function.
#'
#' @details
#' Simplifies the use of \code{\link{calculate_precision}} by providing a
#' graphical user interface for sizing precision analysis.
#'
#' The GUI expects a *slimmed* GeneMapper dataset (one allele per row). The input
#' data frame must contain at least the following columns:
#' \itemize{
#'   \item \code{Run.Name}
#'   \item \code{Cap}
#'   \item \code{Sample.Type}
#'   \item \code{Marker}
#'   \item \code{Allele}
#'   \item \code{Size}
#' }
#'
#' An optional column \code{Plate.ID} may be present. If available, its most
#' frequent non-empty value is used to prefill the Plate ID field in the GUI.
#' The user may override this value before running the analysis.
#'
#' The analysis is performed by calling \code{\link{calculate_precision}}.
#' Results are saved as two separate data frames in \code{env}:
#' \code{*_precision_within} (per-injection repeatability metrics) and
#' \code{*_precision_across} (across-injection reproducibility metrics).
#'
#' @param env environment in which to search for data frames and save results.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#' @seealso \code{\link{calculate_precision}}
#' @export
calculate_precision_gui <- function(env = parent.frame(), savegui = NULL,
                                    debug = FALSE, parent = NULL) {
  # Global variables/state
  .g_data <- NULL
  .g_data_name <- NULL
  
  # Get this functions name from call (GUI scope)
  fnc <- get_gui_scope()
  
  if (debug) {
    print(paste("IN:", fnc))
  }
  
  # Language -----------------------------------------------------------------
  
  lng_strings <- get_strings(gui = fnc)
  
  default_strings <- list(
    STR_WIN_TITLE       = "Precision (Sizing)",
    STR_CHK_GUI         = "Save GUI settings",
    STR_BTN_HELP        = "Help",
    
    STR_FRM_DATASET     = "Dataset and kit",
    STR_LBL_DATASET     = "Sample dataset:",
    STR_DRP_DEFAULT     = "<Select dataset>",
    STR_LBL_ROWS        = "rows",
    STR_LBL_KIT         = "Kit:",
    
    STR_FRM_OPTIONS     = "Options",
    STR_LBL_PLATE_ID    = "Plate ID:",
    STR_CHK_LADDER      = "Include allelic ladder wells",
    
    STR_FRM_SAVE        = "Save as",
    STR_LBL_SAVE_WITHIN = "Name for within table:",
    STR_LBL_SAVE_ACROSS = "Name for across table:",
    
    STR_BTN_CALCULATE   = "Calculate",
    STR_BTN_PROCESSING  = "Processing...",
    STR_BTN_CLOSE       = "Close",
    
    STR_MSG_TITLE_ERROR = "Error",
    STR_MSG_NO_DATASET  = "A dataset must be selected.",
    STR_MSG_NO_KIT      = "A kit must be selected.",
    STR_MSG_BAD_NAMES   = "Please provide names for both result data frames."
  )
  
  strings <- update_strings_with_language_file(default_strings, lng_strings$value)
  
  # Dataset QC requirements (slimmed)
  required_cols <- c("Run.Name", "Cap", "Sample.Type", "Marker", "Allele", "Size")
  
  infer_plate_id <- function(x) {
    if (!("Plate.ID" %in% names(x))) return(NA_character_)
    
    v <- trimws(as.character(x[["Plate.ID"]]))
    v <- v[!is.na(v) & v != ""]
    if (!length(v)) return(NA_character_)
    
    tab <- sort(table(v), decreasing = TRUE)
    names(tab)[1]
  }

  # WINDOW ###################################################################
  
  w <- gwindow(title = strings$STR_WIN_TITLE, visible = FALSE)
  
  addHandlerUnrealize(w, handler = function(h, ...) {
    .saveSettings()
    
    # Focus parent window
    if (!is.null(parent)) {
      focus(parent)
    }
    
    return(FALSE)
  })
  
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = w,
    expand = TRUE
  )
  
  # Help button group ---------------------------------------------------------
  
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")
  
  savegui_chk <- gcheckbox(text = strings$STR_CHK_GUI, checked = FALSE, container = gh)
  addSpring(gh)
  
  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh)
  addHandlerChanged(help_btn, handler = function(h, ...) {
    print(help(fnc, help_type = "html"))
  })
  
  # FRAME 0: Dataset and kit ##################################################
  
  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )
  
  # Dataset row
  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$STR_LBL_DATASET, container = f0g0)
  
  rows_lbl <- glabel(text = paste("0", strings$STR_LBL_ROWS), container = f0g0)
  
  dataset_list <- c(strings$STR_DRP_DEFAULT, listObjects(env = env, obj.class = "data.frame"))
  dataset_drp <- gcombobox(
    items = dataset_list,
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )
  
  # Kit row
  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$STR_LBL_KIT, container = f0g1)
  
  kit_items <- getKit()
  kit_drp <- gcombobox(
    items = kit_items,
    selected = 1,
    editable = FALSE,
    container = f0g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )
  
  # FRAME 1: Options ##########################################################
  
  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )
  
  f1g0 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$STR_LBL_PLATE_ID, container = f1g0)
  plate_id_edt <- gedit(text = "Plate1", container = f1g0, expand = TRUE, fill = "x")
  
  f1g1 <- ggroup(container = f1, spacing = 1, expand = TRUE, fill = "x")
  include_ladder_chk <- gcheckbox(text = strings$STR_CHK_LADDER, checked = FALSE, container = f1g1)
  
  # FRAME 2: Save #############################################################
  
  f2 <- gframe(
    text = strings$STR_FRM_SAVE,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )
  
  f2g0 <- ggroup(container = f2, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$STR_LBL_SAVE_WITHIN, container = f2g0)
  save_within_edt <- gedit(text = "", container = f2g0, expand = TRUE, fill = "x")
  
  f2g1 <- ggroup(container = f2, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$STR_LBL_SAVE_ACROSS, container = f2g1)
  save_across_edt <- gedit(text = "", container = f2g1, expand = TRUE, fill = "x")
  
  # BUTTONS ###################################################################
  
  btn_grp <- ggroup(container = gv, expand = FALSE, fill = "x", spacing = 1)
  
  calc_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = btn_grp)
  close_btn <- gbutton(text = strings$STR_BTN_CLOSE, container = btn_grp)
  
  # HANDLERS ##################################################################
  
  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)
    
    # Reset on default
    if (is.null(val_obj) || val_obj == "" || identical(val_obj, strings$STR_DRP_DEFAULT)) {
      .g_data <<- NULL
      .g_data_name <<- NULL
      svalue(rows_lbl) <- paste("0", strings$STR_LBL_ROWS)
      svalue(save_within_edt) <- ""
      svalue(save_across_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      return(invisible())
    }
    
    # QC: slimmed dataset required
    ok <- check_dataset(
      name = val_obj,
      reqcol = required_cols,
      slim = TRUE,
      slimcol = c("Allele", "Size"),
      env = env,
      parent = w,
      debug = debug
    )
    
    if (!isTRUE(ok)) {
      # check_dataset() should already have shown a message, but we still reset state
      .g_data <<- NULL
      .g_data_name <<- NULL
      svalue(rows_lbl) <- paste("0", strings$STR_LBL_ROWS)
      svalue(save_within_edt) <- ""
      svalue(save_across_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      return(invisible())
    }
    
    .g_data <<- get(val_obj, envir = env)
    .g_data_name <<- val_obj
    
    # Prefill Plate.ID from input (if present)
    pid <- infer_plate_id(.g_data)
    if (!is.na(pid)) {
      svalue(plate_id_edt) <- pid
    } else {
      svalue(plate_id_edt) <- "Plate1"
    }
    
    svalue(rows_lbl) <- paste(nrow(.g_data), strings$STR_LBL_ROWS)
    
    # Default save names
    svalue(save_within_edt) <- paste0(.g_data_name, "_precision_within")
    svalue(save_across_edt) <- paste0(.g_data_name, "_precision_across")
    
    # Detect kit (best-effort)
    kit_index <- tryCatch(
      detectKit(.g_data, index = TRUE, debug = debug),
      error = function(e) NA_integer_
    )
    
    if (is.numeric(kit_index) && length(kit_index) == 1 &&
        !is.na(kit_index) && kit_index >= 1 && kit_index <= length(kit_items)) {
      svalue(kit_drp, index = TRUE) <- kit_index
    }
  })
  
  addHandlerChanged(calc_btn, handler = function(h, ...) {
    if (is.null(.g_data) || !is.data.frame(.g_data)) {
      gmessage(strings$STR_MSG_NO_DATASET,
               title = strings$STR_MSG_TITLE_ERROR, icon = "error", parent = w)
      return(invisible())
    }
    
    chosen_kit <- svalue(kit_drp)
    if (is.null(chosen_kit) || chosen_kit == "") {
      gmessage(strings$STR_MSG_NO_KIT,
               title = strings$STR_MSG_TITLE_ERROR, icon = "error", parent = w)
      return(invisible())
    }
    
    plate_id <- svalue(plate_id_edt)
    if (is.null(plate_id) || plate_id == "") {
      plate_id <- "Plate1"
    }
    
    include_ladder <- isTRUE(svalue(include_ladder_chk))
    
    out_within <- svalue(save_within_edt)
    out_across <- svalue(save_across_edt)
    
    if (is.null(out_within) || out_within == "" ||
        is.null(out_across) || out_across == "") {
      gmessage(strings$STR_MSG_BAD_NAMES,
               title = strings$STR_MSG_TITLE_ERROR, icon = "error", parent = w)
      return(invisible())
    }
    
    # UI feedback (concordance style)
    old_label <- svalue(calc_btn)
    svalue(calc_btn) <- strings$STR_BTN_PROCESSING
    enabled(calc_btn) <- FALSE
    on.exit({
      enabled(calc_btn) <- TRUE
      svalue(calc_btn) <- old_label
    }, add = TRUE)
    
    kit_sizes <- getKit(chosen_kit, what = "Size")
    
    res <- tryCatch(
      calculate_precision(
        data = .g_data,
        kit_sizes = kit_sizes,
        plate_id = plate_id,
        include_ladder = include_ladder
      ),
      error = function(e) {
        gmessage(conditionMessage(e),
                 title = strings$STR_MSG_TITLE_ERROR,
                 icon = "error",
                 parent = w)
        return(NULL)
      }
    )
    
    if (is.null(res)) return(invisible(FALSE))
    
    # Save as two separate data frames
    if (exists("saveObject", mode = "function", inherits = TRUE)) {
      saveObject(name = out_within, object = res$within, parent = w, env = env)
      saveObject(name = out_across, object = res$across, parent = w, env = env)
    } else {
      assign(out_within, res$within, envir = env)
      assign(out_across, res$across, envir = env)
    }
    
    dispose(w)
    return(TRUE)
  })
  
  addHandlerChanged(close_btn, handler = function(h, ...) {
    dispose(w)
  })

  settings_prefix <- ".strvalidator_calculate_precision_gui_"
  settings_widgets <- list(
    plate_id = plate_id_edt,
    include_ladder = include_ladder_chk,
    save_within = save_within_edt,
    save_across = save_across_edt
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
  
  .loadSavedSettings()
  visible(w) <- TRUE
  return(TRUE)
}
