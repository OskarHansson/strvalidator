#' @title Add Size Information
#'
#' @description
#' GUI wrapper for \code{\link{add_size}}.
#'
#' @details
#' Provides a simple graphical interface for assigning allele sizes using the
#' STR kit definition file.
#'
#' @param env environment used to look up datasets and store results.
#' @param savegui logical indicating whether GUI settings should be saved.
#' @param debug logical for printing debug information.
#' @param parent optional parent widget.
#'
#' @return TRUE (invisibly)
#'
#' @export
#'
#' @importFrom utils help str head
#' @seealso \code{\link{add_size}}
#'

add_size_gui <- function(env = parent.frame(),
                         savegui = NULL,
                         debug = FALSE,
                         parent = NULL) {
  
  # ---------------------------------------------------------------------------
  # GLOBAL STATE
  # ---------------------------------------------------------------------------
  
  .g_data      <- NULL       # dataset content
  .g_data_name <- NULL       # dataset name
  .g_kit       <- 1          # selected kit index
  
  # ---------------------------------------------------------------------------
  # LANGUAGE STRINGS
  # ---------------------------------------------------------------------------
  
  fnc <- get_gui_scope()
  
  if (debug) {
    print(paste("IN:", fnc))
  }
  
  default_strings <- list(
    STR_WIN_TITLE     = "Add size to dataset",
    STR_CHK_GUI       = "Save GUI settings",
    STR_BTN_HELP      = "Help",
    
    STR_FRM_DATASET   = "Dataset and kit",
    STR_LBL_DATASET   = "Sample dataset:",
    STR_DRP_DEFAULT   = "<Select dataset>",
    STR_LBL_SAMPLES   = "samples",
    STR_LBL_KIT       = "Kit:",
    
    STR_FRM_OPTIONS   = "Options",
    STR_LBL_OPTIONS   = "Assign allele sizes using kit definitions",
    
    STR_FRM_SAVE      = "Save as",
    STR_LBL_SAVE      = "Name for result:",
    STR_BTN_ADD       = "Add",
    STR_BTN_PROCESSING = "Processing..."
  )
  
  lng_strings <- get_strings(gui = fnc)
  strings <- update_strings_with_language_file(default_strings,
                                               if (!is.null(lng_strings)) lng_strings$value else NULL)
  
  # ---------------------------------------------------------------------------
  # MAIN WINDOW
  # ---------------------------------------------------------------------------
  
  w <- gwindow(title = strings$STR_WIN_TITLE, visible = FALSE)
  
  addHandlerUnrealize(w, handler = function(h, ...) {
    .save_settings()
    if (!is.null(parent)) focus(parent)
    FALSE
  })
  
  gv <- ggroup(horizontal = FALSE, spacing = 1, container = w, expand = TRUE)
  
  # ---------------------------------------------------------------------------
  # HEADER: Save GUI + Help
  # ---------------------------------------------------------------------------
  
  gh <- ggroup(container = gv)
  savegui_chk <- gcheckbox(text = strings$STR_CHK_GUI, checked = FALSE, container = gh)
  
  addSpring(gh)
  
  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh)
  addHandlerChanged(help_btn, handler = function(h, ...) {
    print(help(fnc, help_type = "html"))
  })
  
  # ---------------------------------------------------------------------------
  # FRAME 0: DATASET AND KIT
  # ---------------------------------------------------------------------------
  
  f0 <- gframe(text = strings$STR_FRM_DATASET, container = gv, spacing = 1)
  
  # Dataset selector
  g0 <- ggroup(container = f0, expand = TRUE, fill = "x")
  glabel(strings$STR_LBL_DATASET, container = g0)
  
  dataset_samples_lbl <- glabel(text = paste("0", strings$STR_LBL_SAMPLES),
                                container = g0)
  
  dataset_drp <- gcombobox(
    items     = c(strings$STR_DRP_DEFAULT, listObjects(env, "data.frame")),
    selected  = 1,
    editable  = FALSE,
    ellipsize = "none",
    container = g0,
    expand    = TRUE,
    fill      = "x"
  )
  
  # KIT selector
  g1 <- ggroup(container = f0, expand = TRUE, fill = "x")
  glabel(strings$STR_LBL_KIT, container = g1)
  
  kit_drp <- gcombobox(
    items     = getKit(),
    selected  = 1,
    editable  = FALSE,
    ellipsize = "none",
    container = g1,
    expand    = TRUE,
    fill      = "x"
  )
  
  # ---------------------------------------------------------------------------
  # Dataset selection handler
  # ---------------------------------------------------------------------------
  
  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)
    
    ok <- check_dataset(
      name   = val_obj,
      reqcol = c("Marker", "Allele"),
      env    = env,
      parent = w,
      debug  = debug
    )
    
    if (ok) {
      .g_data      <<- get(val_obj, envir = env)
      .g_data_name <<- val_obj
      
      samples <- length(unique(.g_data$Sample.Name))
      svalue(dataset_samples_lbl) <- paste(samples, strings$STR_LBL_SAMPLES)
      
      .g_kit <- detectKit(.g_data, index = TRUE)
      svalue(kit_drp, index = TRUE) <- .g_kit
      
      svalue(save_edt) <- paste0(.g_data_name, "_size")
      
    } else {
      .g_data      <<- NULL
      .g_data_name <<- NULL
      svalue(dataset_samples_lbl) <- paste("0", strings$STR_LBL_SAMPLES)
      svalue(save_edt)            <- ""
    }
  })
  
  # ---------------------------------------------------------------------------
  # FRAME 1: OPTIONS
  # ---------------------------------------------------------------------------
  
  f1 <- gframe(text = strings$STR_FRM_OPTIONS, container = gv, spacing = 1)
  
  glabel(strings$STR_LBL_OPTIONS, container = f1)
  
  # (No radio buttons anymore. add_size() handles all logic automatically.)
  
  # ---------------------------------------------------------------------------
  # FRAME 2: SAVE
  # ---------------------------------------------------------------------------
  
  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)
  
  glabel(strings$STR_LBL_SAVE, container = save_frame)
  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)
  
  # ---------------------------------------------------------------------------
  # ACTION BUTTON
  # ---------------------------------------------------------------------------
  
  add_btn <- gbutton(text = strings$STR_BTN_ADD, container = gv)
  
  addHandlerClicked(add_btn, handler = function(h, ...) {
    val_data      <- .g_data
    val_data_name <- .g_data_name
    val_name      <- svalue(save_edt)
    val_kit       <- svalue(kit_drp)
    
    if (debug) {
      print("val_data:")
      print(str(val_data))
      print("val_kit:")
      print(val_kit)
    }
    
    # Update button state
    blockHandlers(add_btn)
    svalue(add_btn) <- strings$STR_BTN_PROCESSING
    unblockHandlers(add_btn)
    enabled(add_btn) <- FALSE
    
    # Merge Size + Offset/Repeat into one kit table for add_size()
    kit_size   <- getKit(val_kit, what = "Size")
    kit_offset <- getKit(val_kit, what = "Offset")
    kit_small  <- unique(kit_offset[, c("Marker", "Offset", "Repeat")])
    kit_merged <- merge(kit_size, kit_small, by = "Marker", all.x = TRUE)
    
    # Apply add_size()
    datanew <- add_size(
      data        = val_data,
      kit         = kit_merged,
      ignore_case = TRUE,
      debug       = debug
    )
    
    # Audit trail
    keys   <- list("data", "kit")
    values <- list(val_data_name, val_kit)
    
    datanew <- audit_trail(
      obj       = datanew,
      key       = keys,
      value     = values,
      label     = fnc,
      arguments = FALSE,
      package   = "strvalidator"
    )
    
    # Save result
    saveObject(val_name, datanew, parent = w, env = env)
    
    # Close GUI
    .save_settings()
    dispose(w)
  })
  
  # ---------------------------------------------------------------------------
  # INTERNAL SETTINGS MANAGEMENT
  # ---------------------------------------------------------------------------
  
  .load_settings <- function() {
    if (!is.null(savegui)) {
      svalue(savegui_chk) <- savegui
      enabled(savegui_chk) <- FALSE
    } else if (exists(".strvalidator_add_size_gui_savegui", envir = env)) {
      svalue(savegui_chk) <- get(".strvalidator_add_size_gui_savegui", envir = env)
    }
  }
  
  .save_settings <- function() {
    if (svalue(savegui_chk)) {
      assign(".strvalidator_add_size_gui_savegui",
             svalue(savegui_chk), envir = env)
    } else {
      if (exists(".strvalidator_add_size_gui_savegui", envir = env))
        remove(".strvalidator_add_size_gui_savegui", envir = env)
    }
  }
  
  # ---------------------------------------------------------------------------
  # INITIALIZE GUI
  # ---------------------------------------------------------------------------
  
  .load_settings()
  
  visible(w) <- TRUE
  focus(w)
  
  invisible(TRUE)
}

################################################################################
#' @rdname add_size_gui
#' @export
#' @usage NULL
#' @keywords internal
#'
#' @description
#' **Deprecated.** Use [add_size_gui()] instead.
################################################################################

addSize_gui <- function(env = parent.frame(), 
                        savegui = NULL,
                        debug = FALSE, 
                        parent = NULL,
                        ...) {
  
  .Deprecated("add_size_gui", package = "strvalidator")
  
  add_size_gui(
    env     = env,
    savegui = savegui,
    debug   = debug,
    parent  = parent,
    ...
  )
}