#' @title Generate EPG
#'
#' @description
#' GUI wrapper for the \code{\link{generate_epg}} function.
#'
#' @details
#' Provides a graphical user interface to the \code{\link{generate_epg}}
#' function, allowing selection of dataset, sample, kit, and plotting options.
#'
#' @param env environment in which to search for data frames and save results.
#' @param savegui logical indicating if GUI settings should be saved.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE (invisibly)
#'
#' @seealso \code{\link{generate_epg}}
#' @aliases generateEPG_gui
#' @export
#'
#' @importFrom utils help
#'

generate_epg_gui <- function(env = parent.frame(),
                             savegui = NULL,
                             debug = FALSE,
                             parent = NULL) {
  
  # Global
  .g_data <- NULL
  .g_plot <- NULL
  
  # Language ------------------------------------------------------------------
  
  fnc <- get_gui_scope()
  if (debug) {
    print(paste("IN:", fnc))
  }
  
  lng_strings <- get_strings(gui = fnc)
  
  default_strings <- list(
    strWinTitle       = "Generate electropherogram (EPG)",
    strChkGui         = "Save GUI settings",
    strBtnHelp        = "Help",
    strFrmDataset     = "Datasets",
    strLblDataset     = "Sample dataset:",
    strDrpDataset     = "<Select dataset>",
    strLblSamples     = "samples",
    strLblSample      = "Sample:",
    strDrpSample      = "<Select sample>",
    strLblKit         = "Kit:",
    strFrmOptions     = "Options",
    strLblPlot        = "Plot title:",
    strLblScales      = "Axis scales:",
    strLblSize        = "Allele label text size:",
    strLblJustV       = "Vertical justification:",
    strLblAngle       = "Allele label angle:",
    strLblJustH       = "Horizontal justification:",
    strLblArea        = "Plot area expansion:",
    strLblAT          = "Analytical threshold:",
    strChkIgnore      = "Ignore case in marker names",
    strChkWrap        = "Wrap by dye and add marker ranges and allele names",
    strChkFix         = "Fix x-axis to size range",
    strChkCollapse    = "Sum peak heights of identical alleles across samples. Discards OL)",
    strChkBox         = "Plot peak height distribution (boxplot)",
    strChkPeaks       = "Plot mean peak height for distributions",
    strFrmSave        = "Save as",
    strLblSave        = "Name for result:",
    strBtnSaveObject  = "Save as object",
    strBtnSaveImage   = "Save as image",
    strBtnObjectSaved = "Object saved",
    strBtnGenerate    = "Generate EPG",
    strBtnProcessing  = "Processing...",
    strMsgDataset     = "A dataset must be selected. Sample is optional.",
    strMsgTitleDataset = "Datasets not selected"
  )
  
  strings <- update_strings_with_language_file(
    default_strings,
    if (!is.null(lng_strings)) lng_strings$value else NULL
  )
  
  # WINDOW --------------------------------------------------------------------
  
  w <- gwindow(title = strings$strWinTitle, visible = FALSE)
  
  addHandlerUnrealize(w, handler = function(h, ...) {
    .save_settings()
    if (!is.null(parent)) focus(parent)
    FALSE
  })
  
  gv <- ggroup(
    horizontal       = FALSE,
    spacing          = 1,
    use.scrollwindow = FALSE,
    container        = w,
    expand           = TRUE
  )
  
  # GUI header: save settings + help -----------------------------------------
  
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")
  
  savegui_chk <- gcheckbox(
    text    = strings$strChkGui,
    checked = FALSE,
    container = gh
  )
  
  addSpring(gh)
  
  help_btn <- gbutton(text = strings$strBtnHelp, container = gh)
  addHandlerChanged(help_btn, handler = function(h, ...) {
    print(help(fnc, help_type = "html"))
  })
  
  # FRAME 0: Dataset, kit, sample --------------------------------------------
  
  f0 <- gframe(
    text       = strings$strFrmDataset,
    horizontal = FALSE,
    spacing    = 1,
    container  = gv
  )
  
  # Dataset selector
  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$strLblDataset, container = g0)
  
  data_samples_lbl <- glabel(
    text = paste(" 0", strings$strLblSamples),
    container = g0
  )
  
  dfs <- c(strings$strDrpDataset, list_objects(env = env, obj_class = "data.frame"))
  data_drp <- gcombobox(
    items     = dfs,
    selected  = 1,
    editable  = FALSE,
    container = g0,
    ellipsize = "none",
    expand    = TRUE,
    fill      = "x"
  )
  
  # Kit selector
  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$strLblKit, container = g1)
  
  kit_drp <- gcombobox(
    items     = get_kit(),
    selected  = 1,
    editable  = FALSE,
    container = g1,
    ellipsize = "none",
    expand    = TRUE,
    fill      = "x"
  )
  
  # Sample selector
  g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$strLblSample, container = g2)
  
  sample_drp <- gcombobox(
    items     = strings$strDrpSample,
    selected  = 1,
    editable  = FALSE,
    container = g2,
    ellipsize = "none",
    expand    = TRUE,
    fill      = "x"
  )
  
  .refresh_sample_drp <- function() {
    if (is.null(.g_data)) return()
    dfs_local <- unique(.g_data$Sample.Name)
    if (!is.null(dfs_local)) {
      blockHandler(sample_drp)
      sample_drp[] <- c(strings$strDrpSample, dfs_local)
      svalue(sample_drp, index = TRUE) <- 1
      unblockHandler(sample_drp)
    }
  }
  
  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)
    
    ok <- check_dataset(
      name   = val_obj,
      reqcol = c("Sample.Name", "Marker", "Allele"),
      env    = env,
      parent = w,
      debug  = debug
    )
    
    if (ok) {
      .g_data <<- get(val_obj, envir = env)
      
      svalue(data_samples_lbl) <- paste(
        length(unique(.g_data$Sample.Name)),
        strings$strLblSamples
      )
      
      kit_index <- detect_kit(.g_data, index = TRUE)
      svalue(kit_drp, index = TRUE) <- kit_index
      
      .refresh_sample_drp()
      
      svalue(title_edt) <- paste(val_obj, " (", svalue(kit_drp), ")", sep = "")
      svalue(save_edt)  <- paste(val_obj, "_ggplot", sep = "")
      
      enabled(plot_epg_btn) <- TRUE
    } else {
      .g_data <<- NULL
      svalue(save_edt) <- ""
      svalue(data_drp, index = TRUE) <- 1
      svalue(data_samples_lbl) <- paste(" 0", strings$strLblSamples)
    }
  })
  
  addHandlerChanged(sample_drp, handler = function(h, ...) {
    val_sample <- svalue(sample_drp)
    if (!is.null(val_sample) && val_sample != strings$strDrpSample) {
      svalue(title_edt) <- paste(val_sample, " (", svalue(kit_drp), ")", sep = "")
      enabled(plot_epg_btn) <- TRUE
    }
  })
  
  # FRAME 1: Options ----------------------------------------------------------
  
  f1 <- gframe(
    text       = strings$strFrmOptions,
    horizontal = FALSE,
    spacing    = 1,
    container  = gv
  )
  
  # Plot title
  glabel(text = strings$strLblPlot, anchor = c(-1, 0), container = f1)
  title_edt <- gedit(text = "", width = 25, container = f1)
  
  # Layout for scale + label options
  f1g1 <- glayout(container = f1, spacing = 1)
  
  f1g1[1, 1] <- glabel(text = strings$strLblScales, anchor = c(-1, 0), container = f1g1)
  f1g1[2:3, 1] <- scale_opt <- gradio(
    items      = c("free", "free_y", "free_x"),
    selected   = 2,
    horizontal = FALSE,
    container  = f1g1
  )
  
  f1g1[1, 2] <- glabel(text = strings$strLblSize, container = f1g1)
  f1g1[1, 3] <- size_spb <- gspinbutton(
    from = 0, to = 10, by = 1, value = 3,
    container = f1g1
  )
  
  f1g1[1, 4] <- glabel(text = strings$strLblJustV, container = f1g1)
  f1g1[1, 5] <- vjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.5, value = 1.0,
    container = f1g1
  )
  
  f1g1[2, 2] <- glabel(text = strings$strLblAngle, container = f1g1)
  f1g1[2, 3] <- angle_spb <- gspinbutton(
    from = 0, to = 360, by = 15, value = 0,
    container = f1g1
  )
  
  f1g1[2, 4] <- glabel(text = strings$strLblJustH, container = f1g1)
  f1g1[2, 5] <- hjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.5, value = 0.5,
    container = f1g1
  )
  
  f1g1[3, 2] <- glabel(text = strings$strLblArea, container = f1g1)
  f1g1[3, 3] <- expand_spb <- gspinbutton(
    from = 0, to = 1, by = 0.05, value = 0.10,
    container = f1g1
  )
  
  f1g1[3, 4] <- glabel(text = strings$strLblAT, container = f1g1)
  f1g1[3, 5] <- at_spb <- gspinbutton(
    from = 0, to = 10000, by = 10, value = 0,
    container = f1g1
  )
  
  ignore_chk <- gcheckbox(
    text    = strings$strChkIgnore,
    checked = TRUE,
    container = f1
  )
  
  wrap_chk <- gcheckbox(
    text    = strings$strChkWrap,
    checked = TRUE,
    container = f1
  )
  
  fix_chk <- gcheckbox(
    text    = strings$strChkFix,
    checked = TRUE,
    container = f1
  )
  
  collapse_chk <- gcheckbox(
    text    = strings$strChkCollapse,
    checked = FALSE,
    container = f1
  )
  
  box_chk <- gcheckbox(
    text    = strings$strChkBox,
    checked = FALSE,
    container = f1
  )
  
  peaks_chk <- gcheckbox(
    text    = strings$strChkPeaks,
    checked = TRUE,
    container = f1
  )
  
  # Option consistency: boxplot requires collapse; peaks valid only with boxplot
  .update_option_state <- function() {
    val_collapse <- svalue(collapse_chk)
    val_box      <- svalue(box_chk)
    
    if (!val_collapse) {
      blockHandlers(box_chk)
      blockHandlers(peaks_chk)
      svalue(box_chk)   <- FALSE
      svalue(peaks_chk) <- FALSE
      enabled(box_chk)  <- FALSE
      enabled(peaks_chk) <- FALSE
      unblockHandlers(box_chk)
      unblockHandlers(peaks_chk)
    } else {
      enabled(box_chk)  <- TRUE
      enabled(peaks_chk) <- val_box
    }
  }
  
  addHandlerChanged(collapse_chk, handler = function(h, ...) {
    .update_option_state()
  })
  
  addHandlerChanged(box_chk, handler = function(h, ...) {
    val_box <- svalue(box_chk)
    
    if (val_box) {
      blockHandlers(collapse_chk)
      svalue(collapse_chk) <- TRUE
      unblockHandlers(collapse_chk)
      enabled(peaks_chk) <- TRUE
    } else {
      enabled(peaks_chk) <- FALSE
    }
    
    .update_option_state()
  })
  
  addHandlerChanged(peaks_chk, handler = function(h, ...) {
    invisible(NULL)
  })
  
  # FRAME: Save ---------------------------------------------------------------
  
  save_frame <- gframe(text = strings$strFrmSave, container = gv)
  glabel(text = strings$strLblSave, container = save_frame)
  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)
  
  save_btn <- gbutton(text = strings$strBtnSaveObject, container = save_frame)
  ggsave_btn <- gbutton(text = strings$strBtnSaveImage, container = save_frame)
  
  addHandlerChanged(save_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)
    
    blockHandlers(save_btn)
    svalue(save_btn) <- strings$strBtnProcessing
    unblockHandlers(save_btn)
    enabled(save_btn) <- FALSE
    
    save_object(
      name   = val_name,
      object = .g_plot,
      parent = w,
      env    = env,
      debug  = debug
    )
    
    blockHandlers(save_btn)
    svalue(save_btn) <- strings$strBtnObjectSaved
    unblockHandlers(save_btn)
  })
  
  addHandlerChanged(ggsave_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)
    
    ggsave_gui(
      ggplot = .g_plot,
      name   = val_name,
      parent = w,
      env    = env,
      savegui = savegui,
      debug  = debug
    )
  })
  
  # BUTTON: Generate ----------------------------------------------------------
  
  plot_epg_btn <- gbutton(text = strings$strBtnGenerate, container = gv)
  
  addHandlerClicked(plot_epg_btn, handler = function(h, ...) {
    val_name    <- svalue(save_edt)
    val_sample  <- svalue(sample_drp)
    val_kit     <- svalue(kit_drp)
    val_peaks   <- svalue(peaks_chk)
    val_scale   <- svalue(scale_opt)
    val_wrap    <- svalue(wrap_chk)
    val_box     <- svalue(box_chk)
    val_collapse <- svalue(collapse_chk)
    val_fix     <- svalue(fix_chk)
    val_ignore  <- svalue(ignore_chk)
    val_title   <- svalue(title_edt)
    val_size    <- svalue(size_spb)
    val_angle   <- svalue(angle_spb)
    val_vjust   <- svalue(vjust_spb)
    val_hjust   <- svalue(hjust_spb)
    val_expand  <- svalue(expand_spb)
    val_at      <- svalue(at_spb)
    val_data    <- .g_data
    
    if (!is.null(val_data)) {
      if (val_sample != strings$strDrpSample) {
        val_data <- val_data[val_data$Sample.Name == val_sample, , drop = FALSE]
      }
      
      blockHandlers(plot_epg_btn)
      svalue(plot_epg_btn) <- strings$strBtnProcessing
      unblockHandlers(plot_epg_btn)
      enabled(plot_epg_btn) <- FALSE
      
      gp <- generate_epg(
        data        = val_data,
        kit         = val_kit,
        title       = val_title,
        wrap        = val_wrap,
        boxplot     = val_box,
        peaks       = val_peaks,
        sum_profiles= val_collapse,
        silent      = FALSE,
        ignore_case = val_ignore,
        at          = val_at,
        scale       = val_scale,
        limit_x     = val_fix,
        label_size  = val_size,
        label_angle = val_angle,
        label_vjust = val_vjust,
        label_hjust = val_hjust,
        expand      = val_expand,
        debug       = debug
      )
      
      .g_plot <<- gp
      
      blockHandlers(plot_epg_btn)
      svalue(plot_epg_btn) <- strings$strBtnGenerate
      unblockHandlers(plot_epg_btn)
      enabled(plot_epg_btn) <- TRUE
    } else {
      gmessage(
        msg   = strings$strMsgDataset,
        title = strings$strMsgTitleDataset,
        icon  = "error",
        parent = w
      )
    }
  })
  
  # SETTINGS ------------------------------------------------------------------
  
  .load_settings <- function() {
    # Reuse old setting names for backward compatibility.
    if (!is.null(savegui)) {
      svalue(savegui_chk) <- savegui
      enabled(savegui_chk) <- FALSE
    } else if (exists(".strvalidator_generate_epg_gui_savegui", envir = env, inherits = FALSE)) {
      svalue(savegui_chk) <- get(".strvalidator_generate_epg_gui_savegui", envir = env)
    }
    
    if (svalue(savegui_chk)) {
      if (exists(".strvalidator_generate_epg_gui_size", envir = env, inherits = FALSE))
        svalue(size_spb) <- get(".strvalidator_generate_epg_gui_size", envir = env)
      if (exists(".strvalidator_generate_epg_gui_angle", envir = env, inherits = FALSE))
        svalue(angle_spb) <- get(".strvalidator_generate_epg_gui_angle", envir = env)
      if (exists(".strvalidator_generate_epg_gui_vjust", envir = env, inherits = FALSE))
        svalue(vjust_spb) <- get(".strvalidator_generate_epg_gui_vjust", envir = env)
      if (exists(".strvalidator_generate_epg_gui_hjust", envir = env, inherits = FALSE))
        svalue(hjust_spb) <- get(".strvalidator_generate_epg_gui_hjust", envir = env)
      if (exists(".strvalidator_generate_epg_gui_expand", envir = env, inherits = FALSE))
        svalue(expand_spb) <- get(".strvalidator_generate_epg_gui_expand", envir = env)
      if (exists(".strvalidator_generate_epg_gui_scales", envir = env, inherits = FALSE))
        svalue(scale_opt) <- get(".strvalidator_generate_epg_gui_scales", envir = env)
      if (exists(".strvalidator_generate_epg_gui_collapse", envir = env, inherits = FALSE))
        svalue(collapse_chk) <- get(".strvalidator_generate_epg_gui_collapse", envir = env)
      if (exists(".strvalidator_generate_epg_gui_fix", envir = env, inherits = FALSE))
        svalue(fix_chk) <- get(".strvalidator_generate_epg_gui_fix", envir = env)
      if (exists(".strvalidator_generate_epg_gui_ignore", envir = env, inherits = FALSE))
        svalue(ignore_chk) <- get(".strvalidator_generate_epg_gui_ignore", envir = env)
      if (exists(".strvalidator_generate_epg_gui_peaks", envir = env, inherits = FALSE))
        svalue(peaks_chk) <- get(".strvalidator_generate_epg_gui_peaks", envir = env)
      if (exists(".strvalidator_generate_epg_gui_box", envir = env, inherits = FALSE))
        svalue(box_chk) <- get(".strvalidator_generate_epg_gui_box", envir = env)
      if (exists(".strvalidator_generate_epg_gui_wrap", envir = env, inherits = FALSE))
        svalue(wrap_chk) <- get(".strvalidator_generate_epg_gui_wrap", envir = env)
      if (exists(".strvalidator_generate_epg_gui_at", envir = env, inherits = FALSE))
        svalue(at_spb) <- get(".strvalidator_generate_epg_gui_at", envir = env)
    }
    
    .update_option_state()
  }
  
  .save_settings <- function() {
    if (svalue(savegui_chk)) {
      assign(".strvalidator_generate_epg_gui_savegui", svalue(savegui_chk), envir = env)
      assign(".strvalidator_generate_epg_gui_size", svalue(size_spb), envir = env)
      assign(".strvalidator_generate_epg_gui_angle", svalue(angle_spb), envir = env)
      assign(".strvalidator_generate_epg_gui_vjust", svalue(vjust_spb), envir = env)
      assign(".strvalidator_generate_epg_gui_hjust", svalue(hjust_spb), envir = env)
      assign(".strvalidator_generate_epg_gui_expand", svalue(expand_spb), envir = env)
      assign(".strvalidator_generate_epg_gui_scales", svalue(scale_opt), envir = env)
      assign(".strvalidator_generate_epg_gui_collapse", svalue(collapse_chk), envir = env)
      assign(".strvalidator_generate_epg_gui_fix", svalue(fix_chk), envir = env)
      assign(".strvalidator_generate_epg_gui_ignore", svalue(ignore_chk), envir = env)
      assign(".strvalidator_generate_epg_gui_peaks", svalue(peaks_chk), envir = env)
      assign(".strvalidator_generate_epg_gui_box", svalue(box_chk), envir = env)
      assign(".strvalidator_generate_epg_gui_wrap", svalue(wrap_chk), envir = env)
      assign(".strvalidator_generate_epg_gui_at", svalue(at_spb), envir = env)
    } else {
      for (nm in c(
        ".strvalidator_generate_epg_gui_savegui",
        ".strvalidator_generate_epg_gui_size",
        ".strvalidator_generate_epg_gui_angle",
        ".strvalidator_generate_epg_gui_vjust",
        ".strvalidator_generate_epg_gui_hjust",
        ".strvalidator_generate_epg_gui_expand",
        ".strvalidator_generate_epg_gui_scales",
        ".strvalidator_generate_epg_gui_collapse",
        ".strvalidator_generate_epg_gui_fix",
        ".strvalidator_generate_epg_gui_ignore",
        ".strvalidator_generate_epg_gui_peaks",
        ".strvalidator_generate_epg_gui_box",
        ".strvalidator_generate_epg_gui_wrap",
        ".strvalidator_generate_epg_gui_at"
      )) {
        if (exists(nm, envir = env, inherits = FALSE)) {
          remove(list = nm, envir = env)
        }
      }
    }
  }
  
  # INIT ----------------------------------------------------------------------
  
  .load_settings()
  
  visible(w) <- TRUE
  focus(w)
  
  invisible(TRUE)
}

