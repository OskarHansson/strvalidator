################################################################################
# CHANGE LOG (last 20 changes)
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 15.03.2020: Added language support.
# 03.03.2019: Compacted and tweaked widgets under tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 31.12.2015: New options 'wrap' and 'at'. 'type' replaced by 'boxplot'.
# 29.08.2015: Added importFrom.
# 09.01.2015: Enable 'generate' after selection of new sample.
# 09.12.2014: First version.


#' @title Generate EPG
#'
#' @description
#' GUI wrapper for the \code{\link{generateEPG}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{generateEPG}} function by providing a graphical
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
#' @seealso \code{\link{generateEPG}}

generateEPG_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gPlot <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Generate electropherogram (EPG)"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Datasets"
  strLblDataset <- "Sample dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strLblSample <- "Sample:"
  strDrpSample <- "<Select sample>"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strLblPlot <- "Plot title:"
  strLblScales <- "Axis scales:"
  strLblSize <- "Allele label text size:"
  strLblJustV <- "Vertical justification:"
  strLblAngle <- "Allele label angle:"
  strLblJustH <- "Horizontal justification:"
  strLblArea <- "Plot area expansion:"
  strLblAT <- "Analytical threshold:"
  strChkIgnore <- "Ignore case in marker names"
  strChkWrap <- "Wrap by dye and add marker ranges and allele names"
  strChkFix <- "Fix x-axis to size range"
  strChkCollapse <- "Collapse (add peak heights of identical alleles. Discards OL)"
  strChkBox <- "Plot peak height distribution (boxplot)"
  strChkPeaks <- "Plot mean peak height for distributions"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strBtnGenerate <- "Generate EPG"
  strBtnProcessing <- "Processing..."
  strMsgDataset <- "A dataset must be selected. Sample is optional."
  strMsgTitleDataset <- "Datasets not selected"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.null(dtStrings)) {
    # Get language strings, use default if not found.

    strtmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)

    strtmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)

    strtmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strtmp <- dtStrings["strFrmDataset"]$value
    strFrmDataset <- ifelse(is.na(strtmp), strFrmDataset, strtmp)

    strtmp <- dtStrings["strLblDataset"]$value
    strLblDataset <- ifelse(is.na(strtmp), strLblDataset, strtmp)

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strtmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strtmp <- dtStrings["strLblSample"]$value
    strLblSample <- ifelse(is.na(strtmp), strLblSample, strtmp)

    strtmp <- dtStrings["strDrpSample"]$value
    strDrpSample <- ifelse(is.na(strtmp), strDrpSample, strtmp)

    strtmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strLblPlot"]$value
    strLblPlot <- ifelse(is.na(strtmp), strLblPlot, strtmp)

    strtmp <- dtStrings["strLblScales"]$value
    strLblScales <- ifelse(is.na(strtmp), strLblScales, strtmp)

    strtmp <- dtStrings["strLblSize"]$value
    strLblSize <- ifelse(is.na(strtmp), strLblSize, strtmp)

    strtmp <- dtStrings["strLblJustV"]$value
    strLblJustV <- ifelse(is.na(strtmp), strLblJustV, strtmp)

    strtmp <- dtStrings["strLblAngle"]$value
    strLblAngle <- ifelse(is.na(strtmp), strLblAngle, strtmp)

    strtmp <- dtStrings["strLblJustH"]$value
    strLblJustH <- ifelse(is.na(strtmp), strLblJustH, strtmp)

    strtmp <- dtStrings["strLblArea"]$value
    strLblArea <- ifelse(is.na(strtmp), strLblArea, strtmp)

    strtmp <- dtStrings["strLblAT"]$value
    strLblAT <- ifelse(is.na(strtmp), strLblAT, strtmp)

    strtmp <- dtStrings["strChkIgnore"]$value
    strChkIgnore <- ifelse(is.na(strtmp), strChkIgnore, strtmp)

    strtmp <- dtStrings["strChkWrap"]$value
    strChkWrap <- ifelse(is.na(strtmp), strChkWrap, strtmp)

    strtmp <- dtStrings["strChkFix"]$value
    strChkFix <- ifelse(is.na(strtmp), strChkFix, strtmp)

    strtmp <- dtStrings["strChkCollapse"]$value
    strChkCollapse <- ifelse(is.na(strtmp), strChkCollapse, strtmp)

    strtmp <- dtStrings["strChkBox"]$value
    strChkBox <- ifelse(is.na(strtmp), strChkBox, strtmp)

    strtmp <- dtStrings["strChkPeaks"]$value
    strChkPeaks <- ifelse(is.na(strtmp), strChkPeaks, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnSaveObject"]$value
    strBtnSaveObject <- ifelse(is.na(strtmp), strBtnSaveObject, strtmp)

    strtmp <- dtStrings["strBtnSaveImage"]$value
    strBtnSaveImage <- ifelse(is.na(strtmp), strBtnSaveImage, strtmp)

    strtmp <- dtStrings["strBtnObjectSaved"]$value
    strBtnObjectSaved <- ifelse(is.na(strtmp), strBtnObjectSaved, strtmp)

    strtmp <- dtStrings["strBtnGenerate"]$value
    strBtnGenerate <- ifelse(is.na(strtmp), strBtnGenerate, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strtmp <- dtStrings["strMsgDataset"]$value
    strMsgDataset <- ifelse(is.na(strtmp), strMsgDataset, strtmp)

    strtmp <- dtStrings["strMsgTitleDataset"]$value
    strMsgTitleDataset <- ifelse(is.na(strtmp), strMsgTitleDataset, strtmp)
  }

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(title = strWinTitle, visible = FALSE)

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
    horizontal = FALSE, spacing = 1, use.scrollwindow = FALSE,
    container = w, expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = strChkGui, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmDataset,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # Dataset -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

  data_samples_lbl <- glabel(
    text = paste(" 0", strLblSamples),
    container = g0
  )

  dfs <- c(strDrpDataset, listObjects(env = env, obj.class = "data.frame"))

  data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # Kit -----------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = g1)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = g1,
    ellipsize = "none", expand = TRUE, fill = "x"
  )

  # Sample --------------------------------------------------------------------

  g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblSample, container = g2)

  sample_drp <- gcombobox(
    items = strDrpSample, selected = 1,
    editable = FALSE, container = g2,
    ellipsize = "none", expand = TRUE, fill = "x"
  )

  # Handlers ------------------------------------------------------------------


  addHandlerChanged(data_drp, handler = function(h, ...) {
    val_obj <- svalue(data_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # Get data.
      .gData <<- get(val_obj, envir = env)

      # Suggest name.
      svalue(save_edt) <- paste(val_obj, "_ggplot", sep = "")

      svalue(data_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strLblSamples
      )

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex

      # Populate dropdown with sample names.
      .refresh_sample_drp()

      # Set dataset as proposed title.
      svalue(f1_title_edt) <- paste(val_obj, " (",
        svalue(kit_drp), ")",
        sep = ""
      )

      # Enable buttons.
      enabled(plot_epg_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(save_edt) <- ""
      svalue(data_drp, index = TRUE) <- 1
      svalue(data_samples_lbl) <- paste(" 0", strLblSamples)
    }
  })

  addHandlerChanged(sample_drp, handler = function(h, ...) {
    # Get selected sample name.
    val_sample <- svalue(sample_drp)

    if (!is.null(val_sample)) {
      if (val_sample != strDrpSample) {
        # Set sample name as proposed title.
        svalue(f1_title_edt) <- paste(val_sample, " (",
          svalue(kit_drp), ")",
          sep = ""
        )

        # Enable buttons.
        enabled(plot_epg_btn) <- TRUE
      }
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions, horizontal = FALSE,
    spacing = 1, container = gv
  )

  glabel(text = strLblPlot, anchor = c(-1, 0), container = f1)
  f1_title_edt <- gedit(text = "", width = 25, container = f1)

  # Layout --------------------------------------------------------------------
  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(text = strLblScales, anchor = c(-1, 0), container = f1g1)
  f1g1[2:3, 1] <- f1_scale_opt <- gradio(
    items = c("free", "free_y", "free_x"),
    selected = 2, horizontal = FALSE, container = f1g1
  )

  f1g1[1, 2] <- glabel(text = strLblSize, container = f1g1)
  f1g1[1, 3] <- f1_size_spb <- gspinbutton(
    from = 0, to = 10, by = 1, value = 2,
    container = f1g1
  )

  f1g1[1, 4] <- glabel(text = strLblJustV, container = f1g1)
  f1g1[1, 5] <- f1_vjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.5, value = 1,
    container = f1g1
  )

  f1g1[2, 2] <- glabel(text = strLblAngle, container = f1g1)
  f1g1[2, 3] <- f1_angle_spb <- gspinbutton(
    from = 0, to = 360, by = 15, value = 0,
    container = f1g1
  )

  f1g1[2, 4] <- glabel(text = strLblJustH, container = f1g1)
  f1g1[2, 5] <- f1_hjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.5, value = 0.5,
    container = f1g1
  )

  f1g1[3, 2] <- glabel(text = strLblArea, container = f1g1)
  f1g1[3, 3] <- f1_expand_spb <- gspinbutton(
    from = 0, to = 1, by = 0.05, value = 0.10,
    container = f1g1
  )

  f1g1[3, 4] <- glabel(text = strLblAT, container = f1g1)
  f1g1[3, 5] <- f1_at_spb <- gspinbutton(
    from = 0, to = 10000, by = 10, value = 0,
    container = f1g1
  )

  f1_ignore_chk <- gcheckbox(
    text = strChkIgnore,
    checked = TRUE, container = f1
  )

  f1_wrap_chk <- gcheckbox(
    text = strChkWrap,
    checked = TRUE, container = f1
  )

  f1_fix_chk <- gcheckbox(
    text = strChkFix,
    checked = TRUE, container = f1
  )

  f1_collapse_chk <- gcheckbox(
    text = strChkCollapse,
    checked = TRUE, container = f1
  )

  f1_box_chk <- gcheckbox(
    text = strChkBox,
    checked = FALSE, container = f1
  )

  f1_peaks_chk <- gcheckbox(
    text = strChkPeaks,
    checked = TRUE, container = f1
  )


  addHandlerChanged(f1_collapse_chk, handler = function(h, ...) {
    val_collapse <- svalue(f1_collapse_chk)

    if (val_collapse) {
      enabled(f1_box_chk) <- TRUE
      enabled(f1_peaks_chk) <- TRUE
    } else {
      enabled(f1_box_chk) <- FALSE
      enabled(f1_peaks_chk) <- FALSE
    }
  })

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  save_btn <- gbutton(text = strBtnSaveObject, container = save_frame)

  ggsave_btn <- gbutton(text = strBtnSaveImage, container = save_frame)

  addHandlerChanged(save_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)

    # Change button.
    blockHandlers(save_btn)
    svalue(save_btn) <- strBtnProcessing
    unblockHandlers(save_btn)
    enabled(save_btn) <- FALSE

    # Save data.
    saveObject(
      name = val_name, object = .gPlot,
      parent = w, env = env, debug = debug
    )

    # Change button.
    blockHandlers(save_btn)
    svalue(save_btn) <- strBtnObjectSaved
    unblockHandlers(save_btn)
  })

  addHandlerChanged(ggsave_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)

    # Save data.
    ggsave_gui(
      ggplot = .gPlot, name = val_name,
      parent = w, env = env, savegui = savegui, debug = debug
    )
  })


  # BUTTON ####################################################################


  plot_epg_btn <- gbutton(text = strBtnGenerate, container = gv)

  addHandlerClicked(plot_epg_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)
    val_sample <- svalue(sample_drp)
    val_kit <- svalue(kit_drp)
    val_peaks <- svalue(f1_peaks_chk)
    val_scale <- svalue(f1_scale_opt)
    val_wrap <- svalue(f1_wrap_chk)
    val_box <- svalue(f1_box_chk)
    val_collapse <- svalue(f1_collapse_chk)
    val_fix <- svalue(f1_fix_chk)
    val_ignore <- svalue(f1_ignore_chk)
    val_title <- svalue(f1_title_edt)
    val_size <- svalue(f1_size_spb)
    val_angle <- svalue(f1_angle_spb)
    val_vjust <- svalue(f1_vjust_spb)
    val_hjust <- svalue(f1_hjust_spb)
    val_expand <- svalue(f1_expand_spb)
    val_at <- svalue(f1_at_spb)
    val_data <- .gData

    if (!is.null(val_data)) {
      if (val_sample != strDrpSample) {
        # Subset selected sample.
        val_data <- val_data[val_data$Sample.Name == val_sample, ]
      }

      # Change button.
      blockHandlers(plot_epg_btn)
      svalue(plot_epg_btn) <- strBtnProcessing
      unblockHandlers(plot_epg_btn)
      enabled(plot_epg_btn) <- FALSE

      gp <- generateEPG(
        data = val_data, kit = val_kit, title = val_title,
        wrap = val_wrap, boxplot = val_box, peaks = val_peaks,
        collapse = val_collapse, silent = FALSE,
        ignore.case = val_ignore, at = val_at,
        scale = val_scale, limit.x = val_fix,
        label.size = val_size, label.angle = val_angle,
        label.vjust = val_vjust, label.hjust = val_hjust,
        expand = val_expand, debug = debug
      )

      # Store in global variable.
      .gPlot <<- gp

      # Change button.
      blockHandlers(plot_epg_btn)
      svalue(plot_epg_btn) <- strBtnGenerate
      unblockHandlers(plot_epg_btn)
      enabled(plot_epg_btn) <- TRUE
    } else {
      gmessage(
        msg = strMsgDataset,
        title = strMsgTitleDataset,
        icon = "error",
        parent = w
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .refresh_sample_drp <- function() {
    if (debug) {
      print("Refresh sample dropdown")
    }

    # Get data frames in global workspace.
    dfs <- unique(.gData$Sample.Name)

    if (!is.null(dfs)) {
      blockHandler(sample_drp)

      # Populate drop list and select first item.
      sample_drp[] <- c(strDrpSample, dfs)
      svalue(sample_drp, index = TRUE) <- 1

      unblockHandler(sample_drp)
    }

    if (debug) {
      print("Sample dropdown refreshed!")
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
      if (exists(".strvalidator_generateEPG_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_generateEPG_gui_savegui", envir = env)
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
      if (exists(".strvalidator_generateEPG_gui_size", envir = env, inherits = FALSE)) {
        svalue(f1_size_spb) <- get(".strvalidator_generateEPG_gui_size", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_angle", envir = env, inherits = FALSE)) {
        svalue(f1_angle_spb) <- get(".strvalidator_generateEPG_gui_angle", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_vjust", envir = env, inherits = FALSE)) {
        svalue(f1_vjust_spb) <- get(".strvalidator_generateEPG_gui_vjust", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_hjust", envir = env, inherits = FALSE)) {
        svalue(f1_hjust_spb) <- get(".strvalidator_generateEPG_gui_hjust", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_expand", envir = env, inherits = FALSE)) {
        svalue(f1_expand_spb) <- get(".strvalidator_generateEPG_gui_expand", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_scales", envir = env, inherits = FALSE)) {
        svalue(f1_scale_opt) <- get(".strvalidator_generateEPG_gui_scales", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_collapse", envir = env, inherits = FALSE)) {
        svalue(f1_collapse_chk) <- get(".strvalidator_generateEPG_gui_collapse", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_fix", envir = env, inherits = FALSE)) {
        svalue(f1_fix_chk) <- get(".strvalidator_generateEPG_gui_fix", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_generateEPG_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_peaks", envir = env, inherits = FALSE)) {
        svalue(f1_peaks_chk) <- get(".strvalidator_generateEPG_gui_peaks", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_box", envir = env, inherits = FALSE)) {
        svalue(f1_box_chk) <- get(".strvalidator_generateEPG_gui_box", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_wrap", envir = env, inherits = FALSE)) {
        svalue(f1_wrap_chk) <- get(".strvalidator_generateEPG_gui_wrap", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_at", envir = env, inherits = FALSE)) {
        svalue(f1_at_spb) <- get(".strvalidator_generateEPG_gui_at", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_generateEPG_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_size", value = svalue(f1_size_spb), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_angle", value = svalue(f1_angle_spb), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_vjust", value = svalue(f1_vjust_spb), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_hjust", value = svalue(f1_hjust_spb), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_expand", value = svalue(f1_expand_spb), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_scales", value = svalue(f1_scale_opt), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_collapse", value = svalue(f1_collapse_chk), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_fix", value = svalue(f1_fix_chk), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_peaks", value = svalue(f1_peaks_chk), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_box", value = svalue(f1_box_chk), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_wrap", value = svalue(f1_wrap_chk), envir = env)
      assign(x = ".strvalidator_generateEPG_gui_at", value = svalue(f1_at_spb), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_generateEPG_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_size", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_angle", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_vjust", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_vjust", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_hjust", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_hjust", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_expand", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_expand", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_scales", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_scales", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_collapse", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_collapse", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_fix", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_fix", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_peaks", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_peaks", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_box", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_box", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_wrap", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_wrap", envir = env)
      }
      if (exists(".strvalidator_generateEPG_gui_at", envir = env, inherits = FALSE)) {
        remove(".strvalidator_generateEPG_gui_at", envir = env)
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
