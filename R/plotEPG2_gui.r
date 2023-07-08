################################################################################
# CHANGE LOG (last 20 changes)
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 12.08.2022: Removed 'Save as image' option. ggsave not compatible with plotly.
# 04.08.2022: Fixed switched options for Y-scale radio button.
# 04.08.2022: Removed unused strings.
# 18.07.2022: First version.


#' @title Plot EPG
#'
#' @description
#' GUI wrapper for the \code{\link{plotEPG2}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{plotEPG2}} function by providing a graphical
#' user interface.
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
#' @seealso \code{\link{plotEPG2}}

plotEPG2_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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
  strWinTitle <- "Plot electropherogram (EPG)"
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
  strLblScale <- "Y axis scale:"
  strOptYmax <- "Across dye max y-scale"
  strOptYind <- "Individual dye y-scale"
  strLblAT <- "Analytical threshold:"
  strLblST <- "Stochastic threshold:"
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnObjectSaved <- "Object saved"
  strBtnPlot <- "Plot EPG"
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

    strtmp <- dtStrings["strLblScale"]$value
    strLblScale <- ifelse(is.na(strtmp), strLblScale, strtmp)

    strtmp <- dtStrings["strOptYmax"]$value
    strOptYmax <- ifelse(is.na(strtmp), strOptYmax, strtmp)

    strtmp <- dtStrings["strOptYind"]$value
    strOptYind <- ifelse(is.na(strtmp), strOptYind, strtmp)

    strtmp <- dtStrings["strLblAT"]$value
    strLblAT <- ifelse(is.na(strtmp), strLblAT, strtmp)

    strtmp <- dtStrings["strLblST"]$value
    strLblST <- ifelse(is.na(strtmp), strLblST, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strtmp <- dtStrings["strBtnSaveObject"]$value
    strBtnSaveObject <- ifelse(is.na(strtmp), strBtnSaveObject, strtmp)

    strtmp <- dtStrings["strBtnObjectSaved"]$value
    strBtnObjectSaved <- ifelse(is.na(strtmp), strBtnObjectSaved, strtmp)

    strtmp <- dtStrings["strBtnPlot"]$value
    strBtnPlot <- ifelse(is.na(strtmp), strBtnPlot, strtmp)

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

    return(FALSE) # Destroy window.
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
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
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

  glabel(text = strLblScale, anchor = c(-1, 0), container = f1)
  f1_scale_opt <- gradio(
    items = c(strOptYmax, strOptYind),
    selected = 2, horizontal = FALSE, container = f1
  )

  # Layout --------------------------------------------------------------------
  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(text = strLblAT, container = f1g1)
  f1g1[1, 2] <- f1_at_spb <- gspinbutton(
    from = 0, to = 1000, by = 10, value = 0,
    container = f1g1
  )

  f1g1[2, 1] <- glabel(text = strLblST, container = f1g1)
  f1g1[2, 2] <- f1_st_spb <- gspinbutton(
    from = 0, to = 10000, by = 100, value = 0,
    container = f1g1
  )

  # SAVE ######################################################################

  save_frame <- gframe(text = strFrmSave, container = gv)

  glabel(text = strLblSave, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  save_btn <- gbutton(text = strBtnSaveObject, container = save_frame)

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

  # BUTTON ####################################################################


  plot_epg_btn <- gbutton(text = strBtnPlot, container = gv)

  addHandlerClicked(plot_epg_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)
    val_sample <- svalue(sample_drp)
    val_kit <- svalue(kit_drp)
    val_scale <- svalue(f1_scale_opt, index = TRUE)
    val_at <- svalue(f1_at_spb)
    val_st <- svalue(f1_st_spb)
    val_data <- .gData

    if (val_scale == 1) {
      # Use Max across dye y-scale.
      val_scale <- FALSE
    } else {
      # Use individual dye y-scale.
      val_scale <- TRUE
    }

    if (!is.null(val_data)) {
      if (val_sample != strDrpSample) {
        # Subset selected sample.
        val_data <- val_data[val_data$Sample.Name == val_sample, ]

        # Off-ladder alleles must be removed before plotting the EPG.
        if ("OL" %in% val_data$Allele) {
          n1 <- nrow(val_data)

          val_data <- val_data[!val_data$Allele == "OL", ]

          n2 <- nrow(val_data)

          message("Removed ", n1 - n2, " OL alleles.")
        }

        # Convert the data.
        val_data <- sample_tableToList(val_data)
      }

      # Change button.
      blockHandlers(plot_epg_btn)
      svalue(plot_epg_btn) <- strBtnProcessing
      unblockHandlers(plot_epg_btn)
      enabled(plot_epg_btn) <- FALSE

      gp <- plotEPG2(
        mixData = val_data, kit = val_kit,
        AT = val_at, ST = val_st, dyeYmax = val_scale
      )

      # Store in global variable.
      .gPlot <<- gp

      # Change button.
      blockHandlers(plot_epg_btn)
      svalue(plot_epg_btn) <- strBtnPlot
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
      if (exists(".strvalidator_plotEPG2_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotEPG2_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotEPG2_gui_at", envir = env, inherits = FALSE)) {
        svalue(f1_at_spb) <- get(".strvalidator_plotEPG2_gui_at", envir = env)
      }
      if (exists(".strvalidator_plotEPG2_gui_st", envir = env, inherits = FALSE)) {
        svalue(f1_st_spb) <- get(".strvalidator_plotEPG2_gui_st", envir = env)
      }
      if (exists(".strvalidator_plotEPG2_gui_scale", envir = env, inherits = FALSE)) {
        svalue(f1_scale_opt) <- get(".strvalidator_plotEPG2_gui_scale", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotEPG2_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotEPG2_gui_at", value = svalue(f1_at_spb), envir = env)
      assign(x = ".strvalidator_plotEPG2_gui_st", value = svalue(f1_st_spb), envir = env)
      assign(x = ".strvalidator_plotEPG2_gui_scale", value = svalue(f1_scale_opt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotEPG2_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotEPG2_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotEPG2_gui_at", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotEPG2_gui_at", envir = env)
      }
      if (exists(".strvalidator_plotEPG2_gui_st", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotEPG2_gui_st", envir = env)
      }
      if (exists(".strvalidator_plotEPG2_gui_scale", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotEPG2_gui_scale", envir = env)
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
