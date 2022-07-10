################################################################################
# CHANGE LOG (last 20 changes)
# 09.07.2022: Fixed "...URLs which should use \doi (with the DOI name only)".
# 18.04.2020: Added language support.
# 23.02.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 04.08.2016: First version.

#' @title Plot Contamination
#'
#' @description
#' GUI simplifying the creation of plots from negative control data.
#'
#' @details Select data to plot in the drop-down menu.
#' Automatic plot titles can be replaced by custom titles.
#' A name for the result is automatically suggested.
#' The resulting plot can be saved as either a plot object or as an image.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help
#' @importFrom data.table data.table
#' @importFrom stats dpois
#' @importFrom MASS fitdistr
#' @importFrom ggplot2 ggplot aes_string theme_gray theme_bw
#'  theme_linedraw theme_light theme_dark theme_minimal theme_classic
#'  theme_void scale_y_log10 scale_colour_manual coord_cartesian geom_point
#'  geom_line
#'
#' @references
#' Duncan Taylor et.al.,
#'  Validating multiplexes for use in conjunction with modern interpretation strategies,
#'  Forensic Science International: Genetics, Volume 20, January 2016,
#'  Pages 6-19, ISSN 1872-4973, 10.1016/j.fsigen.2015.09.011.
#' \doi{10.1016/j.fsigen.2015.09.011}

plotContamination_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

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
  strWinTitle <- "Plot contamination"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Contamination dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strChkOverride <- "Override automatic titles"
  strLblTitlePlot <- "Plot title:"
  strLblTitleX <- "X title:"
  strLblTitleY <- "Y title:"
  strLblTheme <- "Plot theme:"
  strBtnPlot <- "Plot"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitle <- "Observed and expected number of peaks per profile (n="
  strLblXTitle <- "Number of peaks per control"
  strLblYTitle <- "Relative occurance"
  strMsgNull <- "Data frame is NULL or NA!"
  strMsgTitleError <- "Error"

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

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkOverride"]$value
    strChkOverride <- ifelse(is.na(strtmp), strChkOverride, strtmp)

    strtmp <- dtStrings["strLblTitlePlot"]$value
    strLblTitlePlot <- ifelse(is.na(strtmp), strLblTitlePlot, strtmp)

    strtmp <- dtStrings["strLblTitleX"]$value
    strLblTitleX <- ifelse(is.na(strtmp), strLblTitleX, strtmp)

    strtmp <- dtStrings["strLblTitleY"]$value
    strLblTitleY <- ifelse(is.na(strtmp), strLblTitleY, strtmp)

    strtmp <- dtStrings["strLblTheme"]$value
    strLblTheme <- ifelse(is.na(strtmp), strLblTheme, strtmp)

    strtmp <- dtStrings["strBtnPlot"]$value
    strBtnPlot <- ifelse(is.na(strtmp), strBtnPlot, strtmp)

    strtmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

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

    strtmp <- dtStrings["strLblMainTitle"]$value
    strLblMainTitle <- ifelse(is.na(strtmp), strLblMainTitle, strtmp)

    strtmp <- dtStrings["strLblXTitle"]$value
    strLblXTitle <- ifelse(is.na(strtmp), strLblXTitle, strtmp)

    strtmp <- dtStrings["strLblYTitle"]$value
    strLblYTitle <- ifelse(is.na(strtmp), strLblYTitle, strtmp)

    strtmp <- dtStrings["strMsgNull"]$value
    strMsgNull <- ifelse(is.na(strtmp), strMsgNull, strtmp)

    strtmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)
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

    # Check which toolkit we are using.
    if (gtoolkit() == "tcltk") {
      if (as.numeric(gsub("[^0-9]", "", packageVersion("gWidgets2tcltk"))) <= 106) {
        # Version <= 1.0.6 have the wrong implementation:
        # See: https://stackoverflow.com/questions/54285836/how-to-retrieve-checkbox-state-in-gwidgets2tcltk-works-in-gwidgets2rgtk2
        message("tcltk version <= 1.0.6, returned TRUE!")
        return(TRUE) # Destroys window under tcltk, but not RGtk2.
      } else {
        # Version > 1.0.6 will be fixed:
        # https://github.com/jverzani/gWidgets2tcltk/commit/9388900afc57454b6521b00a187ca4a16829df53
        message("tcltk version >1.0.6, returned FALSE!")
        return(FALSE) # Destroys window under tcltk, but not RGtk2.
      }
    } else {
      message("RGtk2, returned FALSE!")
      return(FALSE) # Destroys window under RGtk2, but not with tcltk.
    }
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 5,
    use.scrollwindow = FALSE,
    container = w,
    expand = TRUE
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
    horizontal = TRUE,
    spacing = 2,
    container = gv
  )

  glabel(text = strLblDataset, container = f0)

  dataset_drp <- gcombobox(
    items = c(
      strDrpDataset,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0,
    ellipsize = "none"
  )

  f0_samples_lbl <- glabel(
    text = paste(" (0 ", strLblSamples, ")"),
    container = f0
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Peaks", "Id")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # Get data.
      .gData <<- get(val_obj, envir = env)

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")

      svalue(f0_samples_lbl) <- paste(" (",
        length(unique(.gData$Sample.Name)),
        " ", strLblSamples, ")",
        sep = ""
      )

      # Enable buttons.
      enabled(plot_poiss_btn) <- TRUE
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste(" (0 ", strLblSamples, ")")
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 2,
    container = gv
  )

  titles_chk <- gcheckbox(
    text = strChkOverride,
    checked = FALSE, container = f1
  )

  addHandlerChanged(titles_chk, handler = function(h, ...) {
    .updateGui()
  })

  titles_group <- ggroup(
    container = f1, spacing = 1, horizontal = FALSE,
    expand = TRUE, fill = TRUE
  )

  # Legends
  glabel(text = strLblTitlePlot, container = titles_group, anchor = c(-1, 0))
  title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblTitleX, container = titles_group, anchor = c(-1, 0))
  x_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblTitleY, container = titles_group, anchor = c(-1, 0))
  y_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)


  f1g2 <- glayout(container = f1)
  f1g2[1, 1] <- glabel(text = strLblTheme, anchor = c(-1, 0), container = f1g2)
  items_theme <- c(
    "theme_grey()", "theme_bw()", "theme_linedraw()",
    "theme_light()", "theme_dark()", "theme_minimal()",
    "theme_classic()", "theme_void()"
  )
  f1g2[1, 2] <- f1_theme_drp <- gcombobox(
    items = items_theme,
    selected = 1,
    container = f1g2,
    ellipsize = "none"
  )

  # BUTTON ####################################################################

  plot_poiss_btn <- gbutton(
    text = strBtnPlot, container = gv,
    expand = TRUE, fill = TRUE
  )
  tooltip(plot_poiss_btn) <- "Plot observed and expected contamination rate"

  addHandlerChanged(plot_poiss_btn, handler = function(h, ...) {
    enabled(plot_poiss_btn) <- FALSE
    .plot()
    enabled(plot_poiss_btn) <- TRUE
  })

  # FRAME 5 ###################################################################

  f5 <- gframe(
    text = strFrmSave,
    horizontal = TRUE,
    spacing = 2,
    container = gv
  )

  glabel(text = strLblSave, container = f5)

  f5_save_edt <- gedit(expand = TRUE, fill = TRUE, container = f5)

  f5_save_btn <- gbutton(text = strBtnSaveObject, container = f5)

  f5_ggsave_btn <- gbutton(text = strBtnSaveImage, container = f5)

  addHandlerClicked(f5_save_btn, handler = function(h, ...) {
    val_name <- svalue(f5_save_edt)

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- strBtnProcessing
    unblockHandlers(f5_save_btn)
    enabled(f5_save_btn) <- FALSE

    # Save data.
    saveObject(
      name = val_name, object = .gPlot,
      parent = w, env = env, debug = debug
    )

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- strBtnObjectSaved
    unblockHandlers(f5_save_btn)
  })

  addHandlerChanged(f5_ggsave_btn, handler = function(h, ...) {
    val_name <- svalue(f5_save_edt)

    # Save data.
    ggsave_gui(
      ggplot = .gPlot, name = val_name,
      parent = w, env = env, savegui = savegui, debug = debug
    )
  })

  # FUNCTIONS #################################################################


  .plot <- function() {

    # Get values.
    val_data <- .gData
    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_theme <- svalue(f1_theme_drp)

    if (!is.na(val_data) && !is.null(val_data)) {

      # Convert to data table.
      DT <- data.table(val_data)

      # Get number of peaks per sample (one row per sample).
      DTsample <- DT[, list(Peaks = unique(Peaks)), by = list(Id)]

      # Get count of samples with N peaks and sort by # peaks.
      DTtable <- DTsample[, list(Samples = .N), keyby = list(Peaks)]

      # Calculate total number of sampels.
      totalSamples <- sum(DTtable$Samples)

      # Calculate and add proportion (relative occurance).
      DTtable[, Proportion := Samples / totalSamples]

      # Add method.
      DTtable$Method <- "Observed"

      # Print some output.
      print("Observed:")
      print(DTtable)

      # Fit a poisson distribution (estimate lambda parameter)
      maxpeaks <- max(DTsample$Peaks)
      parms <- MASS::fitdistr(DTsample$Peaks, "poisson")
      lambda <- parms$estimate
      sd_x <- as.numeric(parms$sd)
      model <- paste("Pois(", signif(lambda, 3), "), sd=", signif(sd_x, 3), sep = "")
      message("Best fit: ", model)

      # Create data frame with expected observations.
      expected <- data.frame(
        Peaks = seq(0, maxpeaks),
        Proportion = stats::dpois(seq(0, maxpeaks),
          lambda = lambda
        ),
        Method = "Poisson"
      )

      # Print some output.
      print("Expected:")
      print(expected)

      # Prepare some plot settings.
      title <- paste(strLblMainTitle, totalSamples, ")", sep = "")
      legend <- model
      ymin <- floor(log10(min(DTtable$Proportion)))
      breaks <- 10^seq(1, ymin) # Pretty breaks.

      # Create plot.
      gp <- ggplot(data = expected)
      gp <- gp + geom_line(aes_string(x = "Peaks", y = "Proportion", colour = "Method"))
      gp <- gp + scale_y_log10(breaks = breaks)
      gp <- gp + coord_cartesian(ylim = c(1, min(breaks)))
      gp <- gp + geom_point(data = DTtable, aes_string(x = "Peaks", y = "Proportion", colour = "Method"))
      gp <- gp + scale_colour_manual(
        name = NULL,
        values = c("black", "black"),
        labels = c(legend, "Observed"),
        guide = guide_legend(override.aes = list(
          linetype = c("solid", "blank"),
          shape = c(NA, 16)
        ))
      )

      # Add titles.
      if (val_titles) {
        # User defined titles.

        gp <- .applyPlotSettings(
          gp = gp, theme = val_theme,
          main.title = val_title,
          x.title = val_xtitle, y.title = val_ytitle
        )
      } else {
        # Automatic titles.

        gp <- .applyPlotSettings(
          gp = gp, theme = val_theme,
          main.title = title,
          x.title = strLblXTitle,
          y.title = strLblYTitle
        )
      }

      # Show plot.
      print(gp)

      # Store in global variable.
      .gPlot <<- gp

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
      enabled(f5_save_btn) <- TRUE
    } else {
      gmessage(
        msg = strMsgNull,
        title = strMsgTitleError,
        icon = "error"
      )
    }
  }

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {

    # Override titles.
    val <- svalue(titles_chk)
    if (val) {
      enabled(titles_group) <- TRUE
    } else {
      enabled(titles_group) <- FALSE
    }
  }

  .applyPlotSettings <- function(gp, theme = "theme_grey()",
                                 main.title = NULL, x.title = NULL, y.title = NULL) {

    # Apply theme.
    gp <- gp + eval(parse(text = theme))
    gp <- gp + theme(legend.justification = c(1, 1), legend.position = c(1, 1))

    # Apply titles.
    gp <- gp + labs(title = main.title)
    gp <- gp + xlab(x.title)
    gp <- gp + ylab(y.title)

    return(gp)
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
      if (exists(".strvalidator_plotContamination_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotContamination_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotContamination_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotContamination_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(titles_chk) <- get(".strvalidator_plotContamination_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotContamination_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_plotContamination_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_theme", envir = env, inherits = FALSE)) {
        svalue(f1_theme_drp) <- get(".strvalidator_plotContamination_gui_theme", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotContamination_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_title_chk", value = svalue(titles_chk), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_theme", value = svalue(f1_theme_drp), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotContamination_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_theme", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_theme", envir = env)
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
  .updateGui()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
