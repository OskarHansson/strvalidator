################################################################################
# CHANGE LOG (last 20 changes)
# 10.04.2020: Added language support.
# 23.02.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed expanded 'gexpandgroup'.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 11.11.2015: Added importFrom ggplot2.
# 29.08.2015: Added importFrom.
# 28.06.2015: Changed confidence interval level to match one-sided critical t-value.
# 01.06.2015: First version.

#' @title Plot Analytical Threshold
#'
#' @description
#' GUI simplifying the creation of plots from analytical threshold data.
#'
#' @details Select data to plot in the drop-down menu. Plot regression data
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
#' @importFrom utils help str
#' @importFrom ggplot2 stat_smooth geom_abline xlim ggplot aes_string geom_point
#' position_jitter coord_cartesian theme element_text labs xlab ylab
#'
#' @seealso \url{http://docs.ggplot2.org/current/} for details on plot settings.

plotAT_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gPlot <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Plot analytical threshold"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "AT6 dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strChkOverride <- "Override automatic titles"
  strLblTitlePlot <- "Plot title:"
  strLblTitleX <- "X title:"
  strLblTitleY <- "Y title:"
  strLblTheme <- "Plot theme:"
  strExpPoints <- "Data points"
  strLblShape <- "Shape:"
  strLblAlpha <- "Alpha:"
  strLblJitter <- "Jitter (width):"
  strExpAxes <- "Axes"
  strLblLimitY <- "Limit Y axis (min-max)"
  strLblLimitX <- "Limit X axis (min-max)"
  strLblScales <- "Scales:"
  strExpLabels <- "X labels"
  strLblSize <- "Text size (pts):"
  strLblAngle <- "Angle:"
  strLblJustification <- "Justification (v/h):"
  strFrmPlot <- "Plot analytical threshold data"
  strBtnPlot <- "Plot AT6"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitleLinear <- "Linear regression"
  strLblMainTitleWeighted <- "Weighted linear regression"
  strLblXTitle <- "Amount (pg)"
  strLblYTitle <- "Average Peak Height (RFU)"
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

    strtmp <- dtStrings["strExpPoints"]$value
    strExpPoints <- ifelse(is.na(strtmp), strExpPoints, strtmp)

    strtmp <- dtStrings["strLblShape"]$value
    strLblShape <- ifelse(is.na(strtmp), strLblShape, strtmp)

    strtmp <- dtStrings["strLblAlpha"]$value
    strLblAlpha <- ifelse(is.na(strtmp), strLblAlpha, strtmp)

    strtmp <- dtStrings["strLblJitter"]$value
    strLblJitter <- ifelse(is.na(strtmp), strLblJitter, strtmp)

    strtmp <- dtStrings["strExpAxes"]$value
    strExpAxes <- ifelse(is.na(strtmp), strExpAxes, strtmp)

    strtmp <- dtStrings["strLblLimitY"]$value
    strLblLimitY <- ifelse(is.na(strtmp), strLblLimitY, strtmp)

    strtmp <- dtStrings["strLblLimitX"]$value
    strLblLimitX <- ifelse(is.na(strtmp), strLblLimitX, strtmp)

    strtmp <- dtStrings["strLblScales"]$value
    strLblScales <- ifelse(is.na(strtmp), strLblScales, strtmp)

    strtmp <- dtStrings["strExpLabels"]$value
    strExpLabels <- ifelse(is.na(strtmp), strExpLabels, strtmp)

    strtmp <- dtStrings["strLblSize"]$value
    strLblSize <- ifelse(is.na(strtmp), strLblSize, strtmp)

    strtmp <- dtStrings["strLblAngle"]$value
    strLblAngle <- ifelse(is.na(strtmp), strLblAngle, strtmp)

    strtmp <- dtStrings["strLblJustification"]$value
    strLblJustification <- ifelse(is.na(strtmp), strLblJustification, strtmp)

    strtmp <- dtStrings["strFrmPlot"]$value
    strFrmPlot <- ifelse(is.na(strtmp), strFrmPlot, strtmp)

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

    strtmp <- dtStrings["strLblMainTitleLinear"]$value
    strLblMainTitleLinear <- ifelse(is.na(strtmp), strLblMainTitleLinear, strtmp)

    strtmp <- dtStrings["strLblMainTitleWeighted"]$value
    strLblMainTitleWeighted <- ifelse(is.na(strtmp), strLblMainTitleWeighted, strtmp)

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
    text = paste("0", strLblSamples),
    container = f0
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Amount", "Height", "AT6")
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

      svalue(f0_samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strLblSamples
      )

      # Enable buttons.
      enabled(plot_at6_btn) <- TRUE
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste("0", strLblSamples)
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
  f1g2[1, 2] <- f1_theme_drp <- gcombobox(
    items = c("theme_grey()", "theme_bw()"),
    selected = 1,
    container = f1g2,
    ellipsize = "none"
  )

  # FRAME 7 ###################################################################

  f7 <- gframe(
    text = strFrmPlot,
    horizontal = FALSE,
    container = gv
  )

  grid7 <- glayout(container = f7)

  grid7[1, 1] <- plot_at6_btn <- gbutton(text = strBtnPlot, container = grid7)

  addHandlerChanged(plot_at6_btn, handler = function(h, ...) {
    enabled(plot_at6_btn) <- FALSE
    .plotAT(what = "AT6")
    enabled(plot_at6_btn) <- TRUE
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

  # ADVANCED OPTIONS ##########################################################

  e2 <- gexpandgroup(
    text = strExpPoints,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e2) <- FALSE

  grid2 <- glayout(container = e2)

  grid2[1, 1] <- glabel(text = strLblShape, container = grid2)
  grid2[1, 2] <- shape_spb <- gspinbutton(
    from = 0, to = 25,
    by = 1, value = 18,
    container = grid2
  )

  grid2[1, 3] <- glabel(text = strLblAlpha, container = grid2)
  grid2[1, 4] <- alpha_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = 1,
    container = grid2
  )

  grid2[1, 5] <- glabel(text = strLblJitter, container = grid2)
  grid2[1, 6] <- jitter_txt <- gedit(text = "0", width = 4, container = grid2)

  # FRAME 3 ###################################################################

  e3 <- gexpandgroup(
    text = strExpAxes,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e3) <- FALSE

  grid3 <- glayout(container = e3, spacing = 1)

  grid3[1, 1:2] <- glabel(text = strLblLimitY, container = grid3)
  grid3[2, 1] <- y_min_txt <- gedit(text = "", width = 5, container = grid3)
  grid3[2, 2] <- y_max_txt <- gedit(text = "", width = 5, container = grid3)

  grid3[3, 1:2] <- glabel(text = strLblLimitX, container = grid3)
  grid3[4, 1] <- x_min_txt <- gedit(text = "", width = 5, container = grid3)
  grid3[4, 2] <- x_max_txt <- gedit(text = "", width = 5, container = grid3)

  grid3[1, 3] <- glabel(text = "    ", container = grid3) # Add some space.

  grid3[1, 4] <- glabel(text = strLblScales, container = grid3)
  grid3[2:4, 4] <- scales_opt <- gradio(
    items = c("fixed", "free_x", "free_y", "free"),
    selected = 2,
    horizontal = FALSE,
    container = grid3
  )

  # FRAME 4 ###################################################################

  e4 <- gexpandgroup(
    text = strExpLabels,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e4) <- FALSE

  grid4 <- glayout(container = e4)

  grid4[1, 1] <- glabel(text = strLblSize, container = grid4)
  grid4[1, 2] <- size_txt <- gedit(text = "10", width = 4, container = grid4)

  grid4[1, 3] <- glabel(text = strLblAngle, container = grid4)
  grid4[1, 4] <- angle_spb <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 270,
    container = grid4
  )

  grid4[2, 1] <- glabel(text = strLblJustification, container = grid4)
  grid4[2, 2] <- vjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 0.5,
    container = grid4
  )

  grid4[2, 3] <- hjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 0,
    container = grid4
  )



  # FUNCTIONS #################################################################


  .plotAT <- function(what) {

    # Get values.
    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_shape <- as.numeric(svalue(shape_spb))
    val_alpha <- as.numeric(svalue(alpha_spb))
    val_jitter <- as.numeric(svalue(jitter_txt))
    val_ymin <- as.numeric(svalue(y_min_txt))
    val_ymax <- as.numeric(svalue(y_max_txt))
    val_xmin <- as.numeric(svalue(x_min_txt))
    val_xmax <- as.numeric(svalue(x_max_txt))
    val_angle <- as.numeric(svalue(angle_spb))
    val_vjust <- as.numeric(svalue(vjust_spb))
    val_hjust <- as.numeric(svalue(hjust_spb))
    val_size <- as.numeric(svalue(size_txt))
    val_scales <- svalue(scales_opt)
    val_theme <- svalue(f1_theme_drp)

    if (debug) {
      print("ARGUMENTS:")
      print("val_title")
      print(val_title)
      print("val_xtitle")
      print(val_xtitle)
      print("val_ytitle")
      print(val_ytitle)
      print("val_shape")
      print(val_shape)
      print("val_alpha")
      print(val_alpha)
      print("val_jitter")
      print(val_jitter)
      print("val_ymin")
      print(val_ymin)
      print("val_ymax")
      print(val_ymax)
      print("val_angle")
      print(val_angle)
      print("val_vjust")
      print(val_vjust)
      print("val_hjust")
      print(val_hjust)
      print("val_size")
      print(val_size)
      print("val_scales")
      print(val_scales)
      print("str(.gData)")
      print(str(.gData))
      print("val_theme")
      print(val_theme)
    }

    if (!is.na(.gData) && !is.null(.gData)) {

      # Plotting data and regression for AT6.
      if (what == "AT6") {
        if (val_titles) {
          mainTitle <- val_title
          xTitle <- val_xtitle
          yTitle <- val_ytitle
        } else {
          if (all(is.na(.gData$Weight))) {
            mainTitle <- strLblMainTitleLinear
          } else {
            mainTitle <- strLblMainTitleWeighted
          }
          subTitle <- paste("AT6:", round(unique(.gData$AT6), 0), "(RFU)")
          xTitle <- strLblXTitle
          yTitle <- strLblYTitle
        }

        # Create plot.
        gp <- ggplot(data = .gData, aes_string(x = "Amount", y = "Height"))

        # Get
        npoints <- unique(.gData$N)
        alpha <- unique(.gData$Alpha)
        atinterc <- unique(.gData$AT6)

        if (all(is.na(.gData$Weight))) {

          # Add regression line.
          gp <- gp + stat_smooth(aes_string(x = "Amount", y = "Height"),
            method = "lm", se = TRUE, n = npoints,
            fullrange = TRUE, level = 1 - alpha * 2
          )
        } else {

          # Add weighted regression line.
          gp <- gp + stat_smooth(aes_string(x = "Amount", y = "Height", weight = "Weight"),
            method = "lm", se = TRUE, n = npoints,
            fullrange = TRUE, level = 1 - alpha * 2
          )
        }

        # Addthreshold line.
        gp <- gp + geom_abline(
          intercept = atinterc,
          slope = 0, linetype = "dotted"
        )

        # Set x-axis to extend regression line.
        gp <- gp + xlim(0, max(.gData$Amount))
      }

      # Apply theme.
      gp <- gp + eval(parse(text = val_theme))

      # Plot settings.
      gp <- gp + geom_point(
        shape = val_shape, alpha = val_alpha,
        position = position_jitter(height = 0, width = val_jitter)
      )

      # Restrict y axis.
      if (!is.na(val_ymin) && !is.na(val_ymax)) {
        val_y <- c(val_ymin, val_ymax)
      } else {
        val_y <- NULL
      }
      # Restrict x axis.
      if (!is.na(val_xmin) && !is.na(val_xmax)) {
        val_x <- c(val_xmin, val_xmax)
      } else {
        val_x <- NULL
      }
      # Zoom in without dropping observations.
      gp <- gp + coord_cartesian(xlim = val_x, ylim = val_y)

      if (debug) {
        print(paste("Plot zoomed to xlim:", val_x, "ylim:", val_y))
      }

      # Titles.
      gp <- gp + theme(axis.text.x = element_text(
        angle = val_angle,
        hjust = val_hjust,
        vjust = val_vjust,
        size = val_size
      ))
      gp <- gp + labs(title = paste(mainTitle, "\n", subTitle))
      gp <- gp + xlab(xTitle)
      gp <- gp + ylab(yTitle)

      # Restrict y axis.
      if (!is.na(val_ymin) && !is.na(val_ymax)) {
        val_y <- c(val_ymin, val_ymax)
      } else {
        val_y <- NULL
      }
      # Restrict x axis.
      if (!is.na(val_xmin) && !is.na(val_xmax)) {
        val_x <- c(val_xmin, val_xmax)
      } else {
        val_x <- NULL
      }
      # Zoom in without dropping observations.
      gp <- gp + coord_cartesian(xlim = val_x, ylim = val_y)

      if (debug) {
        print(paste("Plot zoomed to xlim:", val_x, "ylim:", val_y))
      }

      # Show plot.
      print(gp)

      # Change save button.
      svalue(f5_save_btn) <- "Save as object"
      enabled(f5_save_btn) <- TRUE

      # Store in global variable.
      .gPlot <<- gp
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
      if (exists(".strvalidator_plotAT_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotAT_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotAT_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotAT_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(titles_chk) <- get(".strvalidator_plotAT_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotAT_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_plotAT_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_points_shape", envir = env, inherits = FALSE)) {
        svalue(shape_spb) <- get(".strvalidator_plotAT_gui_points_shape", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_points_alpha", envir = env, inherits = FALSE)) {
        svalue(alpha_spb) <- get(".strvalidator_plotAT_gui_points_alpha", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_points_jitter", envir = env, inherits = FALSE)) {
        svalue(jitter_txt) <- get(".strvalidator_plotAT_gui_points_jitter", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_y_min", envir = env, inherits = FALSE)) {
        svalue(y_min_txt) <- get(".strvalidator_plotAT_gui_axes_y_min", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_y_max", envir = env, inherits = FALSE)) {
        svalue(y_max_txt) <- get(".strvalidator_plotAT_gui_axes_y_max", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_x_min", envir = env, inherits = FALSE)) {
        svalue(x_min_txt) <- get(".strvalidator_plotAT_gui_axes_x_min", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_x_max", envir = env, inherits = FALSE)) {
        svalue(x_max_txt) <- get(".strvalidator_plotAT_gui_axes_x_max", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_scales", envir = env, inherits = FALSE)) {
        svalue(scales_opt) <- get(".strvalidator_plotAT_gui_axes_scales", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_xlabel_size", envir = env, inherits = FALSE)) {
        svalue(size_txt) <- get(".strvalidator_plotAT_gui_xlabel_size", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_xlabel_angle", envir = env, inherits = FALSE)) {
        svalue(angle_spb) <- get(".strvalidator_plotAT_gui_xlabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_xlabel_justh", envir = env, inherits = FALSE)) {
        svalue(hjust_spb) <- get(".strvalidator_plotAT_gui_xlabel_justh", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_xlabel_justv", envir = env, inherits = FALSE)) {
        svalue(vjust_spb) <- get(".strvalidator_plotAT_gui_xlabel_justv", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_theme", envir = env, inherits = FALSE)) {
        svalue(f1_theme_drp) <- get(".strvalidator_plotAT_gui_theme", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotAT_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotAT_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_title_chk", value = svalue(titles_chk), envir = env)
      assign(x = ".strvalidator_plotAT_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_points_shape", value = svalue(shape_spb), envir = env)
      assign(x = ".strvalidator_plotAT_gui_points_alpha", value = svalue(alpha_spb), envir = env)
      assign(x = ".strvalidator_plotAT_gui_points_jitter", value = svalue(jitter_txt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_axes_y_min", value = svalue(y_min_txt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_axes_y_max", value = svalue(y_max_txt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_axes_x_min", value = svalue(x_min_txt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_axes_x_max", value = svalue(x_max_txt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_axes_scales", value = svalue(scales_opt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_xlabel_size", value = svalue(size_txt), envir = env)
      assign(x = ".strvalidator_plotAT_gui_xlabel_angle", value = svalue(angle_spb), envir = env)
      assign(x = ".strvalidator_plotAT_gui_xlabel_justh", value = svalue(hjust_spb), envir = env)
      assign(x = ".strvalidator_plotAT_gui_xlabel_justv", value = svalue(vjust_spb), envir = env)
      assign(x = ".strvalidator_plotAT_gui_theme", value = svalue(f1_theme_drp), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotAT_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_points_shape", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_points_shape", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_points_alpha", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_points_alpha", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_points_jitter", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_points_jitter", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_y_min", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_axes_y_min", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_y_max", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_axes_y_max", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_x_min", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_axes_x_min", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_x_max", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_axes_x_max", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_axes_scales", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_axes_scales", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_xlabel_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_xlabel_size", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_xlabel_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_xlabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_xlabel_justh", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_xlabel_justh", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_xlabel_justv", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_xlabel_justv", envir = env)
      }
      if (exists(".strvalidator_plotAT_gui_theme", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotAT_gui_theme", envir = env)
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
