################################################################################
# CHANGE LOG (last 20 changes)
# 13.04.2020: Added language support.
# 13.04.2020: Implemented function checkDataset.
# 23.02.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed expanded 'gexpandgroup'.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 16.06.2016: 'Save as' textbox expandable.
# 11.11.2015: Added importFrom ggplot2.
# 29.08.2015: Added importFrom.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 07.08.2014: Fixed boxplot error
#  Error: stat_boxplot requires the following missing aesthetics: x, y
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 28.02.2014: Fixed plot object not saved in '.gPlot'.
# 20.01.2014: Changed 'saveImage_gui' for 'ggsave_gui'.
# 19.11.2013: Added distribution plot.
# 28.10.2013: First version.

#' @title Plot Capillary Balance
#'
#' @description
#' GUI simplifying the creation of plots from capillary balance data.
#'
#' @details Select a dataset to plot from the drop-down menu.
#' Plot capillary balance as a dotplot, boxplot or as a distribution.
#' Automatic plot titles can be replaced by custom titles.
#' A name for the result is automatically suggested.
#' The resulting plot can be saved as either a plot object or as an image.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help str
#' @importFrom ggplot2 ggplot aes_string facet_grid geom_point geom_line labs
#'  geom_boxplot stat_boxplot theme geom_density coord_cartesian
#'
#' @seealso \url{http://docs.ggplot2.org/current/} for details on plot settings.

plotCapillary_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

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
  strWinTitle <- "Plot capillary balance"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblRows <- "rows"
  strFrmOptions <- "Options"
  strChkOverride <- "Override automatic titles"
  strLblTitlePlot <- "Plot title:"
  strLblTitleSub <- "Sub title:"
  strLblTitleX <- "X title:"
  strLblTitleY <- "Y title:"
  strExpPoints <- "Data points"
  strLblShape <- "Shape:"
  strLblAlpha <- "Alpha:"
  strExpAxes <- "Axes"
  strLblLimitY <- "Limit Y axis (min-max)"
  strFrmPlot <- "Plot capillary balance data"
  strBtnDotplot <- "Dotplot"
  strBtnBoxplot <- "Boxplot"
  strBtnDistribution <- "Distribution"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitleDotplot <- "Mean peak height grouped by capillary for"
  strLblMainTitleBoxplot <- "Mean peak height by capillary for"
  strLblMainTitleDistribution <- "Mean peak height for"
  strLblMainSubTitleDotplot <- "[dotted blue line indicate global mean, red line indicate median per capillary]"
  strLblXTitleInjection <- "Injection"
  strLblXTitleCapillary <- "Capillary"
  strLblXTitleDensity <- "Density"
  strLblYTitleMean <- "Mean peak height (RFU)"
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

    strtmp <- dtStrings["strLblRows"]$value
    strLblRows <- ifelse(is.na(strtmp), strLblRows, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkOverride"]$value
    strChkOverride <- ifelse(is.na(strtmp), strChkOverride, strtmp)

    strtmp <- dtStrings["strLblTitlePlot"]$value
    strLblTitlePlot <- ifelse(is.na(strtmp), strLblTitlePlot, strtmp)

    strtmp <- dtStrings["strLblTitleSub"]$value
    strLblTitleSub <- ifelse(is.na(strtmp), strLblTitleSub, strtmp)

    strtmp <- dtStrings["strLblTitleX"]$value
    strLblTitleX <- ifelse(is.na(strtmp), strLblTitleX, strtmp)

    strtmp <- dtStrings["strLblTitleY"]$value
    strLblTitleY <- ifelse(is.na(strtmp), strLblTitleY, strtmp)

    strtmp <- dtStrings["strExpPoints"]$value
    strExpPoints <- ifelse(is.na(strtmp), strExpPoints, strtmp)

    strtmp <- dtStrings["strLblShape"]$value
    strLblShape <- ifelse(is.na(strtmp), strLblShape, strtmp)

    strtmp <- dtStrings["strLblAlpha"]$value
    strLblAlpha <- ifelse(is.na(strtmp), strLblAlpha, strtmp)

    strtmp <- dtStrings["strExpAxes"]$value
    strExpAxes <- ifelse(is.na(strtmp), strExpAxes, strtmp)

    strtmp <- dtStrings["strLblLimitY"]$value
    strLblLimitY <- ifelse(is.na(strtmp), strLblLimitY, strtmp)

    strtmp <- dtStrings["strFrmPlot"]$value
    strFrmPlot <- ifelse(is.na(strtmp), strFrmPlot, strtmp)

    strtmp <- dtStrings["strBtnDotplot"]$value
    strBtnDotplot <- ifelse(is.na(strtmp), strBtnDotplot, strtmp)

    strtmp <- dtStrings["strBtnBoxplot"]$value
    strBtnBoxplot <- ifelse(is.na(strtmp), strBtnBoxplot, strtmp)

    strtmp <- dtStrings["strBtnDistribution"]$value
    strBtnDistribution <- ifelse(is.na(strtmp), strBtnDistribution, strtmp)

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

    strtmp <- dtStrings["strLblMainTitleDotplot"]$value
    strLblMainTitleDotplot <- ifelse(is.na(strtmp), strLblMainTitleDotplot, strtmp)

    strtmp <- dtStrings["strLblMainTitleBoxplot"]$value
    strLblMainTitleBoxplot <- ifelse(is.na(strtmp), strLblMainTitleBoxplot, strtmp)

    strtmp <- dtStrings["strLblMainTitleDistribution"]$value
    strLblMainTitleDistribution <- ifelse(is.na(strtmp), strLblMainTitleDistribution, strtmp)

    strtmp <- dtStrings["strLblMainSubTitleDotplot"]$value
    strLblMainSubTitleDotplot <- ifelse(is.na(strtmp), strLblMainSubTitleDotplot, strtmp)

    strtmp <- dtStrings["strLblXTitleInjection"]$value
    strLblXTitleInjection <- ifelse(is.na(strtmp), strLblXTitleInjection, strtmp)

    strtmp <- dtStrings["strLblXTitleCapillary"]$value
    strLblXTitleCapillary <- ifelse(is.na(strtmp), strLblXTitleCapillary, strtmp)

    strtmp <- dtStrings["strLblXTitleDensity"]$value
    strLblXTitleDensity <- ifelse(is.na(strtmp), strLblXTitleDensity, strtmp)

    strtmp <- dtStrings["strLblYTitleMean"]$value
    strLblYTitleMean <- ifelse(is.na(strtmp), strLblYTitleMean, strtmp)

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
    text = paste(" (0 ", strLblRows, ")", sep = ""),
    container = f0
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c(
      "Instrument", "Instrument.ID", "Run",
      "Mean.Height", "SQ", "Injection", "Capillary", "Well"
    )
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")

      svalue(f0_samples_lbl) <- paste(" (",
        nrow(.gData), " ", strLblRows, ")",
        sep = ""
      )

      # Enable buttons.
      enabled(plot_dot_btn) <- TRUE
      enabled(plot_box_btn) <- TRUE
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste(" (0 ", strLblRows, ")", sep = "")
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

  glabel(text = strLblTitleSub, container = titles_group, anchor = c(-1, 0))
  sub_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblTitleX, container = titles_group, anchor = c(-1, 0))
  x_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblTitleY, container = titles_group, anchor = c(-1, 0))
  y_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)


  # FRAME 7 ###################################################################

  f7 <- gframe(
    text = strFrmPlot,
    horizontal = TRUE,
    container = gv,
    spacing = 5
  )

  plot_dot_btn <- gbutton(text = strBtnDotplot, container = f7)

  plot_box_btn <- gbutton(text = strBtnBoxplot, container = f7)

  plot_dst_btn <- gbutton(text = strBtnDistribution, container = f7)

  addHandlerChanged(plot_dot_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c(
      "Instrument", "Instrument.ID", "Run", "Mean.Height",
      "Injection", "Capillary", "Well", "Comment"
    )
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(plot_dot_btn) <- FALSE
      .plotBalance(what = "dotplot")
      enabled(plot_dot_btn) <- TRUE
    }
  })

  addHandlerChanged(plot_box_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c(
      "Instrument", "Instrument.ID", "Run", "Mean.Height",
      "Injection", "Capillary", "Well", "Comment"
    )
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(plot_box_btn) <- FALSE
      .plotBalance(what = "boxplot")
      enabled(plot_box_btn) <- TRUE
    }
  })

  addHandlerChanged(plot_dst_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c(
      "Instrument", "Instrument.ID", "Run", "Mean.Height",
      "Injection", "Capillary", "Well", "Comment"
    )
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(plot_dst_btn) <- FALSE
      .plotBalance(what = "dstplot")
      enabled(plot_dst_btn) <- TRUE
    }
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
  grid2[1, 2] <- e2_shape_spb <- gspinbutton(
    from = 0, to = 25,
    by = 1, value = 18,
    container = grid2
  )

  grid2[1, 3] <- glabel(text = strLblAlpha, container = grid2)
  grid2[1, 4] <- e2_alpha_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = 1,
    container = grid2
  )

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
  grid3[2, 1] <- e3_y_min_edt <- gedit(text = "", width = 5, container = grid3)
  grid3[2, 2] <- e3_y_max_edt <- gedit(text = "", width = 5, container = grid3)

  # FRAME 4 ###################################################################

  #   e4 <- gexpandgroup(text="X labels",
  #                      horizontal=FALSE,
  #                      container = f1)
  #
  #   grid4 <- glayout(container = e4)
  #
  #   grid4[1,1] <- glabel(text="Text size (pts):", container=grid4)
  #   grid4[1,2] <- e4_size_edt <- gedit(text="8", width=4, container=grid4)

  # FUNCTIONS #################################################################


  .plotBalance <- function(what) {

    # Get values.
    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_sub_title <- svalue(sub_title_edt)
    val_x_title <- svalue(x_title_edt)
    val_y_title <- svalue(y_title_edt)
    val_shape <- as.numeric(svalue(e2_shape_spb))
    val_alpha <- as.numeric(svalue(e2_alpha_spb))
    val_ymin <- as.numeric(svalue(e3_y_min_edt))
    val_ymax <- as.numeric(svalue(e3_y_max_edt))
    # val_size <- as.numeric(svalue(e4_size_edt))

    if (debug) {
      print("val_titles")
      print(val_titles)
      print("val_title")
      print(val_title)
      print("val_sub_title")
      print(val_sub_title)
      print("val_x_title")
      print(val_x_title)
      print("val_y_title")
      print(val_y_title)
      print("val_shape")
      print(val_shape)
      print("val_alpha")
      print(val_alpha)
      print("val_ymin")
      print(val_ymin)
      print("val_ymax")
      print(val_ymax)
      #       print("val_size")
      #       print(val_size)
      print("str(.gData)")
      print(str(.gData))
    }

    if (!is.na(.gData) && !is.null(.gData)) {

      # Remove NA.
      if (any(is.na(.gData$Mean.Height))) {
        n0 <- nrow(.gData)
        .gData <- .gData[!is.na(.gData$Mean.Height), ]
        n1 <- nrow(.gData)
        print(paste(n0 - n1, "NA rows removed from 'Mean.Height'."))
      }

      # Height must be numeric.
      if (!is.numeric(.gData$Mean.Height)) {
        .gData$Mean.Height <- as.numeric(.gData$Mean.Height)
        message("'Mean.Height' not numeric, converting to numeric!")
      }

      # Capillary must be numeric.
      if (!is.numeric(.gData$Capillary)) {
        .gData$Capillary <- as.numeric(.gData$Capillary)
        message("'Capillary' not numeric, converting to numeric!")
      }

      # Injection must be numeric.
      if (!is.numeric(.gData$Injection)) {
        .gData$Injection <- as.numeric(.gData$Injection)
        message("'Injection' not numeric, converting to numeric!")
      }

      if (debug) {
        print("Before plot: str(.gData)")
        print(str(.gData))
      }


      # Select what to plot.
      if (what == "dotplot") {
        if (val_titles) {
          mainTitle <- val_title
          subTitle <- val_sub_title
          xTitle <- val_x_title
          yTitle <- val_y_title
        } else {
          mainTitle <- paste(strLblMainTitleDotplot, " ",
            unique(.gData$Instrument.ID),
            " (", unique(.gData$Instrument), ")",
            sep = ""
          )
          subTitle <- strLblMainSubTitleDotplot
          xTitle <- strLblXTitleInjection
          yTitle <- strLblYTitleMean
        }

        # POINT (best for few replicates (injections))
        gp <- ggplot(.gData, aes_string(x = "Injection", y = "Mean.Height", group = "Capillary"))
        gp <- gp + facet_grid(. ~ Capillary, space = "fixed", scales = "fixed")
        gp <- gp + geom_point(shape = val_shape, alpha = val_alpha)
        gp <- gp + geom_line()
        gp <- gp + geom_line(y = mean(.gData$Mean.Height, na.rm = TRUE), lty = "dotted", color = "blue")
        gp <- gp + geom_line(stat = "hline", yintercept = "median", color = "red")
        gp <- gp + labs(
          title = paste(mainTitle, "\n", subTitle),
          x = xTitle, y = yTitle
        )
      } else if (what == "boxplot") {
        if (val_titles) {
          mainTitle <- val_title
          subTitle <- val_sub_title
          xTitle <- val_x_title
          yTitle <- val_y_title
        } else {
          mainTitle <- paste(strLblMainTitleBoxplot, " ",
            unique(.gData$Instrument.ID),
            " (", unique(.gData$Instrument), ")",
            sep = ""
          )
          subTitle <- ""
          xTitle <- strLblXTitleCapillary
          yTitle <- strLblYTitleMean
        }

        # BOXPLOT (best suited for many replicates (injections))
        # Note: aes allows the first two arguments to be unnamed and are assumed to be x and y (respectively)
        # Note: aes_string does not have this shortcut, and so all arguments must be named.
        .gData$Capillary <- factor(.gData$Capillary)
        gp <- ggplot(.gData, aes_string(x = "Capillary", y = "Mean.Height"))
        gp <- gp + geom_boxplot()
        gp <- gp + stat_boxplot(geom = "errorbar")
        gp <- gp + labs(
          title = paste(mainTitle, "\n", subTitle),
          x = xTitle, y = yTitle
        )
        gp <- gp + theme(legend.position = "none")
      } else if (what == "dstplot") {
        if (val_titles) {
          mainTitle <- val_title
          subTitle <- val_sub_title
          xTitle <- val_x_title
          yTitle <- val_y_title
        } else {
          mainTitle <- paste(strLblMainTitleDistribution, " ",
            unique(.gData$Instrument.ID),
            " (", unique(.gData$Instrument), ")",
            sep = ""
          )
          subTitle <- ""
          xTitle <- strLblYTitleMean
          yTitle <- strLblXTitleDensity
        }

        # DISTRIBUTION PLOT.
        gp <- ggplot(.gData, aes_string(x = "Mean.Height"))
        gp <- gp + geom_density()
        gp <- gp + geom_point(y = 0, aes_string(colour = "Run"), shape = 108, size = 10, alpha = val_alpha)
        gp <- gp + labs(
          title = paste(mainTitle, "\n", subTitle),
          x = xTitle, y = yTitle
        )
      }

      # Restrict y axis.
      if (!is.na(val_ymin) && !is.na(val_ymax)) {
        # Zoom in.
        gp <- gp + coord_cartesian(ylim = c(val_ymin, val_ymax))
      }

      # Apply theme.
      #         gp <- gp + theme(axis.text.x=element_text(size=val_size))

      # plot.
      print(gp)

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
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
      if (exists(".strvalidator_plotCapillary_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotCapillary_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotCapillary_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotCapillary_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(titles_chk) <- get(".strvalidator_plotCapillary_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_sub_title", envir = env, inherits = FALSE)) {
        svalue(sub_title_edt) <- get(".strvalidator_plotCapillary_gui_sub_title", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotCapillary_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_plotCapillary_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_points_shape", envir = env, inherits = FALSE)) {
        svalue(e2_shape_spb) <- get(".strvalidator_plotCapillary_gui_points_shape", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_points_alpha", envir = env, inherits = FALSE)) {
        svalue(e2_alpha_spb) <- get(".strvalidator_plotCapillary_gui_points_alpha", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_axes_y_min", envir = env, inherits = FALSE)) {
        svalue(e3_y_min_edt) <- get(".strvalidator_plotCapillary_gui_axes_y_min", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_axes_y_max", envir = env, inherits = FALSE)) {
        svalue(e3_y_max_edt) <- get(".strvalidator_plotCapillary_gui_axes_y_max", envir = env)
      }
      #       if(exists(".strvalidator_plotCapillary_gui_xlabel_size", envir=env, inherits = FALSE)){
      #         svalue(e4_size_edt) <- get(".strvalidator_plotCapillary_gui_xlabel_size", envir=env)
      #       }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotCapillary_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_title_chk", value = svalue(titles_chk), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_sub_title", value = svalue(sub_title_edt), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_points_shape", value = svalue(e2_shape_spb), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_points_alpha", value = svalue(e2_alpha_spb), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_axes_y_min", value = svalue(e3_y_min_edt), envir = env)
      assign(x = ".strvalidator_plotCapillary_gui_axes_y_max", value = svalue(e3_y_max_edt), envir = env)
      #      assign(x=".strvalidator_plotCapillary_gui_xlabel_size", value=svalue(e4_size_edt), envir=env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotCapillary_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_sub_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_sub_title", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_points_shape", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_points_shape", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_points_alpha", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_points_alpha", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_axes_y_min", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_axes_y_min", envir = env)
      }
      if (exists(".strvalidator_plotCapillary_gui_axes_y_max", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotCapillary_gui_axes_y_max", envir = env)
      }
      #       if(exists(".strvalidator_plotCapillary_gui_xlabel_size", envir=env, inherits = FALSE)){
      #         remove(".strvalidator_plotCapillary_gui_xlabel_size", envir = env)
      #       }

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
