################################################################################
# CHANGE LOG (last 20 changes)
# 20.06.2023: Fixed Error in !is.null(val_data) && !is.na(val_data) in coercion to 'logical(1)
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 19.04.2020: Added language support.
# 24.02.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 20.07.2018: Fixed blank drop-down menues after selecting a dataset.
# 20.07.2017: Removed unused argument 'spacing' from 'gexpandgroup'.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed expanded 'gexpandgroup'.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 06.03.2017: Removed dead web page references.
# 01.11.2016: 'Probability' on y axis changed to 'Density'.
# 11.10.2016: Added controls for x and y axis range.
# 11.10.2016: No longer required to select a group if column Group is present.
# 19.09.2016: Fixed factor levels in group drop-down after change in calculatePeaks.
# 27.06.2016: Fixed 'bins' not saved.
# 16.06.2016: Implemented log option and number of bins.
# 19.05.2016: Fixed update of drop-down and information when selecting a new dataset.

#' @title Plot Distribution
#'
#' @description
#' GUI simplifying the creation of distribution plots.
#'
#' @details Plot the distribution of data as cumulative distribution function,
#' probability density function, or count. First select a dataset, then select
#' a group (in column 'Group' if any), finally select a column to plot the distribution of.
#' It is possible to overlay a boxplot and to plot logarithms.
#' Various smoothing kernels and bandwidths can be specified.
#' The bandwidth or the number of bins can be specified for the histogram.
#' Automatic plot titles can be replaced by custom titles.
#' A name for the result is automatically suggested.
#' The resulting plot can be saved as either a plot object or as an image.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help str head
#' @importFrom ggplot2 ggplot aes_string stat_ecdf geom_density ggplot_build
#'  geom_boxplot geom_segment geom_point labs theme_gray theme_bw
#'  theme_linedraw theme_light theme_dark theme_minimal theme_classic
#'  theme_void geom_histogram
#'
#' @return TRUE
#'
#' @seealso \code{\link{log}}, \code{\link{geom_density}}


plotDistribution_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gPlot <- NULL
  .palette <- c(
    "Set1", "Set2", "Set3", "Accent", "Dark2",
    "Paired", "Pastel1", "Pastel2"
  )
  # Qualitative palette, do not imply magnitude differences between legend
  # classes, and hues are used to create the primary visual differences
  # between classes. Qualitative schemes are best suited to representing
  # nominal or categorical data.

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Plot distribution"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strLblGroup <- "Group:"
  strLblColumn <- "Column:"
  strDrpDataset <- "<Select dataset>"
  strDrpGroup <- "<Select group>"
  strDrpColumn <- "<Select column>"
  strLblRows <- "rows"
  strFrmOptions <- "Options"
  strChkOverride <- "Override automatic titles"
  strLblTitlePlot <- "Plot title:"
  strLblTitleX <- "X title:"
  strLblTitleY <- "Y title:"
  strLblTheme <- "Plot theme:"
  strChkBoxplot <- "Overlay boxplot"
  strLblWidth <- "Width of boxplot:"
  strChkLog <- "Transform to logarithms."
  strLblBase <- "Base:"
  strTipBase <- "Default is the natural logarithm, approximately 2.718282. Other common values are 10 for the common logarithm, and 2 for binary logarithms."
  strExpDistribution <- "Distribution function"
  strLblSmoothing <- "Smoothing kernel:"
  strLblBandwidth <- "Adjust bandwidth:"
  strExpHistogram <- "Distribution function"
  strLblBinwidth <- "Adjust bindwidth:"
  strTipBin <- "The width of the bins. The default is to use 30 bins, that cover the range of the data. You should always override this value, exploring multiple widths to find the best to illustrate your data. Leave empty to use 'bins'."
  strLblBins <- "Number of bins:"
  strTipBins <- "Overridden by binwidth. Defaults to 30."
  strExpAxes <- "Axes"
  strLblNB <- "NB! Must provide both min and max value."
  strLblLimitY <- "Limit Y axis (min-max)"
  strLblLimitX <- "Limit X axis (min-max)"
  strFrmPlot <- "Plot distribution"
  strBtnCDF <- "CDF"
  strTipCDF <- "Cumulative density function"
  strBtnPDF <- "PDF"
  strTipPDF <- "Probability density function"
  strBtnHistogram <- "Histogram"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitleCDF <- "Cumulative density function"
  strLblMainTitlePDF <- "Probability density function"
  strLblMainTitleHistogram <- "Histogram"
  strLblYTitleDensity <- "Density"
  strLblYTitleCount <- "Count"
  strLblXTitleHeight <- "Peak height (RFU)"
  strLblXTitleSize <- "Fragment size (bp)"
  strLblXTitleDataPoint <- "Data point"
  strLblObservations <- "observations"
  strMsgColumn <- "A data column must be specified!"
  strMsgNotDf <- "Data set must be a data.frame!"
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

    strtmp <- dtStrings["strLblGroup"]$value
    strLblGroup <- ifelse(is.na(strtmp), strLblGroup, strtmp)

    strtmp <- dtStrings["strLblColumn"]$value
    strLblColumn <- ifelse(is.na(strtmp), strLblColumn, strtmp)

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strtmp <- dtStrings["strDrpGroup"]$value
    strDrpGroup <- ifelse(is.na(strtmp), strDrpGroup, strtmp)

    strtmp <- dtStrings["strDrpColumn"]$value
    strDrpColumn <- ifelse(is.na(strtmp), strDrpColumn, strtmp)

    strtmp <- dtStrings["strLblRows"]$value
    strLblRows <- ifelse(is.na(strtmp), strLblRows, strtmp)

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

    strtmp <- dtStrings["strChkBoxplot"]$value
    strChkBoxplot <- ifelse(is.na(strtmp), strChkBoxplot, strtmp)

    strtmp <- dtStrings["strLblWidth"]$value
    strLblWidth <- ifelse(is.na(strtmp), strLblWidth, strtmp)

    strtmp <- dtStrings["strChkLog"]$value
    strChkLog <- ifelse(is.na(strtmp), strChkLog, strtmp)

    strtmp <- dtStrings["strLblBase"]$value
    strLblBase <- ifelse(is.na(strtmp), strLblBase, strtmp)

    strtmp <- dtStrings["strTipBase"]$value
    strTipBase <- ifelse(is.na(strtmp), strTipBase, strtmp)

    strtmp <- dtStrings["strExpDistribution"]$value
    strExpDistribution <- ifelse(is.na(strtmp), strExpDistribution, strtmp)

    strtmp <- dtStrings["strLblSmoothing"]$value
    strLblSmoothing <- ifelse(is.na(strtmp), strLblSmoothing, strtmp)

    strtmp <- dtStrings["strLblBandwidth"]$value
    strLblBandwidth <- ifelse(is.na(strtmp), strLblBandwidth, strtmp)

    strtmp <- dtStrings["strExpHistogram"]$value
    strExpHistogram <- ifelse(is.na(strtmp), strExpHistogram, strtmp)

    strtmp <- dtStrings["strLblBinwidth"]$value
    strLblBinwidth <- ifelse(is.na(strtmp), strLblBinwidth, strtmp)

    strtmp <- dtStrings["strTipBin"]$value
    strTipBin <- ifelse(is.na(strtmp), strTipBin, strtmp)

    strtmp <- dtStrings["strLblBins"]$value
    strLblBins <- ifelse(is.na(strtmp), strLblBins, strtmp)

    strtmp <- dtStrings["strTipBins"]$value
    strTipBins <- ifelse(is.na(strtmp), strTipBins, strtmp)

    strtmp <- dtStrings["strExpAxes"]$value
    strExpAxes <- ifelse(is.na(strtmp), strExpAxes, strtmp)

    strtmp <- dtStrings["strLblNB"]$value
    strLblNB <- ifelse(is.na(strtmp), strLblNB, strtmp)

    strtmp <- dtStrings["strLblLimitY"]$value
    strLblLimitY <- ifelse(is.na(strtmp), strLblLimitY, strtmp)

    strtmp <- dtStrings["strLblLimitX"]$value
    strLblLimitX <- ifelse(is.na(strtmp), strLblLimitX, strtmp)

    strtmp <- dtStrings["strFrmPlot"]$value
    strFrmPlot <- ifelse(is.na(strtmp), strFrmPlot, strtmp)

    strtmp <- dtStrings["strBtnCDF"]$value
    strBtnCDF <- ifelse(is.na(strtmp), strBtnCDF, strtmp)

    strtmp <- dtStrings["strTipCDF"]$value
    strTipCDF <- ifelse(is.na(strtmp), strTipCDF, strtmp)

    strtmp <- dtStrings["strBtnPDF"]$value
    strBtnPDF <- ifelse(is.na(strtmp), strBtnPDF, strtmp)

    strtmp <- dtStrings["strTipPDF"]$value
    strTipPDF <- ifelse(is.na(strtmp), strTipPDF, strtmp)

    strtmp <- dtStrings["strBtnHistogram"]$value
    strBtnHistogram <- ifelse(is.na(strtmp), strBtnHistogram, strtmp)

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

    strtmp <- dtStrings["strLblMainTitleCDF"]$value
    strLblMainTitleCDF <- ifelse(is.na(strtmp), strLblMainTitleCDF, strtmp)

    strtmp <- dtStrings["strLblMainTitlePDF"]$value
    strLblMainTitlePDF <- ifelse(is.na(strtmp), strLblMainTitlePDF, strtmp)

    strtmp <- dtStrings["strLblMainTitleHistogram"]$value
    strLblMainTitleHistogram <- ifelse(is.na(strtmp), strLblMainTitleHistogram, strtmp)

    strtmp <- dtStrings["strLblYTitleDensity"]$value
    strLblYTitleDensity <- ifelse(is.na(strtmp), strLblYTitleDensity, strtmp)

    strtmp <- dtStrings["strLblYTitleCount"]$value
    strLblYTitleCount <- ifelse(is.na(strtmp), strLblYTitleCount, strtmp)

    strtmp <- dtStrings["strLblXTitleHeight"]$value
    strLblXTitleHeight <- ifelse(is.na(strtmp), strLblXTitleHeight, strtmp)

    strtmp <- dtStrings["strLblXTitleSize"]$value
    strLblXTitleSize <- ifelse(is.na(strtmp), strLblXTitleSize, strtmp)

    strtmp <- dtStrings["strLblXTitleDataPoint"]$value
    strLblXTitleDataPoint <- ifelse(is.na(strtmp), strLblXTitleDataPoint, strtmp)

    strtmp <- dtStrings["strLblObservations"]$value
    strLblObservations <- ifelse(is.na(strtmp), strLblObservations, strtmp)

    strtmp <- dtStrings["strMsgColumn"]$value
    strMsgColumn <- ifelse(is.na(strtmp), strMsgColumn, strtmp)

    strtmp <- dtStrings["strMsgNotDf"]$value
    strMsgNotDf <- ifelse(is.na(strtmp), strMsgNotDf, strtmp)

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

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g0)

  samples_lbl <- glabel(
    text = paste(" 0 ", strLblRows),
    container = f0g0
  )

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
    container = f0g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # Group ---------------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblGroup, container = f0g1)

  rows_lbl <- glabel(
    text = paste(" 0 ", strLblRows, sep = ""),
    container = f0g1
  )

  group_drp <- gcombobox(
    items = strDrpGroup,
    selected = 1, container = f0g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # Column --------------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblColumn, container = f0g2)

  column_drp <- gcombobox(
    items = strDrpColumn,
    selected = 1, container = f0g2,
    ellipsize = "none", expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- NULL
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      # Refresh column in drop lists.
      .refresh_column_drp()

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")

      # Get number of observations.
      svalue(samples_lbl) <- paste(" ", nrow(.gData), " ", strLblRows,
        sep = ""
      )

      # Get number of observations in subset.
      val <- svalue(group_drp)
      if (length(val) > 0 && val %in% names(.gData)) {
        rows <- nrow(.gData[.gData$Group == val, ])
        svalue(rows_lbl) <- paste(" ", rows, " ", strLblRows,
          sep = ""
        )
      } else {
        svalue(rows_lbl) <- paste(" 0 ", strLblRows,
          sep = ""
        )
      }

      # Enable buttons.
      enabled(f7_ecdf_btn) <- TRUE
      enabled(f7_pdf_btn) <- TRUE
      enabled(f7_histogram_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(samples_lbl) <- paste(" 0 ", strLblRows,
        sep = ""
      )
    }
  })

  addHandlerChanged(group_drp, handler = function(h, ...) {
    val <- svalue(group_drp)
    rows <- nrow(.gData[.gData$Group == val, ])

    # Update number of observations.
    svalue(rows_lbl) <- paste(" ", rows, " ", strLblRows,
      sep = ""
    )
  })

  addHandlerChanged(column_drp, handler = function(h, ...) {
    # Enable buttons.
    enabled(f7_ecdf_btn) <- TRUE
    enabled(f7_pdf_btn) <- TRUE
    enabled(f7_histogram_btn) <- TRUE
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 1,
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


  f1g2 <- glayout(container = f1, spacing = 1)
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

  # Boxplot.
  f1g3 <- glayout(container = f1, spacing = 1)
  f1g3[1, 1] <- f1_box_chk <- gcheckbox(
    text = strChkBoxplot, checked = TRUE,
    container = f1g3
  )
  f1g3[1, 2] <- glabel(text = strLblWidth, container = f1g3)
  f1g3[1, 3] <- f1_width_spn <- gspinbutton(
    from = 0, to = 1, by = 0.01, value = 0.25,
    container = f1g3
  )

  addHandlerChanged(f1_box_chk, handler = function(h, ...) {
    .updateGui()
  })

  # Transformation.
  f1g3[2, 1] <- f1_log_chk <- gcheckbox(text = strChkLog, container = f1g3)
  f1g3[2, 2] <- glabel(text = strLblBase, container = f1g3)
  f1g3[2, 3] <- f1_base_edt <- gedit(text = "2.718282", width = 8, container = f1g3)
  tooltip(f1_base_edt) <- strTipBase

  addHandlerChanged(f1_log_chk, handler = function(h, ...) {
    .updateGui()
  })

  f1e2 <- gexpandgroup(
    text = strExpDistribution,
    horizontal = FALSE, container = f1
  )

  # Start collapsed.
  visible(f1e2) <- FALSE

  f1g4 <- glayout(container = f1e2, spacing = 1)

  f1_kernel <- c(
    "gaussian", "rectangular", "triangular", "epanechnikov",
    "biweight", "cosine", "optcosine"
  )
  f1g4[1, 1] <- glabel(text = strLblSmoothing, container = f1g4)
  f1g4[1, 2] <- f1_kernel_drp <- gcombobox(
    items = f1_kernel,
    selected = 1, container = f1g4,
    ellipsize = "none"
  )

  f1_adjust <- c(4, 2, 1, 0.5, 0.25)
  f1g4[2, 1] <- glabel(text = strLblBandwidth, container = f1g4)
  f1g4[2, 2] <- f1_adjustbw_cbo <- gcombobox(
    items = f1_adjust,
    selected = 3, editable = TRUE,
    container = f1g4, ellipsize = "none"
  )

  f1e3 <- gexpandgroup(
    text = strExpHistogram,
    horizontal = FALSE, container = f1
  )

  # Start collapsed.
  visible(f1e3) <- FALSE

  f1g5 <- glayout(container = f1e3, spacing = 1)

  f1g5[1, 1] <- glabel(text = strLblBinwidth, container = f1g5)
  f1g5[1, 2] <- f1_binwidth_edt <- gedit(text = "", width = 6, container = f1g5)
  tooltip(f1_binwidth_edt) <- strTipBin
  f1g5[2, 1] <- glabel(text = strLblBins, container = f1g5)
  f1g5[2, 2] <- f1_bins_edt <- gedit(text = "30", width = 6, container = f1g5)
  tooltip(f1_bins_edt) <- strTipBins

  addHandlerKeystroke(f1_binwidth_edt, handler = function(h, ...) {
    .updateGui()
  })

  addHandlerChanged(f1_binwidth_edt, handler = function(h, ...) {
    .updateGui()
  })


  f1e4 <- gexpandgroup(text = strExpAxes, horizontal = FALSE, container = f1)

  # Start collapsed.
  visible(f1e4) <- FALSE

  #  f1g6 <- gframe(text = "", horizontal = FALSE, container = f1e4)

  glabel(
    text = strLblNB,
    anchor = c(-1, 0), container = f1e4
  )

  f1g6 <- glayout(container = f1e4, spacing = 1)
  f1g6[1, 1:2] <- glabel(text = strLblLimitY, container = f1g6)
  f1g6[2, 1] <- f1g6_y_min_edt <- gedit(text = "", width = 5, container = f1g6)
  f1g6[2, 2] <- f1g6_y_max_edt <- gedit(text = "", width = 5, container = f1g6)

  f1g6[3, 1:2] <- glabel(text = strLblLimitX, container = f1g6)
  f1g6[4, 1] <- f1g6_x_min_edt <- gedit(text = "", width = 5, container = f1g6)
  f1g6[4, 2] <- f1g6_x_max_edt <- gedit(text = "", width = 5, container = f1g6)

  # FRAME 7 ###################################################################

  f7 <- gframe(
    text = strFrmPlot,
    horizontal = TRUE,
    container = gv
  )

  f7_ecdf_btn <- gbutton(text = strBtnCDF, container = f7)
  tooltip(f7_ecdf_btn) <- strTipCDF

  addHandlerChanged(f7_ecdf_btn, handler = function(h, ...) {
    val_column <- svalue(column_drp)

    if (val_column == strDrpColumn) {
      gmessage(
        msg = strMsgColumn,
        title = strMsgTitleError,
        icon = "error"
      )
    } else {
      enabled(f7_ecdf_btn) <- FALSE
      .plot(how = "cdf")
      enabled(f7_ecdf_btn) <- TRUE
    }
  })

  f7_pdf_btn <- gbutton(text = strBtnPDF, container = f7)
  tooltip(f7_pdf_btn) <- strTipPDF

  addHandlerChanged(f7_pdf_btn, handler = function(h, ...) {
    val_column <- svalue(column_drp)

    if (val_column == strDrpColumn) {
      gmessage(
        msg = strMsgColumn,
        title = strMsgTitleError,
        icon = "error"
      )
    } else {
      enabled(f7_pdf_btn) <- FALSE
      .plot(how = "pdf")
      enabled(f7_pdf_btn) <- TRUE
    }
  })

  f7_histogram_btn <- gbutton(text = strBtnHistogram, container = f7)

  addHandlerChanged(f7_histogram_btn, handler = function(h, ...) {
    val_column <- svalue(column_drp)

    if (val_column == strDrpColumn) {
      gmessage(
        msg = strMsgColumn,
        title = strMsgTitleError,
        icon = "error"
      )
    } else {
      enabled(f7_histogram_btn) <- FALSE
      .plot(how = "histogram")
      enabled(f7_histogram_btn) <- TRUE
    }
  })

  # FRAME 5 ###################################################################

  f5 <- gframe(
    text = strFrmSave,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strLblSave, container = f5)

  f5_save_edt <- gedit(container = f5, expand = TRUE, fill = TRUE)

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

  .plot <- function(how) {
    # Get values.
    val_data <- .gData
    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_x_title <- svalue(x_title_edt)
    val_y_title <- svalue(y_title_edt)
    val_theme <- svalue(f1_theme_drp)
    val_group <- svalue(group_drp)
    val_column <- svalue(column_drp)
    val_kernel <- svalue(f1_kernel_drp)
    val_adjustbw <- as.numeric(svalue(f1_adjustbw_cbo))
    val_boxplot <- svalue(f1_box_chk)
    val_width <- svalue(f1_width_spn)
    val_binwidth <- as.numeric(svalue(f1_binwidth_edt))
    val_log <- svalue(f1_log_chk)
    val_base <- as.numeric(svalue(f1_base_edt))
    val_bins <- as.numeric(svalue(f1_bins_edt))
    val_xmin <- as.numeric(svalue(f1g6_x_min_edt))
    val_xmax <- as.numeric(svalue(f1g6_x_max_edt))
    val_ymin <- as.numeric(svalue(f1g6_y_min_edt))
    val_ymax <- as.numeric(svalue(f1g6_y_max_edt))

    if (debug) {
      print("val_titles")
      print(val_titles)
      print("val_title")
      print(val_title)
      print("val_x_title")
      print(val_x_title)
      print("val_y_title")
      print(val_y_title)
      print("val_kernel")
      print(val_kernel)
      print("val_column")
      print(val_column)
      print("str(val_data)")
      print(str(val_data))
      print("val_adjustbw")
      print(val_adjustbw)
      print("val_binwidth")
      print(val_binwidth)
      print("val_log")
      print(val_log)
      print("val_base")
      print(val_base)
      print("val_bins")
      print(val_bins)
      print("val_xmin")
      print(val_xmin)
      print("val_xmax")
      print(val_xmax)
      print("val_ymin")
      print(val_ymin)
      print("val_ymax")
      print(val_ymax)
    }

    # Check if data.
    if (is.data.frame(val_data)) {
      if (debug) {
        print("Before plot: str(val_data)")
        print(str(val_data))
        print(head(val_data))
      }

      # Get number of observations.
      nb <- nrow(val_data)

      # Get data for selected group.
      if ("Group" %in% names(val_data)) {
        if (val_group != strDrpGroup) {
          # Store nb of observations.
          nb0 <- nb

          # Subset according to group.
          val_data <- val_data[val_data$Group == val_group, ]

          # Update number of observations.
          nb <- nrow(val_data)

          # Show message.
          message(paste("Subset group = '", val_group,
            "', removed ", nb0 - nb, " rows.",
            sep = ""
          ))
        }

        message("No group selected.")
      }

      # Convert to numeric.
      if (!is.numeric(val_data[, val_column])) {
        val_data[, val_column] <- as.numeric(val_data[, val_column])
        message(paste(val_column, " converted to numeric."))
      }

      # Transform data.
      if (val_log) {
        # Calculate the logarithms using specified base.
        val_data[, val_column] <- log(val_data[, val_column], base = val_base)
        message("Transformed values to logarithms of base ", val_base, ".")
      }

      if (debug) {
        print("After subsetting (val_data)")
        print(str(val_data))
        print(head(val_data))
      }

      # Remove NA's
      if (any(is.na(val_data[, val_column]))) {
        # Store nb of observations.
        nb0 <- nb

        # Update number of observations.
        nb <- nrow(val_data[!is.na(val_data[val_column]), ])

        # Show message.
        message(paste("Removed ", nb0 - nb, " NA rows.", sep = ""))

        if (debug) {
          print("After subsetting (val_data)")
          print(str(val_data))
          print(head(val_data))
        }
      }

      # Create titles.
      if (val_titles) {
        if (debug) {
          print("Custom titles")
        }

        mainTitle <- val_title
        xTitle <- val_x_title
        yTitle <- val_y_title
      } else {
        if (debug) {
          print("Default titles")
        }

        # Different titles.
        if (how == "cdf") {
          mainTitle <- paste(strLblMainTitleCDF, " (",
            nb, " ", strLblObservations, ")",
            sep = ""
          )

          yTitle <- strLblYTitleDensity
        } else if (how == "pdf") {
          mainTitle <- paste(strLblMainTitlePDF, " (",
            nb, " ", strLblObservations, ")",
            sep = ""
          )

          yTitle <- strLblYTitleDensity
        } else if (how == "histogram") {
          mainTitle <- paste(strLblMainTitleHistogram, " (",
            nb, " ", strLblObservations, ")",
            sep = ""
          )

          yTitle <- strLblYTitleCount
        } else {
          warning(paste("how=", how, "not implemented for titles!"))
        }

        # Different X axis depending on chosen column.
        if (val_column == "Height") {
          xTitle <- strLblXTitleHeight
        } else if (val_column == "Size") {
          xTitle <- strLblXTitleSize
        } else if (val_column == "Data.Point") {
          xTitle <- strLblXTitleDataPoint
        } else {
          xTitle <- val_column
        }
      }

      # Create plots.
      if (how == "cdf") {
        if (debug) {
          print("Create cdf plot")
        }

        # ECDP
        gp <- ggplot(data = val_data, aes_string(x = val_column))
        gp <- gp + stat_ecdf()
      } else if (how == "pdf") {
        if (debug) {
          print("Create pdf plot")
        }

        gp <- ggplot(data = val_data, aes_string(x = val_column))
        gp <- gp + geom_density(aes_string(x = val_column), kernel = val_kernel, adjust = val_adjustbw)
      } else if (how == "histogram") {
        if (debug) {
          print("Create Histogram")
        }

        # Create plot.
        gp <- ggplot(data = val_data, aes_string(x = val_column))

        # Binwidth overrides bins.
        if (!is.na(val_binwidth)) {
          gp <- gp + geom_histogram(binwidth = val_binwidth)
        } else {
          if (is.na(val_bins)) {
            val_bins <- 30
          }
          gp <- gp + geom_histogram(bins = val_bins)
        }
      } else {
        warning(paste("how=", how, "not implemented for plots!"))
      }

      if (debug) {
        print("Plot created")
      }

      # Overlay boxplot.
      if (val_boxplot) {
        if (debug) {
          print("Overlay boxplot")
        }

        # Extract information from plot:
        gb <- ggplot_build(gp)
        ywidth <- max(gb$data[[1]]$y, na.rm = TRUE) * (val_width / 2)
        ymean <- max(gb$data[[1]]$y, na.rm = TRUE) / 2

        # Create a normal boxplot.
        gbox <- ggplot(data = val_data, aes_string(x = 1, y = val_column))
        gbox <- gbox + geom_boxplot()

        # Extract information from boxplot.
        gb <- ggplot_build(gbox)
        xmax <- gb$data[[1]]$ymax
        xmin <- gb$data[[1]]$ymin
        left <- gb$data[[1]]$lower
        middle <- gb$data[[1]]$middle
        right <- gb$data[[1]]$upper
        dots <- unlist(gb$data[[1]]$outliers)

        val_box <- data.frame(
          xmin = xmin, xmax = xmax,
          ymin = ymean - ywidth, ymax = ymean + ywidth, ymean = ymean,
          left = left, middle = middle, right = right
        )


        if (debug) {
          print("val_box")
          print(val_box)
          print("dots")
          print(dots)
        }

        # Manually overlay a boxplot:
        # Add box.
        # Should work...
        #        gp <- gp + geom_polygon(data=val_box, aes_string(x = c("left","left","right","right"),
        #                                                         y = c("ymin","ymax","ymax","ymin")),
        #                                color=1, alpha=0)
        #        gp <- gp + geom_rect(data=val_box, aes_string(xmin = "left", xmax="right",
        #                                                      ymin = "ymin", ymax="ymax"),
        #                             color=1, alpha=0)
        # Add top.
        gp <- gp + geom_segment(data = val_box, aes_string(
          x = "left", y = "ymax",
          xend = "right", yend = "ymax"
        ))

        # Add bottom.
        gp <- gp + geom_segment(data = val_box, aes_string(
          x = "left", y = "ymin",
          xend = "right", yend = "ymin"
        ))

        # Add left.
        gp <- gp + geom_segment(data = val_box, aes_string(
          x = "left", y = "ymin",
          xend = "left", yend = "ymax"
        ))

        # Add right.
        gp <- gp + geom_segment(data = val_box, aes_string(
          x = "right", y = "ymin",
          xend = "right", yend = "ymax"
        ))

        # Add median.
        gp <- gp + geom_segment(data = val_box, aes_string(
          x = "middle", y = "ymin",
          xend = "middle", yend = "ymax"
        ))
        # Add whiskers.
        gp <- gp + geom_segment(data = val_box, aes_string(
          x = "xmin", y = "ymean",
          xend = "left", yend = "ymean"
        ))
        gp <- gp + geom_segment(data = val_box, aes_string(
          x = "xmax", y = "ymean",
          xend = "right", yend = "ymean"
        ))
        # Add outliers.
        out <- data.frame(x = dots, y = rep(ymean, length(dots)))
        gp <- gp + geom_point(data = out, aes_string(x = "x", y = "y"))

        if (debug) {
          print("Boxplot created")
        }
      } # End if boxplot.

      # Add titles.
      gp <- gp + labs(title = mainTitle, x = xTitle, y = yTitle, fill = NULL)

      # Apply theme.
      gp <- gp + eval(parse(text = val_theme))

      # Limit y axis.
      if (!is.na(val_ymin) && !is.na(val_ymax)) {
        val_y <- c(val_ymin, val_ymax)
      } else {
        val_y <- NULL
      }

      # Limit x axis.
      if (!is.na(val_xmin) && !is.na(val_xmax)) {
        val_x <- c(val_xmin, val_xmax)
      } else {
        val_x <- NULL
      }

      # Check if any axis limits.
      if (any(!is.null(val_y), !is.null(val_x))) {
        message(
          "Zoom plot xmin/xmax,ymin/ymax:",
          paste(val_x, collapse = "/"), ",",
          paste(val_y, collapse = "/")
        )

        # Zoom in without dropping observations.
        gp <- gp + coord_cartesian(xlim = val_x, ylim = val_y)
      }

      # plot.
      print(gp)

      # Store in global variable.
      .gPlot <<- gp

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
      enabled(f5_save_btn) <- TRUE
    } else {
      gmessage(
        msg = strMsgNotDf,
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

    # Boxplot dependent widgets.
    val <- svalue(f1_box_chk)
    if (val) {
      enabled(f1_width_spn) <- TRUE
    } else {
      enabled(f1_width_spn) <- FALSE
    }

    # Log dependent widgets.
    val <- svalue(f1_log_chk)
    if (val) {
      enabled(f1_base_edt) <- TRUE
    } else {
      enabled(f1_base_edt) <- FALSE
    }

    # Binwidth dependent widgets.
    val <- svalue(f1_binwidth_edt)
    if (nchar(val) == 0) {
      enabled(f1_bins_edt) <- TRUE
    } else {
      enabled(f1_bins_edt) <- FALSE
    }
  }

  .refresh_column_drp <- function() {
    if (debug) {
      print("Refresh group and column dropdown")
    }

    # Get data frames in global workspace.
    groups <- unique(as.character(.gData$Group))
    columns <- names(.gData)

    if (length(groups) > 0) {
      blockHandler(group_drp)

      # Populate drop list.
      group_drp[] <- c(strDrpGroup, groups)
      svalue(group_drp, index = TRUE) <- 1

      unblockHandler(group_drp)
    } else {
      blockHandler(group_drp)

      # Reset drop list and select first item.
      group_drp[] <- c(strDrpGroup)
      svalue(group_drp, index = TRUE) <- 1

      unblockHandler(group_drp)
    }


    if (!is.null(columns)) {
      blockHandler(column_drp)

      # Populate drop list.
      column_drp[] <- c(strDrpColumn, columns)
      svalue(column_drp, index = TRUE) <- 1

      unblockHandler(column_drp)
    } else {
      blockHandler(column_drp)

      # Reset drop list and select first item.
      column_drp[] <- c(strDrpColumn)
      svalue(column_drp, index = TRUE) <- 1

      unblockHandler(column_drp)
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
      if (exists(".strvalidator_plotDistribution_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotDistribution_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotDistribution_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotDistribution_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(titles_chk) <- get(".strvalidator_plotDistribution_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotDistribution_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_plotDistribution_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_box", envir = env, inherits = FALSE)) {
        svalue(f1_box_chk) <- get(".strvalidator_plotDistribution_gui_box", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_kernel", envir = env, inherits = FALSE)) {
        svalue(f1_kernel_drp) <- get(".strvalidator_plotDistribution_gui_kernel", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_theme", envir = env, inherits = FALSE)) {
        svalue(f1_theme_drp) <- get(".strvalidator_plotDistribution_gui_theme", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_width", envir = env, inherits = FALSE)) {
        svalue(f1_width_spn) <- get(".strvalidator_plotDistribution_gui_width", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_binwidth", envir = env, inherits = FALSE)) {
        svalue(f1_binwidth_edt) <- get(".strvalidator_plotDistribution_gui_binwidth", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_bins", envir = env, inherits = FALSE)) {
        svalue(f1_bins_edt) <- get(".strvalidator_plotDistribution_gui_bins", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_log", envir = env, inherits = FALSE)) {
        svalue(f1_log_chk) <- get(".strvalidator_plotDistribution_gui_log", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_base", envir = env, inherits = FALSE)) {
        svalue(f1_base_edt) <- get(".strvalidator_plotDistribution_gui_base", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotDistribution_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_title_chk", value = svalue(titles_chk), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_box", value = svalue(f1_box_chk), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_kernel", value = svalue(f1_kernel_drp), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_theme", value = svalue(f1_theme_drp), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_width", value = svalue(f1_width_spn), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_binwidth", value = svalue(f1_binwidth_edt), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_bins", value = svalue(f1_bins_edt), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_log", value = svalue(f1_log_chk), envir = env)
      assign(x = ".strvalidator_plotDistribution_gui_base", value = svalue(f1_base_edt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotDistribution_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_box", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_box", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_kernel", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_kernel", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_theme", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_theme", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_width", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_width", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_binwidth", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_binwidth", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_binws", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_binws", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_log", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_log", envir = env)
      }
      if (exists(".strvalidator_plotDistribution_gui_base", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotDistribution_gui_base", envir = env)
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

  # Update widget status.
  .updateGui()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
