################################################################################
# CHANGE LOG (last 20 changes)
# 20.06.2023: Fixed Error in facet_wrap, `ncol` must be a whole number or `NULL`, not an integer vector.
# 20.06.2023: Fixed Error in !is.null(val_data) && !is.na(val_data) in coercion to 'logical(1)
# 10.09.2022: Compacted the gui. Removed destroy workaround.
# 26.04.2020: Added language support.
# 08.09.2019: Added option to override labels and titles.
# 24.02.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 23.07.2018: Made 'Save as' textbox expandable.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed expanded 'gexpandgroup'.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 04.10.2016: Added '(data point)' to y title when mean is from data points.
# 06.01.2016: Fixed theme methods not found and added more themes.
# 11.11.2015: Added importFrom grid unit.c, gridExtra arrangeGrob, and ggplot2.
# 11.11.2015: Added more themes.
# 29.08.2015: Added importFrom.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 12.09.2014: Filter rows with Allele=NA (Fixes issue #6).

#' @title Plot Precision
#'
#' @description
#' GUI simplifying the creation of plots from precision data.
#'
#' @details Plot precision data for size, height, or data point as dotplot or
#' boxplot. Plot per marker or all in one. Use the mean value or the allele
#' designation as x-axis labels.
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
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid unit textGrob grid.newpage grid.draw unit.c
# @importFrom gtable gtable_add_grob gtable
#' @importFrom utils help str head
#' @importFrom grDevices palette
#' @importFrom stats as.formula
#' @importFrom ggplot2 ggplot geom_point aes_string geom_boxplot facet_grid
#'  facet_wrap coord_cartesian labs xlab ylab theme element_text element_blank
#'  ggplotGrob theme_gray theme_bw theme_linedraw theme_light theme_dark
#'  theme_minimal theme_classic theme_void
#'
#' @seealso \url{https://ggplot2.tidyverse.org/} for details on plot settings.
#'

plotPrecision_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables and constants.
  .gData <- NULL
  .gDataName <- NULL
  .gPlot <- NULL
  .theme <- c(
    "theme_grey()", "theme_bw()", "theme_linedraw()",
    "theme_light()", "theme_dark()", "theme_minimal()",
    "theme_classic()", "theme_void()"
  )
  .scales <- c("fixed", "free_x", "free_y", "free")


  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Plot precision"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset and kit"
  strLblDataset <- "Precision dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblKit <- "Kit:"
  strFrmOptions <- "Options"
  strChkOverride <- "Override automatic titles"
  strExpTitles <- "Titles"
  strLblTitlePlot <- "Plot title:"
  strLblTitleX <- "X title:"
  strLblTitleY <- "Y title:"
  strLblNB <- "NB! Title size, angle, vjust, and hjust does not always work as expected with different number of facets per row."
  strLblTitleSettings <- "Main title size, angle, vjust, hjust:"
  strLblXTitleSettings <- "X title size, angle, vjust, hjust:"
  strLblYTitleSettings <- "Y title size, angle, vjust, hjust:"
  strChkByMarker <- "Plot by marker"
  strLblXAxis <- "X axis:"
  strRadMean <- "Mean"
  strRadAllele <- "Allele"
  strLblTheme <- "Plot theme:"
  strExpPoints <- "Data points"
  strLblShape <- "Shape:"
  strLblAlpha <- "Alpha:"
  strLblColor <- "Colour:"
  strExpAxes <- "Axes"
  strLblLimitY <- "Limit Y axis (min-max)"
  strLblLimitX <- "Limit X axis (min-max)"
  strLblScales <- "Scales:"
  strChkOverrideLabels <- "Override default x/y/facet labels"
  strExpXLabels <- "X labels"
  strLblSize <- "Text size (pts):"
  strLblAngle <- "Angle:"
  strLblJustification <- "Justification (v/h):"
  strExpYLabels <- "Y labels"
  strExpFacets <- "Facets"
  strLblSizeX <- "Text size X (pts):"
  strLblSizeY <- "Text size Y (pts):"
  strFrmPlotDot <- "Plot precision data as dotplot"
  strBtnSize <- "Size"
  strBtnHeight <- "Height"
  strBtnPoint <- "Data point"
  strFrmPlotBox <- "Plot precision data as boxplot"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitleSize <- "Allele size range"
  strLblMainTitleHeight <- "Allele height range"
  strLblMainTitlePoint <- "Allele data point range"
  strLblYTitleSize <- "Deviation from mean (bp)"
  strLblYTitleHeight <- "Deviation from mean (RFU)"
  strLblYTitlePoint <- "Deviation from mean (data point)"
  strLblXTitleSize <- "Mean size in basepairs (bp)"
  strLblXTitleHeight <- "Mean height in relative fluorescent units (RFU)"
  strLblXTitlePoints <- "Mean scan number in data points"
  strLblXTitleAllele <- "Allele"
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

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strtmp <- dtStrings["strLblKit"]$value
    strLblKit <- ifelse(is.na(strtmp), strLblKit, strtmp)

    strtmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strtmp <- dtStrings["strChkOverride"]$value
    strChkOverride <- ifelse(is.na(strtmp), strChkOverride, strtmp)

    strtmp <- dtStrings["strExpTitles"]$value
    strExpTitles <- ifelse(is.na(strtmp), strExpTitles, strtmp)

    strtmp <- dtStrings["strLblTitlePlot"]$value
    strLblTitlePlot <- ifelse(is.na(strtmp), strLblTitlePlot, strtmp)

    strtmp <- dtStrings["strLblTitleX"]$value
    strLblTitleX <- ifelse(is.na(strtmp), strLblTitleX, strtmp)

    strtmp <- dtStrings["strLblTitleY"]$value
    strLblTitleY <- ifelse(is.na(strtmp), strLblTitleY, strtmp)

    strtmp <- dtStrings["strLblNB"]$value
    strLblNB <- ifelse(is.na(strtmp), strLblNB, strtmp)

    strtmp <- dtStrings["strLblTitleSettings"]$value
    strLblTitleSettings <- ifelse(is.na(strtmp), strLblTitleSettings, strtmp)

    strtmp <- dtStrings["strLblXTitleSettings"]$value
    strLblXTitleSettings <- ifelse(is.na(strtmp), strLblXTitleSettings, strtmp)

    strtmp <- dtStrings["strLblYTitleSettings"]$value
    strLblYTitleSettings <- ifelse(is.na(strtmp), strLblYTitleSettings, strtmp)

    strtmp <- dtStrings["strChkByMarker"]$value
    strChkByMarker <- ifelse(is.na(strtmp), strChkByMarker, strtmp)

    strtmp <- dtStrings["strLblXAxis"]$value
    strLblXAxis <- ifelse(is.na(strtmp), strLblXAxis, strtmp)

    strtmp <- dtStrings["strRadMean"]$value
    strRadMean <- ifelse(is.na(strtmp), strRadMean, strtmp)

    strtmp <- dtStrings["strRadAllele"]$value
    strRadAllele <- ifelse(is.na(strtmp), strRadAllele, strtmp)

    strtmp <- dtStrings["strLblTheme"]$value
    strLblTheme <- ifelse(is.na(strtmp), strLblTheme, strtmp)

    strtmp <- dtStrings["strExpPoints"]$value
    strExpPoints <- ifelse(is.na(strtmp), strExpPoints, strtmp)

    strtmp <- dtStrings["strLblShape"]$value
    strLblShape <- ifelse(is.na(strtmp), strLblShape, strtmp)

    strtmp <- dtStrings["strLblAlpha"]$value
    strLblAlpha <- ifelse(is.na(strtmp), strLblAlpha, strtmp)

    strtmp <- dtStrings["strLblColor"]$value
    strLblColor <- ifelse(is.na(strtmp), strLblColor, strtmp)

    strtmp <- dtStrings["strExpAxes"]$value
    strExpAxes <- ifelse(is.na(strtmp), strExpAxes, strtmp)

    strtmp <- dtStrings["strLblLimitY"]$value
    strLblLimitY <- ifelse(is.na(strtmp), strLblLimitY, strtmp)

    strtmp <- dtStrings["strLblLimitX"]$value
    strLblLimitX <- ifelse(is.na(strtmp), strLblLimitX, strtmp)

    strtmp <- dtStrings["strLblScales"]$value
    strLblScales <- ifelse(is.na(strtmp), strLblScales, strtmp)

    strtmp <- dtStrings["strChkOverrideLabels"]$value
    strChkOverrideLabels <- ifelse(is.na(strtmp), strChkOverrideLabels, strtmp)

    strtmp <- dtStrings["strExpXLabels"]$value
    strExpXLabels <- ifelse(is.na(strtmp), strExpXLabels, strtmp)

    strtmp <- dtStrings["strLblSize"]$value
    strLblSize <- ifelse(is.na(strtmp), strLblSize, strtmp)

    strtmp <- dtStrings["strLblAngle"]$value
    strLblAngle <- ifelse(is.na(strtmp), strLblAngle, strtmp)

    strtmp <- dtStrings["strLblJustification"]$value
    strLblJustification <- ifelse(is.na(strtmp), strLblJustification, strtmp)

    strtmp <- dtStrings["strExpYLabels"]$value
    strExpYLabels <- ifelse(is.na(strtmp), strExpYLabels, strtmp)

    strtmp <- dtStrings["strExpFacets"]$value
    strExpFacets <- ifelse(is.na(strtmp), strExpFacets, strtmp)

    strtmp <- dtStrings["strLblSizeX"]$value
    strLblSizeX <- ifelse(is.na(strtmp), strLblSizeX, strtmp)

    strtmp <- dtStrings["strLblSizeY"]$value
    strLblSizeY <- ifelse(is.na(strtmp), strLblSizeY, strtmp)

    strtmp <- dtStrings["strFrmPlotDot"]$value
    strFrmPlotDot <- ifelse(is.na(strtmp), strFrmPlotDot, strtmp)

    strtmp <- dtStrings["strBtnSize"]$value
    strBtnSize <- ifelse(is.na(strtmp), strBtnSize, strtmp)

    strtmp <- dtStrings["strBtnHeight"]$value
    strBtnHeight <- ifelse(is.na(strtmp), strBtnHeight, strtmp)

    strtmp <- dtStrings["strBtnPoint"]$value
    strBtnPoint <- ifelse(is.na(strtmp), strBtnPoint, strtmp)

    strtmp <- dtStrings["strFrmPlotBox"]$value
    strFrmPlotBox <- ifelse(is.na(strtmp), strFrmPlotBox, strtmp)

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

    strtmp <- dtStrings["strLblMainTitleSize"]$value
    strLblMainTitleSize <- ifelse(is.na(strtmp), strLblMainTitleSize, strtmp)

    strtmp <- dtStrings["strLblMainTitleHeight"]$value
    strLblMainTitleHeight <- ifelse(is.na(strtmp), strLblMainTitleHeight, strtmp)

    strtmp <- dtStrings["strLblMainTitlePoint"]$value
    strLblMainTitlePoint <- ifelse(is.na(strtmp), strLblMainTitlePoint, strtmp)

    strtmp <- dtStrings["strLblYTitleSize"]$value
    strLblYTitleSize <- ifelse(is.na(strtmp), strLblYTitleSize, strtmp)

    strtmp <- dtStrings["strLblYTitleHeight"]$value
    strLblYTitleHeight <- ifelse(is.na(strtmp), strLblYTitleHeight, strtmp)

    strtmp <- dtStrings["strLblYTitlePoint"]$value
    strLblYTitlePoint <- ifelse(is.na(strtmp), strLblYTitlePoint, strtmp)

    strtmp <- dtStrings["strLblXTitleSize"]$value
    strLblXTitleSize <- ifelse(is.na(strtmp), strLblXTitleSize, strtmp)

    strtmp <- dtStrings["strLblXTitleHeight"]$value
    strLblXTitleHeight <- ifelse(is.na(strtmp), strLblXTitleHeight, strtmp)

    strtmp <- dtStrings["strLblXTitlePoints"]$value
    strLblXTitlePoints <- ifelse(is.na(strtmp), strLblXTitlePoints, strtmp)

    strtmp <- dtStrings["strLblXTitleAllele"]$value
    strLblXTitleAllele <- ifelse(is.na(strtmp), strLblXTitleAllele, strtmp)

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

  # Vertical main group.
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

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = g0)

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
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # Kit -----------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblKit, container = g1)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      string = "OL", stringcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex

      # Enable buttons.
      enabled(f7_size_btn) <- TRUE
      enabled(f7_height_btn) <- TRUE
      enabled(f7_data_btn) <- TRUE
      enabled(f8_size_btn) <- TRUE
      enabled(f8_height_btn) <- TRUE
      enabled(f8_data_btn) <- TRUE
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
    }
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

  # Titles --------------------------------------------------------------------

  titles_group <- gexpandgroup(
    text = strExpTitles,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(titles_group) <- FALSE

  glabel(text = strLblTitlePlot, container = titles_group, anchor = c(-1, 0))
  title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblTitleX, container = titles_group, anchor = c(-1, 0))
  x_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblTitleY, container = titles_group, anchor = c(-1, 0))
  y_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strLblNB, anchor = c(-1, 0), container = titles_group)

  titles_layout <- glayout(container = titles_group)

  title_lbl <- glabel(
    text = strLblTitleSettings,
    anchor = c(-1, 0), container = titles_layout
  )
  title_size_txt <- gedit(
    text = "14", width = 2,
    container = titles_layout
  )
  title_angle_spb <- gspinbutton(
    from = 0, to = 360, by = 1, value = 0,
    container = titles_layout
  )
  title_vjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1, value = 0.5,
    container = titles_layout
  )

  title_hjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1, value = 0.5,
    container = titles_layout
  )

  titles_layout[1, 1] <- title_lbl
  titles_layout[1, 2] <- title_size_txt
  titles_layout[1, 3] <- title_angle_spb
  titles_layout[1, 4] <- title_vjust_spb
  titles_layout[1, 5] <- title_hjust_spb

  x_title_lbl <- glabel(
    text = strLblXTitleSettings,
    anchor = c(-1, 0), container = titles_layout
  )
  x_title_size_txt <- gedit(
    text = "12", width = 2,
    container = titles_layout
  )
  x_title_angle_spb <- gspinbutton(
    from = 0, to = 360, by = 1, value = 0,
    container = titles_layout
  )
  x_title_vjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1, value = 0.5,
    container = titles_layout
  )

  x_title_hjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1, value = 0.5,
    container = titles_layout
  )
  titles_layout[2, 1] <- x_title_lbl
  titles_layout[2, 2] <- x_title_size_txt
  titles_layout[2, 3] <- x_title_angle_spb
  titles_layout[2, 4] <- x_title_vjust_spb
  titles_layout[2, 5] <- x_title_hjust_spb

  y_title_lbl <- glabel(
    text = strLblYTitleSettings,
    anchor = c(-1, 0), container = titles_layout
  )
  y_title_size_txt <- gedit(
    text = "12", width = 2,
    container = titles_layout
  )
  y_title_angle_spb <- gspinbutton(
    from = 0, to = 360, by = 1, value = 90,
    container = titles_layout
  )
  y_title_vjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1, value = 0.5,
    container = titles_layout
  )

  y_title_hjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1, value = 0.5,
    container = titles_layout
  )
  titles_layout[3, 1] <- y_title_lbl
  titles_layout[3, 2] <- y_title_size_txt
  titles_layout[3, 3] <- y_title_angle_spb
  titles_layout[3, 4] <- y_title_vjust_spb
  titles_layout[3, 5] <- y_title_hjust_spb

  # Plot options --------------------------------------------------------------

  f1_facet_chk <- gcheckbox(
    text = strChkByMarker,
    checked = TRUE,
    container = f1
  )

  f1g2 <- glayout(container = f1)
  f1g2[1, 1] <- glabel(text = strLblXAxis, anchor = c(-1, 0), container = f1g2)
  f1g2[1, 2] <- f1_axis_opt <- gradio(
    items = c(strRadMean, strRadAllele),
    selected = 2,
    horizontal = TRUE,
    container = f1g2
  )

  # Theme ---------------------------------------------------------------------

  f1g2[2, 1] <- glabel(text = strLblTheme, anchor = c(-1, 0), container = f1g2)
  f1g2[2, 2] <- f1_theme_drp <- gcombobox(
    items = .theme,
    selected = 1,
    container = f1g2,
    ellipsize = "none"
  )


  # FRAME 7 ###################################################################

  f7 <- gframe(
    text = strFrmPlotDot,
    horizontal = TRUE,
    container = gv
  )

  f7_size_btn <- gbutton(text = strBtnSize, container = f7)

  f7_height_btn <- gbutton(text = strBtnHeight, container = f7)

  f7_data_btn <- gbutton(text = strBtnPoint, container = f7)

  addHandlerChanged(f7_size_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Size")

    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f7_size_btn) <- FALSE
      .plot(what = "Size", how = "dotplot")
      enabled(f7_size_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
      enabled(f5_save_btn) <- TRUE
    }
  })

  addHandlerChanged(f7_height_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Height")

    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f7_height_btn) <- FALSE
      .plot(what = "Height", how = "dotplot")
      enabled(f7_height_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
      enabled(f5_save_btn) <- TRUE
    }
  })

  addHandlerChanged(f7_data_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Data.Point")

    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f7_data_btn) <- FALSE
      .plot(what = "Data.Point", how = "dotplot")
      enabled(f7_data_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
      enabled(f5_save_btn) <- TRUE
    }
  })

  # FRAME 8 ###################################################################

  f8 <- gframe(
    text = strFrmPlotBox,
    horizontal = TRUE,
    container = gv
  )

  f8_size_btn <- gbutton(text = strBtnSize, container = f8)

  f8_height_btn <- gbutton(text = strBtnHeight, container = f8)

  f8_data_btn <- gbutton(text = strBtnPoint, container = f8)

  addHandlerChanged(f8_size_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Size")

    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f8_size_btn) <- FALSE
      .plot(what = "Size", how = "boxplot")
      enabled(f8_size_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
      enabled(f5_save_btn) <- TRUE
    }
  })

  addHandlerChanged(f8_height_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Height")

    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f8_height_btn) <- FALSE
      .plot(what = "Height", how = "boxplot")
      enabled(f8_height_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
      enabled(f5_save_btn) <- TRUE
    }
  })

  addHandlerChanged(f8_data_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Data.Point")

    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(f8_data_btn) <- FALSE
      .plot(what = "Data.Point", how = "boxplot")
      enabled(f8_data_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strBtnSaveObject
      enabled(f5_save_btn) <- TRUE
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
    by = 0.01, value = 0.60,
    container = grid2
  )

  grid2[1, 5] <- glabel(text = strLblColor, container = grid2)
  grid2[1, 6] <- colour_drp <- gcombobox(
    items = c("white", palette()),
    selected = 2,
    container = grid2,
    ellipsize = "none"
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
  grid3[2, 1] <- y_min_txt <- gedit(text = "", width = 5, container = grid3)
  grid3[2, 2] <- y_max_txt <- gedit(text = "", width = 5, container = grid3)

  grid3[3, 1:2] <- glabel(text = strLblLimitX, container = grid3)
  grid3[4, 1] <- x_min_txt <- gedit(text = "", width = 5, container = grid3)
  grid3[4, 2] <- x_max_txt <- gedit(text = "", width = 5, container = grid3)

  grid3[1, 3] <- glabel(text = "    ", container = grid3) # Add some space.

  grid3[1, 4] <- glabel(text = strLblScales, container = grid3)
  grid3[2:4, 4] <- scales_opt <- gradio(
    items = .scales,
    selected = 2,
    horizontal = FALSE,
    container = grid3
  )

  # FRAME 4 ###################################################################

  labels_chk <- gcheckbox(
    text = strChkOverrideLabels,
    checked = FALSE,
    container = f1
  )

  addHandlerChanged(labels_chk, handler = function(h, ...) {

    # Enable buttons.
    .updateGui()
  })

  e4 <- gexpandgroup(
    text = strExpXLabels,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e4) <- FALSE

  grid4 <- glayout(container = e4)

  grid4[1, 1] <- glabel(text = strLblSize, container = grid4)
  grid4[1, 2] <- size_txt <- gedit(text = "8", width = 4, container = grid4)

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

  # FRAME 5 ###################################################################

  e5 <- gexpandgroup(
    text = strExpYLabels,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e5) <- FALSE

  grid5 <- glayout(container = e5)

  grid5[1, 1] <- glabel(text = strLblSize, container = grid5)
  grid5[1, 2] <- size_txt_y <- gedit(text = "8", width = 4, container = grid5)

  grid5[1, 3] <- glabel(text = strLblAngle, container = grid5)
  grid5[1, 4] <- angle_spb_y <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 0,
    container = grid5
  )

  grid5[2, 1] <- glabel(text = strLblJustification, container = grid5)
  grid5[2, 2] <- vjust_spb_y <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 0.5,
    container = grid5
  )

  grid5[2, 3] <- hjust_spb_y <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 0,
    container = grid5
  )

  # FRAME 6 ###################################################################

  e6 <- gexpandgroup(
    text = strExpFacets,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e6) <- FALSE

  grid6 <- glayout(container = e6)

  grid6[1, 1] <- glabel(text = strLblSizeX, container = grid6)
  grid6[1, 2] <- size_txt_sx <- gedit(text = "10", width = 4, container = grid6)

  grid6[1, 3] <- glabel(text = strLblAngle, container = grid6)
  grid6[1, 4] <- angle_spb_sx <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 0,
    container = grid6
  )

  grid6[2, 1] <- glabel(text = strLblSizeY, container = grid6)
  grid6[2, 2] <- size_txt_sy <- gedit(text = "10", width = 4, container = grid6)

  grid6[2, 3] <- glabel(text = strLblAngle, container = grid6)
  grid6[2, 4] <- angle_spb_sy <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 0,
    container = grid6
  )

  # FUNCTIONS #################################################################


  .plot <- function(what, how) {

    # Get values.
    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_title_size <- svalue(title_size_txt)
    val_title_angle <- svalue(title_angle_spb)
    val_title_vjust <- svalue(title_vjust_spb)
    val_title_hjust <- svalue(title_hjust_spb)
    val_xtitle <- svalue(x_title_edt)
    val_xtitle_size <- svalue(x_title_size_txt)
    val_xtitle_angle <- svalue(x_title_angle_spb)
    val_xtitle_vjust <- svalue(x_title_vjust_spb)
    val_xtitle_hjust <- svalue(x_title_hjust_spb)
    val_ytitle <- svalue(y_title_edt)
    val_ytitle_size <- svalue(y_title_size_txt)
    val_ytitle_angle <- svalue(y_title_angle_spb)
    val_ytitle_vjust <- svalue(y_title_vjust_spb)
    val_ytitle_hjust <- svalue(y_title_hjust_spb)
    val_shape <- as.numeric(svalue(shape_spb))
    val_alpha <- as.numeric(svalue(alpha_spb))
    val_ymin <- as.numeric(svalue(y_min_txt))
    val_ymax <- as.numeric(svalue(y_max_txt))
    val_xmin <- as.numeric(svalue(x_min_txt))
    val_xmax <- as.numeric(svalue(x_max_txt))
    val_labels <- svalue(labels_chk)
    val_angle <- as.numeric(svalue(angle_spb))
    val_angle_y <- as.numeric(svalue(angle_spb_y))
    val_angle_sx <- as.numeric(svalue(angle_spb_sx))
    val_angle_sy <- as.numeric(svalue(angle_spb_sy))
    val_vjust <- as.numeric(svalue(vjust_spb))
    val_vjust_y <- as.numeric(svalue(vjust_spb_y))
    val_hjust <- as.numeric(svalue(hjust_spb))
    val_hjust_y <- as.numeric(svalue(hjust_spb_y))
    val_size <- as.numeric(svalue(size_txt))
    val_size_y <- as.numeric(svalue(size_txt_y))
    val_size_sx <- as.numeric(svalue(size_txt_sx))
    val_size_sy <- as.numeric(svalue(size_txt_sy))
    val_scales <- svalue(scales_opt)
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_facet <- svalue(f1_facet_chk)
    val_colour <- svalue(colour_drp)
    val_axis <- svalue(f1_axis_opt)
    val_theme <- svalue(f1_theme_drp)

    if (is.data.frame(val_data)) {
      if (debug) {
        print("BEFORE PLOTTING:")
        print("str(val_data)")
        print(str(val_data))
        print("levels(val_data$Allele)")
        print(levels(val_data$Allele))
        print("levels(val_data$Marker)")
        print(levels(val_data$Marker))
      }

      # Calculate mean and deviation ------------------------------------------

      what.mean <- paste(what, "Mean", sep = ".")

      # Add new column.
      val_data[[what.mean]] <- NA
      val_data[[what]] <- as.numeric(val_data[[what]])

      # Remove NA's
      if (any(is.na(val_data$Allele))) {
        tmp1 <- nrow(val_data)
        val_data <- val_data[!is.na(val_data$Allele), ]
        tmp2 <- nrow(val_data)
        message(paste(tmp1 - tmp2, "NA rows removed!"))
      }

      # Get all markers.
      marker <- unique(val_data$Marker)

      # Loop over all markers.
      for (m in seq(along = marker)) {

        # Select current marker.
        selMarker <- val_data$Marker == marker[m]

        # Get unique alleles.
        allele <- unique(val_data[selMarker, ]$Allele)

        # Loop over all alleles.
        for (a in seq(along = allele)) {

          # Select current allele.
          selAllele <- val_data$Allele == allele[a]

          # Combine selection.
          selection <- selMarker & selAllele

          # Calculate mean and save in dataframe.
          val_data[selection, ][[what.mean]] <- mean(val_data[selection, ][[what]], na.rm = TRUE)
        }
      }

      # Calculate deviation.
      dev <- val_data[[what]] - val_data[[what.mean]]
      val_data <- data.frame(
        Marker = val_data$Marker,
        Allele = val_data$Allele,
        Mean = val_data[[what.mean]],
        Deviation = dev,
        stringsAsFactors = FALSE
      )

      # Make sorted allele factors (use low values for X/Y).
      numericAlleles <- unique(val_data$Allele)
      numericAlleles <- gsub("X", "0.0001", numericAlleles, ignore.case = TRUE)
      numericAlleles <- gsub("Y", "0.0002", numericAlleles, ignore.case = TRUE)
      orderedAlleles <- order(as.numeric(numericAlleles))
      val_data$Allele <- factor(val_data$Allele, levels = unique(val_data$Allele)[orderedAlleles])

      # TODO: NB! although plotting min/max values are very tidy,
      #       it may not be very informative. Include as an option?
      #         # Calculate deviation
      #         neg <- val_data$Size.Min - val_data$Size.Mean
      #         pos <- val_data$Size.Max - val_data$Size.Mean
      #         val_data <- data.frame(Marker=rep(val_data$Marker, 2),
      #                                Mean=val_data$Size.Mean,
      #                                Value=c(neg, pos),
      #                                Deviation=rep(c("Min","Max"), each=nrow(val_data)),
      #                                stringsAsFactors=FALSE)

      if (debug) {
        print("val_data after calculating deviation:")
        print(str(val_data))
        print(head(val_data))
      }

      # End calculate mean and deviation --------------------------------------

      # Call functions.
      # Add color information.
      if (!"Dye" %in% names(val_data)) {
        val_data <- addColor(
          data = val_data, kit = val_kit,
          need = "Dye", debug = debug
        )
        message("'Dye' added to dataset!")
      }

      # Sort by marker in kit
      val_data <- sortMarker(
        data = val_data,
        kit = val_kit,
        add.missing.levels = TRUE
      )


      if (debug) {
        print("AFTER SORT MARKERS:")
        print("str(val_data)")
        print(str(val_data))
        print("levels(val_data$Allele)")
        print(levels(val_data$Allele))
        print("levels(val_data$Marker)")
        print(levels(val_data$Marker))
      }

      # Create titles..
      if (val_titles) {
        mainTitle <- val_title
        xTitle <- val_xtitle
        yTitle <- val_ytitle
      } else {
        if (what == "Size") {
          mainTitle <- strLblMainTitleSize
          yTitle <- strLblYTitleSize

          if (val_axis == "Mean") {
            xTitle <- strLblXTitleSize
          } else if (val_axis == "Allele") {
            xTitle <- strLblXTitleAllele
          } else {
            warning(paste("val_axis=", val_axis, "not implemented!"))
          }
        } else if (what == "Height") {
          mainTitle <- strLblMainTitleHeight

          yTitle <- strLblYTitleHeight

          if (val_axis == "Mean") {
            xTitle <- strLblXTitleHeight
          } else if (val_axis == "Allele") {
            xTitle <- strLblXTitleAllele
          } else {
            warning(paste("val_axis=", val_axis, "not implemented!"))
          }
        } else if (what == "Data.Point") {
          mainTitle <- strLblMainTitlePoint

          yTitle <- strLblYTitlePoint

          if (val_axis == "Mean") {
            xTitle <- strLblXTitlePoints
          } else if (val_axis == "Allele") {
            xTitle <- strLblXTitleAllele
          } else {
            warning(paste("val_axis=", val_axis, "not implemented!"))
          }
        } else {
          warning(paste("what=", what, "not implemented!"))
        }
      }

      # TODO: NB! although plotting min/max values are very tidy,
      #       it may not be very informative. Include as an option?
      #       # Create plot.
      #       gp <- ggplot(val_data, aes_string(x="Mean", y="Value", color="Deviation"),
      #                    shape=val_shape, alpha=val_alpha)
      #       gp <- gp + geom_point()

      # Create plot.
      if (how == "dotplot") {

        # Create dotplot.
        gp <- ggplot(val_data)
        gp <- gp + geom_point(aes_string(x = val_axis, y = "Deviation"),
          alpha = val_alpha, shape = val_shape, colour = val_colour
        )
      } else if (how == "boxplot") {

        # Create boxplot (by allele).
        gp <- ggplot(val_data)
        gp <- gp + geom_boxplot(aes_string(x = val_axis, y = "Deviation"),
          alpha = val_alpha, shape = val_shape, fill = val_colour
        )
      } else {
        warning(paste("how=", how, "not implemented!"))
      }

      # Apply theme.
      gp <- gp + eval(parse(text = val_theme))

      # Default is simple plot (length(val_ncol)==1).
      val_ncol <- 0

      # Facet plot.
      if (val_facet) {

        # Check if 'simple' or 'complex' plotting:
        # Get Marker and Dye column.
        markerDye <- val_data[c("Marker", "Dye")]
        # Extract unique elements.
        uniqueMarkerDye <- markerDye[!duplicated(markerDye), ]
        # Calculate number of unique columns per dye.
        val_ncol <- unique(table(uniqueMarkerDye$Dye))
        
        # Facet plot.
        gp <- gp + facet_grid("Dye ~ Marker")
        # NB! 'facet_wrap' does not seem to support strings.
        #     Use 'as.formula(paste("string1", "string2"))' as a workaround.
        gp <- gp + facet_wrap(as.formula(paste("~ Marker")),
                              drop = FALSE, scales = val_scales
        )
      }

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

      # Titles and legends.
      gp <- gp + labs(title = mainTitle)
      gp <- gp + xlab(xTitle)
      gp <- gp + ylab(yTitle)

      # Override default theme for titles.
      if (val_titles) {
        gp <- gp + theme(
          plot.title = element_text(
            angle = val_title_angle,
            hjust = val_title_hjust,
            vjust = val_title_vjust,
            size = val_title_size
          ), axis.title.x = element_text(
            angle = val_xtitle_angle,
            hjust = val_xtitle_hjust,
            vjust = val_xtitle_vjust,
            size = val_xtitle_size
          ), axis.title.y = element_text(
            angle = val_ytitle_angle,
            hjust = val_ytitle_hjust,
            vjust = val_ytitle_vjust,
            size = val_ytitle_size
          )
        )
      }

      # Override default labels.
      if (val_labels) {
        gp <- gp + theme(
          axis.text.x = element_text(
            angle = val_angle,
            hjust = val_hjust,
            vjust = val_vjust,
            size = val_size
          ), axis.text.y = element_text(
            angle = val_angle_y,
            hjust = val_hjust_y,
            vjust = val_vjust_y,
            size = val_size_y
          ), strip.text.x = element_text(size = val_size_sx, angle = val_angle_sx),
          strip.text.y = element_text(size = val_size_sy, angle = val_angle_sy)
        )
      }

      # Check plot type.
      if (length(val_ncol) == 1) {
        # Simple plot, equal number of facets per subplot.

        if (debug) {
          print(paste(
            "Simple plot, val_ncol:",
            paste(val_ncol, collapse = ", ")
          ))
        }

        # Show plot.
        print(gp)

        # Change save button.
        svalue(f5_save_btn) <- strBtnSaveObject
        enabled(f5_save_btn) <- TRUE
      } else if (length(val_ncol) > 1) {
        # Complex plot, unequal number of facets per subplot.

        if (debug) {
          print(paste(
            "Complex plot, val_ncol:",
            paste(val_ncol, collapse = ", ")
          ))
        }

        # With guide:
        # Extract the legend from the 'simple' plot.
        # guide <- gtable::gtable_filter(ggplotGrob(gp), pattern="guide")

        # Get y max/min to be able to use same scale across plots.
        yMax <- max(val_data$Deviation, na.rm = TRUE)
        yMin <- min(val_data$Deviation, na.rm = TRUE)

        if (debug) {
          print("yMax/yMin:")
          print(yMax)
          print(yMin)
        }

        # Get kit colors and convert to dyes.
        dyes <- unique(getKit(val_kit, what = "Color")$Color)
        dyes <- addColor(dyes, have = "Color", need = "Dye")
        # Number of dyes.
        noDyes <- length(dyes)
        # Number of rows in table object (one per dye + title + x title).
        noRows <- length(dyes) + 2

        # Create table object.
        # With guide:
        # Note: width(1.5 for y-title, and the rest for plots + guides)
        #       height(1.5 for plot title, equal for each plot, and 1.5 for x-title)
        #         g <- gtable::gtable(widths=grid::unit.c(grid::unit(1.5, "lines"),
        #                                                 grid::unit(1, "null"),
        #                                                 sum(guide$widths)),
        #                             heights = grid::unit(c(1.5,rep(1,noDyes),1.5),
        #                                                  c("line", rep("null", noDyes), "line")))

        # Without guide:
        # Note: width(1.5 for y-title, and the rest for plots + margin)
        #       height(1.5 for plot title, equal for each plot, and 1.5 for x-title)
        g <- gtable::gtable(
          widths = grid::unit.c(
            grid::unit(1.5, "lines"),
            grid::unit(1, "null"), grid::unit(1.5, "lines")
          ),
          heights = grid::unit(
            c(1.5, rep(1, noDyes), 1.5),
            c("line", rep("null", noDyes), "line")
          )
        )

        # Override default titles.
        if (val_titles) {
          g <- gtable::gtable_add_grob(g,
            grid::textGrob(mainTitle,
              vjust = val_title_vjust,
              hjust = val_title_hjust,
              rot = val_title_angle
            ),
            t = 1, b = 1, l = 2, r = 2
          )
          g <- gtable::gtable_add_grob(g, grid::textGrob(xTitle,
            vjust = val_xtitle_vjust,
            hjust = val_xtitle_hjust,
            rot = val_xtitle_angle
          ),
          t = noRows, b = noRows, l = 2, r = 2
          )
          g <- gtable::gtable_add_grob(g, grid::textGrob(yTitle,
            vjust = val_ytitle_vjust,
            hjust = val_ytitle_hjust,
            rot = val_ytitle_angle
          ), t = 1, b = noRows, l = 1, r = 1)
        } else {
          # Add default titles.

          g <- gtable::gtable_add_grob(g, grid::textGrob(mainTitle), t = 1, b = 1, l = 2, r = 2)
          g <- gtable::gtable_add_grob(g, grid::textGrob(xTitle), t = noRows, b = noRows, l = 2, r = 2)
          g <- gtable::gtable_add_grob(g, grid::textGrob(yTitle, rot = 90), t = 1, b = noRows, l = 1, r = 1)
        }

        # With guide:
        # Add the legend to the table object.
        # g <- gtable::gtable_add_grob(g,guide , t=1,b=noRows,l=3,r=3)

        if (debug) {
          print("Complex plot area created. Looping over dyes.")
        }

        # Loop over all dyes.
        for (d in seq(along = dyes)) {

          # Create a plot for the current subset.
          if (how == "dotplot") {

            # Create a plot for the current subset.
            gp <- ggplot(subset(val_data, val_data$Dye == dyes[d]))
            gp <- gp + geom_point(aes_string(x = val_axis, y = "Deviation"),
              alpha = val_alpha, shape = val_shape, colour = val_colour
            )
          } else if (how == "boxplot") {

            # Create a plot for the current subset.
            gp <- ggplot(subset(val_data, val_data$Dye == dyes[d]))
            gp <- gp + geom_boxplot(aes_string(x = val_axis, y = "Deviation"),
              alpha = val_alpha, shape = val_shape, fill = val_colour
            )
          } else {
            warning(paste("how=", how, "not implemented!"))
          }

          if (debug) {
            print(paste("Complex plot base pane", d, "created."))
          }

          # Plot settings.
          gp <- gp + facet_grid("Dye ~ Marker", scales = val_scales)

          # Apply theme.
          gp <- gp + eval(parse(text = val_theme))

          # Set margin around each plot. Note: top, right, bottom, left.
          gp <- gp + theme(plot.margin = grid::unit(c(0.25, 0, 0, 0), "lines"))

          # Restrict y axis.
          if (!is.na(val_ymin) && !is.na(val_ymax)) {
            val_y <- c(val_ymin, val_ymax)
          } else {
            # Make scales work on multiple plots.
            if (val_scales != "free" && val_scales != "free_y") {
              val_y <- c(yMin, yMax)
            } else {
              val_y <- NULL
            }
          }
          # Restrict x axis.
          if (!is.na(val_xmin) && !is.na(val_xmax)) {
            val_x <- c(val_xmin, val_xmax)
          } else {
            val_x <- NULL
          }
          # Zoom in without dropping observations.
          gp <- gp + coord_cartesian(xlim = val_x, ylim = val_y)

          # Remove titles, axis labels and legend on current subplot.
          gp <- gp + labs(title = element_blank())
          gp <- gp + theme(axis.title.x = element_blank())

          # Override default labels.
          if (val_labels) {
            gp <- gp + theme(
              axis.text.x = element_text(
                angle = val_angle,
                hjust = val_hjust,
                vjust = val_vjust,
                size = val_size
              ), axis.text.y = element_text(
                angle = val_angle_y,
                hjust = val_hjust_y,
                vjust = val_vjust_y,
                size = val_size_y
              ), strip.text.x = element_text(
                size = val_size_sx,
                angle = val_angle_sx
              ), strip.text.y = element_text(
                size = val_size_sy,
                angle = val_angle_sy
              )
            )
          }

          gp <- gp + theme(axis.title.y = element_blank())

          gp <- gp + theme(legend.position = "none")

          # Add plot panel to table object.
          g <- gtable::gtable_add_grob(g, ggplotGrob(gp),
            t = (d + 1), b = (d + 1), l = 2, r = 2
          )
        }

        # Plot.
        grid::grid.newpage()
        grid::grid.draw(g)

        # This is step 1 in workaround to save 'complex plots':
        # Step 1: http://stackoverflow.com/a/20433318/2173340
        # Step 2: http://stackoverflow.com/a/18407452/2173340
        gp <- gridExtra::arrangeGrob(g)

        # Change save button.
        svalue(f5_save_btn) <- "Save as object"
        enabled(f5_save_btn) <- FALSE
      } else {
        # Not supported!
        stop(paste("Unsupported number of columns:", val_ncol))
      }


      # Store in global variable.
      .gPlot <<- gp
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

    # Override labels.
    val <- svalue(labels_chk)
    if (val) {
      enabled(e4) <- TRUE
      enabled(e5) <- TRUE
      enabled(e6) <- TRUE
    } else {
      enabled(e4) <- FALSE
      enabled(e5) <- FALSE
      enabled(e6) <- FALSE
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
      if (exists(".strvalidator_plotPrecision_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotPrecision_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotPrecision_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(titles_chk) <- get(".strvalidator_plotPrecision_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotPrecision_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotPrecision_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_plotPrecision_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_size", envir = env, inherits = FALSE)) {
        svalue(title_size_txt) <- get(".strvalidator_plotPrecision_gui_title_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title_size", envir = env, inherits = FALSE)) {
        svalue(x_title_size_txt) <- get(".strvalidator_plotPrecision_gui_x_title_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title_size", envir = env, inherits = FALSE)) {
        svalue(y_title_size_txt) <- get(".strvalidator_plotPrecision_gui_y_title_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_angle", envir = env, inherits = FALSE)) {
        svalue(title_angle_spb) <- get(".strvalidator_plotPrecision_gui_title_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title_angle", envir = env, inherits = FALSE)) {
        svalue(x_title_angle_spb) <- get(".strvalidator_plotPrecision_gui_x_title_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title_angle", envir = env, inherits = FALSE)) {
        svalue(y_title_angle_spb) <- get(".strvalidator_plotPrecision_gui_y_title_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_vjust", envir = env, inherits = FALSE)) {
        svalue(title_vjust_spb) <- get(".strvalidator_plotPrecision_gui_title_vjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title_vjust", envir = env, inherits = FALSE)) {
        svalue(x_title_vjust_spb) <- get(".strvalidator_plotPrecision_gui_x_title_vjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title_vjust", envir = env, inherits = FALSE)) {
        svalue(y_title_vjust_spb) <- get(".strvalidator_plotPrecision_gui_y_title_vjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_hjust", envir = env, inherits = FALSE)) {
        svalue(title_hjust_spb) <- get(".strvalidator_plotPrecision_gui_title_hjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title_hjust", envir = env, inherits = FALSE)) {
        svalue(x_title_hjust_spb) <- get(".strvalidator_plotPrecision_gui_x_title_hjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title_hjust", envir = env, inherits = FALSE)) {
        svalue(y_title_hjust_spb) <- get(".strvalidator_plotPrecision_gui_y_title_hjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_points_shape", envir = env, inherits = FALSE)) {
        svalue(shape_spb) <- get(".strvalidator_plotPrecision_gui_points_shape", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_points_alpha", envir = env, inherits = FALSE)) {
        svalue(alpha_spb) <- get(".strvalidator_plotPrecision_gui_points_alpha", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_points_colour", envir = env, inherits = FALSE)) {
        svalue(colour_drp) <- get(".strvalidator_plotPrecision_gui_points_colour", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_y_min", envir = env, inherits = FALSE)) {
        svalue(y_min_txt) <- get(".strvalidator_plotPrecision_gui_axes_y_min", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_y_max", envir = env, inherits = FALSE)) {
        svalue(y_max_txt) <- get(".strvalidator_plotPrecision_gui_axes_y_max", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_x_min", envir = env, inherits = FALSE)) {
        svalue(x_min_txt) <- get(".strvalidator_plotPrecision_gui_axes_x_min", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_x_max", envir = env, inherits = FALSE)) {
        svalue(x_max_txt) <- get(".strvalidator_plotPrecision_gui_axes_x_max", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_scales", envir = env, inherits = FALSE)) {
        svalue(scales_opt) <- get(".strvalidator_plotPrecision_gui_axes_scales", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_labels_chk", envir = env, inherits = FALSE)) {
        svalue(labels_chk) <- get(".strvalidator_plotPrecision_gui_labels_chk", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_xlabel_size", envir = env, inherits = FALSE)) {
        svalue(size_txt) <- get(".strvalidator_plotPrecision_gui_xlabel_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_ylabel_size", envir = env, inherits = FALSE)) {
        svalue(size_txt_y) <- get(".strvalidator_plotPrecision_gui_ylabel_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_sxlabel_size", envir = env, inherits = FALSE)) {
        svalue(size_txt_sx) <- get(".strvalidator_plotPrecision_gui_sxlabel_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_sylabel_size", envir = env, inherits = FALSE)) {
        svalue(size_txt_sy) <- get(".strvalidator_plotPrecision_gui_sylabel_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_xlabel_angle", envir = env, inherits = FALSE)) {
        svalue(angle_spb) <- get(".strvalidator_plotPrecision_gui_xlabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_ylabel_angle", envir = env, inherits = FALSE)) {
        svalue(angle_spb_y) <- get(".strvalidator_plotPrecision_gui_ylabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_sxlabel_angle", envir = env, inherits = FALSE)) {
        svalue(angle_spb_sx) <- get(".strvalidator_plotPrecision_gui_sxlabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_sylabel_angle", envir = env, inherits = FALSE)) {
        svalue(angle_spb_sy) <- get(".strvalidator_plotPrecision_gui_sylabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_xlabel_justh", envir = env, inherits = FALSE)) {
        svalue(hjust_spb) <- get(".strvalidator_plotPrecision_gui_xlabel_justh", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_ylabel_justh", envir = env, inherits = FALSE)) {
        svalue(hjust_spb_y) <- get(".strvalidator_plotPrecision_gui_ylabel_justh", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_xlabel_justv", envir = env, inherits = FALSE)) {
        svalue(vjust_spb) <- get(".strvalidator_plotPrecision_gui_xlabel_justv", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_ylabel_justv", envir = env, inherits = FALSE)) {
        svalue(vjust_spb_y) <- get(".strvalidator_plotPrecision_gui_ylabel_justv", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_facet", envir = env, inherits = FALSE)) {
        svalue(f1_facet_chk) <- get(".strvalidator_plotPrecision_gui_facet", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_theme", envir = env, inherits = FALSE)) {
        svalue(f1_theme_drp) <- get(".strvalidator_plotPrecision_gui_theme", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotPrecision_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_title_chk", value = svalue(titles_chk), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_title_size", value = svalue(title_size_txt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_x_title_size", value = svalue(x_title_size_txt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_y_title_size", value = svalue(y_title_size_txt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_title_angle", value = svalue(title_angle_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_x_title_angle", value = svalue(x_title_angle_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_y_title_angle", value = svalue(y_title_angle_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_title_vjust", value = svalue(title_vjust_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_x_title_vjust", value = svalue(x_title_vjust_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_y_title_vjust", value = svalue(y_title_vjust_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_title_hjust", value = svalue(title_hjust_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_x_title_hjust", value = svalue(x_title_hjust_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_y_title_hjust", value = svalue(y_title_hjust_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_points_shape", value = svalue(shape_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_points_alpha", value = svalue(alpha_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_points_colour", value = svalue(colour_drp), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_axes_y_min", value = svalue(y_min_txt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_axes_y_max", value = svalue(y_max_txt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_axes_x_min", value = svalue(x_min_txt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_axes_x_max", value = svalue(x_max_txt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_axes_scales", value = svalue(scales_opt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_labels_chk", value = svalue(labels_chk), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_xlabel_size", value = svalue(size_txt), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_ylabel_size", value = svalue(size_txt_y), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_sxlabel_size", value = svalue(size_txt_sx), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_sylabel_size", value = svalue(size_txt_sy), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_xlabel_angle", value = svalue(angle_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_ylabel_angle", value = svalue(angle_spb_y), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_sxlabel_angle", value = svalue(angle_spb_sx), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_sylabel_angle", value = svalue(angle_spb_sy), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_xlabel_justh", value = svalue(hjust_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_xlabel_justv", value = svalue(vjust_spb), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_ylabel_justh", value = svalue(hjust_spb_y), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_ylabel_justv", value = svalue(vjust_spb_y), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_facet", value = svalue(f1_facet_chk), envir = env)
      assign(x = ".strvalidator_plotPrecision_gui_theme", value = svalue(f1_theme_drp), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotPrecision_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_title_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_x_title_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_y_title_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_title_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_x_title_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_y_title_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_vjust", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_title_vjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title_vjust", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_x_title_vjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title_vjust", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_y_title_vjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_title_hjust", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_title_hjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_x_title_hjust", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_x_title_hjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_y_title_hjust", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_y_title_hjust", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_points_shape", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_points_shape", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_points_alpha", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_points_alpha", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_points_colour", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_points_colour", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_y_min", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_axes_y_min", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_y_max", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_axes_y_max", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_x_min", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_axes_x_min", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_x_max", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_axes_x_max", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_axes_scales", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_axes_scales", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_labels_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_labels_chk", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_xlabel_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_xlabel_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_ylabel_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_ylabel_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_sxlabel_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_sxlabel_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_sylabel_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_sylabel_size", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_xlabel_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_xlabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_ylabel_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_ylabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_sxlabel_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_sxlabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_sylabel_angle", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_sylabel_angle", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_xlabel_justh", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_xlabel_justh", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_xlabel_justv", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_xlabel_justv", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_ylabel_justh", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_ylabel_justh", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_ylabel_justv", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_ylabel_justv", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_facet", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_facet", envir = env)
      }
      if (exists(".strvalidator_plotPrecision_gui_theme", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPrecision_gui_theme", envir = env)
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
