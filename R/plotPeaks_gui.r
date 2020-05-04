################################################################################
# CHANGE LOG (last 20 changes)
# 26.04.2020: Added language support.
# 24.02.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 18.07.2017: Fixed "Warning: Ignoring unknown aesthetics: ymax".
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 29.04.2016: 'Save as' textbox expandable.
# 11.11.2015: Added importFrom ggplot2.
# 29.08.2015: Added importFrom.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 20.01.2014: Changed 'saveImage_gui' for 'ggsave_gui'.
# 12.01.2014: First version.

#' @title Plot Peaks
#'
#' @description
#' GUI simplifying the creation of plots from result type data.
#'
#' @details Plot result type data. It is possible to customize titles and font
#' size. Data can be plotted as as frequency or proportion. The values can be
#' printed on the plot with custom number of decimals. There are several
#' color palettes to chose from.
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
#' @importFrom plyr count
#' @importFrom utils help str
#' @importFrom ggplot2 ggplot aes_string theme_grey geom_bar scale_fill_brewer
#'  labs geom_text theme
#'

plotPeaks_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

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
  strWinTitle <- "Plot peaks"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strDrpDataset <- "<Select dataset>"
  strLblSamples <- "samples"
  strFrmOptions <- "Options"
  strChkOverride <- "Override automatic titles"
  strLblTitlePlot <- "Plot title:"
  strLblTitleX <- "X title:"
  strLblTitleY <- "Y title:"
  strChkProportion <- "Plot proportion"
  strLblBaseSize <- "Base font size (pts):"
  strLblPalette <- "Color palette:"
  strChkLabels <- "Print value as bar labels"
  strLblLabelDecimals <- "Bar label decimals:"
  strLblLabelSize <- "Bar label font size (pts):"
  strBtnPlot <- "Plot"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitle <- "Analysis of peaks from"
  strLblXTitle <- "Group"
  strLblYTitleProp <- "Proportion"
  strLblYTitleCount <- "Count"
  strMsgNull <- "Data frame is NULL or NA!"
  strMsgTitleError <- "Error"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.na(dtStrings)) {
    # Get language strings, use default if not found.

    strTmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)

    strTmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)

    strTmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strTmp <- dtStrings["strFrmDataset"]$value
    strFrmDataset <- ifelse(is.na(strtmp), strFrmDataset, strtmp)

    strTmp <- dtStrings["strLblDataset"]$value
    strLblDataset <- ifelse(is.na(strtmp), strLblDataset, strtmp)

    strTmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strTmp <- dtStrings["strLblSamples"]$value
    strLblSamples <- ifelse(is.na(strtmp), strLblSamples, strtmp)

    strTmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)

    strTmp <- dtStrings["strChkOverride"]$value
    strChkOverride <- ifelse(is.na(strtmp), strChkOverride, strtmp)

    strTmp <- dtStrings["strLblTitlePlot"]$value
    strLblTitlePlot <- ifelse(is.na(strtmp), strLblTitlePlot, strtmp)

    strTmp <- dtStrings["strLblTitleX"]$value
    strLblTitleX <- ifelse(is.na(strtmp), strLblTitleX, strtmp)

    strTmp <- dtStrings["strLblTitleY"]$value
    strLblTitleY <- ifelse(is.na(strtmp), strLblTitleY, strtmp)

    strTmp <- dtStrings["strChkProportion"]$value
    strChkProportion <- ifelse(is.na(strtmp), strChkProportion, strtmp)

    strTmp <- dtStrings["strLblBaseSize"]$value
    strLblBaseSize <- ifelse(is.na(strtmp), strLblBaseSize, strtmp)

    strTmp <- dtStrings["strLblPalette"]$value
    strLblPalette <- ifelse(is.na(strtmp), strLblPalette, strtmp)

    strTmp <- dtStrings["strChkLabels"]$value
    strChkLabels <- ifelse(is.na(strtmp), strChkLabels, strtmp)

    strTmp <- dtStrings["strLblLabelDecimals"]$value
    strLblLabelDecimals <- ifelse(is.na(strtmp), strLblLabelDecimals, strtmp)

    strTmp <- dtStrings["strLblLabelSize"]$value
    strLblLabelSize <- ifelse(is.na(strtmp), strLblLabelSize, strtmp)

    strTmp <- dtStrings["strBtnPlot"]$value
    strBtnPlot <- ifelse(is.na(strtmp), strBtnPlot, strtmp)

    strTmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)

    strTmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strTmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)

    strTmp <- dtStrings["strBtnSaveObject"]$value
    strBtnSaveObject <- ifelse(is.na(strtmp), strBtnSaveObject, strtmp)

    strTmp <- dtStrings["strBtnSaveImage"]$value
    strBtnSaveImage <- ifelse(is.na(strtmp), strBtnSaveImage, strtmp)

    strTmp <- dtStrings["strBtnObjectSaved"]$value
    strBtnObjectSaved <- ifelse(is.na(strtmp), strBtnObjectSaved, strtmp)

    strTmp <- dtStrings["strLblMainTitle"]$value
    strLblMainTitle <- ifelse(is.na(strtmp), strLblMainTitle, strtmp)

    strTmp <- dtStrings["strLblXTitle"]$value
    strLblXTitle <- ifelse(is.na(strtmp), strLblXTitle, strtmp)

    strTmp <- dtStrings["strLblYTitleProp"]$value
    strLblYTitleProp <- ifelse(is.na(strtmp), strLblYTitleProp, strtmp)

    strTmp <- dtStrings["strLblYTitleCount"]$value
    strLblYTitleCount <- ifelse(is.na(strtmp), strLblYTitleCount, strtmp)

    strTmp <- dtStrings["strMsgNull"]$value
    strMsgNull <- ifelse(is.na(strtmp), strMsgNull, strtmp)

    strTmp <- dtStrings["strMsgTitleError"]$value
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
    text = paste(" (0 ", strLblSamples, ")", sep = ""),
    container = f0
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Peaks", "Group", "Id")
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
        length(unique(.gData$Id)),
        " ", strLblSamples, ")",
        sep = ""
      )

      # Enable buttons.
      enabled(plot_btn) <- TRUE
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- paste(" (0 ", strLblSamples, ")", sep = "")
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


  f1_prop_chk <- gcheckbox(
    text = strChkProportion,
    checked = TRUE,
    container = f1
  )

  grid2 <- glayout(container = f1, spacing = 1)
  grid2[1, 1] <- glabel(text = strLblBaseSize, container = grid2)
  grid2[1, 2] <- f1_base_size_edt <- gedit(text = "18", width = 4, container = grid2)

  grid3 <- glayout(container = f1, spacing = 1)
  grid3[1, 1] <- glabel(text = strLblPalette, container = grid3)
  grid3[1, 2] <- f1_palette_drp <- gcombobox(
    items = .palette,
    selected = 1,
    editable = FALSE,
    container = grid3,
    ellipsize = "none"
  )

  grid4 <- glayout(container = f1, spacing = 1)
  grid4[1, 1] <- f1_print_chk <- gcheckbox(
    text = strChkLabels, checked = TRUE,
    container = grid4
  )
  grid4[2, 1] <- glabel(text = strLblLabelDecimals, container = grid4)
  grid4[2, 2] <- f1_decimal_spb <- gspinbutton(
    from = 0, to = 9, by = 1, value = 4,
    container = grid4
  )
  grid4[3, 1] <- glabel(text = strLblLabelSize, container = grid4)
  grid4[3, 2] <- f1_lab_size_edt <- gedit(text = "5", width = 4, container = grid4)

  # FRAME 7 ###################################################################

  plot_btn <- gbutton(text = strBtnPlot, container = gv)

  addHandlerChanged(plot_btn, handler = function(h, ...) {
    enabled(plot_btn) <- FALSE
    .plotBalance()
    enabled(plot_btn) <- TRUE
  })


  # FRAME 5 ###################################################################

  f5 <- gframe(
    text = strFrmSave,
    horizontal = TRUE,
    spacing = 2,
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

  .plotBalance <- function() {

    # Get values.
    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_x_title <- svalue(x_title_edt)
    val_y_title <- svalue(y_title_edt)
    val_base_size <- as.numeric(svalue(f1_base_size_edt))
    val_lab_size <- as.numeric(svalue(f1_lab_size_edt))
    val_palette <- svalue(f1_palette_drp)
    val_decimals <- as.numeric(svalue(f1_decimal_spb))
    val_print <- svalue(f1_print_chk)
    val_prop <- svalue(f1_prop_chk)

    if (debug) {
      print("val_titles")
      print(val_titles)
      print("val_title")
      print(val_title)
      print("val_x_title")
      print(val_x_title)
      print("val_y_title")
      print(val_y_title)
      print("val_base_size")
      print(val_base_size)
      print("val_lab_size")
      print(val_lab_size)
      print("val_palette")
      print(val_palette)
      print("val_decimals")
      print(val_decimals)
      print("val_print")
      print(val_print)
      print("val_prop")
      print(val_prop)
      print("str(.gData)")
      print(str(.gData))
    }

    # Check if data.
    if (!is.na(.gData) && !is.null(.gData)) {
      if (debug) {
        print("Before plot: str(.gData)")
        print(str(.gData))
      }

      # Prepare data.
      # Get one row from each sample for plotting.
      .gData <- .gData[!duplicated(.gData[, "Id"]), ]

      # Create titles.
      if (val_titles) {
        mainTitle <- val_title
        xTitle <- val_x_title
        yTitle <- val_y_title
      } else {
        mainTitle <- paste(strLblMainTitle, nrow(.gData), strLblSamples)
        xTitle <- strLblXTitle
        if (val_prop) {
          yTitle <- strLblYTitleProp
        } else {
          yTitle <- strLblYTitleCount
        }
      }

      # Count samples per group.
      .gData <- plyr::count(.gData, vars = "Group")
      # Calculate frequencies.
      if (val_prop) {
        .gData$freq <- .gData$freq / sum(.gData$freq)
      }
      .gData$lab <- round(.gData$freq, val_decimals)

      # Create plot.
      gp <- ggplot(.gData, aes_string(x = "Group", y = "freq", fill = "Group"))
      gp <- gp + theme_grey(base_size = val_base_size)
      gp <- gp + geom_bar(stat = "identity", position = "stack")

      # Add color.
      gp <- gp + scale_fill_brewer(palette = val_palette) # NB! only 9 colors.

      # Add titles.
      gp <- gp + labs(title = mainTitle, x = xTitle, y = yTitle, fill = NULL)

      # Print value labels on bars.
      if (val_print) {
        gp <- gp + geom_text(aes_string(
          x = "Group", y = "freq", label = "lab",
          hjust = 0.5, vjust = 0
        ), size = val_lab_size)
      }

      # Remove legend.
      gp <- gp + theme(legend.position = "none")

      # plot.
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
      if (exists(".strvalidator_plotPeaks_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotPeaks_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotPeaks_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotPeaks_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(titles_chk) <- get(".strvalidator_plotPeaks_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotPeaks_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_plotPeaks_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_base_size", envir = env, inherits = FALSE)) {
        svalue(f1_base_size_edt) <- get(".strvalidator_plotPeaks_gui_base_size", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_label_size", envir = env, inherits = FALSE)) {
        svalue(f1_lab_size_edt) <- get(".strvalidator_plotPeaks_gui_label_size", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_print", envir = env, inherits = FALSE)) {
        svalue(f1_print_chk) <- get(".strvalidator_plotPeaks_gui_print", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_print", envir = env, inherits = FALSE)) {
        svalue(f1_prop_chk) <- get(".strvalidator_plotPeaks_gui_prop", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_palette", envir = env, inherits = FALSE)) {
        svalue(f1_palette_drp) <- get(".strvalidator_plotPeaks_gui_palette", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_decimal", envir = env, inherits = FALSE)) {
        svalue(f1_decimal_spb) <- get(".strvalidator_plotPeaks_gui_decimal", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotPeaks_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_title_chk", value = svalue(titles_chk), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_base_size", value = svalue(f1_base_size_edt), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_label_size", value = svalue(f1_lab_size_edt), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_print", value = svalue(f1_print_chk), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_prop", value = svalue(f1_prop_chk), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_palette", value = svalue(f1_palette_drp), envir = env)
      assign(x = ".strvalidator_plotPeaks_gui_decimal", value = svalue(f1_decimal_spb), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotPeaks_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_base_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_base_size", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_label_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_label_size", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_print", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_print", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_prop", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_prop", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_palette", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_palette", envir = env)
      }
      if (exists(".strvalidator_plotPeaks_gui_decimal", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotPeaks_gui_decimal", envir = env)
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
