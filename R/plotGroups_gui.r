################################################################################
# CHANGE LOG (last 20 changes)
# 20.06.2023: Fixed Error in !is.null(val_data) && !is.na(val_data) in coercion to 'logical(1)
# 10.09.2022: Compacted the gui. Fixed narrow dropdowns. Removed destroy workaround.
# 26.04.2020: Added language support.
# 24.02.2019: Compacted and tweaked gui for tcltk.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 17.07.2018: First version.

#' @title Plot Empirical Cumulative Distributions
#'
#' @description
#' GUI simplifying the creation of empirical cumulative distribution plots.
#'
#' @details Plot the distribution of data as cumulative distribution function
#' for multiple groups. First select a dataset, then select columns to flat,
#' group, and plot by. For example, if a genotype dataset is selected and data
#' is flattened by Sample.Name the 'group by' and 'plot by' values must be
#' identical for all rows for a given sample.
#' Automatic plot titles can be replaced by custom titles.
#' Group names can be changed. A name for the result is automatically suggested.
#' The resulting plot can be saved as either a plot object or as an image.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help str head
#' @importFrom ggplot2 ggplot aes_string stat_ecdf labs theme_gray theme_bw
#'  theme_linedraw theme_light theme_dark theme_minimal theme_classic
#'  theme_void geom_histogram
#'
#' @return TRUE
#'
#' @seealso \code{\link{stat_ecdf}}


plotGroups_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .groups <- NULL
  .gPlot <- NULL
  .theme <- c(
    "theme_grey()", "theme_bw()", "theme_linedraw()",
    "theme_light()", "theme_dark()", "theme_minimal()",
    "theme_classic()", "theme_void()"
  )
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
  strWinTitle <- "Plot groups"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmDataset <- "Dataset"
  strLblDataset <- "Dataset:"
  strLblColumn <- "Flat data by:"
  strLblGroup <- "Group data by:"
  strLblPlot <- "Plot data by:"
  strDrpDataset <- "<Select dataset>"
  strDrpColumn <- "<Select column>"
  strDrpGroup <- "<Select group>"
  strLblRows <- "rows"
  strFrmOptions <- "Options"
  strChkOverride <- "Override automatic titles"
  strLblTitlePlot <- "Plot title:"
  strLblTitleX <- "X title:"
  strLblTitleY <- "Y title:"
  strLblLabels <- "Group labels:"
  strTipLabels <- "Separate by comma"
  strLblTheme <- "Plot theme:"
  strExpAxes <- "Axes"
  strLblNB <- "NB! Must provide both min and max value."
  strLblLimitY <- "Limit Y axis (min-max)"
  strLblLimitX <- "Limit X axis (min-max)"
  strBtnPlot <- "Plot"
  strTipPlot <- "Empirical cumulative distribution function"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitle <- "Empirical cumulative distribution function"
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

    strtmp <- dtStrings["strLblColumn"]$value
    strLblColumn <- ifelse(is.na(strtmp), strLblColumn, strtmp)

    strtmp <- dtStrings["strLblGroup"]$value
    strLblGroup <- ifelse(is.na(strtmp), strLblGroup, strtmp)

    strtmp <- dtStrings["strLblPlot"]$value
    strLblPlot <- ifelse(is.na(strtmp), strLblPlot, strtmp)

    strtmp <- dtStrings["strDrpDataset"]$value
    strDrpDataset <- ifelse(is.na(strtmp), strDrpDataset, strtmp)

    strtmp <- dtStrings["strDrpColumn"]$value
    strDrpColumn <- ifelse(is.na(strtmp), strDrpColumn, strtmp)

    strtmp <- dtStrings["strDrpGroup"]$value
    strDrpGroup <- ifelse(is.na(strtmp), strDrpGroup, strtmp)

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

    strtmp <- dtStrings["strLblLabels"]$value
    strLblLabels <- ifelse(is.na(strtmp), strLblLabels, strtmp)

    strtmp <- dtStrings["strTipLabels"]$value
    strTipLabels <- ifelse(is.na(strtmp), strTipLabels, strtmp)

    strtmp <- dtStrings["strLblTheme"]$value
    strLblTheme <- ifelse(is.na(strtmp), strLblTheme, strtmp)

    strtmp <- dtStrings["strExpAxes"]$value
    strExpAxes <- ifelse(is.na(strtmp), strExpAxes, strtmp)

    strtmp <- dtStrings["strLblNB"]$value
    strLblNB <- ifelse(is.na(strtmp), strLblNB, strtmp)

    strtmp <- dtStrings["strLblLimitY"]$value
    strLblLimitY <- ifelse(is.na(strtmp), strLblLimitY, strtmp)

    strtmp <- dtStrings["strLblLimitX"]$value
    strLblLimitX <- ifelse(is.na(strtmp), strLblLimitX, strtmp)

    strtmp <- dtStrings["strBtnPlot"]$value
    strBtnPlot <- ifelse(is.na(strtmp), strBtnPlot, strtmp)

    strtmp <- dtStrings["strTipPlot"]$value
    strTipPlot <- ifelse(is.na(strtmp), strTipPlot, strtmp)

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
    horizontal = FALSE, spacing = 1, use.scrollwindow = FALSE,
    container = w, expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(
    text = strChkGui, checked = FALSE,
    container = gh
  )

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(text = strFrmDataset, horizontal = FALSE, spacing = 1, container = gv)

  # Dataset -------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblDataset, container = f0g0)

  samples_lbl <- glabel(
    text = paste(" 0 ", strLblRows, sep = ""),
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
    selected = 1, editable = FALSE,
    container = f0g0, ellipsize = "none",
    expand = TRUE, fill = "x"
  )

  # Column --------------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblColumn, container = f0g1)

  flat_drp <- gcombobox(
    items = strDrpColumn, selected = 1,
    container = f0g1, ellipsize = "none",
    expand = TRUE, fill = "x"
  )

  # Group ---------------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblGroup, container = f0g2)

  group_drp <- gcombobox(
    items = strDrpGroup, selected = 1,
    container = f0g2, ellipsize = "none",
    expand = TRUE, fill = "x"
  )

  # Plot ----------------------------------------------------------------------

  f0g3 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strLblPlot, container = f0g3)

  axis_drp <- gcombobox(
    items = strDrpColumn, selected = 1,
    container = f0g3, ellipsize = "none",
    expand = TRUE, fill = "x"
  )

  # Handlers ------------------------------------------------------------------

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
      svalue(samples_lbl) <- paste(" ", nrow(.gData), " ",
        strLblRows,
        sep = ""
      )

      # Enable buttons.
      enabled(plot_ecdf_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(samples_lbl) <- paste(" 0 ", strLblRows, sep = "")
    }
  })

  addHandlerChanged(group_drp, handler = function(h, ...) {
    # Extract selected column name.
    val <- svalue(group_drp)

    # Get unique groups, save in global variable.
    .groups <<- unique(.gData[[val]])

    # Update groups field.
    svalue(f1_labels_edt) <- paste(.groups, collapse = ",")
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions, horizontal = FALSE, spacing = 1,
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


  f1g2 <- ggroup(container = f1, spacing = 1, horizontal = TRUE, expand = TRUE)

  glabel(text = strLblLabels, container = f1g2)
  f1_labels_edt <- gedit(expand = TRUE, fill = TRUE, container = f1g2)
  tooltip(f1_labels_edt) <- strTipLabels

  f1g3 <- glayout(container = f1, spacing = 1, expand = TRUE)

  f1g3[1, 1] <- glabel(text = strLblTheme, anchor = c(-1, 0), container = f1g3)

  f1g3[1, 2] <- f1_theme_drp <- gcombobox(
    items = .theme, selected = 1,
    container = f1g3, ellipsize = "none", expand = TRUE
  )

  f1e4 <- gexpandgroup(text = strExpAxes, horizontal = FALSE, container = f1)

  # Start collapsed.
  visible(f1e4) <- FALSE

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

  # BUTTON ####################################################################

  plot_ecdf_btn <- gbutton(
    text = strBtnPlot,
    container = gv
  )
  tooltip(plot_ecdf_btn) <- strTipPlot

  addHandlerChanged(plot_ecdf_btn, handler = function(h, ...) {
    val_column <- svalue(axis_drp)

    if (val_column == strDrpColumn) {
      gmessage(
        msg = strMsgColumn,
        title = strMsgTitleError,
        icon = "error"
      )
    } else {
      enabled(plot_ecdf_btn) <- FALSE
      .plot()
      enabled(plot_ecdf_btn) <- TRUE
    }
  })

  # FRAME 5 ###################################################################

  f5 <- gframe(text = strFrmSave, horizontal = TRUE, spacing = 1, container = gv)

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

  .plot <- function() {
    # Get values.
    val_data <- .gData
    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_x_title <- svalue(x_title_edt)
    val_y_title <- svalue(y_title_edt)
    val_theme <- svalue(f1_theme_drp)
    val_group <- svalue(group_drp)
    val_axis <- svalue(axis_drp)
    val_xmin <- as.numeric(svalue(f1g6_x_min_edt))
    val_xmax <- as.numeric(svalue(f1g6_x_max_edt))
    val_ymin <- as.numeric(svalue(f1g6_y_min_edt))
    val_ymax <- as.numeric(svalue(f1g6_y_max_edt))
    val_labels <- unlist(strsplit(svalue(f1_labels_edt), ","))
    val_names <- .groups
    val_flatten <- svalue(flat_drp)

    if (debug) {
      print("val_titles")
      print(val_titles)
      print("val_title")
      print(val_title)
      print("val_x_title")
      print(val_x_title)
      print("val_y_title")
      print(val_y_title)
      print("str(val_data)")
      print(str(val_data))
      print("val_xmin")
      print(val_xmin)
      print("val_xmax")
      print(val_xmax)
      print("val_ymin")
      print(val_ymin)
      print("val_ymax")
      print(val_ymax)
      print("val_labels")
      print(val_labels)
    }

    # Check if data.
    if (is.data.frame(val_data)) {
      if (debug) {
        print("Input data:")
        print(str(val_data))
        print(head(val_data))
      }

      # Get number of observations.
      nb <- nrow(val_data)

      # Create new data.farame from selected data.
      df <- data.frame(
        Group = val_data[[val_group]],
        Axis = val_data[[val_axis]],
        By = val_data[[val_flatten]],
        stringsAsFactors = FALSE
      )


      # Convert to data.table.
      dt <- data.table(df)

      # Flatten data (NB! requires single value on all rows collapsed).
      dt <- dt[, list(Group = unique(Group), Axis = unique(Axis)), by = By]

      # Convert to numeric.
      if (!is.numeric(dt$Axis)) {
        dt$Axis <- as.numeric(dt$Axis)
        message(val_axis, " converted to numeric.")
      }

      # Check if labels have been customized.
      if (!all(val_labels %in% val_names)) {
        # Replace values.
        names(val_labels) <- val_names
        dt$Group <- plyr::revalue(dt$Group, val_labels)

        message("Original group names: ", paste(val_names, collapse = ", "))
        message("New group names: ", paste(val_labels, collapse = ", "))
      }

      # Remove NA's
      if (any(is.na(dt$Group))) {
        # Show affected rows.
        message(val_group, "=NA on the following rows:")
        print(dt[is.na(dt$Group), ])

        # Store nb of observations.
        nb0 <- nrow(dt)

        # Remove NA rows.
        dt <- dt[!is.na(dt$Group), ]

        # Update number of observations.
        nb <- nrow(dt)

        # Show message.
        message("Removed ", nb0 - nb, " NA rows (", val_group, ").")
      }

      # Remove NA's
      if (any(is.na(dt$Axis))) {
        # Show affected rows.
        message(val_axis, "=NA on the following rows:")
        print(dt[is.na(dt$Axis), ])

        # Store nb of observations.
        nb0 <- nrow(dt)

        # Remove NA rows.
        dt <- dt[!is.na(dt$Axis), ]

        # Update number of observations.
        nb <- nrow(dt)

        # Show message.
        message("Removed ", nb0 - nb, " NA rows (", val_axis, ").")
      }

      # Calculate count for each group.
      dt[, N := .N, by = Group]

      # Convert back to data.frame.
      df <- data.frame(dt)

      message("Data flattened by ", val_flatten, ".")


      # Create titles.
      if (val_titles) {
        message("Custom titles.")

        mainTitle <- val_title

        xTitle <- val_x_title

        yTitle <- val_y_title
      } else {
        message("Default titles.")

        mainTitle <- paste(strLblMainTitle, " (",
          nb, " ", strLblObservations, ")",
          sep = ""
        )

        yTitle <- NULL

        xTitle <- val_axis
      }

      # Create legend.
      df$Legend <- paste(df$Group, " (n=", df$N, ")", sep = "")

      # Create plot.
      gp <- ggplot(df, aes_string(x = "Axis", colour = "Legend", group = "Group"))
      gp <- gp + stat_ecdf()

      if (debug) {
        print("Plot created")
      }

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
  }

  .refresh_column_drp <- function() {
    if (debug) {
      print("Refresh group and axis dropdown")
    }

    # Get names of columns in selected data frame.
    columns <- names(.gData)

    # Block handlers.
    blockHandler(group_drp)
    blockHandler(axis_drp)
    blockHandler(flat_drp)

    if (!is.null(columns)) {
      # Populate drop list.
      group_drp[] <- c(strDrpGroup, columns)
      axis_drp[] <- c(strDrpColumn, columns)
      flat_drp[] <- c(strDrpColumn, columns)
    } else {
      # Reset drop list and select first item.
      group_drp[] <- c(strDrpGroup)
      axis_drp[] <- c(strDrpColumn)
      flat_drp[] <- c(strDrpColumn)
    }

    # Select helpful text.
    svalue(group_drp, index = TRUE) <- 1
    svalue(axis_drp, index = TRUE) <- 1
    svalue(flat_drp, index = TRUE) <- 1

    # Unblock handlers.
    unblockHandler(group_drp)
    unblockHandler(axis_drp)
    unblockHandler(flat_drp)
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
      if (exists(".strvalidator_plotGroups_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotGroups_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotGroups_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotGroups_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(titles_chk) <- get(".strvalidator_plotGroups_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotGroups_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_plotGroups_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_theme", envir = env, inherits = FALSE)) {
        svalue(f1_theme_drp) <- get(".strvalidator_plotGroups_gui_theme", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotGroups_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotGroups_gui_title_chk", value = svalue(titles_chk), envir = env)
      assign(x = ".strvalidator_plotGroups_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotGroups_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotGroups_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_plotGroups_gui_theme", value = svalue(f1_theme_drp), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotGroups_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotGroups_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotGroups_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotGroups_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotGroups_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotGroups_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_theme", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotGroups_gui_theme", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_width", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotGroups_gui_width", envir = env)
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
