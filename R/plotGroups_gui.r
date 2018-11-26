################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
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
  .palette <- c(
    "Set1", "Set2", "Set3", "Accent", "Dark2",
    "Paired", "Pastel1", "Pastel2"
  )
  .defaultGroup <- "<Select group>"
  .defaultColumn <- "<Select column>"
  # Qualitative palette, do not imply magnitude differences between legend
  # classes, and hues are used to create the primary visual differences
  # between classes. Qualitative schemes are best suited to representing
  # nominal or categorical data.

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Plot groups", visible = FALSE)

  # Runs when window is closed.
  addHandlerDestroy(w, handler = function(h, ...) {

    # Save GUI state.
    .saveSettings()

    # Focus on parent window.
    if (!is.null(parent)) {
      focus(parent)
    }
  })

  gv <- ggroup(
    horizontal = FALSE, spacing = 8, use.scrollwindow = FALSE,
    container = w, expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(
    text = "Save GUI settings", checked = FALSE,
    container = gh
  )

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("plotGroups_gui", help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(text = "Dataset", horizontal = TRUE, spacing = 5, container = gv)

  f0g0 <- glayout(container = f0)

  f0g0[1, 1] <- glabel(text = "Select dataset:", container = f0g0)

  f0g0[1, 2] <- dataset_drp <- gcombobox(
    items = c(
      "<Select dataset>",
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1, editable = FALSE,
    container = f0g0, ellipsize = "none"
  )

  f0g0[1, 3] <- f0_samples_lbl <- glabel(text = " (0 rows)", container = f0g0)

  f0g0[2, 1] <- glabel(text = "Select column to flat data by:", container = f0g0)

  f0g0[2, 2] <- f0_flat_drp <- gcombobox(
    items = .defaultColumn, selected = 1,
    container = f0g0, ellipsize = "none"
  )

  f0g0[3, 1] <- glabel(text = "Select column to group data by:", container = f0g0)

  f0g0[3, 2] <- f0_group_drp <- gcombobox(
    items = .defaultGroup, selected = 1,
    container = f0g0, ellipsize = "none"
  )

  f0g0[4, 1] <- glabel(text = "Select column to plot data by:", container = f0g0)

  f0g0[4, 2] <- f0_axis_drp <- gcombobox(
    items = .defaultColumn, selected = 1,
    container = f0g0, ellipsize = "none"
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
      svalue(f0_samples_lbl) <- paste(" (", nrow(.gData), " rows)", sep = "")

      # Enable buttons.
      enabled(plot_ecdf_btn) <- TRUE
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(f0_samples_lbl) <- " (0 rows)"
    }
  })

  addHandlerChanged(f0_group_drp, handler = function(h, ...) {

    # Extract selected column name.
    val <- svalue(f0_group_drp)

    # Get unique groups, save in global variable.
    .groups <<- unique(.gData[[val]])

    # Update groups field.
    svalue(f1_labels_edt) <- paste(.groups, collapse = ",")
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = "Options", horizontal = FALSE, spacing = 5,
    container = gv
  )

  f1_titles_chk <- gcheckbox(
    text = "Override automatic titles.",
    checked = FALSE, container = f1
  )


  addHandlerChanged(f1_titles_chk, handler = function(h, ...) {
    val <- svalue(f1_titles_chk)
    if (val) {
      enabled(f1g1) <- TRUE
    } else {
      enabled(f1g1) <- FALSE
    }
  })

  f1g1 <- ggroup(container = f1, spacing = 1, horizontal = FALSE)
  enabled(f1g1) <- svalue(f1_titles_chk)

  glabel(text = "Plot title:", container = f1g1)
  f1_title_edt <- gedit(text = "", expand = TRUE, container = f1g1)

  glabel(text = "X title:", container = f1g1)
  f1_xtitle_edt <- gedit(text = "", expand = TRUE, container = f1g1)

  glabel(text = "Y title:", container = f1g1)
  f1_ytitle_edt <- gedit(text = "", expand = TRUE, container = f1g1)

  f1g2 <- ggroup(container = f1, spacing = 5, horizontal = TRUE, expand = TRUE)

  glabel(text = "Group labels:", container = f1g2)
  f1_labels_edt <- gedit(text = "", expand = TRUE, container = f1g2, spacing = 5)

  f1g3 <- glayout(container = f1, spacing = 5, expand = TRUE)

  f1g3[1, 1] <- glabel(text = "Plot theme:", anchor = c(-1, 0), container = f1g3)
  items_theme <- c(
    "theme_grey()", "theme_bw()", "theme_linedraw()",
    "theme_light()", "theme_dark()", "theme_minimal()",
    "theme_classic()", "theme_void()"
  )
  f1g3[1, 2] <- f1_theme_drp <- gcombobox(
    items = items_theme, selected = 1,
    container = f1g3, ellipsize = "none", expand = TRUE
  )

  f1e4 <- gexpandgroup(text = "Axes", horizontal = FALSE, container = f1)

  # Start collapsed.
  visible(f1e4) <- FALSE

  glabel(
    text = "NB! Must provide both min and max value.",
    anchor = c(-1, 0), container = f1e4
  )

  f1g6 <- glayout(container = f1e4, spacing = 1)
  f1g6[1, 1:2] <- glabel(text = "Limit Y axis (min-max)", container = f1g6)
  f1g6[2, 1] <- f1g6_y_min_edt <- gedit(text = "", width = 5, container = f1g6)
  f1g6[2, 2] <- f1g6_y_max_edt <- gedit(text = "", width = 5, container = f1g6)

  f1g6[3, 1:2] <- glabel(text = "Limit X axis (min-max)", container = f1g6)
  f1g6[4, 1] <- f1g6_x_min_edt <- gedit(text = "", width = 5, container = f1g6)
  f1g6[4, 2] <- f1g6_x_max_edt <- gedit(text = "", width = 5, container = f1g6)

  # BUTTON ####################################################################

  plot_ecdf_btn <- gbutton(
    text = "Empirical cumulative distribution function",
    container = gv
  )

  addHandlerChanged(plot_ecdf_btn, handler = function(h, ...) {
    val_column <- svalue(f0_axis_drp)

    if (val_column == .defaultColumn) {
      gmessage(
        msg = "A data column must be specified!",
        title = "Error",
        icon = "error"
      )
    } else {
      enabled(plot_ecdf_btn) <- FALSE
      .plot()
      enabled(plot_ecdf_btn) <- TRUE
    }
  })

  # FRAME 5 ###################################################################

  f5 <- gframe(text = "Save as", horizontal = TRUE, spacing = 5, container = gv)

  glabel(text = "Name for result:", container = f5)

  f5_save_edt <- gedit(text = "", container = f5, expand = TRUE)

  f5_save_btn <- gbutton(text = "Save as object", container = f5)

  f5_ggsave_btn <- gbutton(text = "Save as image", container = f5)

  addHandlerClicked(f5_save_btn, handler = function(h, ...) {
    val_name <- svalue(f5_save_edt)

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- "Processing..."
    unblockHandlers(f5_save_btn)
    enabled(f5_save_btn) <- FALSE

    # Save data.
    saveObject(
      name = val_name, object = .gPlot,
      parent = w, env = env, debug = debug
    )

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- "Object saved"
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
    val_titles <- svalue(f1_titles_chk)
    val_title <- svalue(f1_title_edt)
    val_x_title <- svalue(f1_xtitle_edt)
    val_y_title <- svalue(f1_ytitle_edt)
    val_theme <- svalue(f1_theme_drp)
    val_group <- svalue(f0_group_drp)
    val_axis <- svalue(f0_axis_drp)
    val_xmin <- as.numeric(svalue(f1g6_x_min_edt))
    val_xmax <- as.numeric(svalue(f1g6_x_max_edt))
    val_ymin <- as.numeric(svalue(f1g6_y_min_edt))
    val_ymax <- as.numeric(svalue(f1g6_y_max_edt))
    val_labels <- unlist(strsplit(svalue(f1_labels_edt), ","))
    val_names <- .groups
    val_flatten <- svalue(f0_flat_drp)

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
    if (!is.na(val_data) && !is.null(val_data)) {
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

        mainTitle <- paste("Empirical cumulative distribution function (",
          nb, " observations)",
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
      svalue(f5_save_btn) <- "Save as object"
      enabled(f5_save_btn) <- TRUE
    } else {
      gmessage(
        msg = "Data frame is NULL or NA!",
        title = "Error",
        icon = "error"
      )
    }
  }

  # INTERNAL FUNCTIONS ########################################################

  .refresh_column_drp <- function() {
    if (debug) {
      print("Refresh group and axis dropdown")
    }

    # Get names of columns in selected data frame.
    columns <- names(.gData)

    # Block handlers.
    blockHandler(f0_group_drp)
    blockHandler(f0_axis_drp)
    blockHandler(f0_flat_drp)

    if (!is.null(columns)) {

      # Populate drop list.
      f0_group_drp[] <- c(.defaultGroup, columns)
      f0_axis_drp[] <- c(.defaultColumn, columns)
      f0_flat_drp[] <- c(.defaultColumn, columns)
    } else {

      # Reset drop list and select first item.
      f0_group_drp[] <- c(.defaultGroup)
      f0_axis_drp[] <- c(.defaultColumn)
      f0_flat_drp[] <- c(.defaultColumn)
    }

    # Select helpful text.
    svalue(f0_group_drp, index = TRUE) <- 1
    svalue(f0_axis_drp, index = TRUE) <- 1
    svalue(f0_flat_drp, index = TRUE) <- 1

    # Unblock handlers.
    unblockHandler(f0_group_drp)
    unblockHandler(f0_axis_drp)
    unblockHandler(f0_flat_drp)
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
        svalue(f1_title_edt) <- get(".strvalidator_plotGroups_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(f1_titles_chk) <- get(".strvalidator_plotGroups_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(f1_xtitle_edt) <- get(".strvalidator_plotGroups_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotGroups_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(f1_ytitle_edt) <- get(".strvalidator_plotGroups_gui_y_title", envir = env)
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
      assign(x = ".strvalidator_plotGroups_gui_title_chk", value = svalue(f1_titles_chk), envir = env)
      assign(x = ".strvalidator_plotGroups_gui_title", value = svalue(f1_title_edt), envir = env)
      assign(x = ".strvalidator_plotGroups_gui_x_title", value = svalue(f1_xtitle_edt), envir = env)
      assign(x = ".strvalidator_plotGroups_gui_y_title", value = svalue(f1_ytitle_edt), envir = env)
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

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
