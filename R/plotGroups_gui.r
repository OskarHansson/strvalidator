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
#' @seealso \code{\link[ggplot2:stat_ecdf]{stat_ecdf}}


plot_groups_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Plot groups",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Dataset:",
    STR_LBL_COLUMN          = "Flat data by:",
    STR_LBL_GROUP           = "Group data by:",
    STR_LBL_PLOT            = "Plot data by:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_DRP_COLUMN          = "<Select column>",
    STR_DRP_GROUP           = "<Select group>",
    STR_LBL_ROWS            = "rows",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_OVERRIDE        = "Override automatic titles",
    STR_LBL_TITLE_PLOT      = "Plot title:",
    STR_LBL_TITLE_X         = "X title:",
    STR_LBL_TITLE_Y         = "Y title:",
    STR_LBL_LABELS          = "Group labels:",
    STR_TIP_LABELS          = "Separate by comma",
    STR_LBL_THEME           = "Plot theme:",
    STR_EXP_AXES            = "Axes",
    STR_LBL_NB              = "NB! Must provide both min and max value.",
    STR_LBL_LIMIT_Y         = "Limit Y axis (min-max)",
    STR_LBL_LIMIT_X         = "Limit X axis (min-max)",
    STR_BTN_PLOT            = "Plot",
    STR_TIP_PLOT            = "Empirical cumulative distribution function",
    STR_BTN_PROCESSING      = "Processing...",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_SAVE_OBJECT     = "Save as object",
    STR_BTN_SAVE_IMAGE      = "Save as image",
    STR_BTN_OBJECT_SAVED    = "Object saved",
    STR_LBL_MAIN_TITLE      = "Empirical cumulative distribution function",
    STR_LBL_OBSERVATIONS    = "observations",
    STR_MSG_COLUMN          = "A data column must be specified!",
    STR_MSG_NOT_DF          = "Data set must be a data.frame!",
    STR_MSG_TITLE_ERROR     = "Error"
  )

  strings <- update_strings_with_language_file(default_strings, lng_strings$value)

  # WINDOW ####################################################################

  # Main window.
  w <- gwindow(title = strings$STR_WIN_TITLE, visible = FALSE)

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
    text = strings$STR_CHK_GUI, checked = FALSE,
    container = gh
  )

  addSpring(gh)

  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(text = strings$STR_FRM_DATASET, horizontal = FALSE, spacing = 1, container = gv)

  # Dataset -------------------------------------------------------------------

  f0g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = f0g0)

  samples_lbl <- glabel(
    text = paste(" 0 ", strings$STR_LBL_ROWS, sep = ""),
    container = f0g0
  )

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      list_objects(
        env = env,
        obj_class = "data.frame"
      )
    ),
    selected = 1, editable = FALSE,
    container = f0g0, ellipsize = "none",
    expand = TRUE, fill = "x"
  )

  # Column --------------------------------------------------------------------

  f0g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_COLUMN, container = f0g1)

  flat_drp <- gcombobox(
    items = strings$STR_DRP_COLUMN, selected = 1,
    container = f0g1, ellipsize = "none",
    expand = TRUE, fill = "x"
  )

  # Group ---------------------------------------------------------------------

  f0g2 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_GROUP, container = f0g2)

  group_drp <- gcombobox(
    items = strings$STR_DRP_GROUP, selected = 1,
    container = f0g2, ellipsize = "none",
    expand = TRUE, fill = "x"
  )

  # Plot ----------------------------------------------------------------------

  f0g3 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_PLOT, container = f0g3)

  axis_drp <- gcombobox(
    items = strings$STR_DRP_COLUMN, selected = 1,
    container = f0g3, ellipsize = "none",
    expand = TRUE, fill = "x"
  )

  # Handlers ------------------------------------------------------------------

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- NULL
    ok <- check_dataset(
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
        strings$STR_LBL_ROWS,
        sep = ""
      )

      # Enable buttons.
      enabled(plot_ecdf_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(samples_lbl) <- paste(" 0 ", strings$STR_LBL_ROWS, sep = "")
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
    text = strings$STR_FRM_OPTIONS, horizontal = FALSE, spacing = 1,
    container = gv
  )

  titles_chk <- gcheckbox(
    text = strings$STR_CHK_OVERRIDE,
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
  glabel(text = strings$STR_LBL_TITLE_PLOT, container = titles_group, anchor = c(-1, 0))
  title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strings$STR_LBL_TITLE_X, container = titles_group, anchor = c(-1, 0))
  x_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strings$STR_LBL_TITLE_Y, container = titles_group, anchor = c(-1, 0))
  y_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)


  f1g2 <- ggroup(container = f1, spacing = 1, horizontal = TRUE, expand = TRUE)

  glabel(text = strings$STR_LBL_LABELS, container = f1g2)
  f1_labels_edt <- gedit(expand = TRUE, fill = TRUE, container = f1g2)
  tooltip(f1_labels_edt) <- strings$STR_TIP_LABELS

  f1g3 <- glayout(container = f1, spacing = 1, expand = TRUE)

  f1g3[1, 1] <- glabel(text = strings$STR_LBL_THEME, anchor = c(-1, 0), container = f1g3)

  f1g3[1, 2] <- f1_theme_drp <- gcombobox(
    items = .theme, selected = 1,
    container = f1g3, ellipsize = "none", expand = TRUE
  )

  f1e4 <- gexpandgroup(text = strings$STR_EXP_AXES, horizontal = FALSE, container = f1)

  # Start collapsed.
  visible(f1e4) <- FALSE

  glabel(
    text = strings$STR_LBL_NB,
    anchor = c(-1, 0), container = f1e4
  )

  f1g6 <- glayout(container = f1e4, spacing = 1)
  f1g6[1, 1:2] <- glabel(text = strings$STR_LBL_LIMIT_Y, container = f1g6)
  f1g6[2, 1] <- f1g6_y_min_edt <- gedit(text = "", width = 5, container = f1g6)
  f1g6[2, 2] <- f1g6_y_max_edt <- gedit(text = "", width = 5, container = f1g6)

  f1g6[3, 1:2] <- glabel(text = strings$STR_LBL_LIMIT_X, container = f1g6)
  f1g6[4, 1] <- f1g6_x_min_edt <- gedit(text = "", width = 5, container = f1g6)
  f1g6[4, 2] <- f1g6_x_max_edt <- gedit(text = "", width = 5, container = f1g6)

  # BUTTON ####################################################################

  plot_ecdf_btn <- gbutton(
    text = strings$STR_BTN_PLOT,
    container = gv
  )
  tooltip(plot_ecdf_btn) <- strings$STR_TIP_PLOT

  addHandlerChanged(plot_ecdf_btn, handler = function(h, ...) {
    val_column <- svalue(axis_drp)

    if (val_column == strings$STR_DRP_COLUMN) {
      gmessage(
        msg = strings$STR_MSG_COLUMN,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    } else {
      enabled(plot_ecdf_btn) <- FALSE
      .plot()
      enabled(plot_ecdf_btn) <- TRUE
    }
  })

  # FRAME 5 ###################################################################

  f5 <- gframe(text = strings$STR_FRM_SAVE, horizontal = TRUE, spacing = 1, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = f5)

  f5_save_edt <- gedit(container = f5, expand = TRUE, fill = TRUE)

  f5_save_btn <- gbutton(text = strings$STR_BTN_SAVE_OBJECT, container = f5)

  f5_ggsave_btn <- gbutton(text = strings$STR_BTN_SAVE_IMAGE, container = f5)

  addHandlerClicked(f5_save_btn, handler = function(h, ...) {
    val_name <- svalue(f5_save_edt)

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- strings$STR_BTN_PROCESSING
    unblockHandlers(f5_save_btn)
    enabled(f5_save_btn) <- FALSE

    # Save data.
    save_object(
      name = val_name, object = .gPlot,
      parent = w, env = env, debug = debug
    )

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- strings$STR_BTN_OBJECT_SAVED
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

        mainTitle <- paste(strings$STR_LBL_MAIN_TITLE, " (",
          nb, " ", strings$STR_LBL_OBSERVATIONS, ")",
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
      svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
      enabled(f5_save_btn) <- TRUE
    } else {
      gmessage(
        msg = strings$STR_MSG_NOT_DF,
        title = strings$STR_MSG_TITLE_ERROR,
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
      group_drp[] <- c(strings$STR_DRP_GROUP, columns)
      axis_drp[] <- c(strings$STR_DRP_COLUMN, columns)
      flat_drp[] <- c(strings$STR_DRP_COLUMN, columns)
    } else {
      # Reset drop list and select first item.
      group_drp[] <- c(strings$STR_DRP_GROUP)
      axis_drp[] <- c(strings$STR_DRP_COLUMN)
      flat_drp[] <- c(strings$STR_DRP_COLUMN)
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

  settings_prefix <- ".strvalidator_plotGroups_gui_"
  settings_widgets <- list(
    title_chk = titles_chk,
    title = title_edt,
    x_title = x_title_edt,
    y_title = y_title_edt,
    theme = f1_theme_drp
  )

  settings_key <- function(name) {
    paste0(settings_prefix, name)
  }

  get_saved_setting <- function(name) {
    key <- settings_key(name)
    if (exists(key, envir = env, inherits = FALSE)) {
      return(get(key, envir = env))
    }
    NULL
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
      saved_savegui <- get_saved_setting("savegui")
      if (!is.null(saved_savegui)) {
        svalue(savegui_chk) <- saved_savegui
      }
      if (debug) {
        print("Save GUI status loaded!")
      }
    }
    if (debug) {
      print(svalue(savegui_chk))
    }

    # Then load settings if true.
    if (isTRUE(svalue(savegui_chk))) {
      for (name in names(settings_widgets)) {
        value <- get_saved_setting(name)
        if (!is.null(value)) {
          svalue(settings_widgets[[name]]) <- value
        }
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {
    # Then save settings if true.
    if (isTRUE(svalue(savegui_chk))) {
      assign(x = settings_key("savegui"), value = svalue(savegui_chk), envir = env)
      for (name in names(settings_widgets)) {
        assign(x = settings_key(name), value = svalue(settings_widgets[[name]]), envir = env)
      }
    } else { # or remove all saved values if false.
      for (name in c("savegui", names(settings_widgets))) {
        key <- settings_key(name)
        if (exists(key, envir = env, inherits = FALSE)) {
          remove(key, envir = env)
        }
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
