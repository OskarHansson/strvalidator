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
#' @seealso \url{https://ggplot2.tidyverse.org/} for details on plot settings.

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

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Plot capillary balance",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_ROWS            = "rows",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_OVERRIDE        = "Override automatic titles",
    STR_LBL_TITLE_PLOT      = "Plot title:",
    STR_LBL_TITLE_SUB       = "Sub title:",
    STR_LBL_TITLE_X         = "X title:",
    STR_LBL_TITLE_Y         = "Y title:",
    STR_EXP_POINTS          = "Data points",
    STR_LBL_SHAPE           = "Shape:",
    STR_LBL_ALPHA           = "Alpha:",
    STR_EXP_AXES            = "Axes",
    STR_LBL_LIMIT_Y         = "Limit Y axis (min-max)",
    STR_FRM_PLOT            = "Plot capillary balance data",
    STR_BTN_DOTPLOT         = "Dotplot",
    STR_BTN_BOXPLOT         = "Boxplot",
    STR_BTN_DISTRIBUTION    = "Distribution",
    STR_BTN_PROCESSING      = "Processing...",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_SAVE_OBJECT     = "Save as object",
    STR_BTN_SAVE_IMAGE      = "Save as image",
    STR_BTN_OBJECT_SAVED    = "Object saved",
    STR_LBL_MAIN_TITLE_DOTPLOT= "Mean peak height grouped by capillary for",
    STR_LBL_MAIN_TITLE_BOXPLOT= "Mean peak height by capillary for",
    STR_LBL_MAIN_TITLE_DISTRIBUTION= "Mean peak height for",
    STR_LBL_MAIN_SUB_TITLE_DOTPLOT= "[dotted blue line indicate global mean, red line indicate median per capillary]",
    STR_LBL_X_TITLE_INJECTION= "Injection",
    STR_LBL_X_TITLE_CAPILLARY= "Capillary",
    STR_LBL_X_TITLE_DENSITY = "Density",
    STR_LBL_Y_TITLE_MEAN    = "Mean peak height (RFU)",
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
    horizontal = FALSE,
    spacing = 1,
    use.scrollwindow = FALSE,
    container = w,
    expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = strings$STR_CHK_GUI, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strings$STR_BTN_HELP, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {
    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strings$STR_FRM_DATASET,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strings$STR_LBL_DATASET, container = f0)

  samples_lbl <- glabel(
    text = paste(" 0 ", strings$STR_LBL_ROWS, sep = ""),
    container = f0
  )

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c(
      "Instrument", "Instrument.ID", "Run",
      "Mean.Height", "SQ", "Injection", "Capillary", "Well"
    )
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")

      svalue(samples_lbl) <- paste(" ",
        nrow(.gData), " ", strings$STR_LBL_ROWS,
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
      svalue(samples_lbl) <- paste(" 0 ", strings$STR_LBL_ROWS, sep = "")
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
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

  glabel(text = strings$STR_LBL_TITLE_SUB, container = titles_group, anchor = c(-1, 0))
  sub_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strings$STR_LBL_TITLE_X, container = titles_group, anchor = c(-1, 0))
  x_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strings$STR_LBL_TITLE_Y, container = titles_group, anchor = c(-1, 0))
  y_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)


  # FRAME 7 ###################################################################

  f7 <- gframe(
    text = strings$STR_FRM_PLOT,
    horizontal = TRUE,
    container = gv,
    spacing = 1
  )

  plot_dot_btn <- gbutton(text = strings$STR_BTN_DOTPLOT, container = f7)

  plot_box_btn <- gbutton(text = strings$STR_BTN_BOXPLOT, container = f7)

  plot_dst_btn <- gbutton(text = strings$STR_BTN_DISTRIBUTION, container = f7)

  addHandlerChanged(plot_dot_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c(
      "Instrument", "Instrument.ID", "Run", "Mean.Height",
      "Injection", "Capillary", "Well", "Comment"
    )
    ok <- check_dataset(
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
    ok <- check_dataset(
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
    ok <- check_dataset(
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
    text = strings$STR_FRM_SAVE,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strings$STR_LBL_SAVE, container = f5)

  f5_save_edt <- gedit(expand = TRUE, fill = TRUE, container = f5)

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
    saveObject(
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

  # ADVANCED OPTIONS ##########################################################

  e2 <- gexpandgroup(
    text = strings$STR_EXP_POINTS,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e2) <- FALSE

  grid2 <- glayout(container = e2)

  grid2[1, 1] <- glabel(text = strings$STR_LBL_SHAPE, container = grid2)
  grid2[1, 2] <- e2_shape_spb <- gspinbutton(
    from = 0, to = 25,
    by = 1, value = 18,
    container = grid2
  )

  grid2[1, 3] <- glabel(text = strings$STR_LBL_ALPHA, container = grid2)
  grid2[1, 4] <- e2_alpha_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = 1,
    container = grid2
  )

  # FRAME 3 ###################################################################

  e3 <- gexpandgroup(
    text = strings$STR_EXP_AXES,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e3) <- FALSE

  grid3 <- glayout(container = e3, spacing = 1)

  grid3[1, 1:2] <- glabel(text = strings$STR_LBL_LIMIT_Y, container = grid3)
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

    if (is.data.frame(.gData)) {
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
          mainTitle <- paste(strings$STR_LBL_MAIN_TITLE_DOTPLOT, " ",
            unique(.gData$Instrument.ID),
            " (", unique(.gData$Instrument), ")",
            sep = ""
          )
          subTitle <- strings$STR_LBL_MAIN_SUB_TITLE_DOTPLOT
          xTitle <- strings$STR_LBL_X_TITLE_INJECTION
          yTitle <- strings$STR_LBL_Y_TITLE_MEAN
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
          mainTitle <- paste(strings$STR_LBL_MAIN_TITLE_BOXPLOT, " ",
            unique(.gData$Instrument.ID),
            " (", unique(.gData$Instrument), ")",
            sep = ""
          )
          subTitle <- ""
          xTitle <- strings$STR_LBL_X_TITLE_CAPILLARY
          yTitle <- strings$STR_LBL_Y_TITLE_MEAN
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
          mainTitle <- paste(strings$STR_LBL_MAIN_TITLE_DISTRIBUTION, " ",
            unique(.gData$Instrument.ID),
            " (", unique(.gData$Instrument), ")",
            sep = ""
          )
          subTitle <- ""
          xTitle <- strings$STR_LBL_Y_TITLE_MEAN
          yTitle <- strings$STR_LBL_X_TITLE_DENSITY
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
      svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
      enabled(f5_save_btn) <- TRUE

      # Store in global variable.
      .gPlot <<- gp
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
  settings_prefix <- ".strvalidator_plotCapillary_gui_"
  settings_widgets <- list(
    title_chk = titles_chk,
    title = title_edt,
    sub_title = sub_title_edt,
    x_title = x_title_edt,
    y_title = y_title_edt,
    points_shape = e2_shape_spb,
    points_alpha = e2_alpha_spb,
    axes_y_min = e3_y_min_edt,
    axes_y_max = e3_y_max_edt,
    xlabel_size = e4_size_edt
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
