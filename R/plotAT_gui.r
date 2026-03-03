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
#' @seealso \url{https://ggplot2.tidyverse.org/} for details on plot settings.

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

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Plot analytical threshold",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "AT6 dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_OVERRIDE        = "Override automatic titles",
    STR_LBL_TITLE_PLOT      = "Plot title:",
    STR_LBL_TITLE_X         = "X title:",
    STR_LBL_TITLE_Y         = "Y title:",
    STR_LBL_THEME           = "Plot theme:",
    STR_EXP_POINTS          = "Data points",
    STR_LBL_SHAPE           = "Shape:",
    STR_LBL_ALPHA           = "Alpha:",
    STR_LBL_JITTER          = "Jitter (width):",
    STR_EXP_AXES            = "Axes",
    STR_LBL_LIMIT_Y         = "Limit Y axis (min-max)",
    STR_LBL_LIMIT_X         = "Limit X axis (min-max)",
    STR_LBL_SCALES          = "Scales:",
    STR_EXP_LABELS          = "X labels",
    STR_LBL_SIZE            = "Text size (pts):",
    STR_LBL_ANGLE           = "Angle:",
    STR_LBL_JUSTIFICATION   = "Justification (v/h):",
    STR_FRM_PLOT            = "Plot analytical threshold data",
    STR_BTN_PLOT            = "Plot AT6",
    STR_BTN_PROCESSING      = "Processing...",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_SAVE_OBJECT     = "Save as object",
    STR_BTN_SAVE_IMAGE      = "Save as image",
    STR_BTN_OBJECT_SAVED    = "Object saved",
    STR_LBL_MAIN_TITLE_LINEAR= "Linear regression",
    STR_LBL_MAIN_TITLE_WEIGHTED= "Weighted linear regression",
    STR_LBL_X_TITLE         = "Amount (pg)",
    STR_LBL_Y_TITLE         = "Average Peak Height (RFU)",
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
    text = paste("0", strings$STR_LBL_SAMPLES),
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
    requiredCol <- c("Amount", "Height", "AT6")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # Get data.
      .gData <<- get(val_obj, envir = env)

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")

      svalue(samples_lbl) <- paste(
        length(unique(.gData$Sample.Name)),
        strings$STR_LBL_SAMPLES
      )

      # Enable buttons.
      enabled(plot_at6_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste("0", strings$STR_LBL_SAMPLES)
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

  glabel(text = strings$STR_LBL_TITLE_X, container = titles_group, anchor = c(-1, 0))
  x_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strings$STR_LBL_TITLE_Y, container = titles_group, anchor = c(-1, 0))
  y_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  f1g2 <- glayout(container = f1)
  f1g2[1, 1] <- glabel(text = strings$STR_LBL_THEME, anchor = c(-1, 0), container = f1g2)
  f1g2[1, 2] <- f1_theme_drp <- gcombobox(
    items = c("theme_grey()", "theme_bw()"),
    selected = 1,
    container = f1g2,
    ellipsize = "none"
  )

  # FRAME 7 ###################################################################

  f7 <- gframe(
    text = strings$STR_FRM_PLOT,
    horizontal = FALSE,
    container = gv
  )

  grid7 <- glayout(container = f7)

  grid7[1, 1] <- plot_at6_btn <- gbutton(text = strings$STR_BTN_PLOT, container = grid7)

  addHandlerChanged(plot_at6_btn, handler = function(h, ...) {
    enabled(plot_at6_btn) <- FALSE
    .plotAT(what = "AT6")
    enabled(plot_at6_btn) <- TRUE
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
  grid2[1, 2] <- shape_spb <- gspinbutton(
    from = 0, to = 25,
    by = 1, value = 18,
    container = grid2
  )

  grid2[1, 3] <- glabel(text = strings$STR_LBL_ALPHA, container = grid2)
  grid2[1, 4] <- alpha_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = 1,
    container = grid2
  )

  grid2[1, 5] <- glabel(text = strings$STR_LBL_JITTER, container = grid2)
  grid2[1, 6] <- jitter_txt <- gedit(text = "0", width = 4, container = grid2)

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
  grid3[2, 1] <- y_min_txt <- gedit(text = "", width = 5, container = grid3)
  grid3[2, 2] <- y_max_txt <- gedit(text = "", width = 5, container = grid3)

  grid3[3, 1:2] <- glabel(text = strings$STR_LBL_LIMIT_X, container = grid3)
  grid3[4, 1] <- x_min_txt <- gedit(text = "", width = 5, container = grid3)
  grid3[4, 2] <- x_max_txt <- gedit(text = "", width = 5, container = grid3)

  grid3[1, 3] <- glabel(text = "    ", container = grid3) # Add some space.

  grid3[1, 4] <- glabel(text = strings$STR_LBL_SCALES, container = grid3)
  grid3[2:4, 4] <- scales_opt <- gradio(
    items = c("fixed", "free_x", "free_y", "free"),
    selected = 2,
    horizontal = FALSE,
    container = grid3
  )

  # FRAME 4 ###################################################################

  e4 <- gexpandgroup(
    text = strings$STR_EXP_LABELS,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e4) <- FALSE

  grid4 <- glayout(container = e4)

  grid4[1, 1] <- glabel(text = strings$STR_LBL_SIZE, container = grid4)
  grid4[1, 2] <- size_txt <- gedit(text = "10", width = 4, container = grid4)

  grid4[1, 3] <- glabel(text = strings$STR_LBL_ANGLE, container = grid4)
  grid4[1, 4] <- angle_spb <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 270,
    container = grid4
  )

  grid4[2, 1] <- glabel(text = strings$STR_LBL_JUSTIFICATION, container = grid4)
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

    if (is.data.frame(.gData)) {
      # Plotting data and regression for AT6.
      if (what == "AT6") {
        if (val_titles) {
          mainTitle <- val_title
          xTitle <- val_xtitle
          yTitle <- val_ytitle
        } else {
          if (all(is.na(.gData$Weight))) {
            mainTitle <- strings$STR_LBL_MAIN_TITLE_LINEAR
          } else {
            mainTitle <- strings$STR_LBL_MAIN_TITLE_WEIGHTED
          }
          subTitle <- paste("AT6:", round(unique(.gData$AT6), 0), "(RFU)")
          xTitle <- strings$STR_LBL_X_TITLE
          yTitle <- strings$STR_LBL_Y_TITLE
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

  settings_prefix <- ".strvalidator_plotAT_gui_"
  settings_widgets <- list(
    title = title_edt,
    title_chk = titles_chk,
    x_title = x_title_edt,
    y_title = y_title_edt,
    points_shape = shape_spb,
    points_alpha = alpha_spb,
    points_jitter = jitter_txt,
    axes_y_min = y_min_txt,
    axes_y_max = y_max_txt,
    axes_x_min = x_min_txt,
    axes_x_max = x_max_txt,
    axes_scales = scales_opt,
    xlabel_size = size_txt,
    xlabel_angle = angle_spb,
    xlabel_justh = hjust_spb,
    xlabel_justv = vjust_spb,
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
