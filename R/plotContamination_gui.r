#' @title Plot Contamination
#'
#' @description
#' GUI simplifying the creation of plots from negative control data.
#'
#' @details Select data to plot in the drop-down menu.
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
#' @importFrom utils help
#' @importFrom data.table data.table
#' @importFrom stats dpois
#' @importFrom MASS fitdistr
#' @importFrom ggplot2 ggplot aes_string theme_gray theme_bw
#'  theme_linedraw theme_light theme_dark theme_minimal theme_classic
#'  theme_void scale_y_log10 scale_colour_manual coord_cartesian geom_point
#'  geom_line
#'
#' @references
#' Duncan Taylor et.al.,
#'  Validating multiplexes for use in conjunction with modern interpretation strategies,
#'  Forensic Science International: Genetics, Volume 20, January 2016,
#'  Pages 6-19, ISSN 1872-4973, 10.1016/j.fsigen.2015.09.011.
#' \doi{10.1016/j.fsigen.2015.09.011}

plot_contamination_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gPlot <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Plot contamination",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Contamination dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_OVERRIDE        = "Override automatic titles",
    STR_LBL_TITLE_PLOT      = "Plot title:",
    STR_LBL_TITLE_X         = "X title:",
    STR_LBL_TITLE_Y         = "Y title:",
    STR_LBL_THEME           = "Plot theme:",
    STR_BTN_PLOT            = "Plot",
    STR_BTN_PROCESSING      = "Processing...",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_SAVE_OBJECT     = "Save as object",
    STR_BTN_SAVE_IMAGE      = "Save as image",
    STR_BTN_OBJECT_SAVED    = "Object saved",
    STR_LBL_MAIN_TITLE      = "Observed and expected number of peaks per profile (n=",
    STR_LBL_X_TITLE         = "Number of peaks per control",
    STR_LBL_Y_TITLE         = "Relative occurance",
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
    text = paste(" 0 ", strings$STR_LBL_SAMPLES),
    container = f0
  )

  dataset_drp <- gcombobox(
    items = c(
      strings$STR_DRP_DATASET,
      list_objects(
        env = env,
        obj_class = "data.frame"
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
    requiredCol <- c("Peaks", "Id")
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

      svalue(samples_lbl) <- paste(" ",
        length(unique(.gData$Sample.Name)),
        " ", strings$STR_LBL_SAMPLES,
        sep = ""
      )

      # Enable buttons.
      enabled(plot_poiss_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste(" 0 ", strings$STR_LBL_SAMPLES)
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

  # BUTTON ####################################################################

  plot_poiss_btn <- gbutton(
    text = strings$STR_BTN_PLOT, container = gv,
    expand = TRUE, fill = TRUE
  )
  tooltip(plot_poiss_btn) <- "Plot observed and expected contamination rate"

  addHandlerChanged(plot_poiss_btn, handler = function(h, ...) {
    enabled(plot_poiss_btn) <- FALSE
    .plot()
    enabled(plot_poiss_btn) <- TRUE
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
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_theme <- svalue(f1_theme_drp)

    if (is.data.frame(val_data)) {
      # Convert to data table.
      DT <- data.table(val_data)

      # Get number of peaks per sample (one row per sample).
      DTsample <- DT[, list(Peaks = unique(Peaks)), by = list(Id)]

      # Get count of samples with N peaks and sort by # peaks.
      DTtable <- DTsample[, list(Samples = .N), keyby = list(Peaks)]

      # Calculate total number of sampels.
      totalSamples <- sum(DTtable$Samples)

      # Calculate and add proportion (relative occurance).
      DTtable[, Proportion := Samples / totalSamples]

      # Add method.
      DTtable$Method <- "Observed"

      # Print some output.
      print("Observed:")
      print(DTtable)

      # Fit a poisson distribution (estimate lambda parameter)
      maxpeaks <- max(DTsample$Peaks)
      parms <- MASS::fitdistr(DTsample$Peaks, "poisson")
      lambda <- parms$estimate
      sd_x <- as.numeric(parms$sd)
      model <- paste("Pois(", signif(lambda, 3), "), sd=", signif(sd_x, 3), sep = "")
      message("Best fit: ", model)

      # Create data frame with expected observations.
      expected <- data.frame(
        Peaks = seq(0, maxpeaks),
        Proportion = stats::dpois(seq(0, maxpeaks),
          lambda = lambda
        ),
        Method = "Poisson"
      )

      # Print some output.
      print("Expected:")
      print(expected)

      # Prepare some plot settings.
      title <- paste(strings$STR_LBL_MAIN_TITLE, totalSamples, ")", sep = "")
      legend <- model
      ymin <- floor(log10(min(DTtable$Proportion)))
      breaks <- 10^seq(1, ymin) # Pretty breaks.

      # Create plot.
      gp <- ggplot(data = expected)
      gp <- gp + geom_line(aes_string(x = "Peaks", y = "Proportion", colour = "Method"))
      gp <- gp + scale_y_log10(breaks = breaks)
      gp <- gp + coord_cartesian(ylim = c(1, min(breaks)))
      gp <- gp + geom_point(data = DTtable, aes_string(x = "Peaks", y = "Proportion", colour = "Method"))
      gp <- gp + scale_colour_manual(
        name = NULL,
        values = c("black", "black"),
        labels = c(legend, "Observed"),
        guide = guide_legend(override.aes = list(
          linetype = c("solid", "blank"),
          shape = c(NA, 16)
        ))
      )

      # Add titles.
      if (val_titles) {
        # User defined titles.

        gp <- .apply_plot_settings(
          gp = gp, theme = val_theme,
          main_title = val_title,
          x_title = val_xtitle, y_title = val_ytitle
        )
      } else {
        # Automatic titles.

        gp <- .apply_plot_settings(
          gp = gp, theme = val_theme,
          main_title = title,
          x_title = strings$STR_LBL_X_TITLE,
          y_title = strings$STR_LBL_Y_TITLE
        )
      }

      # Show plot.
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

  .apply_plot_settings <- function(gp, theme = "theme_grey()",
                                   main_title = NULL, x_title = NULL, y_title = NULL) {
    # Apply theme.
    gp <- gp + eval(parse(text = theme))
    gp <- gp + theme(legend.justification = c(1, 1), legend.position = c(1, 1))

    # Apply titles.
    gp <- gp + labs(title = main_title)
    gp <- gp + xlab(x_title)
    gp <- gp + ylab(y_title)

    return(gp)
  }

  settings_prefix <- ".strvalidator_plotContamination_gui_"
  settings_widgets <- list(
    title = title_edt,
    title_chk = titles_chk,
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
