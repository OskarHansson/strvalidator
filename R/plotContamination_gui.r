################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 04.08.2016: First version.

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
#' \url{http://www.sciencedirect.com/science/article/pii/S1872497315300739}

plotContamination_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gPlot <- NULL

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Plot contamination", visible = FALSE)

  # Runs when window is closed.
  addHandlerDestroy(w, handler = function(h, ...) {

    # Save GUI state.
    .saveSettings()

    # Focus on parent window.
    if (!is.null(parent)) {
      focus(parent)
    }

  })

  # Vertical main group.
  gv <- ggroup(horizontal = FALSE,
               spacing = 8,
               use.scrollwindow = FALSE,
               container = w,
               expand = TRUE)

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("plotContamination_gui", help_type = "html"))

  })

  # FRAME 0 ###################################################################

  f0 <- gframe(text = "Dataset",
               horizontal = TRUE,
               spacing = 5,
               container = gv)

  glabel(text = "Select dataset:", container = f0)

  dataset_drp <- gcombobox(items = c("<Select dataset>",
                                   listObjects(env = env,
                                               obj.class = "data.frame")),
                           selected = 1,
                           editable = FALSE,
                           container = f0,
                           ellipsize = "none")

  f0_samples_lbl <- glabel(text = " (0 samples)", container = f0)

  addHandlerChanged(dataset_drp, handler = function(h, ...) {

    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Peaks", "Id")
    ok <- checkDataset(name = val_obj, reqcol = requiredCol,
                       env = env, parent = w, debug = debug)

    if (ok) {
      # Load or change components.

      # Get data.
      .gData <<- get(val_obj, envir = env)

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")

      svalue(f0_samples_lbl) <- paste(" (",
                                      length(unique(.gData$Sample.Name)),
                                      " samples)", sep = "")

      # Enable buttons.
      enabled(plot_poiss_btn) <- TRUE

    } else {

      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0_samples_lbl) <- " (0 samples)"

    }

  })

  # FRAME 1 ###################################################################

  f1 <- gframe(text = "Options",
               horizontal = FALSE,
               spacing = 5,
               container = gv)

  f1_titles_chk <- gcheckbox(text = "Override automatic titles.",
                             checked = FALSE, container = f1)

  addHandlerChanged(f1_titles_chk, handler = function(h, ...) {
    val <- svalue(f1_titles_chk)
    if (val) {
      enabled(f1g1) <- TRUE
    } else {
      enabled(f1g1) <- FALSE
    }
  })

  f1g1 <- glayout(container = f1, spacing = 1)
  enabled(f1g1) <- svalue(f1_titles_chk)

  f1g1[1, 1] <- glabel(text = "Plot title:", container = f1g1)
  f1g1[1, 2] <- title_edt <- gedit(text = "",
                                  width = 40,
                                  container = f1g1)

  f1g1[2, 1] <- glabel(text = "X title:", container = f1g1)
  f1g1[2, 2] <- x_title_edt <- gedit(text = "",
                                    container = f1g1)

  f1g1[3, 1] <- glabel(text = "Y title:", container = f1g1)
  f1g1[3, 2] <- y_title_edt <- gedit(text = "",
                                    container = f1g1)

  f1g2 <- glayout(container = f1)
  f1g2[1, 1] <- glabel(text = "Plot theme:", anchor = c(-1, 0), container = f1g2)
  items_theme <- c("theme_grey()", "theme_bw()", "theme_linedraw()",
                   "theme_light()", "theme_dark()", "theme_minimal()",
                   "theme_classic()", "theme_void()")
  f1g2[1, 2] <- f1_theme_drp <- gcombobox(items = items_theme,
                                         selected = 1,
                                         container = f1g2,
                                         ellipsize = "none")

  # FRAME 7 ###################################################################

  f7 <- gframe(text = "Plot contamination rate",
               horizontal = FALSE,
               container = gv)

  grid7 <- glayout(container = f7)

  grid7[1, 1] <- plot_poiss_btn <- gbutton(text = "Plot", container = grid7)
  tooltip(plot_poiss_btn) <- "Plot observed and expected contamination rate"

  addHandlerChanged(plot_poiss_btn, handler = function(h, ...) {

    enabled(plot_poiss_btn) <- FALSE
    .plot()
    enabled(plot_poiss_btn) <- TRUE

  })

  # FRAME 5 ###################################################################

  f5 <- gframe(text = "Save as",
               horizontal = TRUE,
               spacing = 5,
               container = gv)

  glabel(text = "Name for result:", container = f5)

  f5_save_edt <- gedit(text = "", container = f5)

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
    saveObject(name = val_name, object = .gPlot,
               parent = w, env = env, debug = debug)

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- "Object saved"
    unblockHandlers(f5_save_btn)

  })

  addHandlerChanged(f5_ggsave_btn, handler = function(h, ...) {

    val_name <- svalue(f5_save_edt)

    # Save data.
    ggsave_gui(ggplot = .gPlot, name = val_name,
               parent = w, env = env, savegui = savegui, debug = debug)

  })

  # FUNCTIONS #################################################################


  .plot <- function() {

    # Get values.
    val_data <- .gData
    val_titles <- svalue(f1_titles_chk)
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_theme <- svalue(f1_theme_drp)

    if (!is.na(val_data) && !is.null(val_data)) {

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
      expected <- data.frame(Peaks = seq(0, maxpeaks),
                             Proportion = stats::dpois(seq(0, maxpeaks),
                                                     lambda = lambda),
                             Method = "Poisson")

      # Print some output.
      print("Expected:")
      print(expected)

      # Prepare some plot settings.
      title <- paste("Observed and expected number of peaks per profile (n=", totalSamples, ")", sep = "")
      legend <- model
      ymin <- floor(log10(min(DTtable$Proportion)))
      breaks <- 10^seq(1, ymin) # Pretty breaks.

      # Create plot.
      gp <- ggplot(data = expected)
      gp <- gp + geom_line(aes_string(x = "Peaks", y = "Proportion", colour = "Method"))
      gp <- gp + scale_y_log10(breaks = breaks)
      gp <- gp + coord_cartesian(ylim = c(1, min(breaks)))
      gp <- gp + geom_point(data = DTtable, aes_string(x = "Peaks", y = "Proportion", colour = "Method"))
      gp <- gp + scale_colour_manual(name = NULL,
                                     values = c("black", "black"),
                                     labels = c(legend, "Observed"),
                                     guide = guide_legend(override.aes = list(linetype = c("solid", "blank"),
                                                                              shape = c(NA, 16))))

      # Add titles.
      if (val_titles) {
        # User defined titles.

        gp <- .applyPlotSettings(gp = gp, theme = val_theme,
                                 main.title = val_title,
                                 x.title = val_xtitle, y.title = val_ytitle)

      } else {
        # Automatic titles.

        gp <- .applyPlotSettings(gp = gp, theme = val_theme,
                                 main.title = title,
                                 x.title = "Number of peaks per control",
                                 y.title = "Relative occurance")

      }

      # Show plot.
      print(gp)

      # Store in global variable.
      .gPlot <<- gp

      # Change save button.
      svalue(f5_save_btn) <- "Save as object"
      enabled(f5_save_btn) <- TRUE

    } else {

      gmessage(msg = "Data frame is NULL or NA!",
               title = "Error",
               icon = "error")

    }

  }

  # INTERNAL FUNCTIONS ########################################################

  .applyPlotSettings <- function(gp, theme = "theme_grey()",
                                 main.title = NULL, x.title = NULL, y.title = NULL) {

    # Apply theme.
    gp <- gp + eval(parse(text = theme))
    gp <- gp + theme(legend.justification = c(1, 1), legend.position = c(1, 1))

    # Apply titles.
    gp <- gp + labs(title = main.title)
    gp <- gp + xlab(x.title)
    gp <- gp + ylab(y.title)

    return(gp)
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
      if (exists(".strvalidator_plotContamination_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotContamination_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotContamination_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotContamination_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_title_chk", envir = env, inherits = FALSE)) {
        svalue(f1_titles_chk) <- get(".strvalidator_plotContamination_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotContamination_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_y_title", envir = env, inherits = FALSE)) {
        svalue(y_title_edt) <- get(".strvalidator_plotContamination_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_theme", envir = env, inherits = FALSE)) {
        svalue(f1_theme_drp) <- get(".strvalidator_plotContamination_gui_theme", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }

  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {

      assign(x = ".strvalidator_plotContamination_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_title_chk", value = svalue(f1_titles_chk), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_y_title", value = svalue(y_title_edt), envir = env)
      assign(x = ".strvalidator_plotContamination_gui_theme", value = svalue(f1_theme_drp), envir = env)

    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotContamination_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_title_chk", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_title_chk", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_y_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_y_title", envir = env)
      }
      if (exists(".strvalidator_plotContamination_gui_theme", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotContamination_gui_theme", envir = env)
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
