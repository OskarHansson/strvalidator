#' @title Plot Drop-out Events
#'
#' @description
#' GUI simplifying the creation of plots from dropout data.
#'
#' @details Plot dropout data as heatmap arranged by, average peak height,
#' amount, concentration, or sample name. It is also possible to plot the
#' empirical cumulative distribution (ecdp) of the peak heights of surviving heterozygote
#' alleles (with dropout of the partner allele), or a dotplot of all dropout events.
#' The peak height of homozygote alleles can be included in the ecdp.
#' Automatic plot titles can be replaced by custom titles.
#' A name for the result is automatically suggested.
#' The resulting plot can be saved as either a plot object or as an image.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @references
#' Antoinette A. Westen, Laurens J.W. Grol, Joyce Harteveld, Anuska S.Matai,
#' Peter de Knijff, Titia Sijen, Assessment of the stochastic threshold, back- and
#' forward stutter filters and low template techniques for NGM,
#' Forensic Science International: Genetetics, Volume 6, Issue 6, December 2012,
#' Pages 708-715, ISSN 1872-4973, 10.1016/j.fsigen.2012.05.001.
#'  \doi{10.1016/j.fsigen.2012.05.001}
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom scales pretty_breaks
#' @importFrom utils help str
#' @importFrom grDevices rgb
#' @importFrom ggplot2 ggplot aes_string geom_tile scale_fill_manual guides
#'  guide_legend theme element_text labs ylab xlab scale_y_discrete scale_x_discrete
#'  stat_ecdf scale_colour_discrete scale_x_continuous scale_y_continuous
#'  coord_cartesian geom_point position_jitter scale_colour_manual
#'
#' @seealso \url{https://ggplot2.tidyverse.org/} for details on plot settings.

plot_dropout_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gDataColumns <- NULL
  .gPlot <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Plot dropout data",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset and kit",
    STR_LBL_DATASET         = "Dataset:",
    STR_LBL_KIT             = "Kit:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_OVERRIDE        = "Override automatic titles",
    STR_LBL_TITLE_PLOT      = "Plot title:",
    STR_LBL_TITLE_X         = "X title:",
    STR_LBL_TITLE_Y         = "Y title:",
    STR_EXP_AXES            = "Axes (applies to continous axes)",
    STR_LBL_NB              = "NB! Must provide both min and max value.",
    STR_LBL_LIMIT_Y         = "Limit Y axis (min-max)",
    STR_LBL_LIMIT_X         = "Limit X axis (min-max)",
    STR_EXP_LABELS          = "X labels",
    STR_LBL_ROUND           = "Round to digits:",
    STR_LBL_SIZE            = "Text size (pts):",
    STR_LBL_ANGLE           = "Angle:",
    STR_LBL_JUSTIFICATION   = "Justification (v/h):",
    STR_FRM_PLOT            = "Plot heatmap by",
    STR_BTN_H               = "Average peak height",
    STR_BTN_AMOUNT          = "Amount",
    STR_BTN_CONCENTRATION   = "Concentration",
    STR_BTN_SAMPLE          = "Sample",
    STR_BTN_PROCESSING      = "Processing...",
    STR_FRM_OTHER           = "Other plots",
    STR_BTN_ECDP            = "ecdp",
    STR_TIP_ECDP            = "Empirical cumulative distribution plot",
    STR_CHK_HOM             = "Plot homozygous peaks.",
    STR_BTN_DOT             = "Dotplot",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_SAVE_OBJECT     = "Save as object",
    STR_BTN_SAVE_IMAGE      = "Save as image",
    STR_BTN_OBJECT_SAVED    = "Object saved",
    STR_LBL_MAIN_TITLE      = "Allele and locus dropout",
    STR_LBL_MAIN_TITLE_ECDP = "Empirical cumulative distribution for",
    STR_LBL_MAIN_TITLE_AND  = "and",
    STR_LBL_MAIN_TITLE_HETEROZYGOUS= "heterozygous alleles (with dropout of the sister allele)",
    STR_LBL_MAIN_TITLE_HOMOZYGOUS= "homozygous peaks",
    STR_LBL_X_TITLE_AVERAGE = "Average peak height",
    STR_LBL_X_TITLE_AMOUNT  = "Amount amplified DNA",
    STR_LBL_X_TITLE_CONCENTRATION= "Concentration",
    STR_LBL_X_TITLE_SAMPLE  = "Sample name",
    STR_LBL_X_TITLE_HEIGHT  = "Peak height (RFU)",
    STR_LBL_X_TITLE_SURVIVING_HEIGHT= "Peak height of surviving allele (RFU)",
    STR_LBL_Y_TITLE_MARKER  = "Marker",
    STR_LBL_Y_TITLE_CP      = "Cumulative probability",
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
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  # Dataset -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

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
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  # Kit -----------------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_KIT, container = g1)

  kit_drp <- gcombobox(
    items = get_kit(),
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
    requiredCol <- c(
      "Sample.Name", "Marker", "Allele", "Height",
      "Dropout", "Rfu", "Heterozygous"
    )
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataColumns <<- names(.gData)

      # Suggest name.
      svalue(f5_save_edt) <- paste(val_obj, "_ggplot", sep = "")
      # Detect kit.
      kitIndex <- detect_kit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex

      # Enable plot buttons.
      enabled(f7_plot_h_btn) <- TRUE
      enabled(f7_plot_amount_btn) <- TRUE
      enabled(f7_plot_conc_btn) <- TRUE
      enabled(f7_plot_sample_btn) <- TRUE
      enabled(f8_plot_ecdf_btn) <- TRUE
      enabled(f8_plot_dot_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- NULL
      .gDataColumns <<- NULL
      svalue(f5_save_edt) <- ""
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

  # FRAME 7 ###################################################################

  f7 <- gframe(
    text = strings$STR_FRM_PLOT,
    horizontal = TRUE,
    container = gv
  )

  f7_plot_h_btn <- gbutton(text = strings$STR_BTN_H, container = f7)

  f7_plot_amount_btn <- gbutton(text = strings$STR_BTN_AMOUNT, container = f7)

  f7_plot_conc_btn <- gbutton(text = strings$STR_BTN_CONCENTRATION, container = f7)

  f7_plot_sample_btn <- gbutton(text = strings$STR_BTN_SAMPLE, container = f7)

  addHandlerChanged(f7_plot_h_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "H")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(f7_plot_h_btn) <- FALSE
      .plotDropout(what = "heat_h")
      enabled(f7_plot_h_btn) <- TRUE
    }

    # Change save button.
    svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
    enabled(f5_save_btn) <- TRUE
  })

  addHandlerChanged(f7_plot_amount_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "Amount")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(f7_plot_amount_btn) <- FALSE
      .plotDropout(what = "heat_amount")
      enabled(f7_plot_amount_btn) <- TRUE
    }

    # Change save button.
    svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
    enabled(f5_save_btn) <- TRUE
  })

  addHandlerChanged(f7_plot_conc_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "Concentration")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(f7_plot_conc_btn) <- FALSE
      .plotDropout(what = "heat_conc")
      enabled(f7_plot_conc_btn) <- TRUE
    }

    # Change save button.
    svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
    enabled(f5_save_btn) <- TRUE
  })

  addHandlerChanged(f7_plot_sample_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(f7_plot_sample_btn) <- FALSE
      .plotDropout(what = "sample")
      enabled(f7_plot_sample_btn) <- TRUE
    }

    # Change save button.
    svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
    enabled(f5_save_btn) <- TRUE
  })

  # FRAME 8 ###################################################################

  f8 <- gframe(
    text = strings$STR_FRM_OTHER,
    horizontal = TRUE,
    container = gv
  )

  f8_plot_ecdf_btn <- gbutton(text = strings$STR_BTN_ECDP, container = f8)
  tooltip(f8_plot_ecdf_btn) <- strings$STR_TIP_ECDP

  f8_hom_chk <- gcheckbox(
    text = strings$STR_CHK_HOM,
    checked = FALSE,
    container = f8
  )

  f8_plot_dot_btn <- gbutton(text = strings$STR_BTN_DOT, container = f8)

  addHandlerChanged(f8_plot_ecdf_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "Height", "Heterozygous")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(f8_plot_ecdf_btn) <- FALSE
      .plotDropout(what = "ecdf")
      enabled(f8_plot_ecdf_btn) <- TRUE
    }

    # Change save button.
    svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
    enabled(f5_save_btn) <- TRUE
  })

  addHandlerChanged(f8_plot_dot_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Sample.Name", "Marker", "Dropout", "Height", "Heterozygous")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(f8_plot_dot_btn) <- FALSE
      .plotDropout(what = "dot")
      enabled(f8_plot_dot_btn) <- TRUE
    }

    # Change save button.
    svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
    enabled(f5_save_btn) <- TRUE
  })

  # FRAME 5 ###################################################################

  f5 <- gframe(
    text = strings$STR_FRM_SAVE,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  glabel(text = strings$STR_LBL_SAVE, container = f5)

  f5_save_edt <- gedit(text = "", container = f5, expand = TRUE, fill = TRUE)

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

  # ADVANCED OPTIONS ##########################################################

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

  grid3[3, 1:2] <- glabel(text = strings$STR_LBL_LIMIT_X, container = grid3)
  grid3[4, 1] <- e3_x_min_edt <- gedit(text = "", width = 5, container = grid3)
  grid3[4, 2] <- e3_x_max_edt <- gedit(text = "", width = 5, container = grid3)

  # FRAME 4 ###################################################################

  e4 <- gexpandgroup(
    text = strings$STR_EXP_LABELS,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e4) <- FALSE

  grid4 <- glayout(container = e4)

  grid4[1, 1] <- glabel(text = strings$STR_LBL_ROUND, container = grid4)
  grid4[1, 2] <- e4_round_spb <- gspinbutton(
    from = 0, to = 10, by = 1,
    value = 3, container = grid4
  )

  grid4[2, 1] <- glabel(text = strings$STR_LBL_SIZE, container = grid4)
  grid4[2, 2] <- e4_size_txt <- gedit(text = "10", width = 4, container = grid4)

  grid4[2, 3] <- glabel(text = strings$STR_LBL_ANGLE, container = grid4)
  grid4[2, 4] <- e4_angle_spb <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 270, container = grid4
  )

  grid4[3, 1] <- glabel(text = strings$STR_LBL_JUSTIFICATION, container = grid4)
  grid4[3, 2] <- e4_vjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 0.3, container = grid4
  )

  grid4[3, 3] <- e4_hjust_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 0, container = grid4
  )

  # FUNCTIONS #################################################################


  .plotDropout <- function(what) {
    # Get values.
    val_titles <- svalue(titles_chk)
    val_title <- svalue(title_edt)
    val_xtitle <- svalue(x_title_edt)
    val_ytitle <- svalue(y_title_edt)
    val_angle <- as.numeric(svalue(e4_angle_spb))
    val_vjust <- as.numeric(svalue(e4_vjust_spb))
    val_hjust <- as.numeric(svalue(e4_hjust_spb))
    val_size <- as.numeric(svalue(e4_size_txt))
    val_round <- as.numeric(svalue(e4_round_spb))
    val_kit <- svalue(kit_drp)
    val_hom <- svalue(f8_hom_chk)
    val_ymin <- as.numeric(svalue(e3_y_min_edt))
    val_ymax <- as.numeric(svalue(e3_y_max_edt))
    val_xmin <- as.numeric(svalue(e3_x_min_edt))
    val_xmax <- as.numeric(svalue(e3_x_max_edt))

    if (debug) {
      print("val_title")
      print(val_title)
      print("val_xtitle")
      print(val_xtitle)
      print("val_ytitle")
      print(val_ytitle)
      print("val_angle")
      print(val_angle)
      print("val_vjust")
      print(val_vjust)
      print("val_hjust")
      print(val_hjust)
      print("val_size")
      print(val_size)
      print("val_round")
      print(val_round)
      print("val_hom")
      print(val_hom)
      print("str(.gData)")
      print(str(.gData))
    }


    if (is.data.frame(.gData)) {
      # Call functions.

      # Color information.
      if (is.null(.gData$Dye)) {
        .gData <- add_color(data = .gData, kit = val_kit, need = "Dye")
      }

      # Sort by marker in kit
      .gData <- sort_marker(
        data = .gData,
        kit = val_kit,
        add.missing.levels = TRUE
      )


      if (debug) {
        print("Before plot: str(.gData)")
        print(str(.gData))
      }

      # Create custom titles.
      if (val_titles) {
        mainTitle <- val_title
        xTitle <- val_xtitle
        yTitle <- val_ytitle
      }

      # Select what to plot and create default titles.
      if (what == "heat_h") {
        # Create default titles.
        if (!val_titles) {
          mainTitle <- strings$STR_LBL_MAIN_TITLE
          xTitle <- strings$STR_LBL_X_TITLE_AVERAGE
          yTitle <- strings$STR_LBL_Y_TITLE_MARKER
        }

        # Sort according to H.
        if (!is.numeric(.gData$H)) {
          .gData$H <- as.numeric(.gData$H)
          message("'H' converted to numeric.")
        }
        .gData <- .gData[order(.gData$H), ]

        # Add H to sample name.
        .gData$Sample.Name <- paste(.gData$H, " (", .gData$Sample.Name, ")", sep = "")

        # Create factors.
        .gData$Dropout <- factor(.gData$Dropout, levels = c(0, 1, 2))
        .gData$Sample.Name <- factor(.gData$Sample.Name,
          levels = unique(.gData$Sample.Name)
        )

        # Create x labels.
        xlabels <- .gData[!duplicated(.gData[, c("Sample.Name", "H")]), ]$H
        xlabels <- round(as.double(xlabels), digits = 0)

        # Define colours.
        col <- c(rgb(0, 0.737, 0), rgb(1, 0.526, 1), rgb(0.526, 0, 0.526))

        # Create plot.
        gp <- ggplot(.gData, aes_string(x = "Sample.Name", y = "Marker", fill = "Dropout"))
        gp <- gp + geom_tile(colour = "white") # OK
        gp <- gp + scale_fill_manual(
          values = col, name = "Dropout", breaks = c("0", "1", "2"),
          labels = c("none", "allele", "locus")
        )
        gp <- gp + guides(fill = guide_legend(reverse = TRUE)) # OK
        gp <- gp + theme(axis.text.x = element_text(
          angle = val_angle,
          hjust = val_hjust,
          vjust = val_vjust,
          size = val_size
        ))

        gp <- gp + labs(title = mainTitle)
        gp <- gp + ylab(yTitle)
        gp <- gp + xlab(xTitle)

        # Reverse y-axis and relabel x-ticks.
        gp <- gp + scale_y_discrete(limits = rev(levels(.gData$Marker))) +
          scale_x_discrete(labels = formatC(xlabels, 0, format = "f")) +
          theme(axis.text.x = element_text(family = "sans", face = "bold", size = val_size))
      } else if (what == "heat_amount") {
        # Create default titles.
        if (!val_titles) {
          mainTitle <- strings$STR_LBL_MAIN_TITLE
          xTitle <- strings$STR_LBL_X_TITLE_AMOUNT
          yTitle <- strings$STR_LBL_Y_TITLE_MARKER
        }

        # Sort according to average amount of DNA
        if (!is.numeric(.gData$Amount)) {
          .gData$Amount <- as.numeric(.gData$Amount)
          message("'Amount' converted to numeric.")
        }
        .gData <- .gData[order(.gData$Amount), ]

        # Add amount to sample name.
        .gData$Sample.Name <- paste(.gData$Amount, " (", .gData$Sample.Name, ")", sep = "")

        # Create factors.
        .gData$Dropout <- factor(.gData$Dropout, levels = c(0, 1, 2))
        .gData$Sample.Name <- factor(.gData$Sample.Name,
          levels = unique(.gData$Sample.Name)
        )

        # Create x labels.
        xlabels <- .gData[!duplicated(.gData[, c("Sample.Name", "Amount")]), ]$Amount
        xlabels <- round(as.double(xlabels), digits = val_round) # val_round also used below.

        # Define colours.
        col <- c(rgb(0, 0.737, 0), rgb(1, 0.526, 1), rgb(0.526, 0, 0.526))

        # Create plot.
        gp <- ggplot(.gData, aes_string(x = "Sample.Name", y = "Marker", fill = "Dropout"))
        gp <- gp + geom_tile(colour = "white") # OK
        gp <- gp + scale_fill_manual(
          values = col, name = "Dropout", breaks = c("0", "1", "2"),
          labels = c("none", "allele", "locus")
        )
        gp <- gp + guides(fill = guide_legend(reverse = TRUE)) # OK
        gp <- gp + theme(axis.text.x = element_text(
          angle = val_angle,
          hjust = val_hjust,
          vjust = val_vjust,
          size = val_size
        ))
        gp <- gp + labs(title = mainTitle)
        gp <- gp + ylab(yTitle)
        gp <- gp + xlab(xTitle)

        # Reverse y-axis and relabel x-ticks. Note: formatC required for trailing 0.
        gp <- gp + scale_y_discrete(limits = rev(levels(.gData$Marker))) +
          scale_x_discrete(labels = formatC(xlabels, val_round, format = "f")) +
          theme(axis.text.x = element_text(family = "sans", face = "bold", size = val_size))
      } else if (what == "heat_conc") {
        # Sort according to concentration of DNA.

        # Create default titles.
        if (!val_titles) {
          mainTitle <- strings$STR_LBL_MAIN_TITLE
          xTitle <- strings$STR_LBL_X_TITLE_CONCENTRATION
          yTitle <- strings$STR_LBL_Y_TITLE_MARKER
        }

        # Sort according to concentration.
        if (!is.numeric(.gData$Concentration)) {
          .gData$Concentration <- as.numeric(.gData$Concentration)
          message("'Concentration' converted to numeric.")
        }
        .gData <- .gData[order(.gData$Concentration), ]

        # Add concentration to sample name.
        .gData$Sample.Name <- paste(.gData$Concentration, " (", .gData$Sample.Name, ")", sep = "")

        # Create factors.
        .gData$Dropout <- factor(.gData$Dropout, levels = c(0, 1, 2))
        .gData$Sample.Name <- factor(.gData$Sample.Name,
          levels = unique(.gData$Sample.Name)
        )

        # Create x labels.
        xlabels <- .gData[!duplicated(.gData[, c("Sample.Name", "Concentration")]), ]$Concentration
        xlabels <- round(as.double(xlabels), digits = val_round) # val_round also used below.

        # Define colours.
        col <- c(rgb(0, 0.737, 0), rgb(1, 0.526, 1), rgb(0.526, 0, 0.526))

        # Create plot.
        gp <- ggplot(.gData, aes_string(x = "Sample.Name", y = "Marker", fill = "Dropout"))
        gp <- gp + geom_tile(colour = "white") # OK
        gp <- gp + scale_fill_manual(
          values = col, name = "Dropout", breaks = c("0", "1", "2"),
          labels = c("none", "allele", "locus")
        )
        gp <- gp + guides(fill = guide_legend(reverse = TRUE)) # OK
        gp <- gp + theme(axis.text.x = element_text(
          angle = val_angle,
          hjust = val_hjust,
          vjust = val_vjust,
          size = val_size
        ))
        gp <- gp + labs(title = mainTitle)
        gp <- gp + ylab(yTitle)
        gp <- gp + xlab(xTitle)

        # Reverse y-axis and relabel x-ticks. Note: formatC required for trailing 0.
        gp <- gp + scale_y_discrete(limits = rev(levels(.gData$Marker))) +
          scale_x_discrete(labels = formatC(xlabels, val_round, format = "f")) +
          theme(axis.text.x = element_text(family = "sans", face = "bold", size = val_size))
      } else if (what == "sample") {
        # Sort according to sample name.

        # Create default titles.
        if (!val_titles) {
          mainTitle <- strings$STR_LBL_MAIN_TITLE
          xTitle <- strings$STR_LBL_X_TITLE_SAMPLE
          yTitle <- strings$STR_LBL_Y_TITLE_MARKER
        }

        # Sort according to sample name.
        .gData <- .gData[order(.gData$Sample.Name), ]

        # Create factors.
        .gData$Dropout <- factor(.gData$Dropout)

        # Create x labels.
        xlabels <- .gData[!duplicated(.gData[, "Sample.Name"]), ]$Sample.Name

        # Define colours.
        col <- c(rgb(0, 0.737, 0), rgb(1, 0.526, 1), rgb(0.526, 0, 0.526))

        # Create plot.
        gp <- ggplot(.gData, aes_string(x = "Sample.Name", y = "Marker", fill = "Dropout"))
        gp <- gp + geom_tile(colour = "white") # OK
        gp <- gp + scale_fill_manual(
          values = col, name = "Dropout", breaks = c("0", "1", "2"),
          labels = c("none", "allele", "locus")
        )
        gp <- gp + guides(fill = guide_legend(reverse = TRUE)) # OK
        gp <- gp + theme(axis.text.x = element_text(
          angle = val_angle,
          hjust = val_hjust,
          vjust = val_vjust,
          size = val_size
        ))
        gp <- gp + labs(title = mainTitle)
        gp <- gp + ylab(yTitle)
        gp <- gp + xlab(xTitle)

        # Reverse y-axis and relabel x-ticks.
        gp <- gp + scale_y_discrete(limits = rev(levels(.gData$Marker))) +
          scale_x_discrete(labels = xlabels) +
          theme(axis.text.x = element_text(family = "sans", face = "bold", size = val_size))
      } else if (what == "ecdf") {
        # Plot empirical cumulative distribution.

        # Remove NA in dropout col.
        # NB! THIS HAS TO BE CHANGED WHEN A DROPOUT MODEL HAS BEEN SELECTED!
        n0 <- nrow(.gData)
        .gData <- .gData[!is.na(.gData$Dropout), ]
        n1 <- nrow(.gData)
        message(paste("Analyse ", n1,
          " rows (", n0 - n1,
          " rows with NA in Dropout removed.",
          sep = ""
        ))

        if (val_hom) {
          # Remove locus dropouts.
          # NB! THIS HAS TO BE CHANGED WHEN A DROPOUT MODEL HAS BEEN SELECTED!
          n0 <- nrow(.gData)
          .gData <- .gData[!is.na(.gData$Height), ]
          n1 <- nrow(.gData)
          message(paste("Analyse ", n1,
            " rows (", n0 - n1,
            " NA rows i.e. locus dropout, removed from column 'Height').",
            sep = ""
          ))

          # Remove locus dropout=2.
          # NB! THIS HAS TO BE CHANGED WHEN A DROPOUT MODEL HAS BEEN SELECTED!
          n0 <- nrow(.gData)
          .gData <- .gData[.gData$Dropout != 2, ]
          n1 <- nrow(.gData)
          message(paste("Analyse ", n1,
            " rows (", n0 - n1,
            " rows with 2 in Dropout removed.",
            sep = ""
          ))

          # Remove heterozygous loci without dropout.
          n0 <- nrow(.gData)
          .gData <- .gData[!(.gData$Heterozygous == 1 & .gData$Dropout == 0), ]
          n1 <- nrow(.gData)
          message(paste("Analyse ", n1,
            " rows (", n0 - n1,
            " heterozygous loci without dropout removed.",
            sep = ""
          ))
        } else {
          # Remove non-dropouts.
          n0 <- nrow(.gData)
          .gData <- .gData[.gData$Dropout == 1, ]
          n1 <- nrow(.gData)
          message(paste("Analyse ", n1,
            " rows (", n0 - n1,
            " non-dropouts removed).",
            sep = ""
          ))
        }

        # Create plot.
        if (val_hom) {
          # Create default titles.
          if (!val_titles) {
            mainTitle <- paste(
              strings$STR_LBL_MAIN_TITLE_ECDP,
              sum(.gData$Dropout == 1),
              strings$STR_LBL_MAIN_TITLE_HETEROZYGOUS, strings$STR_LBL_MAIN_TITLE_AND,
              sum(.gData$Dropout == 0),
              strings$STR_LBL_MAIN_TITLE_HOMOZYGOUS
            )
            xTitle <- strings$STR_LBL_X_TITLE_HEIGHT
            yTitle <- strings$STR_LBL_Y_TITLE_CP
          }

          # NB! Convert numeric to character (continous to discrete).
          # To avoid Error: Continuous value supplied to discrete scale.
          .gData$Heterozygous <- as.character(.gData$Heterozygous)

          # With homozygous data and heterozygous dropout data.
          gp <- ggplot(data = .gData, aes_string(x = "Height", color = "Heterozygous"))
          gp <- gp + stat_ecdf(data = subset(.gData, .gData$Heterozygous == "0"))
          gp <- gp + stat_ecdf(data = subset(.gData, .gData$Heterozygous == "1"))

          # Add legend.
          gp <- gp + scale_colour_discrete(
            name = "Alleles",
            breaks = c("0", "1"),
            labels = c("Homozygous", "Heterozygous")
          )
        } else {
          # Create default titles.
          if (!val_titles) {
            mainTitle <- paste(
              strings$STR_LBL_MAIN_TITLE_ECDP,
              sum(.gData$Dropout == 1),
              strings$STR_LBL_MAIN_TITLE_HETEROZYGOUS
            )
            xTitle <- strings$STR_LBL_X_TITLE_SURVIVING_HEIGHT
            yTitle <- strings$STR_LBL_Y_TITLE_CP
          }

          # With heterozygous dropout data.
          gp <- ggplot(.gData) +
            stat_ecdf(aes_string(x = "Height"))
        }
        # TODO: Add optional threshold line.
        # Fn(t) = #{xi <= t}/n = 1/n sum(i=1,n) Indicator(xi <= t).
        # x = rfu, Fn(t) = probability
        # Or bootstrap for "confidence" interval..

        # Add titles and settings.
        gp <- gp + theme(axis.text.x = element_text(
          angle = val_angle,
          vjust = val_vjust,
          size = val_size
        ))
        gp <- gp + labs(title = mainTitle)
        gp <- gp + ylab(yTitle)
        gp <- gp + xlab(xTitle)
        gp <- gp + scale_x_continuous(breaks = scales::pretty_breaks())
        gp <- gp + scale_y_continuous(breaks = seq(0, 1, 0.1))

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
      } else if (what == "dot") {
        # Plot dropouts per locus.

        # NA heights.
        n0 <- nrow(.gData)
        .gData <- .gData[!is.na(.gData$Height), ]
        n1 <- nrow(.gData)
        message(paste("Analyse ", n1,
          " rows (", n0 - n1,
          " NA rows removed from column 'Height').",
          sep = ""
        ))

        # NA Dropouts.
        n0 <- nrow(.gData)
        .gData <- .gData[!is.na(.gData$Dropout), ]
        n1 <- nrow(.gData)
        message(paste("Analyse ", n1,
          " rows (", n0 - n1,
          " NA rows removed from column 'Dropout').",
          sep = ""
        ))

        # Remove non-dropouts.
        n0 <- nrow(.gData)
        .gData <- .gData[.gData$Dropout == 1, ]
        n1 <- nrow(.gData)
        message(paste("Analyse ", n1,
          " rows (", n0 - n1,
          " non-dropouts removed).",
          sep = ""
        ))

        # Create default titles.
        if (!val_titles) {
          mainTitle <- paste(nrow(.gData), strings$STR_LBL_MAIN_TITLE_HETEROZYGOUS)
          xTitle <- strings$STR_LBL_Y_TITLE_MARKER
          yTitle <- strings$STR_LBL_X_TITLE_SURVIVING_HEIGHT
        }

        # Create plot.
        plotColor <- get_kit(kit = val_kit, what = "Color")
        plotColor <- unique(plotColor$Color)
        plotColor <- add_color(plotColor, need = "R.Color", have = "Color")

        # Create plot.
        gp <- ggplot(data = .gData, aes_string(x = "Marker", y = "Height"))

        # NB! This colour is only a grouping variable, NOT plot color.
        gp <- gp + geom_point(
          data = .gData, mapping = aes_string(colour = "Dye"),
          position = position_jitter(height = 0, width = 0.2)
        )

        # Specify colour values must be strings, NOT factors!
        # NB! The plot colours are specified as here as strings.
        # NB! Custom colours work on DATA AS SORTED FACTOR + COLOR CHARACTER.
        gp <- gp + scale_colour_manual(guide = FALSE, values = as.character(plotColor), drop = FALSE)

        # Add titles and settings.
        gp <- gp + theme(axis.text.x = element_text(
          angle = val_angle,
          vjust = val_vjust,
          size = val_size
        ))
        gp <- gp + labs(title = mainTitle)
        gp <- gp + ylab(yTitle)
        gp <- gp + xlab(xTitle)
        # gp <- gp + scale_y_continuous(breaks = seq(0, 1, 0.1))

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
      } else if (what == "heat_mx") {
        #         if(!val_titles){
        #           mainTitle <- "Allele and locus dropout"
        #           xTitle <- "Mixture proportion (Mx)"
        #           yTitle <- "Marker"
        #         }
        #
        #         .gData <- .gData[order(.gData$Sample.Name),]
        #
        #         .gData$Dropout <- factor(.gData$Dropout)
        #
        # Mx Data:
        # .gData <- newdata[order(newdata$Ratio),]
        # .gData <- newdata[order(newdata$Proportion),]

        # Mx data:
        # .gData$Sample.Name<-paste(.gData$Ratio, " (", .gData$Sample.Name, ")", sep="")
        # .gData$Sample.Name<-paste(.gData$Proportion, " (", .gData$Sample.Name, ")", sep="")

        # Mx data:
        # .gData <- .gData [order(.gData$Ratio),]
        # .gData <- .gData [order(.gData$Proportion),]

        # Mx data SGM Plus.
        # .gData<-add_color()
        # .gData<-sort_marker(.gData,"SGM Plus")

        # Mx Data:
        # xlabels<-.gData[!duplicated(.gData[, c("Sample.Name", "Ratio")]), ]$Ratio
        # xlabels<-.gData[!duplicated(.gData[, c("Sample.Name", "Proportion")]), ]$Proportion

        # Mx data:
        # hm.title <- "Heatmap: allele and locus dropout for 'F' SGM Plus (3500)"
        # hm.xlab <- "Proportion"
        # Mx data:

        # gp <- gp + scale_y_discrete(limits = rev(levels(.gData$Marker))) +
        #  scale_x_discrete(labels=formatC(xlabels, 4, format = "f")) +
        #  theme(axis.text.x=element_text(angle=-90, hjust = 0, vjust = 0.4, size = 10))
      }

      # Draw plot.
      print(gp)

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

  settings_prefix <- ".strvalidator_plotDropout_gui_"
  settings_widgets <- list(
    title = title_edt,
    title_chk = titles_chk,
    x_title = x_title_edt,
    y_title = y_title_edt,
    axes_y_min = e3_y_min_edt,
    axes_y_max = e3_y_max_edt,
    axes_x_min = e3_x_min_edt,
    axes_x_max = e3_x_max_edt,
    xlabel_round = e4_round_spb,
    xlabel_size = e4_size_txt,
    xlabel_angle = e4_angle_spb,
    xlabel_justh = e4_hjust_spb,
    xlabel_justv = e4_vjust_spb,
    hom = f8_hom_chk
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
