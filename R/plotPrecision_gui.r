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

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Plot precision",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset and kit",
    STR_LBL_DATASET         = "Precision dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_KIT             = "Kit:",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_OVERRIDE        = "Override automatic titles",
    STR_EXP_TITLES          = "Titles",
    STR_LBL_TITLE_PLOT      = "Plot title:",
    STR_LBL_TITLE_X         = "X title:",
    STR_LBL_TITLE_Y         = "Y title:",
    STR_LBL_NB              = "NB! Title size, angle, vjust, and hjust does not always work as expected with different number of facets per row.",
    STR_LBL_TITLE_SETTINGS  = "Main title size, angle, vjust, hjust:",
    STR_LBL_X_TITLE_SETTINGS= "X title size, angle, vjust, hjust:",
    STR_LBL_Y_TITLE_SETTINGS= "Y title size, angle, vjust, hjust:",
    STR_CHK_BY_MARKER       = "Plot by marker",
    STR_LBL_X_AXIS          = "X axis:",
    STR_RAD_MEAN            = "Mean",
    STR_RAD_ALLELE          = "Allele",
    STR_LBL_THEME           = "Plot theme:",
    STR_EXP_POINTS          = "Data points",
    STR_LBL_SHAPE           = "Shape:",
    STR_LBL_ALPHA           = "Alpha:",
    STR_LBL_COLOR           = "Colour:",
    STR_EXP_AXES            = "Axes",
    STR_LBL_LIMIT_Y         = "Limit Y axis (min-max)",
    STR_LBL_LIMIT_X         = "Limit X axis (min-max)",
    STR_LBL_SCALES          = "Scales:",
    STR_CHK_OVERRIDE_LABELS = "Override default x/y/facet labels",
    STR_EXP_X_LABELS        = "X labels",
    STR_LBL_SIZE            = "Text size (pts):",
    STR_LBL_ANGLE           = "Angle:",
    STR_LBL_JUSTIFICATION   = "Justification (v/h):",
    STR_EXP_Y_LABELS        = "Y labels",
    STR_EXP_FACETS          = "Facets",
    STR_LBL_SIZE_X          = "Text size X (pts):",
    STR_LBL_SIZE_Y          = "Text size Y (pts):",
    STR_FRM_PLOT_DOT        = "Plot precision data as dotplot",
    STR_BTN_SIZE            = "Size",
    STR_BTN_HEIGHT          = "Height",
    STR_BTN_POINT           = "Data point",
    STR_FRM_PLOT_BOX        = "Plot precision data as boxplot",
    STR_BTN_PROCESSING      = "Processing...",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_SAVE_OBJECT     = "Save as object",
    STR_BTN_SAVE_IMAGE      = "Save as image",
    STR_BTN_OBJECT_SAVED    = "Object saved",
    STR_LBL_MAIN_TITLE_SIZE = "Allele size range",
    STR_LBL_MAIN_TITLE_HEIGHT= "Allele height range",
    STR_LBL_MAIN_TITLE_POINT= "Allele data point range",
    STR_LBL_Y_TITLE_SIZE    = "Deviation from mean (bp)",
    STR_LBL_Y_TITLE_HEIGHT  = "Deviation from mean (RFU)",
    STR_LBL_Y_TITLE_POINT   = "Deviation from mean (data point)",
    STR_LBL_X_TITLE_SIZE    = "Mean size in basepairs (bp)",
    STR_LBL_X_TITLE_HEIGHT  = "Mean height in relative fluorescent units (RFU)",
    STR_LBL_X_TITLE_POINTS  = "Mean scan number in data points",
    STR_LBL_X_TITLE_ALLELE  = "Allele",
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

  glabel(text = strings$STR_LBL_KIT, container = g1)

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
    ok <- check_dataset(
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

  # Titles --------------------------------------------------------------------

  titles_group <- gexpandgroup(
    text = strings$STR_EXP_TITLES,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(titles_group) <- FALSE

  glabel(text = strings$STR_LBL_TITLE_PLOT, container = titles_group, anchor = c(-1, 0))
  title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strings$STR_LBL_TITLE_X, container = titles_group, anchor = c(-1, 0))
  x_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strings$STR_LBL_TITLE_Y, container = titles_group, anchor = c(-1, 0))
  y_title_edt <- gedit(expand = TRUE, fill = TRUE, container = titles_group)

  glabel(text = strings$STR_LBL_NB, anchor = c(-1, 0), container = titles_group)

  titles_layout <- glayout(container = titles_group)

  title_lbl <- glabel(
    text = strings$STR_LBL_TITLE_SETTINGS,
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
    text = strings$STR_LBL_X_TITLE_SETTINGS,
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
    text = strings$STR_LBL_Y_TITLE_SETTINGS,
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
    text = strings$STR_CHK_BY_MARKER,
    checked = TRUE,
    container = f1
  )

  f1g2 <- glayout(container = f1)
  f1g2[1, 1] <- glabel(text = strings$STR_LBL_X_AXIS, anchor = c(-1, 0), container = f1g2)
  f1g2[1, 2] <- f1_axis_opt <- gradio(
    items = c(strings$STR_RAD_MEAN, strings$STR_RAD_ALLELE),
    selected = 2,
    horizontal = TRUE,
    container = f1g2
  )

  # Theme ---------------------------------------------------------------------

  f1g2[2, 1] <- glabel(text = strings$STR_LBL_THEME, anchor = c(-1, 0), container = f1g2)
  f1g2[2, 2] <- f1_theme_drp <- gcombobox(
    items = .theme,
    selected = 1,
    container = f1g2,
    ellipsize = "none"
  )


  # FRAME 7 ###################################################################

  f7 <- gframe(
    text = strings$STR_FRM_PLOT_DOT,
    horizontal = TRUE,
    container = gv
  )

  f7_size_btn <- gbutton(text = strings$STR_BTN_SIZE, container = f7)

  f7_height_btn <- gbutton(text = strings$STR_BTN_HEIGHT, container = f7)

  f7_data_btn <- gbutton(text = strings$STR_BTN_POINT, container = f7)

  addHandlerChanged(f7_size_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Size")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f7_size_btn) <- FALSE
      .plot(what = "Size", how = "dotplot")
      enabled(f7_size_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
      enabled(f5_save_btn) <- TRUE
    }
  })

  addHandlerChanged(f7_height_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Height")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f7_height_btn) <- FALSE
      .plot(what = "Height", how = "dotplot")
      enabled(f7_height_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
      enabled(f5_save_btn) <- TRUE
    }
  })

  addHandlerChanged(f7_data_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Data.Point")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f7_data_btn) <- FALSE
      .plot(what = "Data.Point", how = "dotplot")
      enabled(f7_data_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
      enabled(f5_save_btn) <- TRUE
    }
  })

  # FRAME 8 ###################################################################

  f8 <- gframe(
    text = strings$STR_FRM_PLOT_BOX,
    horizontal = TRUE,
    container = gv
  )

  f8_size_btn <- gbutton(text = strings$STR_BTN_SIZE, container = f8)

  f8_height_btn <- gbutton(text = strings$STR_BTN_HEIGHT, container = f8)

  f8_data_btn <- gbutton(text = strings$STR_BTN_POINT, container = f8)

  addHandlerChanged(f8_size_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Size")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f8_size_btn) <- FALSE
      .plot(what = "Size", how = "boxplot")
      enabled(f8_size_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
      enabled(f5_save_btn) <- TRUE
    }
  })

  addHandlerChanged(f8_height_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Height")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.
      enabled(f8_height_btn) <- FALSE
      .plot(what = "Height", how = "boxplot")
      enabled(f8_height_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
      enabled(f5_save_btn) <- TRUE
    }
  })

  addHandlerChanged(f8_data_btn, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable for plot.
    requiredCol <- c("Marker", "Allele", "Data.Point")

    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      enabled(f8_data_btn) <- FALSE
      .plot(what = "Data.Point", how = "boxplot")
      enabled(f8_data_btn) <- TRUE

      # Change save button.
      svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
      enabled(f5_save_btn) <- TRUE
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
  grid2[1, 2] <- shape_spb <- gspinbutton(
    from = 0, to = 25,
    by = 1, value = 18,
    container = grid2
  )

  grid2[1, 3] <- glabel(text = strings$STR_LBL_ALPHA, container = grid2)
  grid2[1, 4] <- alpha_spb <- gspinbutton(
    from = 0, to = 1,
    by = 0.01, value = 0.60,
    container = grid2
  )

  grid2[1, 5] <- glabel(text = strings$STR_LBL_COLOR, container = grid2)
  grid2[1, 6] <- colour_drp <- gcombobox(
    items = c("white", palette()),
    selected = 2,
    container = grid2,
    ellipsize = "none"
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
  grid3[2, 1] <- y_min_txt <- gedit(text = "", width = 5, container = grid3)
  grid3[2, 2] <- y_max_txt <- gedit(text = "", width = 5, container = grid3)

  grid3[3, 1:2] <- glabel(text = strings$STR_LBL_LIMIT_X, container = grid3)
  grid3[4, 1] <- x_min_txt <- gedit(text = "", width = 5, container = grid3)
  grid3[4, 2] <- x_max_txt <- gedit(text = "", width = 5, container = grid3)

  grid3[1, 3] <- glabel(text = "    ", container = grid3) # Add some space.

  grid3[1, 4] <- glabel(text = strings$STR_LBL_SCALES, container = grid3)
  grid3[2:4, 4] <- scales_opt <- gradio(
    items = .scales,
    selected = 2,
    horizontal = FALSE,
    container = grid3
  )

  # FRAME 4 ###################################################################

  labels_chk <- gcheckbox(
    text = strings$STR_CHK_OVERRIDE_LABELS,
    checked = FALSE,
    container = f1
  )

  addHandlerChanged(labels_chk, handler = function(h, ...) {
    # Enable buttons.
    .updateGui()
  })

  e4 <- gexpandgroup(
    text = strings$STR_EXP_X_LABELS,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e4) <- FALSE

  grid4 <- glayout(container = e4)

  grid4[1, 1] <- glabel(text = strings$STR_LBL_SIZE, container = grid4)
  grid4[1, 2] <- size_txt <- gedit(text = "8", width = 4, container = grid4)

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

  # FRAME 5 ###################################################################

  e5 <- gexpandgroup(
    text = strings$STR_EXP_Y_LABELS,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e5) <- FALSE

  grid5 <- glayout(container = e5)

  grid5[1, 1] <- glabel(text = strings$STR_LBL_SIZE, container = grid5)
  grid5[1, 2] <- size_txt_y <- gedit(text = "8", width = 4, container = grid5)

  grid5[1, 3] <- glabel(text = strings$STR_LBL_ANGLE, container = grid5)
  grid5[1, 4] <- angle_spb_y <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 0,
    container = grid5
  )

  grid5[2, 1] <- glabel(text = strings$STR_LBL_JUSTIFICATION, container = grid5)
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
    text = strings$STR_EXP_FACETS,
    horizontal = FALSE,
    container = f1
  )

  # Start collapsed.
  visible(e6) <- FALSE

  grid6 <- glayout(container = e6)

  grid6[1, 1] <- glabel(text = strings$STR_LBL_SIZE_X, container = grid6)
  grid6[1, 2] <- size_txt_sx <- gedit(text = "10", width = 4, container = grid6)

  grid6[1, 3] <- glabel(text = strings$STR_LBL_ANGLE, container = grid6)
  grid6[1, 4] <- angle_spb_sx <- gspinbutton(
    from = 0, to = 360, by = 1,
    value = 0,
    container = grid6
  )

  grid6[2, 1] <- glabel(text = strings$STR_LBL_SIZE_Y, container = grid6)
  grid6[2, 2] <- size_txt_sy <- gedit(text = "10", width = 4, container = grid6)

  grid6[2, 3] <- glabel(text = strings$STR_LBL_ANGLE, container = grid6)
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
        val_data <- add_color(
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
          mainTitle <- strings$STR_LBL_MAIN_TITLE_SIZE
          yTitle <- strings$STR_LBL_Y_TITLE_SIZE

          if (val_axis == "Mean") {
            xTitle <- strings$STR_LBL_X_TITLE_SIZE
          } else if (val_axis == "Allele") {
            xTitle <- strings$STR_LBL_X_TITLE_ALLELE
          } else {
            warning(paste("val_axis=", val_axis, "not implemented!"))
          }
        } else if (what == "Height") {
          mainTitle <- strings$STR_LBL_MAIN_TITLE_HEIGHT

          yTitle <- strings$STR_LBL_Y_TITLE_HEIGHT

          if (val_axis == "Mean") {
            xTitle <- strings$STR_LBL_X_TITLE_HEIGHT
          } else if (val_axis == "Allele") {
            xTitle <- strings$STR_LBL_X_TITLE_ALLELE
          } else {
            warning(paste("val_axis=", val_axis, "not implemented!"))
          }
        } else if (what == "Data.Point") {
          mainTitle <- strings$STR_LBL_MAIN_TITLE_POINT

          yTitle <- strings$STR_LBL_Y_TITLE_POINT

          if (val_axis == "Mean") {
            xTitle <- strings$STR_LBL_X_TITLE_POINTS
          } else if (val_axis == "Allele") {
            xTitle <- strings$STR_LBL_X_TITLE_ALLELE
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
        svalue(f5_save_btn) <- strings$STR_BTN_SAVE_OBJECT
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
        dyes <- add_color(dyes, have = "Color", need = "Dye")
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
  settings_prefix <- ".strvalidator_plotPrecision_gui_"
  settings_widgets <- list(
    title_chk = titles_chk,
    title = title_edt,
    x_title = x_title_edt,
    y_title = y_title_edt,
    title_size = title_size_txt,
    x_title_size = x_title_size_txt,
    y_title_size = y_title_size_txt,
    title_angle = title_angle_spb,
    x_title_angle = x_title_angle_spb,
    y_title_angle = y_title_angle_spb,
    title_vjust = title_vjust_spb,
    x_title_vjust = x_title_vjust_spb,
    y_title_vjust = y_title_vjust_spb,
    title_hjust = title_hjust_spb,
    x_title_hjust = x_title_hjust_spb,
    y_title_hjust = y_title_hjust_spb,
    points_shape = shape_spb,
    points_alpha = alpha_spb,
    points_colour = colour_drp,
    axes_y_min = y_min_txt,
    axes_y_max = y_max_txt,
    axes_x_min = x_min_txt,
    axes_x_max = x_max_txt,
    axes_scales = scales_opt,
    labels_chk = labels_chk,
    xlabel_size = size_txt,
    ylabel_size = size_txt_y,
    sxlabel_size = size_txt_sx,
    sylabel_size = size_txt_sy,
    xlabel_angle = angle_spb,
    ylabel_angle = angle_spb_y,
    sxlabel_angle = angle_spb_sx,
    sylabel_angle = angle_spb_sy,
    xlabel_justh = hjust_spb,
    xlabel_justv = vjust_spb,
    ylabel_justh = hjust_spb_y,
    ylabel_justv = vjust_spb_y,
    facet = f1_facet_chk,
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
