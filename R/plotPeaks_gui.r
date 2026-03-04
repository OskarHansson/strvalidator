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

plot_peaks_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
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

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Plot peaks",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Dataset",
    STR_LBL_DATASET         = "Dataset:",
    STR_DRP_DATASET         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_OVERRIDE        = "Override automatic titles",
    STR_LBL_TITLE_PLOT      = "Plot title:",
    STR_LBL_TITLE_X         = "X title:",
    STR_LBL_TITLE_Y         = "Y title:",
    STR_CHK_PROPORTION      = "Plot proportion",
    STR_LBL_BASE_SIZE       = "Base font size (pts):",
    STR_LBL_PALETTE         = "Color palette:",
    STR_CHK_LABELS          = "Print value as bar labels",
    STR_LBL_LABEL_DECIMALS  = "Bar label decimals:",
    STR_LBL_LABEL_SIZE      = "Bar label font size (pts):",
    STR_BTN_PLOT            = "Plot",
    STR_BTN_PROCESSING      = "Processing...",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_SAVE_OBJECT     = "Save as object",
    STR_BTN_SAVE_IMAGE      = "Save as image",
    STR_BTN_OBJECT_SAVED    = "Object saved",
    STR_LBL_MAIN_TITLE      = "Analysis of peaks from",
    STR_LBL_X_TITLE         = "Group",
    STR_LBL_Y_TITLE_PROP    = "Proportion",
    STR_LBL_Y_TITLE_COUNT   = "Count",
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

    return(FALSE) # Destroy window.
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
    text = paste(" 0 ", strings$STR_LBL_SAMPLES, sep = ""),
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
    requiredCol <- c("Sample.Name", "Peaks", "Group", "Id")
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
        length(unique(.gData$Id)),
        " ", strings$STR_LBL_SAMPLES,
        sep = ""
      )

      # Enable buttons.
      enabled(plot_btn) <- TRUE
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(f5_save_edt) <- ""
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(samples_lbl) <- paste(" 0 ", strings$STR_LBL_SAMPLES, sep = "")
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


  f1_prop_chk <- gcheckbox(
    text = strings$STR_CHK_PROPORTION,
    checked = TRUE,
    container = f1
  )

  grid2 <- glayout(container = f1, spacing = 1)
  grid2[1, 1] <- glabel(text = strings$STR_LBL_BASE_SIZE, container = grid2)
  grid2[1, 2] <- f1_base_size_edt <- gedit(text = "18", width = 4, container = grid2)

  grid3 <- glayout(container = f1, spacing = 1)
  grid3[1, 1] <- glabel(text = strings$STR_LBL_PALETTE, container = grid3)
  grid3[1, 2] <- f1_palette_drp <- gcombobox(
    items = .palette,
    selected = 1,
    editable = FALSE,
    container = grid3,
    ellipsize = "none"
  )

  grid4 <- glayout(container = f1, spacing = 1)
  grid4[1, 1] <- f1_print_chk <- gcheckbox(
    text = strings$STR_CHK_LABELS, checked = TRUE,
    container = grid4
  )
  grid4[2, 1] <- glabel(text = strings$STR_LBL_LABEL_DECIMALS, container = grid4)
  grid4[2, 2] <- f1_decimal_spb <- gspinbutton(
    from = 0, to = 9, by = 1, value = 4,
    container = grid4
  )
  grid4[3, 1] <- glabel(text = strings$STR_LBL_LABEL_SIZE, container = grid4)
  grid4[3, 2] <- f1_lab_size_edt <- gedit(text = "5", width = 4, container = grid4)

  # FRAME 7 ###################################################################

  plot_btn <- gbutton(text = strings$STR_BTN_PLOT, container = gv)

  addHandlerChanged(plot_btn, handler = function(h, ...) {
    enabled(plot_btn) <- FALSE
    .plotBalance()
    enabled(plot_btn) <- TRUE
  })


  # FRAME 5 ###################################################################

  f5 <- gframe(
    text = strings$STR_FRM_SAVE,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

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
    if (is.data.frame(.gData)) {
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
        mainTitle <- paste(strings$STR_LBL_MAIN_TITLE, nrow(.gData), strings$STR_LBL_SAMPLES)
        xTitle <- strings$STR_LBL_X_TITLE
        if (val_prop) {
          yTitle <- strings$STR_LBL_Y_TITLE_PROP
        } else {
          yTitle <- strings$STR_LBL_Y_TITLE_COUNT
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
  settings_prefix <- ".strvalidator_plotPeaks_gui_"
  settings_widgets <- list(
    title_chk = titles_chk,
    title = title_edt,
    x_title = x_title_edt,
    y_title = y_title_edt,
    base_size = f1_base_size_edt,
    label_size = f1_lab_size_edt,
    print = f1_print_chk,
    prop = f1_prop_chk,
    palette = f1_palette_drp,
    decimal = f1_decimal_spb
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
