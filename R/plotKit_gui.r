#' @title Plot Kit Marker Ranges
#'
#' @description
#' GUI for plotting marker ranges for kits.
#'
#' @details Create an overview of the size range for markers in different kits.
#' It is possible to select multiple kits, specify titles, font size, distance
#' between two kits, distance between dye channels, and the transparency of dyes.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help
#' @importFrom ggplot2 ggplot geom_rect aes_string geom_text scale_fill_manual
#'  scale_y_reverse theme element_blank labs element_text
#'

plotKit_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {
  # Global variables.
  .gPlot <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Plot kit",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_KIT             = "Select kits",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_TITLE_PLOT      = "Plot title:",
    STR_LBL_TITLE_X         = "X title:",
    STR_LBL_SIZE            = "Size:",
    STR_LBL_KIT_SIZE        = "Kit name size:",
    STR_LBL_KIT_SPACING     = "Spacing between kits:",
    STR_LBL_MARKER_SIZE     = "Marker name size:",
    STR_LBL_MARKER_HEIGHT   = "Marker height:",
    STR_LBL_MARKER_ALPHA    = "Marker transparency:",
    STR_TIP_MARKER          = "Marker range fill color alpha",
    STR_BTN_PLOT            = "Plot",
    STR_TIP_PLOT            = "Plot marker ranges for kit",
    STR_BTN_PROCESSING      = "Processing...",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_SAVE_OBJECT     = "Save as object",
    STR_BTN_SAVE_IMAGE      = "Save as image",
    STR_BTN_OBJECT_SAVED    = "Object saved",
    STR_LBL_MAIN_TITLE      = "Marker size range",
    STR_LBL_X_TITLE         = "Size (bp)",
    STR_MSG_NULL            = "At least one kit must be selected!",
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
    text = strings$STR_FRM_KIT,
    horizontal = TRUE,
    spacing = 1,
    container = gv,
    expand = TRUE,
    fill = TRUE
  )

  scroll_view <- ggroup(
    horizontal = FALSE,
    use.scrollwindow = TRUE,
    container = f0,
    expand = TRUE,
    fill = TRUE
  )

  # Set initial size.
  size(scroll_view) <- c(100, 150)

  kit_checkbox_group <- gcheckboxgroup(
    items = getKit(),
    checked = FALSE,
    horizontal = FALSE,
    container = scroll_view
  )


  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(text = strings$STR_LBL_TITLE_PLOT, container = f1g1)
  f1g1[1, 2] <- title_edt <- gedit(
    text = strings$STR_LBL_MAIN_TITLE,
    width = 40,
    container = f1g1
  )
  f1g1[1, 3] <- glabel(text = strings$STR_LBL_SIZE, container = f1g1)
  f1g1[1, 4] <- title_size_edt <- gedit(
    text = "20",
    width = 4,
    container = f1g1
  )

  f1g1[2, 1] <- glabel(text = strings$STR_LBL_TITLE_X, container = f1g1)
  f1g1[2, 2] <- x_title_edt <- gedit(
    text = strings$STR_LBL_X_TITLE,
    container = f1g1
  )

  f1g1[2, 3] <- glabel(text = strings$STR_LBL_SIZE, container = f1g1)
  f1g1[2, 4] <- x_title_size_edt <- gedit(
    text = "10",
    width = 4,
    container = f1g1
  )

  f1g2 <- glayout(container = f1, spacing = 1)

  f1g2[1, 1] <- glabel(text = strings$STR_LBL_KIT_SIZE, container = f1g2)
  f1g2[1, 2] <- kit_size_edt <- gedit(
    text = "4",
    width = 4,
    container = f1g2
  )

  f1g2[2, 1] <- glabel(text = strings$STR_LBL_KIT_SPACING, container = f1g2)
  f1g2[2, 2] <- kit_spacing_spb <- gspinbutton(
    from = 1, to = 10, by = 1,
    value = 2,
    container = f1g2
  )

  f1g2[3, 1] <- glabel(text = strings$STR_LBL_MARKER_SIZE, container = f1g2)
  f1g2[3, 2] <- marker_size_edt <- gedit(
    text = "3",
    width = 4,
    container = f1g2
  )

  f1g2[4, 1] <- glabel(text = strings$STR_LBL_MARKER_HEIGHT, container = f1g2)
  f1g2[4, 2] <- marker_hight_spb <- gspinbutton(
    from = 0.1, to = 0.5, by = 0.1,
    value = 0.5,
    container = f1g2
  )

  f1g2[5, 1] <- glabel(text = strings$STR_LBL_MARKER_ALPHA, container = f1g2)
  f1g2[5, 2] <- marker_alpha_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 1,
    container = f1g2
  )


  # BUTTON ####################################################################

  plot_btn <- gbutton(text = strings$STR_BTN_PLOT, container = gv)
  tooltip(plot_btn) <- strings$STR_TIP_PLOT

  addHandlerClicked(plot_btn, handler = function(h, ...) {
    val_name <- svalue(plot_btn)
    val_kits <- svalue(kit_checkbox_group)

    if (debug) {
      print("val_kits")
      print(val_kits)
    }

    # Change button.
    blockHandlers(plot_btn)
    svalue(plot_btn) <- strings$STR_BTN_PROCESSING
    unblockHandlers(plot_btn)
    enabled(plot_btn) <- FALSE

    # Plot data.
    .plotKit(selectedKits = val_kits)

    # Change button.
    blockHandlers(plot_btn)
    svalue(plot_btn) <- strings$STR_BTN_PLOT
    unblockHandlers(plot_btn)
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

  f5_save_edt <- gedit(text = "_ggplot", container = f5, expand = TRUE, fill = TRUE)

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

  # FUNCTIONS #################################################################


  .plotKit <- function(selectedKits = NULL) {
    # Get values.
    val_title <- svalue(title_edt)
    val_titlesize <- as.numeric(svalue(title_size_edt))
    val_xtitle <- svalue(x_title_edt)
    val_xtitlesize <- as.numeric(svalue(x_title_size_edt))
    val_kitnamesize <- as.numeric(svalue(kit_size_edt))
    val_kitspacing <- svalue(kit_spacing_spb)
    val_markernamesize <- as.numeric(svalue(marker_size_edt))
    val_markerheight <- svalue(marker_hight_spb)
    val_markeralpha <- svalue(marker_alpha_spb)

    if (debug) {
      print("selectedKits")
      print(selectedKits)
      print("val_title")
      print(val_title)
      print("val_titlesize")
      print(val_titlesize)
      print("val_xtitle")
      print(val_xtitle)
      print("val_xtitlesize")
      print(val_xtitlesize)
      print("val_kitnamesize")
      print(val_kitnamesize)
      print("val_kitspacing")
      print(val_kitspacing)
      print("val_markernamesize")
      print(val_markernamesize)
      print("val_markerheight")
      print(val_markerheight)
      print("val_markeralpha")
      print(val_markeralpha)
    }


    if (length(selectedKits) > 0) {
      # Initiate:
      kitData <- NULL
      kitTitle <- data.frame(Name = NA, X = NA, Y = NA)
      kitName <- data.frame(Name = NULL, X = NULL, Y = NULL)
      yMax <- 0 # To get the starting point for current kit.


      for (k in seq(along = selectedKits)) {
        # Get current kit.
        kit <- getKit(selectedKits[k], what = "Range")

        # Calculate text and rectangle coordinates.
        kit$Xtxt <- (kit$Marker.Min + kit$Marker.Max) / 2
        kit$Ytxt <- yMax + as.numeric(kit$Color) # Use factor levels.
        kit$Ymin <- kit$Ytxt - val_markerheight
        kit$Ymax <- kit$Ytxt + val_markerheight

        # Calculate inter kit spacing.
        yMax <- max(kit$Ytxt) + val_kitspacing



        kitData <- rbind(kitData, kit)

        # Get full name.
        kitTitle$Name <- getKit(selectedKits[k], what = "Full.Name")
        kitTitle$X <- 50
        kitTitle$Y <- min(kit$Ymin) - val_markerheight
        kitName <- rbind(kitName, kitTitle)
      }

      # Get plot fill colors as strings.
      plotColor <- as.character(kitData$Color)


      # Create plot.
      gp <- ggplot()
      gp <- gp + geom_rect(
        data = kitData,
        mapping = aes_string(xmin = "Marker.Min", xmax = "Marker.Max", ymin = "Ymin", ymax = "Ymax", fill = "Color"),
        color = "black", alpha = val_markeralpha
      )
      # Add marker names.
      gp <- gp + geom_text(data = kitData, aes_string(x = "Xtxt", y = "Ytxt", label = "Marker"), size = val_markernamesize)

      gp <- gp + geom_text(
        data = kitName, aes_string(x = "X", y = "Y", label = "Name"),
        size = val_kitnamesize,
        hjust = 0, vjust = 0
      )


      # Map fill colors.
      gp <- gp + scale_fill_manual(values = unique(plotColor))

      # Reverse y axis.
      gp <- gp + scale_y_reverse()

      # Remove legend.
      gp <- gp + theme(legend.position = "none")

      # Remove grid.
      gp <- gp + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

      # Remove y axis.
      gp <- gp + theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
      )

      # Add titles.
      gp <- gp + labs(
        title = val_title,
        x = val_xtitle
      )
      gp <- gp + theme(plot.title = element_text(size = val_titlesize))
      gp <- gp + theme(axis.title.x = element_text(size = val_xtitlesize))


      # Print plot.
      print(gp)

      # Store in global variable.
      .gPlot <<- gp
    } else {
      gmessage(
        msg = strings$STR_MSG_NULL,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    }
  }

  # INTERNAL FUNCTIONS ########################################################

  settings_prefix <- ".strvalidator_plotKit_gui_"
  settings_widgets <- list(
    title = title_edt,
    title_size = title_size_edt,
    x_title = x_title_edt,
    x_title_size = x_title_size_edt,
    kit_size = kit_size_edt,
    kit_spacing = kit_spacing_spb,
    marker_size = marker_size_edt,
    marker_hight = marker_hight_spb,
    marker_alpha = marker_alpha_spb
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

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
