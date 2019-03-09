################################################################################
# CHANGE LOG (last 20 changes)
# 24.02.2019: Adjusted plot button.
# 19.02.2019: Expand text field under tcltk. Scrollable checkbox view.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 25.07.2018: Fixed x title and size not saved.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Added temporary fix for issue #93: https://github.com/jverzani/gWidgets2/issues/93#issue-241974596
# 07.07.2017: Removed argument 'border' for 'gbutton'
# 11.11.2015: Added importFrom ggplot2.
# 29.08.2015: Added importFrom.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 20.01.2014: Implemented ggsave with workaround for complex plots.
# 23.10.2013: Added save as image.
# 21.09.2013: First gui version.

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

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Plot kit", visible = FALSE)

  # Runs when window is closed.
  addHandlerUnrealize(w, handler = function(h, ...) {

    # Save GUI state.
    .saveSettings()

    # Focus on parent window.
    if (!is.null(parent)) {
      focus(parent)
    }

    # Check which toolkit we are using.
    if (gtoolkit() == "tcltk") {
      if (as.numeric(gsub("[^0-9]", "", packageVersion("gWidgets2tcltk"))) <= 106) {
        # Version <= 1.0.6 have the wrong implementation:
        # See: https://stackoverflow.com/questions/54285836/how-to-retrieve-checkbox-state-in-gwidgets2tcltk-works-in-gwidgets2rgtk2
        message("tcltk version <= 1.0.6, returned TRUE!")
        return(TRUE) # Destroys window under tcltk, but not RGtk2.
      } else {
        # Version > 1.0.6 will be fixed:
        # https://github.com/jverzani/gWidgets2tcltk/commit/9388900afc57454b6521b00a187ca4a16829df53
        message("tcltk version >1.0.6, returned FALSE!")
        return(FALSE) # Destroys window under tcltk, but not RGtk2.
      }
    } else {
      message("RGtk2, returned FALSE!")
      return(FALSE) # Destroys window under RGtk2, but not with tcltk.
    }
  })

  # Vertical main group.
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 8,
    use.scrollwindow = FALSE,
    container = w,
    expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("plotKit_gui", help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = "Select kits",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  scroll_view <- ggroup(
    horizontal = FALSE,
    use.scrollwindow = TRUE,
    container = f0,
    expand = TRUE,
    fill = TRUE
  )

  kit_checkbox_group <- gcheckboxgroup(
    items = getKit(),
    checked = FALSE,
    horizontal = FALSE,
    container = scroll_view
  )


  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = "Options",
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(text = "Plot title:", container = f1g1)
  f1g1[1, 2] <- title_edt <- gedit(
    text = "Marker size range",
    width = 40,
    container = f1g1
  )
  f1g1[1, 3] <- glabel(text = "Size:", container = f1g1)
  f1g1[1, 4] <- title_size_edt <- gedit(
    text = "20",
    width = 4,
    container = f1g1
  )

  f1g1[2, 1] <- glabel(text = "X title:", container = f1g1)
  f1g1[2, 2] <- x_title_edt <- gedit(
    text = "Size (bp)",
    container = f1g1
  )

  f1g1[2, 3] <- glabel(text = "Size:", container = f1g1)
  f1g1[2, 4] <- x_title_size_edt <- gedit(
    text = "10",
    width = 4,
    container = f1g1
  )

  f1g2 <- glayout(container = f1, spacing = 1)

  f1g2[1, 1] <- glabel(text = "Kit name size:", container = f1g2)
  f1g2[1, 2] <- kit_size_edt <- gedit(
    text = "4",
    width = 4,
    container = f1g2
  )

  f1g2[2, 1] <- glabel(text = "Inter kit spacing:", container = f1g2)
  f1g2[2, 2] <- kit_spacing_spb <- gspinbutton(
    from = 1, to = 10, by = 1,
    value = 2,
    container = f1g2
  )

  f1g2[3, 1] <- glabel(text = "Marker name size:", container = f1g2)
  f1g2[3, 2] <- marker_size_edt <- gedit(
    text = "3",
    width = 4,
    container = f1g2
  )

  f1g2[4, 1] <- glabel(text = "Marker height:", container = f1g2)
  f1g2[4, 2] <- marker_hight_spb <- gspinbutton(
    from = 0.1, to = 0.5, by = 0.1,
    value = 0.5,
    container = f1g2
  )

  f1g2[5, 1] <- glabel(text = "Marker range alpha:", container = f1g2)
  f1g2[5, 2] <- marker_alpha_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 1,
    container = f1g2
  )


  # BUTTON ####################################################################

  plot_btn <- gbutton(text = "Plot", container = gv)
  tooltip(plot_btn) <- "Plot marker ranges for kit"

  addHandlerClicked(plot_btn, handler = function(h, ...) {
    val_name <- svalue(plot_btn)
    val_kits <- svalue(kit_checkbox_group)

    if (debug) {
      print("val_kits")
      print(val_kits)
    }

    # Change button.
    blockHandlers(plot_btn)
    svalue(plot_btn) <- "Processing..."
    unblockHandlers(plot_btn)
    enabled(plot_btn) <- FALSE

    # Plot data.
    .plotKit(selectedKits = val_kits)

    # Change button.
    blockHandlers(plot_btn)
    svalue(plot_btn) <- "Plot"
    unblockHandlers(plot_btn)
    enabled(plot_btn) <- TRUE
  })

  # FRAME 5 ###################################################################

  f5 <- gframe(
    text = "Save as",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = "Name for result:", container = f5)

  f5_save_edt <- gedit(text = "_ggplot", container = f5, expand = TRUE, fill = TRUE)

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


    if (!is.na(selectedKits) && !is.null(selectedKits)) {

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
        msg = "Data frame is NULL or NA!",
        title = "Error",
        icon = "error"
      )
    }
  }

  # INTERNAL FUNCTIONS ########################################################

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
      if (exists(".strvalidator_plotKit_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_plotKit_gui_savegui", envir = env)
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
      if (exists(".strvalidator_plotKit_gui_title", envir = env, inherits = FALSE)) {
        svalue(title_edt) <- get(".strvalidator_plotKit_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_title_size", envir = env, inherits = FALSE)) {
        svalue(title_size_edt) <- get(".strvalidator_plotKit_gui_title_size", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_x_title", envir = env, inherits = FALSE)) {
        svalue(x_title_edt) <- get(".strvalidator_plotKit_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_title_size", envir = env, inherits = FALSE)) {
        svalue(x_title_size_edt) <- get(".strvalidator_plotKit_gui_x_title_size", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_kit_size", envir = env, inherits = FALSE)) {
        svalue(kit_size_edt) <- get(".strvalidator_plotKit_gui_kit_size", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_kit_spacing", envir = env, inherits = FALSE)) {
        svalue(kit_spacing_spb) <- get(".strvalidator_plotKit_gui_kit_spacing", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_marker_size", envir = env, inherits = FALSE)) {
        svalue(marker_size_edt) <- get(".strvalidator_plotKit_gui_marker_size", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_marker_hight", envir = env, inherits = FALSE)) {
        svalue(marker_hight_spb) <- get(".strvalidator_plotKit_gui_marker_hight", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_marker_alpha", envir = env, inherits = FALSE)) {
        svalue(marker_alpha_spb) <- get(".strvalidator_plotKit_gui_marker_alpha", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_plotKit_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_plotKit_gui_title", value = svalue(title_edt), envir = env)
      assign(x = ".strvalidator_plotKit_gui_title_size", value = svalue(title_size_edt), envir = env)
      assign(x = ".strvalidator_plotKit_gui_x_title", value = svalue(x_title_edt), envir = env)
      assign(x = ".strvalidator_plotKit_gui_x_title_size", value = svalue(x_title_size_edt), envir = env)
      assign(x = ".strvalidator_plotKit_gui_kit_size", value = svalue(kit_size_edt), envir = env)
      assign(x = ".strvalidator_plotKit_gui_kit_spacing", value = svalue(kit_spacing_spb), envir = env)
      assign(x = ".strvalidator_plotKit_gui_marker_size", value = svalue(marker_size_edt), envir = env)
      assign(x = ".strvalidator_plotKit_gui_marker_hight", value = svalue(marker_hight_spb), envir = env)
      assign(x = ".strvalidator_plotKit_gui_marker_alpha", value = svalue(marker_alpha_spb), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_plotKit_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_title", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_title_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_title_size", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_x_title", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_x_title", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_x_title_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_x_title_size", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_kit_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_kit_size", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_kit_spacing", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_kit_spacing", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_marker_size", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_marker_size", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_marker_hight", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_marker_hight", envir = env)
      }
      if (exists(".strvalidator_plotKit_gui_marker_alpha", envir = env, inherits = FALSE)) {
        remove(".strvalidator_plotKit_gui_marker_alpha", envir = env)
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
