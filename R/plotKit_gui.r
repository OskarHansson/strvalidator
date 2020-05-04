################################################################################
# CHANGE LOG (last 20 changes)
# 26.04.2020: Added language support.
# 26.04.2020: Fixed bug when no kit selected.
# 04.08.2019: Expand scrollable checkbox view.
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

  # Language ------------------------------------------------------------------
  
  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])
  
  if (debug) {
    print(paste("IN:", fnc))
  }
  
  # Default strings.
  strWinTitle <- "Plot kit"
  strChkGui <- "Save GUI settings"
  strBtnHelp <- "Help"
  strFrmKit <- "Select kits"
  strFrmOptions <- "Options"
  strLblTitlePlot <- "Plot title:"
  strLblTitleX <- "X title:"
  strLblSize <- "Size:"
  strLblKitSize <- "Kit name size:"
  strLblKitSpacing <- "Spacing between kits:"
  strLblMarkerSize <- "Marker name size:"
  strLblMarkerHeight <- "Marker height:"
  strLblMarkerAlpha <- "Marker transparency:"
  strTipMarker <- "Marker range fill color alpha"
  strBtnPlot <- "Plot"
  strTipPlot <- "Plot marker ranges for kit"
  strBtnProcessing <- "Processing..."
  strFrmSave <- "Save as"
  strLblSave <- "Name for result:"
  strBtnSaveObject <- "Save as object"
  strBtnSaveImage <- "Save as image"
  strBtnObjectSaved <- "Object saved"
  strLblMainTitle <- "Marker size range"
  strLblXTitle <- "Size (bp)"
  strMsgNull <- "At least one kit must be selected!"
  strMsgTitleError <- "Error"
  
  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)
  
  # If language file is found.
  if (!is.na(dtStrings)) {
    # Get language strings, use default if not found.

    strTmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)
    
    strTmp <- dtStrings["strChkGui"]$value
    strChkGui <- ifelse(is.na(strtmp), strChkGui, strtmp)
    
    strTmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)
    
    strTmp <- dtStrings["strFrmKit"]$value
    strFrmKit <- ifelse(is.na(strtmp), strFrmKit, strtmp)
    
    strTmp <- dtStrings["strFrmOptions"]$value
    strFrmOptions <- ifelse(is.na(strtmp), strFrmOptions, strtmp)
    
    strTmp <- dtStrings["strLblTitlePlot"]$value
    strLblTitlePlot <- ifelse(is.na(strtmp), strLblTitlePlot, strtmp)
    
    strTmp <- dtStrings["strLblTitleX"]$value
    strLblTitleX <- ifelse(is.na(strtmp), strLblTitleX, strtmp)
    
    strTmp <- dtStrings["strLblSize"]$value
    strLblSize <- ifelse(is.na(strtmp), strLblSize, strtmp)
    
    strTmp <- dtStrings["strLblKitSize"]$value
    strLblKitSize <- ifelse(is.na(strtmp), strLblKitSize, strtmp)
    
    strTmp <- dtStrings["strLblKitSpacing"]$value
    strLblKitSpacing <- ifelse(is.na(strtmp), strLblKitSpacing, strtmp)
    
    strTmp <- dtStrings["strLblMarkerSize"]$value
    strLblMarkerSize <- ifelse(is.na(strtmp), strLblMarkerSize, strtmp)
    
    strTmp <- dtStrings["strLblMarkerHeight"]$value
    strLblMarkerHeight <- ifelse(is.na(strtmp), strLblMarkerHeight, strtmp)
    
    strTmp <- dtStrings["strLblMarkerAlpha"]$value
    strLblMarkerAlpha <- ifelse(is.na(strtmp), strLblMarkerAlpha, strtmp)
    
    strTmp <- dtStrings["strTipMarker"]$value
    strTipMarker <- ifelse(is.na(strtmp), strTipMarker, strtmp)
    
    strTmp <- dtStrings["strBtnPlot"]$value
    strBtnPlot <- ifelse(is.na(strtmp), strBtnPlot, strtmp)
    
    strTmp <- dtStrings["strTipPlot"]$value
    strTipPlot <- ifelse(is.na(strtmp), strTipPlot, strtmp)
    
    strTmp <- dtStrings["strBtnProcessing"]$value
    strBtnProcessing <- ifelse(is.na(strtmp), strBtnProcessing, strtmp)
    
    strTmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)
    
    strTmp <- dtStrings["strLblSave"]$value
    strLblSave <- ifelse(is.na(strtmp), strLblSave, strtmp)
    
    strTmp <- dtStrings["strBtnSaveObject"]$value
    strBtnSaveObject <- ifelse(is.na(strtmp), strBtnSaveObject, strtmp)
    
    strTmp <- dtStrings["strBtnSaveImage"]$value
    strBtnSaveImage <- ifelse(is.na(strtmp), strBtnSaveImage, strtmp)
    
    strTmp <- dtStrings["strBtnObjectSaved"]$value
    strBtnObjectSaved <- ifelse(is.na(strtmp), strBtnObjectSaved, strtmp)
    
    strTmp <- dtStrings["strLblMainTitle"]$value
    strLblMainTitle <- ifelse(is.na(strtmp), strLblMainTitle, strtmp)
    
    strTmp <- dtStrings["strLblXTitle"]$value
    strLblXTitle <- ifelse(is.na(strtmp), strLblXTitle, strtmp)
    
    strTmp <- dtStrings["strMsgNull"]$value
    strMsgNull <- ifelse(is.na(strtmp), strMsgNull, strtmp)
    
    strTmp <- dtStrings["strMsgTitleError"]$value
    strMsgTitleError <- ifelse(is.na(strtmp), strMsgTitleError, strtmp)
  }
  
  # WINDOW ####################################################################
  
  # Main window.
  w <- gwindow(title = strWinTitle, visible = FALSE)

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

  savegui_chk <- gcheckbox(text = strChkGui, checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmKit,
    horizontal = TRUE,
    spacing = 5,
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

  kit_checkbox_group <- gcheckboxgroup(
    items = getKit(),
    checked = FALSE,
    horizontal = FALSE,
    container = scroll_view
  )


  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmOptions,
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(text = strLblTitlePlot, container = f1g1)
  f1g1[1, 2] <- title_edt <- gedit(
    text = strLblMainTitle,
    width = 40,
    container = f1g1
  )
  f1g1[1, 3] <- glabel(text = strLblSize, container = f1g1)
  f1g1[1, 4] <- title_size_edt <- gedit(
    text = "20",
    width = 4,
    container = f1g1
  )

  f1g1[2, 1] <- glabel(text = strLblTitleX, container = f1g1)
  f1g1[2, 2] <- x_title_edt <- gedit(
    text = strLblXTitle,
    container = f1g1
  )

  f1g1[2, 3] <- glabel(text = strLblSize, container = f1g1)
  f1g1[2, 4] <- x_title_size_edt <- gedit(
    text = "10",
    width = 4,
    container = f1g1
  )

  f1g2 <- glayout(container = f1, spacing = 1)

  f1g2[1, 1] <- glabel(text = strLblKitSize, container = f1g2)
  f1g2[1, 2] <- kit_size_edt <- gedit(
    text = "4",
    width = 4,
    container = f1g2
  )

  f1g2[2, 1] <- glabel(text = strLblKitSpacing, container = f1g2)
  f1g2[2, 2] <- kit_spacing_spb <- gspinbutton(
    from = 1, to = 10, by = 1,
    value = 2,
    container = f1g2
  )

  f1g2[3, 1] <- glabel(text = strLblMarkerSize, container = f1g2)
  f1g2[3, 2] <- marker_size_edt <- gedit(
    text = "3",
    width = 4,
    container = f1g2
  )

  f1g2[4, 1] <- glabel(text = strLblMarkerHeight, container = f1g2)
  f1g2[4, 2] <- marker_hight_spb <- gspinbutton(
    from = 0.1, to = 0.5, by = 0.1,
    value = 0.5,
    container = f1g2
  )

  f1g2[5, 1] <- glabel(text = strLblMarkerAlpha, container = f1g2)
  f1g2[5, 2] <- marker_alpha_spb <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    value = 1,
    container = f1g2
  )


  # BUTTON ####################################################################

  plot_btn <- gbutton(text = strBtnPlot, container = gv)
  tooltip(plot_btn) <- strTipPlot

  addHandlerClicked(plot_btn, handler = function(h, ...) {
    val_name <- svalue(plot_btn)
    val_kits <- svalue(kit_checkbox_group)

    if (debug) {
      print("val_kits")
      print(val_kits)
    }

    # Change button.
    blockHandlers(plot_btn)
    svalue(plot_btn) <- strBtnProcessing
    unblockHandlers(plot_btn)
    enabled(plot_btn) <- FALSE

    # Plot data.
    .plotKit(selectedKits = val_kits)

    # Change button.
    blockHandlers(plot_btn)
    svalue(plot_btn) <- strBtnPlot
    unblockHandlers(plot_btn)
    enabled(plot_btn) <- TRUE
  })

  # FRAME 5 ###################################################################

  f5 <- gframe(
    text = strFrmSave,
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = strLblSave, container = f5)

  f5_save_edt <- gedit(text = "_ggplot", container = f5, expand = TRUE, fill = TRUE)

  f5_save_btn <- gbutton(text = strBtnSaveObject, container = f5)

  f5_ggsave_btn <- gbutton(text = strBtnSaveImage, container = f5)

  addHandlerClicked(f5_save_btn, handler = function(h, ...) {
    val_name <- svalue(f5_save_edt)

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- strBtnProcessing
    unblockHandlers(f5_save_btn)
    enabled(f5_save_btn) <- FALSE

    # Save data.
    saveObject(
      name = val_name, object = .gPlot,
      parent = w, env = env, debug = debug
    )

    # Change button.
    blockHandlers(f5_save_btn)
    svalue(f5_save_btn) <- strBtnObjectSaved
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


    if (length(selectedKits)>0) {

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
        msg = strMsgNull,
        title = strMsgTitleError,
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
