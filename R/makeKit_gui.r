################################################################################
# CHANGE LOG (last 20 changes)
# 10.09.2022: Compacted the gui. Removed destroy workaround.
# 04.07.2020: Fixed spelling error in variables.
# 20.03.2020: Added language support.
# 14.03.2019: Fixed R-Check note.
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 17.07.2017: Fixed changed arguments for 'delete'.
# 13.07.2017: Fixed issue with button handlers.
# 07.07.2017: Removed argument 'border' for 'gbutton'
# 04.07.2016: Fixed bug removing space within marker names.
# 04.07.2016: Added support for quality sensors.
# 29.08.2015: Added importFrom.
# 14.12.2014: Fixed "Error in read.table..." when 'Save' is pressed without data.
# 14.12.2014: Fixed "Error in basename(x) : a character vector argument expected"
# 14.12.2014: Changed single gender marker to general sex markers (multiple).
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.07.2014: Changed some gui text.
# 28.06.2014: Added help button and moved save gui checkbox.
# 10.11.2013: Fixed check that short name is provided.
# 22.09.2013: Fixed duplicate check.
# 21.09.2013: Fixed correct gender marker when reading from file.
# 21.09.2013: Fixed no gender marker by putting 'NA' as option.

#' @title Make Kit
#'
#' @description
#' Add new kits or edit the kit file.
#'
#' @details A graphical user interface for reading information from 'bins' and
#' 'panels' file for the creation of additional kits. It is also possible to
#' edit the short and full name of existing kits or removing kits.
#' The gender marker of each kits is auto detected but can be changed manually.
#' #' NB! Short name must be unique.
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' [Not currently used]
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils write.table read.delim help head tail
#'
#' @seealso \code{\link{readBinsFile}}, \code{\link{readPanelsFile}}, \code{\link{combineBinsAndPanels}}

makeKit_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .f3g1 <- NULL
  .separator <- .Platform$file.sep # Platform dependent path separator.
  .packagePath <- path.package("strvalidator", quiet = FALSE)
  .subFolder <- "extdata"
  .fileName <- "kit.txt"
  .filePath <- paste(.packagePath, .subFolder, .fileName, sep = .separator)
  .newKitInfo <- NULL
  .kitInfo <- NULL
  .binsFiles <- NULL
  .panelsFiles <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  # Default strings.
  strWinTitle <- "Manage kits"
  strBtnHelp <- "Help"
  strFrmAction <- "Action"
  strRadEdit <- "Edit kit file"
  strRadAdd <- "Add new kits"
  strFrmFile <- "Path to current kit file"
  strBtnLoad <- "Load"
  strFrmNew <- "New kits"
  strBtnBins <- "Select Bins file"
  strBtnPanels <- "Select Panels file"
  strBtnCombine <- "Combine"
  strMsgFile <- "The kit file was not found"
  strMsgTitleFile <- "File not found"
  strLblSelectBins <- "Select Bins file..."
  strLblSelectPanels <- "Select Panels file..."
  strLblCombine <- "Reads and combines the Bins and Panels files"
  strMsgCombine <- "One or several files was not found"
  strLblSex <- "List sex markers (separate by comma)"
  strLblSensors <- "List quality sensors (separate by comma)"
  strLblRemove <- "Remove"
  strLblPanel <- "Panel"
  strLblShortName <- "Short name"
  strLblFullName <- "Full name"
  strLblNone <- "<none>"
  strFrmSave <- "Save"
  strRadAppend <- "Append to kit file"
  strRadOverwrite <- "Overwrite kit file"
  strRadSave <- "Save as data frame"
  strBtnSave <- "Save"
  strBtnSaving <- "Saving..."
  strMsgDuplicate1 <- "A kit with short name"
  strMsgDuplicate2 <- "already exist!\n\nShort name must be unique!"
  strMsgTitleDuplicate <- "Duplicate short name"
  strMsgMissing <- "A short name must be provided for all new kits"
  strMsgTitleMissing <- "Missing short name"

  # Get strings from language file.
  dtStrings <- getStrings(gui = fnc)

  # If language file is found.
  if (!is.null(dtStrings)) {
    # Get language strings, use default if not found.

    strtmp <- dtStrings["strWinTitle"]$value
    strWinTitle <- ifelse(is.na(strtmp), strWinTitle, strtmp)

    strtmp <- dtStrings["strBtnHelp"]$value
    strBtnHelp <- ifelse(is.na(strtmp), strBtnHelp, strtmp)

    strtmp <- dtStrings["strFrmAction"]$value
    strFrmAction <- ifelse(is.na(strtmp), strFrmAction, strtmp)

    strtmp <- dtStrings["strRadEdit"]$value
    strRadEdit <- ifelse(is.na(strtmp), strRadEdit, strtmp)

    strtmp <- dtStrings["strRadAdd"]$value
    strRadAdd <- ifelse(is.na(strtmp), strRadAdd, strtmp)

    strtmp <- dtStrings["strFrmFile"]$value
    strFrmFile <- ifelse(is.na(strtmp), strFrmFile, strtmp)

    strtmp <- dtStrings["strBtnLoad"]$value
    strBtnLoad <- ifelse(is.na(strtmp), strBtnLoad, strtmp)

    strtmp <- dtStrings["strFrmNew"]$value
    strFrmNew <- ifelse(is.na(strtmp), strFrmNew, strtmp)

    strtmp <- dtStrings["strBtnBins"]$value
    strBtnBins <- ifelse(is.na(strtmp), strBtnBins, strtmp)

    strtmp <- dtStrings["strBtnPanels"]$value
    strBtnPanels <- ifelse(is.na(strtmp), strBtnPanels, strtmp)

    strtmp <- dtStrings["strBtnCombine"]$value
    strBtnCombine <- ifelse(is.na(strtmp), strBtnCombine, strtmp)

    strtmp <- dtStrings["strMsgFile"]$value
    strMsgFile <- ifelse(is.na(strtmp), strMsgFile, strtmp)

    strtmp <- dtStrings["strMsgTitleFile"]$value
    strMsgTitleFile <- ifelse(is.na(strtmp), strMsgTitleFile, strtmp)

    strtmp <- dtStrings["strLblSelectBins"]$value
    strLblSelectBins <- ifelse(is.na(strtmp), strLblSelectBins, strtmp)

    strtmp <- dtStrings["strLblSelectPanels"]$value
    strLblSelectPanels <- ifelse(is.na(strtmp), strLblSelectPanels, strtmp)

    strtmp <- dtStrings["strLblCombine"]$value
    strLblCombine <- ifelse(is.na(strtmp), strLblCombine, strtmp)

    strtmp <- dtStrings["strMsgCombine"]$value
    strMsgCombine <- ifelse(is.na(strtmp), strMsgCombine, strtmp)

    strtmp <- dtStrings["strLblSex"]$value
    strLblSex <- ifelse(is.na(strtmp), strLblSex, strtmp)

    strtmp <- dtStrings["strLblSensors"]$value
    strLblSensors <- ifelse(is.na(strtmp), strLblSensors, strtmp)

    strtmp <- dtStrings["strLblRemove"]$value
    strLblRemove <- ifelse(is.na(strtmp), strLblRemove, strtmp)

    strtmp <- dtStrings["strLblPanel"]$value
    strLblPanel <- ifelse(is.na(strtmp), strLblPanel, strtmp)

    strtmp <- dtStrings["strLblShortName"]$value
    strLblShortName <- ifelse(is.na(strtmp), strLblShortName, strtmp)

    strtmp <- dtStrings["strLblFullName"]$value
    strLblFullName <- ifelse(is.na(strtmp), strLblFullName, strtmp)

    strtmp <- dtStrings["strLblNone"]$value
    strLblNone <- ifelse(is.na(strtmp), strLblNone, strtmp)

    strtmp <- dtStrings["strFrmSave"]$value
    strFrmSave <- ifelse(is.na(strtmp), strFrmSave, strtmp)

    strtmp <- dtStrings["strRadAppend"]$value
    strRadAppend <- ifelse(is.na(strtmp), strRadAppend, strtmp)

    strtmp <- dtStrings["strRadOverwrite"]$value
    strRadOverwrite <- ifelse(is.na(strtmp), strRadOverwrite, strtmp)

    strtmp <- dtStrings["strRadSave"]$value
    strRadSave <- ifelse(is.na(strtmp), strRadSave, strtmp)

    strtmp <- dtStrings["strBtnSave"]$value
    strBtnSave <- ifelse(is.na(strtmp), strBtnSave, strtmp)

    strtmp <- dtStrings["strBtnSaving"]$value
    strBtnSaving <- ifelse(is.na(strtmp), strBtnSaving, strtmp)

    strtmp <- dtStrings["strMsgDuplicate1"]$value
    strMsgDuplicate1 <- ifelse(is.na(strtmp), strMsgDuplicate1, strtmp)

    strtmp <- dtStrings["strMsgDuplicate2"]$value
    strMsgDuplicate2 <- ifelse(is.na(strtmp), strMsgDuplicate2, strtmp)

    strtmp <- dtStrings["strMsgTitleDuplicate"]$value
    strMsgTitleDuplicate <- ifelse(is.na(strtmp), strMsgTitleDuplicate, strtmp)

    strtmp <- dtStrings["strMsgMissing"]$value
    strMsgMissing <- ifelse(is.na(strtmp), strMsgMissing, strtmp)

    strtmp <- dtStrings["strMsgTitleMissing"]$value
    strMsgTitleMissing <- ifelse(is.na(strtmp), strMsgTitleMissing, strtmp)
  }

  # WINDOW ####################################################################

  if (debug) {
    print("File path")
    print(.filePath)
  }

  # Load existing kit file.
  if (!is.na(.filePath)) {
    if (file.exists(.filePath)) {
      # Read kit info file.
      .kitInfo <- read.delim(
        file = .filePath, header = TRUE,
        sep = "\t", quote = "\"",
        dec = ".", fill = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      gmessage(
        msg = "The kit file was not found",
        title = "File not found",
        icon = "error",
        parent = w
      )
    }
  }


  # Main window.
  w <- gwindow(title = "Manage kits", visible = FALSE)

  # Runs when window is closed.
  addHandlerUnrealize(w, handler = function(h, ...) {

    # Save GUI state.
    # .saveSettings()

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

  # No saved options yet!
  # savegui_chk <- gcheckbox(text="Save GUI settings", checked=FALSE, container=gh)

  addSpring(gh)

  help_btn <- gbutton(text = strBtnHelp, container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help(fnc, help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = strFrmAction,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  kit_opt <- gradio(
    items = c(strRadEdit, strRadAdd),
    selected = 2, container = f0
  )

  addHandlerChanged(kit_opt, handler = function(h, ...) {
    .updateGui()
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strFrmFile,
    horizontal = TRUE,
    spacing = 1,
    container = gv
  )

  # This is disabled by default.
  enabled(f1) <- FALSE

  file_edt <- gedit(
    text = .filePath, container = f1,
    expand = TRUE, fill = TRUE
  )
  file_btn <- gbutton(text = strBtnLoad, container = f1)

  addHandlerChanged(file_btn, handler = function(h, ...) {
    val_obj <- svalue(file_edt)

    # Disable options after loading the kit definition file.
    # This is because the GUI cannot handle adding widgets correct.
    enabled(kit_opt) <- FALSE

    if (debug) {
      print("Kit file:")
      print(val_obj)
    }

    if (!is.na(val_obj)) {
      if (file.exists(val_obj)) {

        # Read kit info file.
        .newKitInfo <<- read.delim(
          file = val_obj, header = TRUE,
          sep = "\t", quote = "\"",
          dec = ".", fill = TRUE,
          stringsAsFactors = FALSE
        )

        # Update GUI.
        .update(kitInfo = .newKitInfo, addKit = FALSE)
      } else {
        gmessage(
          msg = strMsgFile,
          title = strMsgTitleFile,
          icon = "error",
          parent = w
        )
      }
    }
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strFrmNew,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f2g1 <- glayout(container = f2, spacing = 1)

  # BINS ----------------------------------------------------------------------

  f2g1[1, 1] <- f2g1_bins_btn <- gbutton(
    text = strBtnBins,
    container = f2g1
  )

  f2g1[1, 2] <- f2g1_bins_lbl <- glabel(text = "", container = f2g1)

  addHandlerChanged(f2g1_bins_btn, handler = function(h, ...) {
    .binsFiles <<- gfile(
      text = strLblSelectBins,
      type = "open",
      filter = list(
        "text files" = list(mime.types = c("text/plain")),
        "All files" = list(patterns = c("*"))
      ),
      multi = FALSE
    )
    if (!is.na(.binsFiles)) {
      svalue(f2g1_bins_lbl) <- basename(.binsFiles)
    }
  })

  # PANELS --------------------------------------------------------------------

  f2g1[2, 1] <- f2g1_panels_btn <- gbutton(
    text = strBtnPanels,
    container = f2g1
  )

  f2g1[2, 2] <- f2g1_panels_lbl <- glabel(text = "", container = f2g1)

  addHandlerChanged(f2g1_panels_btn, handler = function(h, ...) {
    .panelsFiles <<- gfile(
      text = strLblSelectPanels,
      type = "open",
      filter = list(
        "text files" = list(mime.types = c("text/plain")),
        "All files" = list(patterns = c("*"))
      ),
      multi = FALSE
    )

    if (!is.na(.panelsFiles)) {
      svalue(f2g1_panels_lbl) <- basename(.panelsFiles)
    }
  })

  # READ AND COMBINE ----------------------------------------------------------

  f2g1[3, 1] <- f2g1_read_btn <- gbutton(
    text = strBtnCombine,
    container = f2g1
  )

  f2g1[3, 2] <- glabel(
    text = strLblCombine,
    container = f2g1
  )

  addHandlerChanged(f2g1_read_btn, handler = function(h, ...) {
    if (debug) {
      print("Bins file:")
      print(.binsFiles)
      print("Panels file:")
      print(.panelsFiles)
    }

    if (file.exists(.binsFiles) & file.exists(.panelsFiles)) {

      # Disable options after loading the kit definition file.
      # This is because the GUI cannot handle adding widgets correct.
      enabled(kit_opt) <- FALSE

      # Read and combine files.
      .newKitInfo <<- combineBinsAndPanels(
        bin = readBinsFile(.binsFiles),
        panel = readPanelsFile(.panelsFiles)
      )

      # Update GUI.
      .update(kitInfo = .newKitInfo, addKit = TRUE)
    } else {
      gmessage(
        msg = strMsgCombine,
        title = strMsgTitleFile,
        icon = "error",
        parent = w
      )
    }
  })

  # FRAME 3 ###################################################################

  f3 <- ggroup(
    horizontal = FALSE,
    use.scrollwindow = TRUE,
    expand = TRUE,
    fill = TRUE,
    spacing = 1,
    container = gv
  )

  # Set initial minimal size.
  size(f3) <- c(100, 150)

  # Function for updating GUI with kits.
  .update <- function(kitInfo, addKit) {
    # kitInfo - data.frame with kit information.
    # addKit - logical, if TRUE autodetect sex marker and quality sensor.
    #                   if FALSE read from file.

    if (debug) {
      print("head(kitInfo)")
      print(head(kitInfo))
    }

    # Get kits (short name and panel must be unique).
    if (addKit) {
      panel <- unique(kitInfo$Panel)
      shortName <- rep("", length(panel))
      fullName <- rep("", length(panel))
    } else {
      df <- unique(kitInfo[c("Panel", "Short.Name", "Full.Name")])
      panel <- df$Panel
      shortName <- df$Short.Name
      fullName <- df$Full.Name
    }

    # Clear (does not work as expected).
    if (!is.null(.f3g1)) {
      delete(obj = f3, child = .f3g1)
    }

    # Add container.
    .f3g1 <<- glayout(container = f3, spacing = 1, expand = TRUE, fill = "both")

    # Add titles.
    .f3g1[1, 1] <<- glabel(text = strLblRemove, container = .f3g1)
    .f3g1[1, 2] <<- glabel(text = strLblPanel, container = .f3g1)
    .f3g1[1, 3] <<- glabel(text = strLblShortName, container = .f3g1)
    .f3g1[1, 4] <<- glabel(text = strLblFullName, container = .f3g1)
    .f3g1[1, 5] <<- glabel(text = strLblSex, container = .f3g1)
    .f3g1[1, 6] <<- glabel(text = strLblSensors, container = .f3g1)

    # Loop over panel and add objects.
    for (p in seq(along = panel)) {

      # Get all markers.
      markers <- unique(kitInfo$Marker[kitInfo$Panel == panel[p]])

      # sex marker.
      sexMarkers <- NULL
      if (addKit) {
        # Try to autodetect sex marker.
        sexMarkers <- grep("AM|Y", markers, ignore.case = TRUE, value = TRUE)
        if (length(sexMarkers) == 0) {
          sexMarkers <- strLblNone
        }
      } else {
        # Get current sex marker.
        sexMarkers <- unique(kitInfo$Marker[kitInfo$Panel == panel[p] &
          kitInfo$Sex.Marker])
        if (length(sexMarkers) == 0) {
          # If no matching marker, set to no sex marker string.
          sexMarkers <- strLblNone
        }
      }
      # Collapse to string.
      sexMarkers <- paste(sexMarkers, collapse = ",")

      # Quality sensor.
      qsMarkers <- NULL
      if (addKit) {
        # Try to autodetect quality sensors.
        qsMarkers <- grep("QS", markers, ignore.case = TRUE, value = TRUE)
        if (length(qsMarkers) == 0) {
          qsMarkers <- strLblNone
        }
      } else {
        # Get current quality sensors.
        qsMarkers <- unique(kitInfo$Marker[kitInfo$Panel == panel[p] &
          kitInfo$Quality.Sensor])
        if (length(qsMarkers) == 0) {
          # If no matching marker, set to no qs marker string.
          qsMarkers <- strLblNone
        }
      }
      # Collapse to string.
      qsMarkers <- paste(qsMarkers, collapse = ",")

      if (debug) {
        print("addKit:")
        print(addKit)
        print("markers:")
        print(markers)
        print("sexMarkers:")
        print(sexMarkers)
        print("qsMarkers:")
        print(qsMarkers)
      }

      # Add widgets, and populate.
      .f3g1[p + 1, 1] <<- gcheckbox(text = "", checked = FALSE, container = .f3g1)
      .f3g1[p + 1, 2] <<- glabel(text = panel[p], container = .f3g1)
      .f3g1[p + 1, 3] <<- gedit(text = shortName[p], width = 20, container = .f3g1)
      .f3g1[p + 1, 4] <<- gedit(text = fullName[p], width = 40, container = .f3g1)
      .f3g1[p + 1, 5] <<- gedit(text = sexMarkers, width = 40, container = .f3g1)
      .f3g1[p + 1, 6] <<- gedit(text = qsMarkers, width = 20, container = .f3g1)
    }
  }

  # FRAME 4 ###################################################################

  f4 <- gframe(
    text = strFrmSave,
    horizontal = FALSE,
    expand = FALSE,
    spacing = 1,
    container = gv
  )

  # SAVE ----------------------------------------------------------------------

  save_opt <- gradio(
    items = c(strRadAppend, strRadOverwrite, strRadSave),
    selected = 1, horizontal = FALSE, container = f4
  )

  save_edt <- gedit(expand = TRUE, container = f4)
  enabled(save_edt) <- FALSE

  save_btn <- gbutton(text = strBtnSave, expand = FALSE, container = f4)


  addHandlerChanged(save_opt, handler = function(h, ...) {
    val_obj <- svalue(save_opt, index = TRUE)

    if (debug) {
      print(val_obj)
    }

    if (val_obj == 1) {

      # Disable.
      enabled(save_edt) <- FALSE

      # Update path.
      svalue(file_edt) <- .filePath
    } else if (val_obj == 2) {

      # Disable.
      enabled(save_edt) <- FALSE
    } else if (val_obj == 3) {

      # Enable.
      enabled(save_edt) <- TRUE
    }
  })

  addHandlerClicked(save_btn, handler = function(h, ...) {

    # Get variables.
    val_name <- svalue(save_edt)
    val_opt <- svalue(save_opt, index = TRUE)
    val_check <- svalue(kit_opt, index = TRUE)

    # Check if kit info exist.
    if (!is.null(.newKitInfo)) {

      # Initiate vectors.
      removeKit <- logical()
      shortName <- character()
      fullName <- character()
      sexMarkers <- 0
      qsMarkers <- 0

      # Get panels.
      panel <- unique(.newKitInfo$Panel)

      # Get data.
      for (p in seq(along = panel)) {
        removeKit[p] <- svalue(.f3g1[1 + p, 1])
        shortName[p] <- svalue(.f3g1[1 + p, 3])
        fullName[p] <- svalue(.f3g1[1 + p, 4])

        # Read sex marker value and replace any spaces.
        sexTmp <- svalue(.f3g1[1 + p, 5])
        # Replace comma plus one or more space with just comma.
        sexTmp <- gsub(",\\s+", ",", sexTmp)
        # Replace one or more space plus comma with just comma.
        sexTmp <- gsub("\\s+,", ",", sexTmp)
        sexMarkers[p] <- sexTmp

        # Read quality sensor value and replace any spaces.
        qsTmp <- svalue(.f3g1[1 + p, 6])
        # Replace comma plus one or more space with just comma.
        qsTmp <- gsub(",\\s+", ",", qsTmp)
        # Replace one or more space plus comma with just comma.
        qsTmp <- gsub("\\s+,", ",", qsTmp)
        qsMarkers[p] <- qsTmp
      }

      if (debug) {
        print("Short.Name:")
        print(shortName)
        print("Full.Name:")
        print(fullName)
        print("sexMarkers:")
        print(sexMarkers)
        print("qsMarkers:")
        print(qsMarkers)
      }

      # Check that short name is provided for all kits not removed.
      missing <- shortName[!removeKit] %in% ""

      # Check if short name is missing.
      if (!any(missing)) {
        if (val_opt == 1) { # Append (add new kits).
          # Check if short name exist in kit file.
          exist <- shortName[!removeKit] %in% getKit()
        } else { # Overwrite or save as data frame.
          # Check if short name exist in new kits.
          exist <- duplicated(toupper(shortName[!removeKit]))
        }

        # Check if short name exist.
        if (!any(exist)) {

          # Change button.
          blockHandlers(save_btn)
          svalue(save_btn) <- strBtnSaving
          unblockHandlers(save_btn)
          enabled(save_btn) <- FALSE

          for (p in seq(along = panel)) {
            if (debug) {
              print("Panel:")
              print(panel[p])
            }

            # Crete columns and add data to kits:
            # Set short name.
            if (is.null(.newKitInfo$Short.Name)) {
              .newKitInfo$Short.Name <<- NA
            }
            .newKitInfo$Short.Name[.newKitInfo$Panel == panel[p]] <<- shortName[p]

            # Set full name.
            if (is.null(.newKitInfo$Full.Name)) {
              .newKitInfo$Full.Name <<- NA
            }
            .newKitInfo$Full.Name[.newKitInfo$Panel == panel[p]] <<- fullName[p]

            # Set sex marker flag.
            if (is.null(.newKitInfo$Sex.Marker)) {
              .newKitInfo$Sex.Marker <<- NA # Create a new column.
            }
            currentSexMarkers <- unlist(strsplit(sexMarkers[p], split = ",", fixed = TRUE))
            currentSexMarkers <- currentSexMarkers[currentSexMarkers != strLblNone]
            selPanel <- .newKitInfo$Panel == panel[p]
            selMarker <- .newKitInfo$Marker %in% currentSexMarkers
            selection <- selPanel & selMarker
            .newKitInfo$Sex.Marker[selPanel] <<- FALSE # Reset current panel.
            .newKitInfo$Sex.Marker[selection] <<- TRUE # Flag sex markers.
            # Check for misspelled markers.
            ok <- currentSexMarkers %in% .newKitInfo$Marker[selPanel]
            if (!all(ok)) {
              stop(paste(
                "Given sex marker:",
                paste(currentSexMarkers[!ok], collapse = ","),
                "not found in", panel[p]
              ))
            }

            # Set quality sensor flag.
            if (is.null(.newKitInfo$Quality.Sensor)) {
              .newKitInfo$Quality.Sensor <<- NA # Create a new column.
            }
            currentQsMarkers <- unlist(strsplit(qsMarkers[p], split = ",", fixed = TRUE))
            currentQsMarkers <- currentQsMarkers[currentQsMarkers != strLblNone]
            selPanel <- .newKitInfo$Panel == panel[p]
            selMarker <- .newKitInfo$Marker %in% currentQsMarkers
            selection <- selPanel & selMarker
            .newKitInfo$Quality.Sensor[selPanel] <<- FALSE # Reset current panel.
            .newKitInfo$Quality.Sensor[selection] <<- TRUE # Flag qs markers.
            # Check for misspelled markers.
            ok <- currentQsMarkers %in% .newKitInfo$Marker[selPanel]
            if (!all(ok)) {
              stop(paste(
                "Given quality sensor:",
                paste(currentQsMarkers[!ok], collapse = ","),
                "not found in", panel[p]
              ))
            }
          }

          # Remove kits if any.
          if (any(removeKit)) {
            if (debug) {
              print("panel:")
              print(panel)
              print("removeKit:")
              print(removeKit)
            }

            removePanel <- panel[removeKit]

            if (debug) {
              print("removePanel:")
              print(removePanel)
            }

            for (p in seq(along = removePanel)) {
              .newKitInfo <<- .newKitInfo[.newKitInfo$Panel != removePanel[p], ]
            }
          }

          if (debug) {
            print("newKitInfo:")
            print(head(.newKitInfo))
            print(tail(.newKitInfo))
          }

          # If append.
          if (val_opt == 1) {
            if (debug) {
              print("Append:")
              print(head(.kitInfo))
              print(head(.newKitInfo))
            }
            # .newKitInfo <<- rbind(.kitInfo, .newKitInfo)
            write.table(
              x = .newKitInfo, file = .filePath,
              row.names = FALSE,
              append = TRUE,
              col.names = FALSE,
              quote = FALSE, sep = "\t"
            )
          } else if (val_opt == 2) {
            if (debug) {
              print("Save as kit file")
            }
            write.table(
              x = .newKitInfo, file = .filePath,
              row.names = FALSE,
              append = FALSE,
              col.names = TRUE,
              quote = FALSE, sep = "\t"
            )
          } else if (val_opt == 3) {
            if (debug) {
              print("Save as data frame")
            }
            assign(val_name, .newKitInfo, envir = env)
          }

          if (debug) {
            print(paste("EXIT:", fnc))
          }

          # Close GUI.
          # .saveSettings()
          dispose(w)
        } else {
          gmessage(
            msg = paste(
              strMsgDuplicate1,
              shortName[exist][1],
              strMsgDuplicate2
            ),
            title = strMsgTitleDuplicate,
            icon = "error",
            parent = w
          )
        }
      } else {
        gmessage(
          msg = strMsgMissing,
          title = strMsgTitleMissing,
          icon = "error",
          parent = w
        )
      }
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .updateGui <- function() {

    # Get option.
    val_obj <- svalue(kit_opt, index = TRUE)

    # Clear.
    if (!is.null(.f3g1)) {
      delete(obj = f3, child = .f3g1)
    }

    if (val_obj == 1) {

      # Enable 'edit' objects.
      enabled(f1) <- TRUE

      # Disable 'new' objects.
      enabled(f2) <- FALSE

      # Autoselect 'Overwrite'.
      svalue(save_opt, index = TRUE) <- 2

      # Update path.
      svalue(file_edt) <- .filePath
    } else {

      # Enable 'new' objects.
      enabled(f2) <- TRUE

      # Disable 'edit' objects.
      enabled(f1) <- FALSE

      # Autoselect 'Overwrite'.
      svalue(save_opt, index = TRUE) <- 1
    }
  }

  # Update gui.
  .updateGui()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
