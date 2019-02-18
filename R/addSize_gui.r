################################################################################
# CHANGE LOG (last 20 changes)
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 09.01.2016: Added attributes to result.
# 28.08.2015: Added importFrom
# 27.11.2014: Fixed bug (GitHub issue #7) introduced in strvalidator version 1.3.1.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 06.05.2014: Implemented 'checkDataset'.
# 02.03.2014: Added 'saveGUI' and 'bins' option.
# 11.02.2014: First version.


#' @title Add Size Information
#'
#' @description
#' GUI wrapper for the \code{\link{addSize}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{addSize}} function by providing a
#' graphical user interface to it.
#'
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help str head
#'
#' @seealso \code{\link{addSize}}

addSize_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gKit <- 1

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }


  # Main window.
  w <- gwindow(title = "Add size to dataset", visible = FALSE)

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
    spacing = 15,
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
    print(help("addSize_gui", help_type = "html"))
  })

  # DATASET ###################################################################

  f0 <- gframe(
    text = "Dataset and kit",
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f0g1 <- glayout(container = f0, spacing = 1)

  f0g1[1, 1] <- glabel(text = "Select dataset:", container = f0g1)

  f0g1[1, 2] <- dataset_drp <- gcombobox(
    items = c(
      "<Select dataset>",
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g1,
    ellipsize = "none"
  )

  f0g1[1, 3] <- dataset_samples_lbl <- glabel(
    text = " 0 samples",
    container = f0g1
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(dataset_samples_lbl) <- paste(" ", samples, "samples")
      .gKit <<- detectKit(.gData, index = TRUE)
      svalue(kit_drp, index = TRUE) <- .gKit
      svalue(f2_save_edt) <- paste(.gDataName, "_size", sep = "")

      if (debug) {
        print("Detected kit index")
        print(.gKit)
      }
    } else {

      # Reset components.
      .gData <<- data.frame(No.Data = NA)
      .gDataName <<- NULL
      svalue(dataset_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
    }
  })

  # KIT -----------------------------------------------------------------------

  f0g1[2, 1] <- glabel(text = "Kit:", container = f0g1)

  kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = f0g1,
    ellipsize = "none"
  )

  f0g1[2, 2] <- kit_drp

  # FRAME 1 ###################################################################

  # OPTIONS -----------------------------------------------------------------------

  f1 <- gframe(
    text = "Options",
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f1_items <- c(
    "Get size as defined in bins file (NA if not defined)",
    "Calculate an estimate from locus offset and number of repeats"
  )

  f1_size_opt <- gradio(items = f1_items, selected = 2, container = f1)

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = "Save as",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = "Name for result:", container = f2)

  f2_save_edt <- gedit(text = "", container = f2)

  # BUTTON ####################################################################

  if (debug) {
    print("BUTTON")
  }

  add_btn <- gbutton(text = "Add size", container = gv)

  addHandlerClicked(add_btn, handler = function(h, ...) {

    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_data_name <- .gDataName
    val_name <- svalue(f2_save_edt)
    val_bins <- svalue(f1_size_opt, index = TRUE) == 1 # TRUE / FALSE

    if (debug) {
      print("val_data:")
      print(str(val_data))
      print(head(val_data))
      print("val_kit")
      print(val_kit)
    }

    if (val_bins) {

      # Get kit with size information.
      val_kitinfo <- getKit(kit = val_kit, what = "Size")
    } else {

      # Get kit with offset and repeat information.
      val_kitinfo <- getKit(kit = val_kit, what = "Offset")
    }

    # Change button.
    blockHandlers(add_btn)
    svalue(add_btn) <- "Processing..."
    unblockHandlers(add_btn)
    enabled(add_btn) <- FALSE

    datanew <- addSize(
      data = val_data, kit = val_kitinfo,
      bins = val_bins, debug = debug
    )

    # Add attributes to result.
    attr(datanew, which = "kit") <- val_kit

    # Create key-value pairs to log.
    keys <- list("data", "kit", "bins")

    values <- list(val_data_name, val_kit, val_bins)

    # Update audit trail.
    datanew <- auditTrail(
      obj = datanew, key = keys, value = values,
      label = "addSize_gui", arguments = FALSE,
      package = "strvalidator"
    )

    # Save data.
    saveObject(name = val_name, object = datanew, parent = w, env = env)

    # Close GUI.
    .saveSettings()
    dispose(w)
  })

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
      if (exists(".strvalidator_addSize_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_addSize_gui_savegui", envir = env)
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
      if (exists(".strvalidator_addSize_gui_bins", envir = env, inherits = FALSE)) {
        svalue(f1_size_opt) <- get(".strvalidator_addSize_gui_bins", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_addSize_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_addSize_gui_bins", value = svalue(f1_size_opt), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_addSize_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addSize_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_addSize_gui_bins", envir = env, inherits = FALSE)) {
        remove(".strvalidator_addSize_gui_bins", envir = env)
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
