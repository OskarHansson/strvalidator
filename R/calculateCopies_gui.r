################################################################################
# CHANGE LOG (last 20 changes)
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.08.2016: Added save settings.
# 14.08.2016: Renamed to calculateCopies_gui and implementing new calculateCopies.
# 16.06.2016: 'Save as' textbox expandable.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.
# 24.05.2013: Improved error message for missing columns.
# 20.05.2013: First version.

#' @title Calculate Allele Copies
#'
#' @description
#' GUI wrapper for the \code{link{calculateCopies}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateCopies}} function by
#' providing a graphical user interface to it.
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
#' @importFrom utils help
#'
#' @seealso \code{\link{calculateCopies}}

calculateCopies_gui <- function(env = parent.frame(), savegui = NULL,
                                debug = FALSE, parent = NULL) {


  # Global variables.
  .gData <- NULL
  .gDataName <- NULL

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Calculate allele copies", visible = FALSE)

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

  savegui_chk <- gcheckbox(
    text = "Save GUI settings", checked = FALSE,
    container = gh
  )
  enabled(savegui_chk) <- FALSE

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("calculateCopies_gui", help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = "Datasets", horizontal = FALSE, spacing = 5,
    container = gv
  )

  f0g0 <- glayout(container = f0, spacing = 1)

  # Datasets ------------------------------------------------------------------

  f0g0[1, 1] <- glabel(text = "Select dataset:", container = f0g0)

  dataset_drp <- gcombobox(
    items = c(
      "<Select dataset>",
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1, editable = FALSE, container = f0g0,
    ellipsize = "none"
  )
  f0g0[1, 2] <- dataset_drp

  f0g0_samples_lbl <- glabel(text = " 0 samples", container = f0g0)
  f0g0[1, 3] <- f0g0_samples_lbl

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(f0g0_samples_lbl) <- paste("", samples, "samples")
      svalue(f2_save_edt) <- paste(val_obj, "_cop", sep = "")
    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = "Options", horizontal = FALSE,
    spacing = 10, container = gv
  )

  f1_note <- paste(
    "Note that the 'copies' and 'heterozygous' option are",
    "intended for known complete profiles, while 'observed'",
    "can be used for any samples to count the number of peaks."
  )

  gtext(text = f1_note, container = f1)

  f1_observed_chk <- gcheckbox(
    text = "Add number of unique alleles",
    checked = FALSE, container = f1
  )

  f1_copies_chk <- gcheckbox(
    text = "Add number of allele copies",
    checked = TRUE, container = f1
  )
  tooltip(f1_copies_chk) <- "Indicated by '1' for heterozygotes and '2' for homozygotes."

  f1_het_chk <- gcheckbox(
    text = "Add heterozygote indicator",
    checked = FALSE, container = f1
  )
  tooltip(f1_het_chk) <- "Indicated by '1' for heterozygous and '0' for homozygous loci."

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = "Save as", horizontal = TRUE, spacing = 5,
    container = gv
  )

  glabel(text = "Name for result:", container = f2)

  f2_save_edt <- gedit(text = "", expand = TRUE, container = f2, fill = TRUE)

  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = "Calculate", container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    val_name <- svalue(f2_save_edt)
    val_data <- .gData
    val_data_name <- .gDataName
    val_obs <- svalue(f1_observed_chk)
    val_cop <- svalue(f1_copies_chk)
    val_het <- svalue(f1_het_chk)

    if (!is.null(val_data)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- "Processing..."
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateCopies(
        data = val_data, observed = val_obs,
        copies = val_cop, heterozygous = val_het,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "observed", "copies", "heterozygous")

      values <- list(val_data_name, val_obs, val_cop, val_het)

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = "calculateCopies_gui", arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      message <- "A dataset has to be selected."

      gmessage(
        message = message, title = "Dataset not selected",
        icon = "error", parent = w
      )
    }
  })

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
      if (exists(".strvalidator_calculateCopies_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateCopies_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateCopies_gui_observed", envir = env, inherits = FALSE)) {
        svalue(f1_observed_chk) <- get(".strvalidator_calculateCopies_gui_observed", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_copies", envir = env, inherits = FALSE)) {
        svalue(f1_copies_chk) <- get(".strvalidator_calculateCopies_gui_copies", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_het", envir = env, inherits = FALSE)) {
        svalue(f1_het_chk) <- get(".strvalidator_calculateCopies_gui_het", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateCopies_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateCopies_gui_observed", value = svalue(f1_observed_chk), envir = env)
      assign(x = ".strvalidator_calculateCopies_gui_copies", value = svalue(f1_copies_chk), envir = env)
      assign(x = ".strvalidator_calculateCopies_gui_het", value = svalue(f1_het_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateCopies_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateCopies_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_observed", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateCopies_gui_observed", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_copies", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateCopies_gui_copies", envir = env)
      }
      if (exists(".strvalidator_calculateCopies_gui_het", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateCopies_gui_het", envir = env)
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
