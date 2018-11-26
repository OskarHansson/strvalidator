################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 07.08.2017: Added audit trail.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 02.05.2016: First version.


#' @title Remove Artefact
#'
#' @description
#' GUI wrapper for the \code{\link{removeArtefact}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{removeArtefact}} function by providing a
#'  graphical user interface to it.
#'
#' @param env environment in which to search for data frames.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help
#'
#' @return TRUE


removeArtefact_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gArtefact <- NULL
  .gArtefactName <- NULL

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Remove artefacts", visible = FALSE)

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
  gv <- ggroup(
    horizontal = FALSE,
    spacing = 15,
    use.scrollwindow = FALSE,
    container = w,
    expand = FALSE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("removeArtefact_gui", help_type = "html"))
  })

  # DATASET ###################################################################

  f0 <- gframe(
    text = "Dataset",
    horizontal = FALSE,
    spacing = 10,
    container = gv
  )


  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1, 1] <- glabel(text = "Select dataset:", container = f0g0)

  f0g0[1, 2] <- f0g0_data_drp <- gcombobox(
    items = c(
      "<Select dataset>",
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none"
  )

  f0g0[1, 3] <- f0g0_data_col_lbl <- glabel(
    text = " 0 rows",
    container = f0g0
  )

  addHandlerChanged(f0g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_data_drp)

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

      svalue(f0g0_data_col_lbl) <- paste(" ", nrow(.gData), " rows")
      svalue(f2_name) <- paste(.gDataName, "no_artefacts", sep = "_")
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- " 0 rows"
      svalue(f2_name) <- ""
    }
  })

  f0g0[2, 1] <- glabel(text = "Select artefact list:", container = f0g0)

  f0g0[2, 2] <- f0g0_spike_drp <- gcombobox(
    items = c(
      "<Select dataset>",
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = f0g0,
    ellipsize = "none"
  )

  f0g0[2, 3] <- f0g0_spike_col_lbl <- glabel(
    text = " 0 rows",
    container = f0g0
  )

  addHandlerChanged(f0g0_spike_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_spike_drp)

    # Check if suitable.
    requiredCol <- c("Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gArtefact <<- get(val_obj, envir = env)
      .gArtefactName <<- val_obj

      svalue(f0g0_spike_col_lbl) <- paste(" ", nrow(.gArtefact), " rows")
    } else {
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(f0g0_data_col_lbl) <- " 0 rows"
    }
  })

  # OPTIONS ###################################################################

  f1 <- gframe(text = "Options", horizontal = FALSE, spacing = 10, container = gv)

  f1_na_chk <- gcheckbox(
    text = "Remove Allele=NA",
    checked = FALSE, container = f1
  )

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- glabel(text = "Artefact threshold: ", container = f1g1)
  f1g1[1, 2] <- f1_threshold_spn <- gspinbutton(
    from = 0, to = 1, by = 0.1,
    digits = 2, container = f1g1
  )

  # NAME ######################################################################

  f2 <- gframe(
    text = "Save as",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = "Save as:", container = f2)
  f2_name <- gedit(text = "", width = 40, container = f2, expand = TRUE)

  # BUTTON ####################################################################

  if (debug) {
    print("BUTTON")
  }

  remove_btn <- gbutton(text = "Remove", container = gv)

  addHandlerChanged(remove_btn, handler = function(h, ...) {
    val_data <- .gData
    val_artefact <- .gArtefact
    val_name_data <- .gDataName
    val_name_spike <- .gArtefactName
    val_name <- svalue(f2_name)
    val_na <- svalue(f1_na_chk)
    val_threshold <- svalue(f1_threshold_spn)

    if ((!is.na(val_data) && !is.null(val_data)) &
      (!is.na(val_artefact) && !is.null(val_artefact))) {
      datanew <- removeArtefact(
        data = val_data, artefact = val_artefact,
        na.rm = val_na, threshold = val_threshold,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "artefact", "na.rm", "threshold")

      values <- list(val_name_data, val_name_spike, val_na, val_threshold)

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = "removeArtefact_gui", arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }

      # Close GUI.
      dispose(w)
    } else {
      gmessage(
        msg = "Select a datasets!",
        title = "Error",
        icon = "error"
      )
    }
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
      if (exists(".strvalidator_removeArtefact_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_removeArtefact_gui_savegui", envir = env)
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
      if (exists(".strvalidator_removeArtefact_gui_na", envir = env, inherits = FALSE)) {
        svalue(f1_na_chk) <- get(".strvalidator_removeArtefact_gui_na", envir = env)
      }
      if (exists(".strvalidator_removeArtefact_gui_threshold", envir = env, inherits = FALSE)) {
        svalue(f1_threshold_spn) <- get(".strvalidator_removeArtefact_gui_threshold", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_removeArtefact_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_removeArtefact_gui_na", value = svalue(f1_na_chk), envir = env)
      assign(x = ".strvalidator_removeArtefact_gui_threshold", value = svalue(f1_threshold_spn), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_removeArtefact_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_removeArtefact_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_removeArtefact_gui_na", envir = env, inherits = FALSE)) {
        remove(".strvalidator_removeArtefact_gui_na", envir = env)
      }
      if (exists(".strvalidator_removeArtefact_gui_threshold", envir = env, inherits = FALSE)) {
        remove(".strvalidator_removeArtefact_gui_threshold", envir = env)
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
} # End of GUI
