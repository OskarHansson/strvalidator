################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 25.07.2018: Added option to remove sex markers.
# 17.07.2018: First version.

#' @title Calculate Stochastic Thresholds
#'
#' @description
#' GUI wrapper to the \code{\link{calculateAllT}} function.
#'
#' @details
#' Convenience GUI for the use of \code{\link{calculateAllT}} to calculate
#' point estimates for the stochastic threshold using multiple models.
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
#' @importFrom utils head help
#'
#' @seealso \code{\link{calculateAllT}}

calculateAllT_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- data.frame(No.Data = NA)
  .gDataName <- NULL
  .gKit <- 1
  label_prefix <- "Calculate conservative point estimates at P(dropout>"
  label_suffix <- ")<"

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
    print(head(.gData))
  }


  w <- gwindow(title = "Calculate stochastic thresholds", visible = FALSE)

  # Runs when window is closed.
  addHandlerDestroy(w, handler = function(h, ...) {

    # Save GUI state.
    .saveSettings()

    # Focus on parent window.
    if (!is.null(parent)) {
      focus(parent)
    }
  })

  gv <- ggroup(
    horizontal = FALSE, spacing = 15, use.scrollwindow = FALSE,
    container = w, expand = TRUE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(
    text = "Save GUI settings",
    checked = FALSE, container = gh
  )

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("calculateAllT_gui", help_type = "html"))
  })

  # DATASET ###################################################################

  f0 <- gframe(
    text = "Dataset and kit", horizontal = FALSE,
    spacing = 5, container = gv
  )

  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1, 1] <- glabel(text = "Select dataset:", container = f0g0)

  f0g0[1, 2] <- dataset_drp <- gcombobox(
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

  f0g0[1, 3] <- dataset_samples_lbl <- glabel(
    text = " 0 samples",
    container = f0g0
  )

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c(
      "Sample.Name", "MethodX", "Method1", "Method2", "MethodL",
      "Height", "H", "MethodL.Ph"
    )
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
      svalue(f2_save_edt) <- paste(.gDataName, "_t", sep = "")

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

  f0g0[2, 1] <- glabel(text = "Kit:", container = f0g0)

  kit_drp <- gcombobox(
    items = getKit(), selected = 1, editable = FALSE,
    container = f0g0, ellipsize = "none"
  )

  f0g0[2, 2] <- kit_drp

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = "Options", horizontal = FALSE,
    spacing = 5, container = gv
  )
  f1g1 <- ggroup(container = f1, horizontal = TRUE)
  f1g2 <- ggroup(container = f1, horizontal = TRUE)

  glabel(text = "Calculate point estimates at P(dropout)=", container = f1g1)
  f1_p_dropout <- gspinbutton(
    from = 0, to = 1, by = 0.01,
    value = 0.01, container = f1g1
  )

  label_init <- svalue(f1_p_dropout)
  label_conservative <- glabel(
    text = paste(label_prefix, label_init,
      label_suffix,
      sep = ""
    ),
    container = f1g2
  )
  f1_p_conservative <- gspinbutton(
    from = 0, to = 1, by = 0.01,
    value = 0.05, container = f1g2
  )

  f1_sex_chk <- gcheckbox(
    text = "Remove sex markers", checked = TRUE,
    container = f1
  )

  addHandlerChanged(f1_p_dropout, handler = function(h, ...) {
    label_p <- svalue(f1_p_dropout)
    svalue(label_conservative) <- paste(label_prefix, label_p,
      label_suffix,
      sep = ""
    )
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(text = "Save as", horizontal = TRUE, spacing = 5, container = gv)

  glabel(text = "Name for result:", container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE)

  # BUTTON ####################################################################

  if (debug) {
    print("BUTTON")
  }

  calculate_btn <- gbutton(text = "Calculate", container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    # Get values.
    val_kit <- svalue(kit_drp)
    val_data <- .gData
    val_data_name <- .gDataName
    val_name <- svalue(f2_save_edt)
    val_p <- svalue(f1_p_dropout)
    val_pcons <- svalue(f1_p_conservative)
    val_sex <- svalue(f1_sex_chk)

    if (debug) {
      print("val_data")
      print(names(.gData))
      print("val_kit")
      print(val_kit)
      print("val_p")
      print(val_p)
      print("val_pcons")
      print(val_pcons)
      print("val_sex")
      print(val_sex)
    }

    # Change button.
    blockHandlers(calculate_btn)
    svalue(calculate_btn) <- "Processing..."
    unblockHandlers(calculate_btn)
    enabled(calculate_btn) <- FALSE

    # Calculate stochastic thresholds.
    datanew <- calculateAllT(
      data = val_data, kit = val_kit, p.dropout = val_p,
      p.conservative = val_pcons, rm.sex = val_sex,
      debug = debug
    )


    # Add attributes to result.
    attr(datanew, which = "kit") <- val_kit

    # Create key-value pairs to log.
    keys <- list("data", "kit", "p.dropout", "p.conservative", "rm.sex")

    values <- list(val_data_name, val_kit, val_p, val_pcons, val_sex)

    # Update audit trail.
    datanew <- auditTrail(
      obj = datanew, key = keys, value = values,
      label = "calculateAllT_gui", arguments = FALSE,
      package = "strvalidator"
    )

    # Save data.
    saveObject(name = val_name, object = datanew, parent = w, env = env)

    # Close GUI.
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
      if (exists(".strvalidator_calculateAllT_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateAllT_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateAllT_gui_pdrop", envir = env, inherits = FALSE)) {
        svalue(f1_p_dropout) <- get(".strvalidator_calculateAllT_gui_pdrop", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_pcons", envir = env, inherits = FALSE)) {
        svalue(f1_p_conservative) <- get(".strvalidator_calculateAllT_gui_pcons", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateAllT_gui_sex", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateAllT_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateAllT_gui_pdrop", value = svalue(f1_p_dropout), envir = env)
      assign(x = ".strvalidator_calculateAllT_gui_pcons", value = svalue(f1_p_conservative), envir = env)
      assign(x = ".strvalidator_calculateAllT_gui_sex", value = svalue(f1_sex_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateAllT_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllT_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_pdrop", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllT_gui_pdrop", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_pcons", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllT_gui_pcons", envir = env)
      }
      if (exists(".strvalidator_calculateAllT_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateAllT_gui_sex", envir = env)
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
