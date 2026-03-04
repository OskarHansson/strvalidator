
#' @title Calculate Bins Overlap
#'
#' @description
#' GUI wrapper for the \code{\link{calculateOverlap}} function.
#'
#' @details By analysis of the bins overlap between dye channels a measure of
#' the risk for spectral pull-up artefacts can be obtain. The default result
#' is a matrix with the total bins overlap in number of base pairs. If an allele
#' frequency database is provided the overlap at each bin is multiplied with the
#' frequency of the corresponding allele. If no frequence exist for that allele
#' a frequency of 5/2N will be used. X and Y alleles is given the frequency 1.
#' A scoring matrix can be supplied to reduce the effect by spectral distance,
#' meaning that overlap with the neighboring dye can be counted in full (100%)
#' while a non neighbor dye get its overlap reduced (to e.g. 10%).
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
#' @seealso \code{\link{calculateOverlap}}

calculate_overlap_gui <- function(env = parent.frame(), savegui = NULL, debug = TRUE, parent = NULL) {
  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Analyse bins overlap",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_KITS            = "Select kits",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_MULTIPLY        = "Multiply overlap with allele frequency",
    STR_CHK_VIRTUAL         = "Include virtual bins in analysis",
    STR_TIP_VIRTUAL         = "NB! Not all vendors specify which alleles are virtual in the bins file. This can be done manually in the kit.txt file.",
    STR_CHK_PENALTY         = "Apply spectral channel penalty",
    STR_FRM_PENALTY         = "Penalty",
    STR_LBL_PENALTY         = "Define penalty by the distance between dye channels  (1st neighbor to 5th neighbor)",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_COLOR           = "Kit color set must be identical for multiple kit comparison!\nAnalyse one kit at a time!",
    STR_MSG_KIT             = "At least one kit must be selected.",
    STR_MSG_TITLE_KIT       = "No kit selected",
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
    text = strings$STR_FRM_KITS,
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

  # Set initial minimal size.
  size(scroll_view) <- c(100, 150)

  kit_checkbox_group <- gcheckboxgroup(
    items = get_kit(),
    checked = FALSE,
    horizontal = FALSE,
    container = scroll_view
  )

  addHandlerChanged(kit_checkbox_group, handler = function(h, ...) {
    val_kits <- svalue(kit_checkbox_group)

    if (debug) {
      print("val_kits")
      print(val_kits)
    }

    # Initiate.
    kitColor <- list()

    # check if any selected kit.
    if (length(val_kits) > 0) {
      # Get selected kits.
      for (k in seq(along = val_kits)) {
        kit <- get_kit(val_kits[k], what = "Color")
        kitColor[k] <- list(unique(kit$Color))
      }

      # Check if identical.
      if (all(sapply(kitColor, identical, kitColor[[1]]))) {
        # Calculate number of penalty elements.
        elements <- length(kitColor[[1]]) - 1

        # Enable spin buttons, simple and effective...
        if (elements == 1) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- FALSE
          enabled(f2_penalty3_spb) <- FALSE
          enabled(f2_penalty4_spb) <- FALSE
          enabled(f2_penalty5_spb) <- FALSE
        }
        if (elements == 2) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- TRUE
          enabled(f2_penalty3_spb) <- FALSE
          enabled(f2_penalty4_spb) <- FALSE
          enabled(f2_penalty5_spb) <- FALSE
        }
        if (elements == 3) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- TRUE
          enabled(f2_penalty3_spb) <- TRUE
          enabled(f2_penalty4_spb) <- FALSE
          enabled(f2_penalty5_spb) <- FALSE
        }
        if (elements == 4) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- TRUE
          enabled(f2_penalty3_spb) <- TRUE
          enabled(f2_penalty4_spb) <- TRUE
          enabled(f2_penalty5_spb) <- FALSE
        }
        if (elements == 5) {
          enabled(f2_penalty1_spb) <- TRUE
          enabled(f2_penalty2_spb) <- TRUE
          enabled(f2_penalty3_spb) <- TRUE
          enabled(f2_penalty4_spb) <- TRUE
          enabled(f2_penalty5_spb) <- TRUE
        }

        # Enable analyse button.
        enabled(analyse_btn) <- TRUE

        # Suggest a save name.
        svalue(save_edt) <- paste(paste(val_kits, collapse = "_"),
          "_overlap",
          sep = ""
        )
      } else {
        # Show error message.
        gmessage(
          msg = strings$STR_MSG_COLOR,
          title = strings$STR_MSG_TITLE_ERROR,
          icon = "error", parent = w
        )

        # Disable analyse button.
        enabled(analyse_btn) <- FALSE
      }
    } else {
      # Disable all.
      enabled(f2_penalty1_spb) <- FALSE
      enabled(f2_penalty2_spb) <- FALSE
      enabled(f2_penalty3_spb) <- FALSE
      enabled(f2_penalty4_spb) <- FALSE
      enabled(f2_penalty5_spb) <- FALSE

      # Disable analyse button.
      enabled(analyse_btn) <- FALSE

      # Empty save name.
      svalue(save_edt) <- ""
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1_db_chk <- gcheckbox(
    text = strings$STR_CHK_MULTIPLY,
    checked = TRUE,
    container = f1
  )

  f1_db_names <- get_db()

  f1_db_drp <- gcombobox(
    items = f1_db_names, fill = FALSE, selected = 1,
    container = f1, ellipsize = "none"
  )

  f1_virtual_chk <- gcheckbox(
    text = strings$STR_CHK_VIRTUAL,
    checked = TRUE,
    container = f1
  )
  tooltip(f1_virtual_chk) <- strings$STR_TIP_VIRTUAL

  f1_penalty_chk <- gcheckbox(
    text = strings$STR_CHK_PENALTY,
    checked = TRUE,
    container = f1
  )

  # HANDLERS ------------------------------------------------------------------

  addHandlerChanged(f1_db_chk, handler = function(h, ...) {
    val_chk <- svalue(f1_db_chk)
    if (val_chk) {
      enabled(f1_db_drp) <- TRUE
    } else {
      enabled(f1_db_drp) <- FALSE
    }
  })

  addHandlerChanged(f1_penalty_chk, handler = function(h, ...) {
    val_chk <- svalue(f1_penalty_chk)
    if (val_chk) {
      enabled(f2) <- TRUE
    } else {
      enabled(f2) <- FALSE
    }
  })

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = strings$STR_FRM_PENALTY,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  glabel(
    text = strings$STR_LBL_PENALTY,
    container = f2
  )

  f2g1 <- ggroup(
    horizontal = TRUE,
    spacing = 1,
    container = f2
  )

  # Penalty vector elements.
  f2_penalty1_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 1, container = f2g1
  )
  f2_penalty2_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0.1, container = f2g1
  )
  f2_penalty3_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0.01, container = f2g1
  )
  f2_penalty4_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0, container = f2g1
  )
  f2_penalty5_spb <- gspinbutton(
    from = 0, to = 1, by = 0.001,
    value = 0, container = f2g1
  )

  # Disable all until a kit is selected.
  enabled(f2_penalty1_spb) <- FALSE
  enabled(f2_penalty2_spb) <- FALSE
  enabled(f2_penalty3_spb) <- FALSE
  enabled(f2_penalty4_spb) <- FALSE
  enabled(f2_penalty5_spb) <- FALSE

  # SAVE ######################################################################

  save_frame <- gframe(text = strings$STR_FRM_SAVE, container = gv)

  glabel(text = strings$STR_LBL_SAVE, container = save_frame)

  save_edt <- gedit(expand = TRUE, fill = TRUE, container = save_frame)

  # BUTTON ####################################################################

  analyse_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(analyse_btn, handler = function(h, ...) {
    val_name <- svalue(save_edt)
    val_kits <- svalue(kit_checkbox_group)
    val_kitData <- data.frame() # Filled further down.
    val_db_chk <- svalue(f1_db_chk)
    val_db_selected <- svalue(f1_db_drp)
    val_db <- NULL # Filled further down.
    val_virtual <- svalue(f1_virtual_chk)
    val_penalty_chk <- svalue(f1_penalty_chk)
    val_penalty <- NULL

    if (length(val_kits) > 0) {
      # Change button.
      blockHandlers(analyse_btn)
      svalue(analyse_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(analyse_btn)
      enabled(analyse_btn) <- FALSE

      # Get kits.
      for (k in seq(along = val_kits)) {
        tmp <- get_kit(val_kits[k])
        val_kitData <- rbind(val_kitData, tmp)
      }

      # Get allele frequency database.
      if (val_db_chk) {
        val_db <- get_db(val_db_selected)
      } else {
        val_db <- NULL
      }

      # Get penalty.
      if (val_penalty_chk) {
        # Simple but not so nice solution...
        if (enabled(f2_penalty1_spb)) {
          val_penalty[1] <- svalue(f2_penalty1_spb)
        }
        if (enabled(f2_penalty2_spb)) {
          val_penalty[2] <- svalue(f2_penalty2_spb)
        }
        if (enabled(f2_penalty3_spb)) {
          val_penalty[3] <- svalue(f2_penalty3_spb)
        }
        if (enabled(f2_penalty4_spb)) {
          val_penalty[4] <- svalue(f2_penalty4_spb)
        }
        if (enabled(f2_penalty5_spb)) {
          val_penalty[5] <- svalue(f2_penalty5_spb)
        }
      } else {
        val_penalty <- NULL
      }

      if (debug) {
        print("val_kits")
        print(val_kits)
        print("val_db_chk")
        print(val_db_chk)
        print("val_penalty_chk")
        print(val_penalty_chk)
        print("val_penalty")
        print(val_penalty)
        print("val_virtual")
        print(val_virtual)
      }

      # Analyse bins overlap.
      datanew <- calculate_overlap(
        data = val_kitData,
        db = val_db,
        penalty = val_penalty,
        virtual = val_virtual,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("data", "db", "penalty", "virtual")

      values <- list(val_kits, val_db_selected, val_penalty, val_virtual)

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )



      # Save data.
      save_object(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(datanew)
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        msg = strings$STR_MSG_KIT,
        title = strings$STR_MSG_TITLE_KIT,
        icon = "error",
        parent = w
      )
    }
  })


  # INTERNAL FUNCTIONS ########################################################

  settings_prefix <- ".strvalidator_calculateOverlap_gui_"
  settings_widgets <- list(
    db = f1_db_chk,
    db_name = f1_db_drp,
    virtual = f1_virtual_chk,
    penalty = f1_penalty_chk,
    kit_p1 = f2_penalty1_spb,
    kit_p2 = f2_penalty2_spb,
    kit_p3 = f2_penalty3_spb,
    kit_p4 = f2_penalty4_spb,
    kit_p5 = f2_penalty5_spb
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
