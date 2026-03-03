
#' @title Analyze Off-ladder Alleles
#'
#' @description
#' GUI wrapper for the \code{\link{calculateOL}} function.
#'
#' @details By analysis of the allelic ladder the risk for getting off-ladder
#' (OL) alleles are calculated. The frequencies from a provided population
#' database is used to calculate the risk per marker and in total for the given
#' kit(s). Virtual alleles can be excluded from the calculation.
#' Small frequencies can be limited to the estimate 5/2N.
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
#' @importFrom utils help str
#'
#' @seealso \code{\link{calculateOL}}

calculateOL_gui <- function(env = parent.frame(), savegui = NULL, debug = TRUE, parent = NULL) {
  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Analyse off-ladder alleles",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_KITS            = "Select kits",
    STR_FRM_OPTIONS         = "Options",
    STR_LBL_DATABASE        = "Select allele frequency database:",
    STR_CHK_VIRTUAL         = "Include virtual bins in analysis",
    STR_TIP_VIRTUAL         = "NB! Not all vendors specify which alleles are virtual in the bins file. This can be done manually in the kit.txt file.",
    STR_CHK_LOW_FREQ        = "Limit small frequencies to 5/2N",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_MSG_KIT             = "At least one kit must be selected.",
    STR_MSG_TITLE_KIT       = "No kit selected"
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
    items = getKit(),
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

    # check if any selected kit.
    if (length(val_kits) > 0) {
      # Enable analyse button.
      enabled(analyse_btn) <- TRUE

      # Suggest a save name.
      svalue(save_edt) <- paste(paste(val_kits, collapse = "_"),
        "_OL",
        sep = ""
      )
    } else {
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

  glabel(
    text = strings$STR_LBL_DATABASE,
    anchor = c(-1, 0), container = f1
  )

  f1_db_names <- getDb()

  f1_db_drp <- gcombobox(
    items = f1_db_names, fill = FALSE,
    selected = 1, container = f1, ellipsize = "none"
  )

  f1_virtual_chk <- gcheckbox(
    text = strings$STR_CHK_VIRTUAL,
    checked = TRUE,
    container = f1
  )
  tooltip(f1_virtual_chk) <- strings$STR_TIP_VIRTUAL

  f1_limit_chk <- gcheckbox(
    text = strings$STR_CHK_LOW_FREQ,
    checked = TRUE,
    container = f1
  )

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
    val_db_selected <- svalue(f1_db_drp)
    val_db <- NULL # Filled further down.
    val_virtual <- svalue(f1_virtual_chk)
    val_limit <- svalue(f1_limit_chk)

    if (length(val_kits) > 0) {
      # Change button.
      blockHandlers(analyse_btn)
      svalue(analyse_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(analyse_btn)
      enabled(analyse_btn) <- FALSE

      # Get kits.
      for (k in seq(along = val_kits)) {
        tmp <- getKit(val_kits[k])
        val_kitData <- rbind(val_kitData, tmp)
      }

      # Get allele frequency database.
      val_db <- getDb(val_db_selected)

      if (debug) {
        print("val_kits")
        print(val_kits)
        print("val_virtual")
        print(val_virtual)
        print("val_limit")
        print(val_limit)
      }

      # Analyse bins overlap.
      datanew <- calculateOL(
        kit = val_kitData,
        db = val_db,
        virtual = val_virtual,
        limit = val_limit,
        debug = debug
      )

      # Create key-value pairs to log.
      keys <- list("kit", "db", "virtual", "limit")

      values <- list(val_kits, val_db_selected, val_virtual, val_limit)

      # Update audit trail.
      datanew <- audit_trail(
        obj = datanew, key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(str(datanew))
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      gmessage(
        mmsg = strings$STR_MSG_KIT,
        title = strings$STR_MSG_TITLE_KIT,
        icon = "error",
        parent = w
      )
    }
  })


  # INTERNAL FUNCTIONS ########################################################

  settings_prefix <- ".strvalidator_calculateOL_gui_"
  settings_widgets <- list(
    db_name = f1_db_drp,
    virtual = f1_virtual_chk,
    limit = f1_limit_chk
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
