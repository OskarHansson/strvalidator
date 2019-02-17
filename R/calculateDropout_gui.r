################################################################################
# CHANGE LOG (last 20 changes)
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 17.07.2018: 'Save as' textbox expandable.
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.08.2016: Implemented new calculateHeight, removed calculateHeterozygous.
# 29.06.2016: Added option to remove sex markers and quality sensor.
# 16.06.2016: 'Save as' textbox expandable.
# 05.10.2015: Added more attributes to result.
# 04.10.2015: Added automatic calculation of average peak height 'H'.
# 28.08.2015: Added importFrom
# 05.05.2015: Changed parameter 'ignoreCase' to 'ignore.case' for 'checkSubset' function.
# 13.12.2014: Added kit dropdown and kit attribute to result.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 28.06.2014: Added help button and moved save gui checkbox.
# 06.05.2014: Implemented 'checkDataset'.
# 16.01.2014: Adding 'option' for drop-out scoring method.
# 13.11.2013: Removed 'allele' argument in call.

#' @title Calculate Dropout Events
#'
#' @description
#' GUI wrapper for the \code{\link{calculateDropout}} function.
#'
#' @details Scores dropouts for a dataset.
#' @param env environment in which to search for data frames and save result.
#' @param savegui logical indicating if GUI settings should be saved in the environment.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @return TRUE
#'
#' @export
#'
#' @importFrom utils help head
#' @importFrom graphics title
#'
#' @seealso \code{\link{calculateDropout}}, \code{\link{checkSubset}}

calculateDropout_gui <- function(env = parent.frame(), savegui = NULL,
                                 debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gRef <- NULL

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Calculate drop-out", visible = FALSE)

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
    print(help("calculateDropout_gui", help_type = "html"))
  })

  # FRAME 0 ###################################################################

  f0 <- gframe(
    text = "Datasets",
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  g0 <- glayout(container = f0, spacing = 1)

  # Datasets ------------------------------------------------------------------

  g0[1, 1] <- glabel(text = "Select dataset:", container = g0)

  g0[1, 2] <- dataset_drp <- gcombobox(
    items = c(
      "<Select dataset>",
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )

  g0[1, 3] <- g0_samples_lbl <- glabel(text = " 0 samples", container = g0)

  addHandlerChanged(dataset_drp, handler = function(h, ...) {
    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele", "Height")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gData <<- get(val_obj, envir = env)
      samples <- length(unique(.gData$Sample.Name))
      svalue(g0_samples_lbl) <- paste("", samples, "samples")
      svalue(f1g1_ldt_edt) <- min(as.numeric(.gData$Height), na.rm = TRUE)

      # Suggest a name for result.
      svalue(f2_save_edt) <- paste(val_obj, "_dropout", sep = "")

      # Detect kit.
      kitIndex <- detectKit(.gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex
    } else {

      # Reset components.
      .gData <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(g0_samples_lbl) <- " 0 samples"
      svalue(f1g1_ldt_edt) <- ""
      svalue(f2_save_edt) <- ""
    }
  })

  g0[2, 1] <- glabel(text = "Select reference dataset:", container = g0)

  g0[2, 2] <- refset_drp <- gcombobox(
    items = c(
      "<Select dataset>",
      listObjects(
        env = env,
        obj.class = "data.frame"
      )
    ),
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none"
  )

  g0[2, 3] <- g0_ref_lbl <- glabel(text = " 0 references", container = g0)

  addHandlerChanged(refset_drp, handler = function(h, ...) {
    val_obj <- svalue(refset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(
      name = val_obj, reqcol = requiredCol,
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gRef <<- get(val_obj, envir = env)
      ref <- length(unique(.gRef$Sample.Name))
      svalue(g0_ref_lbl) <- paste("", ref, "references")
    } else {

      # Reset components.
      .gRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(g0_ref_lbl) <- " 0 references"
    }
  })

  # CHECK ---------------------------------------------------------------------

  if (debug) {
    print("CHECK")
  }

  g0[3, 2] <- g0_check_btn <- gbutton(text = "Check subsetting", container = g0)

  addHandlerChanged(g0_check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_case_chk)

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = "Check subsetting",
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- checkSubset(
        data = val_data,
        ref = val_ref,
        console = FALSE,
        ignore.case = val_ignore,
        word = FALSE
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
        wrap = FALSE, container = chksubset_w
      )

      visible(chksubset_w) <- TRUE
    } else {
      gmessage(
        msg = "Data frame is NULL!\n\n
               Make sure to select a dataset and a reference set",
        title = "Error",
        icon = "error"
      )
    }
  })

  # Kit -----------------------------------------------------------------------

  g0[4, 1] <- glabel(text = "Select the kit used:", container = g0)

  g0[4, 2] <- kit_drp <- gcombobox(
    items = getKit(), selected = 1,
    editable = FALSE, container = g0,
    ellipsize = "none"
  )

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = "Options",
    horizontal = FALSE,
    spacing = 5,
    container = gv
  )

  f1_ignore_case_chk <- gcheckbox(
    text = "Ignore case", checked = TRUE,
    container = f1
  )

  f1_sex_chk <- gcheckbox(
    text = "Remove sex markers", checked = FALSE,
    container = f1
  )

  f1_qs_chk <- gcheckbox(
    text = "Remove quality sensors", checked = TRUE,
    container = f1
  )

  f1_h_chk <- gcheckbox(
    text = "Calculate average peak height", checked = TRUE,
    container = f1
  )

  f1g1 <- glayout(container = f1)

  f1g1[1, 1] <- glabel(
    text = "Limit of detection threshold (LDT):",
    container = f1g1, anchor = c(-1, 0)
  )

  f1g1[1, 2] <- f1g1_ldt_edt <- gedit(text = "", width = 6, container = f1g1)

  glabel(
    text = "Drop-out scoring method for modelling of drop-out probabilities:",
    container = f1, anchor = c(-1, 0)
  )

  f1_score1_chk <- gcheckbox(
    text = "Score drop-out relative to the low molecular weight allele",
    checked = TRUE, container = f1
  )

  f1_score2_chk <- gcheckbox(
    text = "Score drop-out relative to the high molecular weight allele",
    checked = TRUE, container = f1
  )

  f1_scorex_chk <- gcheckbox(
    text = "Score drop-out relative to a random allele",
    checked = TRUE, container = f1
  )

  f1_scorel_chk <- gcheckbox(
    text = "Score drop-out per locus",
    checked = TRUE, container = f1
  )

  # FRAME 2 ###################################################################

  f2 <- gframe(
    text = "Save as",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = "Name for result:", container = f2)

  f2_save_edt <- gedit(text = "", expad = TRUE, container = f2, expand = TRUE)

  # BUTTON ####################################################################


  dropout_btn <- gbutton(text = "Calculate dropout", container = gv)

  addHandlerClicked(dropout_btn, handler = function(h, ...) {
    val_data <- .gData
    val_ref <- .gRef
    val_name_data <- svalue(dataset_drp)
    val_name_ref <- svalue(refset_drp)
    val_ignore_case <- svalue(f1_ignore_case_chk)
    val_h <- svalue(f1_h_chk)
    val_threshold <- as.numeric(svalue(f1g1_ldt_edt))
    val_name <- svalue(f2_save_edt)
    val_kit <- svalue(kit_drp)
    val_method <- vector()
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)

    # Get methods:
    if (svalue(f1_score1_chk)) {
      val_method <- c(val_method, "1")
    }
    if (svalue(f1_score2_chk)) {
      val_method <- c(val_method, "2")
    }
    if (svalue(f1_scorex_chk)) {
      val_method <- c(val_method, "X")
    }
    if (svalue(f1_scorel_chk)) {
      val_method <- c(val_method, "L")
    }

    if (debug) {
      print("GUI options:")
      print("val_ignore_case")
      print(val_ignore_case)
      print("val_threshold")
      print(val_threshold)
      print("val_name")
      print(val_name)
      print("val_method")
      print(val_method)
    }

    # No threshold is represented by NULL (not needed).
    if (length(val_threshold) == 0) {
      val_threshold <- NULL
    }

    if (debug) {
      print("Function arguments:")
      print("val_ignore_case")
      print(val_ignore_case)
      print("val_threshold")
      print(val_threshold)
      print("val_name")
      print(val_name)
      print("val_sex")
      print(val_sex)
      print("val_qs")
      print(val_qs)
    }

    if (!is.null(val_data) & !is.null(val_ref)) {

      # Change button.
      blockHandlers(dropout_btn)
      svalue(dropout_btn) <- "Processing..."
      unblockHandlers(dropout_btn)
      enabled(dropout_btn) <- FALSE

      datanew <- calculateDropout(
        data = val_data,
        ref = val_ref,
        threshold = val_threshold,
        method = val_method,
        ignore.case = val_ignore_case,
        sex.rm = val_sex,
        qs.rm = val_qs,
        kit = val_kit,
        debug = debug
      )

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "threshold", "method",
        "ignore.case", "sex.rm", "qs.rm", "kit", "calculate.h"
      )

      values <- list(
        val_name_data, val_name_ref, val_threshold, val_method,
        val_ignore_case, val_sex, val_qs, val_kit, val_h
      )

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = "calculateDropout_gui", arguments = FALSE,
        package = "strvalidator"
      )

      # Calculate and add average peak height.
      if (val_h) {
        message("User selected to add average peak height...")

        # Calculate average peak height.
        dfH <- calculateHeight(
          data = val_data, ref = val_ref, na.replace = 0,
          add = FALSE, exclude = "OL", sex.rm = val_sex,
          qs.rm = val_qs, kit = val_kit,
          ignore.case = val_ignore_case, exact = FALSE,
          debug = debug
        )

        message("Average peak height calculated.")

        # Add average peak height to dataset.
        datanew <- addData(
          data = datanew, new.data = dfH,
          by.col = "Sample.Name", then.by.col = NULL,
          exact = TRUE, ignore.case = val_ignore_case,
          debug = debug
        )

        message("Average peak height added to result.")
      }

      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(head(datanew))
        print(paste("EXIT:", match.call()[[1]]))
      }

      # Close GUI.
      dispose(w)
    } else {
      message <- "A dataset and a reference dataset have to be selected."

      gmessage(message,
        title = "Datasets not selected",
        icon = "error",
        parent = w
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
      if (exists(".strvalidator_calculateDropout_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateDropout_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateDropout_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_case_chk) <- get(".strvalidator_calculateDropout_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateDropout_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_calculateDropout_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_h", envir = env, inherits = FALSE)) {
        svalue(f1_h_chk) <- get(".strvalidator_calculateDropout_gui_h", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score1", envir = env, inherits = FALSE)) {
        svalue(f1_score1_chk) <- get(".strvalidator_calculateDropout_gui_score1", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score2", envir = env, inherits = FALSE)) {
        svalue(f1_score2_chk) <- get(".strvalidator_calculateDropout_gui_score2", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorex", envir = env, inherits = FALSE)) {
        svalue(f1_scorex_chk) <- get(".strvalidator_calculateDropout_gui_scorex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorel", envir = env, inherits = FALSE)) {
        svalue(f1_scorel_chk) <- get(".strvalidator_calculateDropout_gui_scorel", envir = env)
      }

      if (debug) {
        print("Saved settings loaded!")
      }
    }
  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {
      assign(x = ".strvalidator_calculateDropout_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_ignore", value = svalue(f1_ignore_case_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_qs", value = svalue(f1_qs_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_h", value = svalue(f1_h_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_score1", value = svalue(f1_score1_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_score2", value = svalue(f1_score2_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_scorex", value = svalue(f1_scorex_chk), envir = env)
      assign(x = ".strvalidator_calculateDropout_gui_scorel", value = svalue(f1_scorel_chk), envir = env)
    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateDropout_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_h", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_h", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score1", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_score1", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_score2", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_score2", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_scorex", envir = env)
      }
      if (exists(".strvalidator_calculateDropout_gui_scorel", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateDropout_gui_scorel", envir = env)
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
