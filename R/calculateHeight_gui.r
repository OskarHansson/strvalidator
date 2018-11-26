################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG (last 20 changes)
# 06.08.2017: Added audit trail.
# 13.07.2017: Fixed issue with button handlers.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 15.08.2016: Implemented new calculateHeight, selection of reference, and check subsetting.
# 08.07.2016: Fixed options 'sex.rm' and 'qs.rm' not saved.
# 29.06.2016: Implement 'checkDataset'.
# 29.06.2016: Added option to remove sex markers and quality sensor.
# 29.04.2016: 'Save as' textbox expandable.
# 06.01.2016: Added attributes to result.
# 28.08.2015: Added importFrom
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 26.09.2014: Implemented text field for 'exclude'.
# 12.09.2014: Implemented new options 'exclude OL'.
# 28.06.2014: Added help button and moved save gui checkbox.
# 25.02.2014: Implemented new options 'replace NA' and 'add to dataset'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 04.06.2013: Fixed bug in 'missingCol'.


#' @title Calculate Peak Height
#'
#' @description
#' GUI wrapper for the \code{\link{calculateHeight}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateHeight}} function by providing a graphical
#' user interface to it.
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
#' @references
#' Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
#'  Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
#'  Journal of the Royal Statistical Society: Series C (Applied Statistics),
#'  Volume 59, Issue 5, 2010,
#'  Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
#' \url{http://dx.doi.org/10.1111/j.1467-9876.2010.00722.x}
#'
#' @seealso \code{\link{calculateHeight}}

calculateHeight_gui <- function(env = parent.frame(), savegui = NULL, debug = FALSE, parent = NULL) {

  # Global variables.
  .gData <- NULL
  .gDataName <- NULL
  .gRef <- NULL
  .gRefName <- NULL

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Calculate peak height", visible = FALSE)

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
  gv <- ggroup(horizontal = FALSE,
               spacing = 8,
               use.scrollwindow = FALSE,
               container = w,
               expand = TRUE)

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("calculateHeight_gui", help_type = "html"))

  })

  # FRAME 0 ###################################################################

  f0 <- gframe(text = "Datasets",
               horizontal = FALSE,
               spacing = 5,
               container = gv)

  f0g0 <- glayout(container = f0, spacing = 1)

  # Datasets ------------------------------------------------------------------

  f0g0[1, 1] <- glabel(text = "Select dataset:", container = f0g0)

  f0g0[1, 2] <- dataset_drp <- gcombobox(items = c("<Select dataset>",
                                                listObjects(env = env,
                                                            obj.class = "data.frame")),
                                        selected = 1, editable = FALSE,
                                        container = f0g0,
                                        ellipsize = "none")

  f0g0[1, 3] <- f0g0_samples_lbl <- glabel(text = " 0 samples", container = f0g0)

  addHandlerChanged(dataset_drp, handler = function(h, ...) {

    val_obj <- svalue(dataset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Height")
    ok <- checkDataset(name = val_obj, reqcol = requiredCol,
                       slim = TRUE, slimcol = "Height",
                       env = env, parent = w, debug = debug)

    if (ok) {

      # Load or change components.
      .gData <<- get(val_obj, envir = env)
      .gDataName <<- val_obj
      samples <- length(unique(.gData$Sample.Name))
      svalue(f0g0_samples_lbl) <- paste("", samples, "samples")

      # Suggest name for the result.
      svalue(f2_save_edt) <- paste(val_obj, "_height", sep = "")

      # Detect kit.
      kitIndex <- detectKit(data = .gData, index = TRUE)
      # Select in dropdown.
      svalue(kit_drp, index = TRUE) <- kitIndex

    } else {

      # Reset components.
      .gData <<- NULL
      .gDataName <<- NULL
      svalue(dataset_drp, index = TRUE) <- 1
      svalue(f0g0_samples_lbl) <- " 0 samples"
      svalue(f2_save_edt) <- ""

    }
  })

  f0g0[2, 1] <- glabel(text = "Select reference:", container = f0g0)

  f0g0[2, 2] <- refset_drp <- gcombobox(items = c("<Select dataset>",
                                               listObjects(env = env,
                                                           obj.class = "data.frame")),
                                       selected = 1, editable = FALSE,
                                       container = f0g0,
                                       ellipsize = "none")

  f0g0[2, 3] <- f0g0_ref_lbl <- glabel(text = " 0 references", container = f0g0)

  addHandlerChanged(refset_drp, handler = function(h, ...) {

    val_obj <- svalue(refset_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- checkDataset(name = val_obj, reqcol = requiredCol,
                       slim = TRUE, slimcol = "Allele",
                       env = env, parent = w, debug = debug)

    if (ok) {

      # Load or change components.
      .gRef <<- get(val_obj, envir = env)
      .gRefName <<- val_obj
      ref <- length(unique(.gRef$Sample.Name))
      svalue(f0g0_ref_lbl) <- paste("", ref, "references")

    } else {

      # Reset components.
      .gRef <<- NULL
      svalue(refset_drp, index = TRUE) <- 1
      svalue(f0g0_ref_lbl) <- " 0 references"

    }

  })

  # CHECK ---------------------------------------------------------------------

  f0g0[3, 2] <- check_btn <- gbutton(text = "Check subsetting", container = f0g0)

  addHandlerChanged(check_btn, handler = function(h, ...) {

    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_exact <- svalue(f1_exact_chk)
    val_word <- FALSE

    if (!is.null(.gData) || !is.null(.gRef)) {

      chksubset_w <- gwindow(title = "Check subsetting",
                             visible = FALSE, name = title,
                             width = NULL, height = NULL, parent = w,
                             handler = NULL, action = NULL)

      chksubset_txt <- checkSubset(data = val_data,
                                   ref = val_ref,
                                   console = FALSE,
                                   ignore.case = val_ignore,
                                   exact = val_exact,
                                   word = val_word)

      gtext(text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
             wrap = FALSE, container = chksubset_w)

      visible(chksubset_w) <- TRUE

    } else {

      gmessage(msg = "Data frame is NULL!\n\n
               Make sure to select a dataset and a reference set",
               title = "Error",
               icon = "error")

    }

  })

  # Kit -----------------------------------------------------------------------

  f0g0[4, 1] <- glabel(text = "Select the kit used:", container = f0g0)

  f0g0[4, 2] <- kit_drp <- gcombobox(items = getKit(), selected = 1,
                                  editable = FALSE, container = f0g0,
                                  ellipsize = "none")

  # FRAME 1 ###################################################################

  f1 <- gframe(text = "Options",
               horizontal = FALSE,
               spacing = 10,
               container = gv)

  glabel(text = "Pre-processing:", anchor = c(-1, 0), container = f1)

  f1_sex_chk <- gcheckbox(text = "Remove sex markers",
                          checked = FALSE, container = f1)

  f1_qs_chk <- gcheckbox(text = "Remove quality sensors",
                         checked = TRUE, container = f1)

  f1_exclude_chk <- gcheckbox(text = "Exclude values in 'Allele' column",
                              checked = TRUE, container = f1)

  f1_exclude_lbl <- glabel(text = "Case sensitive values separated by comma:",
                           anchor = c(-1, 0), container = f1)

  f1_exclude_edt <- gedit(text = "OL", container = f1)

  glabel(text = "Reference sample name matching:", anchor = c(-1, 0), container = f1)

  f1_ignore_chk <- gcheckbox(text = "Ignore case in sample name matching",
                             checked = TRUE, container = f1)

  f1_exact_chk <- gcheckbox(text = "Exact sample name matching",
                            checked = FALSE, container = f1)

  glabel(text = "Post-processing:", anchor = c(-1, 0), container = f1)

  f1_replace_chk <- gcheckbox(text = "Replace NA in the result with 0",
                              checked = TRUE, container = f1)

  f1_add_chk <- gcheckbox(text = "Add result to dataset",
                          checked = TRUE, container = f1)

  addHandlerChanged(f1_exclude_chk, handler = function(h, ...) {

    if (svalue(f1_exclude_chk)) {

      enabled(f1_exclude_lbl) <- TRUE
      enabled(f1_exclude_edt) <- TRUE

    } else {

      enabled(f1_exclude_lbl) <- FALSE
      enabled(f1_exclude_edt) <- FALSE

    }

  })

  # FRAME 2 ###################################################################

  f2 <- gframe(text = "Save as",
               horizontal = TRUE,
               spacing = 5,
               container = gv)

  glabel(text = "Name for result:", container = f2)

  f2_save_edt <- gedit(text = "", container = f2, expand = TRUE)

  # BUTTON ####################################################################


  calculate_btn <- gbutton(text = "Calculate", container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {

    val_data <- .gData
    val_data_name <- .gDataName
    val_ref <- .gRef
    val_ref_name <- .gRefName
    val_name <- svalue(f2_save_edt)
    val_add <- svalue(f1_add_chk)
    val_sex <- svalue(f1_sex_chk)
    val_qs <- svalue(f1_qs_chk)
    val_kit <- svalue(kit_drp)
    val_ignore <- svalue(f1_ignore_chk)
    val_exact <- svalue(f1_exact_chk)
    val_replace <- svalue(f1_replace_chk)
    val_exclude <- svalue(f1_exclude_chk)
    val_ex_values <- svalue(f1_exclude_edt)
    val_na <- NULL
    val_ex <- NULL

    if (val_replace) {
      val_na <- 0
    } else {
      val_na <- NULL
    }

    if (val_exclude) {
      val_ex <- unlist(strsplit(val_ex_values, ",", fixed = TRUE))
    } else {
      val_ex <- NULL
    }

    if (!is.null(val_data)) {

      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- "Processing..."
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateHeight(data = val_data, ref = val_ref, na.replace = val_na,
                                 add = val_add, sex.rm = val_sex, qs.rm = val_qs,
                                 kit = val_kit, exclude = val_ex,
                                 ignore.case = val_ignore, exact = val_exact,
                                 debug = debug)

      # Add attributes to result.
      attr(datanew, which = "kit") <- val_kit

      # Create key-value pairs to log.
      keys <- list("data", "ref", "na.replace", "add", "sex.rm",
                   "qs.rm", "kit", "exclude", "ignore.case", "exact")

      values <- list(val_data_name, val_ref_name, val_na, val_add, val_sex,
                     val_qs, val_kit, val_ex, val_ignore, val_exact)

      # Update audit trail.
      datanew <- auditTrail(obj = datanew, key = keys, value = values,
                            label = "calculateHeight_gui", arguments = FALSE,
                            package = "strvalidator")


      # Save data.
      saveObject(name = val_name, object = datanew, parent = w, env = env)

      if (debug) {
        print(datanew)
        print(paste("EXIT:", match.call()[[1]]))
      }

      # Close GUI.
      dispose(w)

    } else {

      message <- "A dataset and a reference dataset have to be selected."

      gmessage(message, title = "Datasets not selected",
               icon = "error",
               parent = w)

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
      if (exists(".strvalidator_calculateHeight_gui_savegui", envir = env, inherits = FALSE)) {
        svalue(savegui_chk) <- get(".strvalidator_calculateHeight_gui_savegui", envir = env)
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
      if (exists(".strvalidator_calculateHeight_gui_sex", envir = env, inherits = FALSE)) {
        svalue(f1_sex_chk) <- get(".strvalidator_calculateHeight_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_qs", envir = env, inherits = FALSE)) {
        svalue(f1_qs_chk) <- get(".strvalidator_calculateHeight_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_ignore", envir = env, inherits = FALSE)) {
        svalue(f1_ignore_chk) <- get(".strvalidator_calculateHeight_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exact", envir = env, inherits = FALSE)) {
        svalue(f1_exact_chk) <- get(".strvalidator_calculateHeight_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_replace", envir = env, inherits = FALSE)) {
        svalue(f1_replace_chk) <- get(".strvalidator_calculateHeight_gui_replace", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_add", envir = env, inherits = FALSE)) {
        svalue(f1_add_chk) <- get(".strvalidator_calculateHeight_gui_add", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exclude", envir = env, inherits = FALSE)) {
        svalue(f1_exclude_chk) <- get(".strvalidator_calculateHeight_gui_exclude", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exclude_edt", envir = env, inherits = FALSE)) {
        svalue(f1_exclude_edt) <- get(".strvalidator_calculateHeight_gui_exclude_edt", envir = env)
      }
      if (debug) {
        print("Saved settings loaded!")
      }
    }

  }

  .saveSettings <- function() {

    # Then save settings if true.
    if (svalue(savegui_chk)) {

      assign(x = ".strvalidator_calculateHeight_gui_savegui", value = svalue(savegui_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_sex", value = svalue(f1_sex_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_qs", value = svalue(f1_qs_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_ignore", value = svalue(f1_ignore_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_exact", value = svalue(f1_exact_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_replace", value = svalue(f1_replace_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_add", value = svalue(f1_add_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_exclude", value = svalue(f1_exclude_chk), envir = env)
      assign(x = ".strvalidator_calculateHeight_gui_exclude_edt", value = svalue(f1_exclude_edt), envir = env)

    } else { # or remove all saved values if false.

      if (exists(".strvalidator_calculateHeight_gui_savegui", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_savegui", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_sex", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_sex", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_qs", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_qs", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_ignore", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_ignore", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exact", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_exact", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_replace", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_replace", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_add", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_add", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exclude", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_exclude", envir = env)
      }
      if (exists(".strvalidator_calculateHeight_gui_exclude_edt", envir = env, inherits = FALSE)) {
        remove(".strvalidator_calculateHeight_gui_exclude_edt", envir = env)
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
