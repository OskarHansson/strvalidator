################################################################################
# CHANGE LOG (last 20 changes)
# 17.02.2019: Fixed Error in if (svalue(savegui_chk)) { : argument is of length zero (tcltk)
# 07.08.2017: Added audit trail.
# 26.07.2017: Added expand=TRUE to save name text field.
# 13.07.2017: Fixed narrow dropdown with hidden argument ellipsize = "none".
# 07.07.2017: Replaced 'droplist' with 'gcombobox'.
# 07.07.2017: Removed argument 'border' for 'gbutton'.
# 10.10.2016: Changed to rbind.fill
# 10.10.2016: Check for column names no longer require identical order.
# 28.08.2015: Added importFrom.
# 11.10.2014: Added 'focus', added 'parent' parameter.
# 29.07.2014: Changed name concatenate_gui -> combine_gui.
# 28.06.2014: Added help button and moved save gui checkbox.
# 08.05.2014: Implemented 'checkDataset'.
# 18.07.2013: Check before overwrite object.
# 11.06.2013: Added 'inherits=FALSE' to 'exists'.
# 17.05.2013: First version.


#' @title Combine Datasets
#'
#' @description
#' GUI for combining two datasets.
#'
#' @details
#' Simple GUI to combine two datasets using the \code{\link{rbind.fill}}
#' function.
#' NB! Datasets must have identical column names but not necessarily
#' in the same order.
#'
#' @param env environment in which to search for data frames.
#' @param debug logical indicating printing debug information.
#' @param parent widget to get focus when finished.
#'
#' @export
#'
#' @importFrom utils help
#' @importFrom plyr rbind.fill
#'
#' @return TRUE


combine_gui <- function(env = parent.frame(), debug = FALSE, parent = NULL) {

  # Global variables.
  .gData1 <- NULL
  .gData2 <- NULL
  .gData1Name <- NULL
  .gData2Name <- NULL

  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Main window.
  w <- gwindow(title = "Combine", visible = FALSE)

  # Runs when window is closed.
  addHandlerUnrealize(w, handler = function(h, ...) {

    # Save GUI state.
    # .saveSettings()

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
    expand = FALSE
  )

  # Help button group.
  gh <- ggroup(container = gv, expand = FALSE, fill = "both")

  savegui_chk <- gcheckbox(text = "Save GUI settings", checked = FALSE, container = gh)

  addSpring(gh)

  help_btn <- gbutton(text = "Help", container = gh)

  addHandlerChanged(help_btn, handler = function(h, ...) {

    # Open help page for function.
    print(help("combine_gui", help_type = "html"))
  })

  # DATASET ###################################################################

  f0 <- gframe(
    text = "Datasets",
    horizontal = FALSE,
    spacing = 10,
    container = gv
  )


  f0g0 <- glayout(container = f0, spacing = 1)

  f0g0[1, 1] <- glabel(text = "Select dataset 1:", container = f0g0)

  f0g0[1, 2] <- f0g0_data1_drp <- gcombobox(
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

  f0g0[1, 3] <- f0g0_data1_col_lbl <- glabel(
    text = " 0 columns",
    container = f0g0
  )

  addHandlerChanged(f0g0_data1_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_data1_drp)

    # Check if suitable.
    ok <- checkDataset(
      name = val_obj, reqcol = NULL,
      env = env, parent = w, debug = debug
    )

    if (ok) {

      # Load or change components.
      .gData1 <<- get(val_obj, envir = env)
      .gData1Name <<- val_obj

      svalue(f0g0_data1_col_lbl) <- paste(" ", ncol(.gData1), " columns")
      svalue(f2_name) <- paste(.gData1Name, .gData2Name, sep = "_")
    } else {
      .gData1 <<- NULL
      .gData1Name <<- NULL
      svalue(f0g0_data1_col_lbl) <- " 0 columns"
      svalue(f2_name) <- ""
    }
  })

  f0g0[2, 1] <- glabel(text = "Select dataset 2:", container = f0g0)

  f0g0[2, 2] <- f0g0_data2_drp <- gcombobox(
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

  f0g0[2, 3] <- f0g0_data2_col_lbl <- glabel(
    text = " 0 columns",
    container = f0g0
  )

  addHandlerChanged(f0g0_data2_drp, handler = function(h, ...) {
    val_obj <- svalue(f0g0_data2_drp)

    if (exists(val_obj, envir = env, inherits = FALSE)) {
      .gData2 <<- get(val_obj, envir = env)
      .gData2Name <<- val_obj

      svalue(f0g0_data2_col_lbl) <- paste(" ", ncol(.gData2), " columns")
      svalue(f2_name) <- paste(.gData1Name, .gData2Name, sep = "_")
    } else {
      .gData2 <<- NULL
      .gData1Name <<- NULL
      svalue(f0g0_data2_col_lbl) <- " 0 samples"
      svalue(f2_name) <- ""
    }
  })

  # FRAME 1 ###################################################################
  # # No options yet.
  #   f1 <- gframe("Options", horizontal=FALSE, container=gv)
  #
  #   f1g0 <- glayout(container = f1, expand=TRUE, fill="both")

  # NAME ######################################################################

  f2 <- gframe(
    text = "Save as",
    horizontal = TRUE,
    spacing = 5,
    container = gv
  )

  glabel(text = "Save as:", container = f2)
  f2_name <- gedit(text = "", expand = TRUE, container = f2)

  # BUTTON ####################################################################

  if (debug) {
    print("BUTTON")
  }

  combine_btn <- gbutton(text = "Combine", container = gv)

  addHandlerChanged(combine_btn, handler = function(h, ...) {
    colOk <- all(names(.gData1) %in% names(.gData2))
    val_data_1 <- .gData1Name
    val_data_2 <- .gData2Name
    val_name <- svalue(f2_name)

    if (colOk) {

      # Combine the datasets.
      datanew <- plyr::rbind.fill(.gData1, .gData2)

      # Create key-value pairs to log.
      keys <- list("data1", "data2")

      values <- list(val_data_1, val_data_2)

      # Update audit trail.
      datanew <- auditTrail(
        obj = datanew, key = keys, value = values,
        label = "combine_gui", arguments = FALSE,
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
        msg = "Datasets must have identical columns!",
        title = "Error",
        icon = "error"
      )
    }
  })

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
} # End of GUI
