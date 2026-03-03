

#' @title Calculate Analytical Threshold
#'
#' @description
#' GUI wrapper for the \code{\link{maskAT}} and \code{\link{calculateAT}} function.
#'
#' @details
#' Simplifies the use of the \code{\link{calculateAT}} and
#'  \code{\link{calculateAT}} function by providing a graphical user interface.
#'  In addition there are integrated control functions.
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
#' @importFrom utils help head str
#' @importFrom graphics title
#' @importFrom ggplot2 ggtitle scale_shape_discrete ggplot facet_wrap geom_point
#'  aes_string scale_colour_manual geom_rect
#'
#' @seealso \code{\link{calculateAT}}, \code{\link{maskAT}},
#'  \code{\link{check_subset}}


calculateAT_gui <- function(env = parent.frame(), savegui = NULL,
                            debug = FALSE, parent = NULL) {
  # Global variables.
  .gData <- NULL
  .gSamples <- NULL
  .gDataPrep <- NULL
  .gPlot <- NULL
  .gRef <- NULL

  # Language ------------------------------------------------------------------

  # Get this functions name from call.
  fnc <- as.character(match.call()[[1]])

  if (debug) {
    print(paste("IN:", fnc))
  }

  lng_strings <- get_strings(gui = fnc)
  default_strings <- list(
    STR_WIN_TITLE           = "Calculate analytical threshold",
    STR_CHK_GUI             = "Save GUI settings",
    STR_BTN_HELP            = "Help",
    STR_FRM_DATASET         = "Datasets",
    STR_LBL_DATASET         = "Sample dataset:",
    STR_DRP_DEFAULT         = "<Select dataset>",
    STR_LBL_SAMPLES         = "samples",
    STR_LBL_DATASET_REF     = "Reference dataset:",
    STR_LBL_REF             = "references",
    STR_BTN_CHECK           = "Check subsetting",
    STR_LBL_KIT             = "Kit:",
    STR_TIP_KIT             = "Only used to shade masked ranges in plot.",
    STR_FRM_OPTIONS         = "Options",
    STR_CHK_IGNORE          = "Ignore case",
    STR_CHK_WORD            = "Add word boundaries",
    STR_CHK_MASK_HIGH       = "Mask high peaks",
    STR_LBL_MASK_HIGH       = "Mask all peaks above (RFU): ",
    STR_CHK_MASK_ALLELE     = "Mask sample alleles",
    STR_LBL_DP_ALLELE       = "Range (data points) around known alleles:",
    STR_CHK_MASK_DYE        = "Mask sample alleles per dye channel",
    STR_CHK_ILS             = "Mask ILS peaks",
    STR_LBL_DP_PEAK         = "Range (data points) around known peak: ",
    STR_LBL_CONF            = "Confidence level 'k' (AT1, AT7): ",
    STR_LBL_RANK            = "Percentile rank threshold (AT2): ",
    STR_LBL_ALPHA           = "Upper confidence 'alpha' (AT4): ",
    STR_FRM_PREPARE         = "Prepare data and check masking",
    STR_BTN_MASK            = "Prepare and mask",
    STR_DRP_DEFAULT2        = "<Select sample>",
    STR_BTN_SAVE            = "Save plot",
    STR_FRM_SAVE            = "Save as",
    STR_LBL_SAVE            = "Name for result:",
    STR_LBL_SAVE_RANK       = "Name for percentile rank list:",
    STR_LBL_SAVE_MASKED     = "Name for masked raw data:",
    STR_BTN_CALCULATE       = "Calculate",
    STR_BTN_PROCESSING      = "Processing...",
    STR_WIN_CHECK           = "Check subsetting",
    STR_MSG_PLOT            = "Click 'Prepare and mask' and select a sample before saving.",
    STR_MSG_TITLE_PLOT      = "No plot!",
    STR_MSG_TITLE_ERROR     = "Error",
    STR_MSG_CHECK           = "Data frame is NULL!\n\nMake sure to select a dataset and a reference set",
    STR_MSG_DATASET         = "A dataset and a reference dataset must be selected.",
    STR_MSG_TITLE_DATASET   = "Datasets not selected"
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
    text = strings$STR_FRM_DATASET,
    horizontal = FALSE,
    spacing = 1,
    container = gv,
    expand = FALSE,
    fill = "x"
  )

  # Dataset -------------------------------------------------------------------

  g0 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")

  glabel(text = strings$STR_LBL_DATASET, container = g0)

  g0_data_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_SAMPLES),
    container = g0
  )

  # Create default dropdown.
  dfs <- c(strings$STR_DRP_DEFAULT, listObjects(env = env, obj.class = "data.frame"))

  g0_data_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g0,
    ellipsize = "none",
    expand = TRUE,
    fill = "x"
  )

  addHandlerChanged(g0_data_drp, handler = function(h, ...) {
    val_obj <- svalue(g0_data_drp)

    # Check if suitable.
    requiredCol <- c(
      "Dye.Sample.Peak", "Sample.File.Name", "Marker", "Allele",
      "Marker", "Height", "Data.Point"
    )
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = c("Allele", "Height", "Data.Point"),
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      # get dataset.
      .gData <<- get(val_obj, envir = env)
      svalue(g0_data_samples_lbl) <- paste(
        length(unique(.gData$Sample.File.Name)),
        strings$STR_LBL_SAMPLES
      )
      .refresh_sample_drp()
      .gDataPrep <- NULL # Erase any previously prepared data.

      # Suggest a name for result.
      svalue(f4_save1_edt) <- paste(val_obj, "_at", sep = "")
      svalue(f4_save2_edt) <- paste(val_obj, "_rank", sep = "")
      svalue(f4_save3_edt) <- paste(val_obj, "_masked", sep = "")

      # Detect kit.
      kitIndex <- detectKit(data = .gData, index = TRUE, debug = debug)
      # Select in dropdown.
      svalue(g2_kit_drp, index = TRUE) <- kitIndex
    } else {
      # Reset components.
      .gData <<- NULL
      svalue(g0_data_drp, index = TRUE) <- 1
      svalue(g0_data_samples_lbl) <- paste(" 0", strings$STR_LBL_SAMPLES)
      svalue(f4_save1_edt) <- ""
      svalue(f4_save2_edt) <- ""
      svalue(f4_save3_edt) <- ""
      .refresh_sample_drp()
      .gDataPrep <- NULL # Erase any previously prepared data.
    }
  })

  # Reference -----------------------------------------------------------------

  g1 <- ggroup(container = f0, spacing = 1, expand = TRUE, fill = "x")
  glabel(text = strings$STR_LBL_DATASET_REF, container = g1)

  g1_ref_samples_lbl <- glabel(
    text = paste(" 0", strings$STR_LBL_REF),
    container = g1
  )

  # NB! dfs defined in previous section.
  g1_ref_drp <- gcombobox(
    items = dfs,
    selected = 1,
    editable = FALSE,
    container = g1,
    ellipsize = "none", expand = TRUE, fill = "x"
  )

  addHandlerChanged(g1_ref_drp, handler = function(h, ...) {
    val_obj <- svalue(g1_ref_drp)

    # Check if suitable.
    requiredCol <- c("Sample.Name", "Marker", "Allele")
    ok <- check_dataset(
      name = val_obj, reqcol = requiredCol,
      slim = TRUE, slimcol = "Allele",
      env = env, parent = w, debug = debug
    )

    if (ok) {
      # Load or change components.

      .gRef <<- get(val_obj, envir = env)
      svalue(g1_ref_samples_lbl) <- paste(
        length(unique(.gRef$Sample.Name)),
        strings$STR_LBL_SAMPLES
      )
    } else {
      # Reset components.
      .gRef <<- NULL
      svalue(g1_ref_drp, index = TRUE) <- 1
      svalue(g1_ref_samples_lbl) <- paste(" 0", strings$STR_LBL_REF)
    }
  })

  # Kit -----------------------------------------------------------------------

  g2 <- ggroup(container = f0, expand = TRUE, fill = "x")
  glabel(text = strings$STR_LBL_KIT, container = g2)

  g2_kit_drp <- gcombobox(
    items = getKit(),
    selected = 1,
    editable = FALSE,
    container = g2,
    ellipsize = "none", expand = TRUE, fill = "x"
  )
  tooltip(g2_kit_drp) <- strings$STR_TIP_KIT

  # CHECK #####################################################################

  check_btn <- gbutton(text = strings$STR_BTN_CHECK, expande = TRUE, container = gv)

  addHandlerChanged(check_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)

    if (!is.null(.gData) || !is.null(.gRef)) {
      chksubset_w <- gwindow(
        title = strings$STR_WIN_CHECK,
        visible = FALSE, name = title,
        width = NULL, height = NULL, parent = w,
        handler = NULL, action = NULL
      )

      chksubset_txt <- check_subset(
        data = val_data,
        ref = val_ref,
        console = FALSE,
        ignore_case = val_ignore,
        word = val_word
      )

      gtext(
        text = chksubset_txt, width = NULL, height = 300, font.attr = NULL,
        wrap = FALSE, container = chksubset_w
      )

      visible(chksubset_w) <- TRUE
    } else {
      gmessage(
        msg = strings$STR_MSG_CHECK,
        title = strings$STR_MSG_TITLE_ERROR,
        icon = "error"
      )
    }
  })

  # FRAME 1 ###################################################################

  f1 <- gframe(
    text = strings$STR_FRM_OPTIONS,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  f1_ignore_chk <- gcheckbox(
    text = strings$STR_CHK_IGNORE,
    checked = TRUE,
    container = f1
  )

  f1_word_chk <- gcheckbox(
    text = strings$STR_CHK_WORD,
    checked = FALSE,
    container = f1
  )

  # LAYOUT --------------------------------------------------------------------

  f1g1 <- glayout(container = f1, spacing = 1)

  f1g1[1, 1] <- f1_mask_h_chk <- gcheckbox(
    text = strings$STR_CHK_MASK_HIGH,
    checked = TRUE, container = f1g1
  )
  f1g1[1, 2] <- glabel(text = strings$STR_LBL_MASK_HIGH, anchor = c(-1, 0), container = f1g1)

  f1g1[1, 3] <- f1_mask_h_edt <- gedit(text = "200", width = 6, container = f1g1)

  f1g1[2, 1] <- f1_mask_chk <- gcheckbox(
    text = strings$STR_CHK_MASK_ALLELE,
    checked = TRUE, container = f1g1
  )
  f1g1[3, 1] <- f1_mask_d_chk <- gcheckbox(
    text = strings$STR_CHK_MASK_DYE,
    checked = TRUE, container = f1g1
  )
  f1g1[2, 2] <- glabel(text = strings$STR_LBL_DP_ALLELE, anchor = c(-1, 0), container = f1g1)
  f1g1[2, 3] <- f1_mask_spb <- gspinbutton(from = 0, to = 100, by = 10, value = 50, container = f1g1)

  f1g1[4, 1] <- f1_mask_ils_chk <- gcheckbox(
    text = strings$STR_CHK_ILS,
    checked = TRUE, container = f1g1
  )
  f1g1[4, 2] <- glabel(text = strings$STR_LBL_DP_PEAK, anchor = c(-1, 0), container = f1g1)
  f1g1[4, 3] <- f1_mask_ils_spb <- gspinbutton(from = 0, to = 100, by = 20, value = 10, container = f1g1)

  # LAYOUT --------------------------------------------------------------------

  f1g2 <- glayout(container = f1, spacing = 1)

  f1g2[1, 1] <- glabel(text = strings$STR_LBL_CONF, container = f1g2)
  f1g2[1, 2] <- f1_k_spb <- gspinbutton(from = 0, to = 100, by = 1, value = 3, container = f1g2)


  f1g2[2, 1] <- glabel(text = strings$STR_LBL_RANK, container = f1g2)
  f1g2[2, 2] <- f1_t_spb <- gspinbutton(from = 0, to = 1, by = 0.01, value = 0.99, container = f1g2)

  f1g2[3, 1] <- glabel(text = strings$STR_LBL_ALPHA, container = f1g2)
  f1g2[3, 2] <- f1_a_spb <- gspinbutton(from = 0, to = 1, by = 0.01, value = 0.01, container = f1g2)

  # Handlers ------------------------------------------------------------------

  addHandlerChanged(f1_mask_h_chk, handler = function(h, ...) {
    # Update otions.
    .refresh_options()
  })

  addHandlerChanged(f1_mask_chk, handler = function(h, ...) {
    # Update otions.
    .refresh_options()
  })

  addHandlerChanged(f1_mask_ils_chk, handler = function(h, ...) {
    # Update otions.
    .refresh_options()
  })


  # FRAME 3 ###################################################################

  f3 <- gframe(
    text = strings$STR_FRM_PREPARE,
    horizontal = TRUE,
    spacing = 1,
    container = gv,
    expand = TRUE,
    fill = "x"
  )

  mask_btn <- gbutton(text = strings$STR_BTN_MASK, container = f3)

  f3_sample_drp <- gcombobox(
    items = strings$STR_DRP_DEFAULT2, selected = 1,
    editable = FALSE, container = f3, ellipsize = "none", expand = TRUE, fill = "x"
  )

  save_btn <- gbutton(text = strings$STR_BTN_SAVE, container = f3)

  addHandlerClicked(mask_btn, handler = function(h, ...) {
    # Get values.
    val_data <- .gData
    val_ref <- .gRef
    val_mask_h <- svalue(f1_mask_h_chk)
    val_mask <- svalue(f1_mask_chk)
    val_mask_d <- svalue(f1_mask_d_chk)
    val_mask_ils <- svalue(f1_mask_ils_chk)
    val_height <- as.numeric(svalue(f1_mask_h_edt))
    val_range <- svalue(f1_mask_spb)
    val_range_ils <- svalue(f1_mask_ils_spb)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_mask_h")
      print(val_mask_h)
      print("val_mask")
      print(val_mask)
      print("val_range")
      print(val_range)
      print("val_mask_d")
      print(val_mask_d)
      print("val_mask_ils")
      print(val_mask_ils)
      print("val_range_ils")
      print(val_range_ils)
      print("val_ignore")
      print(val_ignore)
      print("val_word")
      print(val_word)
    }

    # Change button.
    blockHandlers(mask_btn)
    svalue(mask_btn) <- strings$STR_BTN_PROCESSING
    unblockHandlers(mask_btn)
    enabled(mask_btn) <- FALSE
    enabled(f3_sample_drp) <- FALSE

    # Prepare data.
    .gDataPrep <<- maskAT(
      data = val_data, ref = val_ref,
      mask.height = val_mask_h,
      height = val_height,
      mask.sample = val_mask,
      per.dye = val_mask_d,
      range.sample = val_range,
      mask.ils = val_mask_ils,
      range.ils = val_range_ils,
      ignore.case = val_ignore,
      word = val_word,
      debug = debug
    )

    # Change button.
    blockHandlers(mask_btn)
    svalue(mask_btn) <- strings$STR_BTN_MASK
    unblockHandlers(mask_btn)
    enabled(mask_btn) <- TRUE
    enabled(f3_sample_drp) <- TRUE

    # Unselect sample.
    svalue(f3_sample_drp, index = TRUE) <- 1
  })

  addHandlerChanged(f3_sample_drp, handler = function(h, ...) {
    # Get values.
    val_sample <- svalue(f3_sample_drp)

    if (!is.null(.gDataPrep) & !is.null(val_sample)) {
      # Get values.
      val_mask_h <- svalue(f1_mask_h_chk)
      val_mask <- svalue(f1_mask_chk)
      val_mask_d <- svalue(f1_mask_d_chk)
      val_mask_ils <- svalue(f1_mask_ils_chk)
      val_range <- svalue(f1_mask_spb)
      val_range_ils <- svalue(f1_mask_ils_spb)
      val_kit <- svalue(g2_kit_drp)

      if (val_sample %in% unique(.gDataPrep$Sample.File.Name)) {
        # Must come after 'val_sample'.
        val_data <- subset(.gDataPrep, Sample.File.Name == val_sample)

        if (debug) {
          print("Read Values:")
          print("val_data")
          print(head(val_data))
          print("val_sample")
          print(val_sample)
          print("val_mask_h")
          print(val_mask_h)
          print("val_mask")
          print(val_mask)
          print("val_range")
          print(val_range)
          print("val_mask_d")
          print(val_mask_d)
          print("val_mask_ils")
          print(val_mask_ils)
          print("val_range_ils")
          print(val_range_ils)
          print("val_kit")
          print(val_kit)
        }

        # Get all dyes.
        dyes <- as.character(unique(val_data$Dye))
        colorsKit <- unique(getKit(val_kit, what = "Color")$Color)
        dyesKit <- add_color(colorsKit, have = "Color", need = "Dye")
        dyeILS <- setdiff(dyes, dyesKit)

        # Refactor and keep order of levels.
        val_data$Dye <- factor(val_data$Dye, levels = unique(val_data$Dye))

        # Create plot.
        gp <- ggplot(data = val_data)
        gp <- gp + ggtitle(paste("Masked data for", val_sample))
        gp <- gp + facet_wrap(~Dye, ncol = 1, scales = "fixed", drop = FALSE)
        if (any(val_mask, val_mask_h, val_mask_ils, val_mask_d)) {
          # Change shape, color, and legend.
          gp <- gp + geom_point(aes_string(
            x = "Data.Point", y = "Height",
            colour = "Masked", shape = "Masked"
          ))
          gp <- gp + scale_shape_discrete(
            name = "Peaks",
            breaks = c(FALSE, TRUE),
            labels = c("Included", "Excluded")
          )
          gp <- gp + scale_colour_manual(
            values = c("black", "red"),
            name = "Peaks",
            breaks = c(FALSE, TRUE),
            labels = c("Included", "Excluded")
          )
        } else {
          # Use default color and shape.
          gp <- gp + geom_point(aes_string(x = "Data.Point", y = "Height"))
        }

        if (val_mask_ils) {
          # ILS masking data frame for plot:
          dfIls <- val_data[val_data$ILS == TRUE, ]
          ilsDye <- unique(dfIls$Dye)
          dpMask <- dfIls$Data.Point
          dyeMask <- rep(unique(val_data$Dye), each = length(dpMask))
          dpMask <- rep(dpMask, length(unique(val_data$Dye)))
          dfMask <- data.frame(
            Dye = dyeMask, Data.Point = dpMask,
            Xmin = dpMask - val_range_ils,
            Xmax = dpMask + val_range_ils
          )

          if (nrow(dfMask) > 0) {
            # Add masking range to plot.
            gp <- gp + geom_rect(
              data = dfMask,
              aes_string(
                ymin = -Inf, ymax = Inf,
                xmin = "Xmin", xmax = "Xmax"
              ),
              alpha = 0.2,
              fill = add_color(ilsDye, have = "Dye", need = "Color")
            )
          }
        }

        if (val_mask) {
          # Sample masking data frame for plot:
          dfSample <- val_data[!is.na(val_data$Min), ]

          if (val_mask_d) {
            # Loop over dyes and add mask ranges.
            for (d in seq(along = dyesKit)) {
              # Get data points for selected sample.
              dpMask <- dfSample$Data.Point[dfSample$Dye == dyesKit[d]]
              dpMin <- dfSample$Min[dfSample$Dye == dyesKit[d]]
              dpMax <- dfSample$Max[dfSample$Dye == dyesKit[d]]

              # Create mask data.frame.
              dyeMask <- rep(dyesKit[d], length(dpMask))
              dfMask <- data.frame(
                Dye = dyeMask, Data.Point = dpMask,
                Xmin = dpMin, Xmax = dpMax
              )

              if (nrow(dfMask) > 0) {
                # Add masking range to plot.
                gp <- gp + geom_rect(
                  data = dfMask,
                  aes_string(
                    ymin = -Inf, ymax = Inf,
                    xmin = "Xmin", xmax = "Xmax"
                  ),
                  alpha = 0.2, fill = colorsKit[d]
                )
              }
            }
          } else {
            # Get data points for selected sample.
            dpMask <- dfSample$Data.Point
            dpMin <- dfSample$Min
            dpMax <- dfSample$Max

            # Create mask data.frame.
            dyeMask <- rep(dyesKit, each = length(dpMask))
            dpMask <- rep(dpMask, length(dyesKit))
            dpMin <- rep(dpMin, length(dyesKit))
            dpMax <- rep(dpMax, length(dyesKit))
            dfMask <- data.frame(
              Dye = dyeMask, Data.Point = dpMask,
              Xmin = dpMin, Xmax = dpMax
            )

            if (nrow(dfMask) > 0) {
              # Add masking range to plot.
              gp <- gp + geom_rect(
                data = dfMask,
                aes_string(
                  ymin = -Inf, ymax = Inf,
                  xmin = "Xmin", xmax = "Xmax"
                ),
                alpha = 0.2, fill = "red"
              )
            }
          }
        }

        # Show plot.
        print(gp)

        # Save plot object.
        .gPlot <<- gp
      } # End 'sample exist' if.
    } # End 'data exist' if.
  })

  addHandlerChanged(save_btn, handler = function(h, ...) {
    # Get sample name.
    val_name <- svalue(f3_sample_drp)

    if (!is.null(.gPlot)) {
      # Save data.
      ggsave_gui(
        ggplot = .gPlot, name = val_name, parent = w, env = env,
        savegui = savegui, debug = debug
      )
    } else {
      gmessage(
        msg = strings$STR_MSG_PLOT,
        title = strings$STR_MSG_TITLE_PLOT,
        icon = "info", parent = w
      )
    }
  })


  # FRAME 4 ###################################################################

  f4 <- gframe(
    text = strings$STR_FRM_SAVE,
    horizontal = FALSE,
    spacing = 1,
    container = gv
  )

  glabel(text = strings$STR_LBL_SAVE, anchor = c(-1, 0), container = f4)

  f4_save1_edt <- gedit(text = "", container = f4, expand = TRUE)

  glabel(text = strings$STR_LBL_SAVE_RANK, anchor = c(-1, 0), container = f4)

  f4_save2_edt <- gedit(text = "", container = f4, expand = TRUE)

  glabel(text = strings$STR_LBL_SAVE_MASKED, anchor = c(-1, 0), container = f4)

  f4_save3_edt <- gedit(text = "", container = f4, expand = TRUE)


  # BUTTON ####################################################################

  calculate_btn <- gbutton(text = strings$STR_BTN_CALCULATE, container = gv)

  addHandlerClicked(calculate_btn, handler = function(h, ...) {
    # Get values.
    if (is.null(.gDataPrep)) {
      val_data <- .gData
    } else {
      val_data <- .gDataPrep
    }
    val_ref <- .gRef
    val_name_data <- svalue(g0_data_drp)
    val_name_ref <- svalue(g1_ref_drp)
    val_ignore <- svalue(f1_ignore_chk)
    val_word <- svalue(f1_word_chk)
    val_mask_h <- svalue(f1_mask_h_chk)
    val_mask <- svalue(f1_mask_chk)
    val_mask_d <- svalue(f1_mask_d_chk)
    val_mask_ils <- svalue(f1_mask_ils_chk)
    val_height <- as.numeric(svalue(f1_mask_h_edt))
    val_range <- svalue(f1_mask_spb)
    val_range_ils <- svalue(f1_mask_ils_spb)
    val_k <- svalue(f1_k_spb)
    val_t <- svalue(f1_t_spb)
    val_a <- svalue(f1_a_spb)
    val_name1 <- svalue(f4_save1_edt)
    val_name2 <- svalue(f4_save2_edt)
    val_name3 <- svalue(f4_save3_edt)

    if (debug) {
      print("Read Values:")
      print("val_data")
      print(head(val_data))
      print("val_ref")
      print(head(val_ref))
      print("val_ignore")
      print(val_ignore)
      print("val_word")
      print(val_word)
      print("val_mask_h")
      print(val_mask_h)
      print("val_height")
      print(val_height)
      print("val_mask")
      print(val_mask)
      print("val_range")
      print(val_range)
      print("val_mask_d")
      print(val_mask_d)
      print("val_mask_ils")
      print(val_mask_ils)
      print("val_range_ils")
      print(val_range_ils)
      print("val_k")
      print(val_k)
      print("val_t")
      print(val_t)
      print("val_a")
      print(val_a)
      print("val_name1")
      print(val_name1)
      print("val_name2")
      print(val_name2)
    }

    # Check if data.
    if (!is.null(val_data)) {
      # Change button.
      blockHandlers(calculate_btn)
      svalue(calculate_btn) <- strings$STR_BTN_PROCESSING
      unblockHandlers(calculate_btn)
      enabled(calculate_btn) <- FALSE

      datanew <- calculateAT(
        data = val_data,
        ref = val_ref,
        mask.height = val_mask_h,
        height = val_height,
        mask.sample = val_mask,
        per.dye = val_mask_d,
        range.sample = val_range,
        mask.ils = val_mask_ils,
        range.ils = val_range_ils,
        k = val_k,
        rank.t = val_t,
        alpha = val_a,
        ignore.case = val_ignore,
        word = val_word,
        debug = debug
      )


      # Create key-value pairs to log.
      keys <- list(
        "data", "ref", "k", "rank.t", "alpha",
        "mask.height", "height", "mask", "range.sample",
        "mask.ils", "range.ils", "per.dye", "ignore.case",
        "word"
      )

      values <- list(
        val_name_data, val_name_ref, val_k, val_t,
        val_a, val_mask_h, val_height, val_mask, val_range,
        val_mask_ils, val_range_ils, val_mask_d, val_ignore,
        val_word
      )

      # Update audit trail.
      datanew[[1]] <- audit_trail(
        obj = datanew[[1]], key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      datanew[[2]] <- audit_trail(
        obj = datanew[[2]], key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      datanew[[3]] <- audit_trail(
        obj = datanew[[3]], key = keys, value = values,
        label = fnc, arguments = FALSE,
        package = "strvalidator"
      )

      # Save data.
      saveObject(name = val_name1, object = datanew[[1]], parent = w, env = env)
      saveObject(name = val_name2, object = datanew[[2]], parent = w, env = env)
      saveObject(name = val_name3, object = datanew[[3]], parent = w, env = env)

      if (debug) {
        print(str(datanew))
        print(head(datanew))
        print(paste("EXIT:", fnc))
      }

      # Close GUI.
      .saveSettings()
      dispose(w)
    } else {
      message <- strings$STR_MSG_DATASET

      gmessage(message,
        title = strings$STR_MSG_TITLE_DATASET,
        icon = "error",
        parent = w
      )
    }
  })

  # INTERNAL FUNCTIONS ########################################################

  .refresh_sample_drp <- function() {
    # Get data frames in global workspace.
    samples <- unique(.gData$Sample.File.Name)

    if (!is.null(samples)) {
      # Populate drop list.
      f3_sample_drp[] <- c(strings$STR_DRP_DEFAULT2, samples)
      svalue(f3_sample_drp, index = TRUE) <- 1
    } else {
      # Populate drop list.
      f3_sample_drp[] <- c(strings$STR_DRP_DEFAULT2)
      svalue(f3_sample_drp, index = TRUE) <- 1
    }
  }

  .refresh_options <- function() {
    val_mask_h <- svalue(f1_mask_h_chk)
    val_mask <- svalue(f1_mask_chk)
    val_mask_d <- svalue(f1_mask_d_chk)
    val_mask_ils <- svalue(f1_mask_ils_chk)

    # Update dependent widgets.
    if (val_mask_h) {
      enabled(f1_mask_h_edt) <- TRUE
    } else {
      enabled(f1_mask_h_edt) <- FALSE
    }

    # Update dependent widgets.
    if (val_mask) {
      enabled(f1_mask_d_chk) <- TRUE
      enabled(f1_mask_spb) <- TRUE
    } else {
      enabled(f1_mask_d_chk) <- FALSE
      enabled(f1_mask_spb) <- FALSE
    }

    # Update dependent widgets.
    if (val_mask_ils) {
      enabled(f1_mask_ils_spb) <- TRUE
    } else {
      enabled(f1_mask_ils_spb) <- FALSE
    }
  }

  settings_prefix <- ".strvalidator_calculateAT_gui_"
  settings_widgets <- list(
    mask_h = f1_mask_h_chk,
    mask = f1_mask_chk,
    mask_ils = f1_mask_ils_chk,
    dye = f1_mask_d_chk,
    height = f1_mask_h_edt,
    range = f1_mask_spb,
    range_ils = f1_mask_ils_spb,
    k = f1_k_spb,
    t = f1_t_spb,
    a = f1_a_spb,
    ignore = f1_ignore_chk,
    word = f1_word_chk
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

  # Update otions.
  .refresh_options()

  # Show GUI.
  visible(w) <- TRUE
  focus(w)
}
