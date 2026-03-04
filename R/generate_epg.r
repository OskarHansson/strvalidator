#' @title Generate Electropherogram (EPG)
#'
#' @description
#' Generate an electropherogram (EPG) plot from STR peak data with optional
#' aggregation, boxplots, and mean peak overlays. The function supports
#' kit-based marker ranges, canonical allele sizing, dye-based faceting, and
#' both classic peak polygon plots and allele-based boxplots.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Validates required input columns.
#'   \item Adds missing dye and color information from the kit definition.
#'   \item Ensures numeric peak heights and character allele designations.
#'   \item Assigns canonical allele sizes using the kit size table when needed.
#'   \item Optionally sums peak heights across profiles for peak polygon plots.
#'   \item Generates either classic peak polygons or allele-based boxplots.
#'   \item Optionally overlays mean peak heights as pseudo-peaks.
#' }
#'
#' When \code{boxplot = TRUE}, peak height distributions are calculated from
#' uncollapsed, per-sample data and grouped by marker and allele designation.
#' Off-ladder (OL) alleles are excluded from boxplots by default. Canonical
#' allele sizes from the kit definition are used to position boxplots, ensuring
#' consistent alignment and eliminating minor electrophoretic size variation.
#'
#' @param data A \code{data.frame} containing STR peak data. Must include at
#'   least the columns \code{Marker} and \code{Allele}. If \code{Height},
#'   \code{Dye}, \code{R.Color}, or \code{Size} are missing, they may be added
#'   automatically when possible.
#' @param kit Character string identifying the STR kit definition to use.
#' @param title Optional character string used as the plot title.
#' @param wrap Logical. If \code{TRUE}, facets the plot by dye.
#' @param boxplot Logical. If \code{TRUE}, plots allele-based boxplots of peak
#'   height distributions instead of classic peak polygons.
#' @param peaks Logical. If \code{TRUE}, overlays mean peak heights as
#'   pseudo-peaks. For boxplots, the mean is calculated per marker and allele.
#' @param sum_profiles Logical. If \code{TRUE}, peak heights are summed across
#'   profiles for identical markers and alleles before plotting peak polygons,
#'   resulting in a single composite electropherogram. If \code{FALSE}, peak
#'   polygons from individual profiles are overplotted. This argument does not
#'   affect boxplot distributions.
#' @param ol_filter Character vector of text patterns used to identify
#'   off-ladder (OL) alleles based on the \code{Allele} column. Alleles matching
#'   any of these patterns (via \code{grepl}) are excluded from boxplots.
#' @param silent Logical. If \code{TRUE}, suppresses printing of the plot.
#' @param ignore_case Logical. If \code{TRUE}, marker name matching is
#'   case-insensitive.
#' @param at Numeric. Analytical threshold (RFU). Peaks below this value are
#'   excluded from plotting.
#' @param scale Character. Passed to \code{facet_wrap(scales = ...)}. Typical
#'   values are \code{"fixed"}, \code{"free"}, or \code{"free_y"}.
#' @param limit_x Logical. If \code{TRUE}, limits the x-axis to the marker
#'   ranges defined in the kit.
#' @param label_size Numeric. Text size for allele labels.
#' @param label_angle Numeric. Rotation angle for allele labels.
#' @param label_vjust Numeric. Vertical justification for allele labels.
#' @param label_hjust Numeric. Horizontal justification for allele labels.
#' @param expand Numeric. Expansion factor for the y-axis.
#' @param debug Logical. If \code{TRUE}, prints additional diagnostic output.
#'
#' @return A \code{ggplot} object representing the electropherogram.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_polygon geom_boxplot geom_rect geom_text
#' @importFrom ggplot2 coord_cartesian scale_y_continuous labs theme expansion
#' @importFrom ggplot2 facet_wrap

# -----------------------------------------------------------------------------

generate_epg <- function(
  data,
  kit,
  title = NULL,
  wrap = TRUE,
  boxplot = FALSE,
  peaks = TRUE,
  sum_profiles = TRUE,
  ol_filter = c("OL", "OMR", "?"),
  silent = FALSE,
  ignore_case = TRUE,
  at = 0,
  scale = "free",
  limit_x = TRUE,
  label_size = 3,
  label_angle = 0,
  label_vjust = 1,
  label_hjust = 0.5,
  expand = 0.1,
  debug = FALSE
) {
  # ---------------------------------------------------------------------------
  # VALIDATION
  # ---------------------------------------------------------------------------

  required_cols <- c("Marker", "Allele")
  if (!all(required_cols %in% names(data))) {
    missing <- required_cols[!required_cols %in% names(data)]
    stop(
      paste0(
        "Data must contain columns: ",
        paste(required_cols, collapse = ", "),
        ". Missing: ",
        paste(missing, collapse = ", ")
      )
    )
  }

  # Copy data to avoid overwriting input
  raw_data <- data

  # Height column
  if (!"Height" %in% names(raw_data)) {
    message("'Height' missing. Using default=1000 RFU.")
    raw_data$Height <- 1000
  }

  n0 <- nrow(raw_data)
  raw_data <- raw_data[!is.na(raw_data$Height) & raw_data$Height > 0, ]
  n1 <- nrow(raw_data)
  if (n0 > n1) {
    message(paste0("Removed ", n0 - n1, " rows where 'Height' was NA or 0."))
  }

  # Allele column
  if (!is.character(raw_data$Allele)) {
    raw_data$Allele <- as.character(raw_data$Allele)
    message("'Allele' column converted to character.")
  }

  # ---------------------------------------------------------------------------
  # ADD DYE + R.Color
  # ---------------------------------------------------------------------------

  need_dye <- !"Dye" %in% names(raw_data)
  need_r_color <- !"R.Color" %in% names(raw_data)

  if (need_dye && need_r_color) {
    message("'Dye' and 'R.Color' missing. Adding both from kit definition.")
    raw_data <- add_color(
      data        = raw_data,
      kit         = kit,
      need        = c("Dye", "R.Color"),
      ignore_case = ignore_case
    )
  } else if (need_dye && !need_r_color) {
    message("'Dye' missing. Adding from kit definition.")
    raw_data <- add_color(
      data        = raw_data,
      kit         = kit,
      need        = "Dye",
      ignore_case = ignore_case
    )
  } else if (!need_dye && need_r_color) {
    message("'R.Color' missing. Adding from kit definition using existing Dye.")
    raw_data <- add_color(
      data        = raw_data,
      kit         = kit,
      have        = "Dye",
      need        = "R.Color",
      ignore_case = ignore_case
    )
  } else {
    # message("Both 'Dye' and 'R.Color' found in data.")
  }

  # ---------------------------------------------------------------------------
  # NORMALIZE MARKER NAMES
  # ---------------------------------------------------------------------------

  if (ignore_case) {
    raw_data$Marker <- toupper(raw_data$Marker)
  }

  # ---------------------------------------------------------------------------
  # KIT INFORMATION
  # ---------------------------------------------------------------------------

  kit_range <- get_kit(kit, what = "Range") # Marker shading
  kit_size <- get_kit(kit, what = "Size") # Size table

  if (ignore_case) {
    kit_range$Marker <- toupper(kit_range$Marker)
    kit_size$Marker <- toupper(kit_size$Marker)
  }

  # ---------------------------------------------------------------------------
  # SIZE HANDLING
  # ---------------------------------------------------------------------------

  # Note: For boxplots, canonical allele sizes from the kit are used
  # for x-positioning to eliminate minor electrophoretic size variation.

  if (!"Size" %in% names(raw_data)) {
    message("'Size' information not in data. Estimating from kit.")
    raw_data <- add_size(raw_data, kit_size, ignore_case = ignore_case, debug = debug)
  }

  # Remove unknown sizes
  raw_data <- raw_data[!is.na(raw_data$Size), ]

  # ---------------------------------------------------------------------------
  # BOXPLOT DATA PREPARATION (allele-based, OL excluded)
  # ---------------------------------------------------------------------------

  if (boxplot) {
    is_ol <- grepl(
      paste(ol_filter, collapse = "|"),
      raw_data$Allele,
      ignore.case = TRUE
    )

    # Boxplot uses uncollapsed, allele-based data
    box_data <- raw_data[!is_ol & !is.na(raw_data$Allele), ]

    if (nrow(box_data) == 0) {
      warning("Boxplot requested but no non-OL alleles available.")
    }

    # Use canonical allele size for boxplot positioning
    box_data <- add_size(
      data        = box_data,
      kit         = kit_size,
      ignore_case = ignore_case,
      debug       = debug
    )
  }

  # ---------------------------------------------------------------------------
  # APPLY AT (analytical threshold)
  # ---------------------------------------------------------------------------

  below <- sum(raw_data$Height < at)
  if (below > 0) {
    message(below, " peaks removed below AT = ", at)
  }

  raw_data <- raw_data[raw_data$Height >= at, ]

  if (boxplot) {
    box_data <- box_data[box_data$Height >= at, ]
  }

  # ---------------------------------------------------------------------------
  # ORDER MARKER & DYE FACTORS ACCORDING TO KIT
  # ---------------------------------------------------------------------------

  if (debug) {
    message("Marker levels before sort_marker():")
    print(levels(raw_data$Marker))
    message("Dye levels before sort_marker():")
    print(levels(raw_data$Dye))
  }

  raw_data <- sort_markers(data = raw_data, kit = kit)

  if (debug) {
    message("Marker levels after sort_marker():")
    print(levels(raw_data$Marker))
    message("Dye levels after sort_marker():")
    print(levels(raw_data$Dye))
  }

  # ---------------------------------------------------------------------------
  # COLLAPSE LOGIC
  # ---------------------------------------------------------------------------

  dt <- data.table::data.table(raw_data)

  if (sum_profiles) {
    message("Summing peak heights across profiles.")
    peak_data <- dt[, list(Height = sum(.data$Height)),
      by = list(.data$Marker, .data$Dye, .data$Allele, .data$Size)
    ]
  } else {
    peak_data <- dt
  }

  # For boxplots, use prepared allele-based data
  if (boxplot) {
    box_data <- data.table::data.table(box_data)
  }

  # ---------------------------------------------------------------------------
  # PEAK POLYGON CONVERSION
  # ---------------------------------------------------------------------------

  height_to_peak <- function(df, width = 1) {
    # Construct triangles: left, top, right
    df_key <- paste(df$Marker, df$Allele, df$Size, sep = "_")

    left <- data.frame(
      Size = df$Size - width / 2,
      Height = 0,
      group = df_key,
      Dye = df$Dye
    )
    top <- data.frame(
      Size = df$Size,
      Height = df$Height,
      group = df_key,
      Dye = df$Dye
    )
    right <- data.frame(
      Size = df$Size + width / 2,
      Height = 0,
      group = df_key,
      Dye = df$Dye
    )

    rbind(left, top, right)
  }

  peaks_poly <- height_to_peak(peak_data)

  if (boxplot && peaks) {
    mean_data <- box_data[, list(Height = mean(.data$Height)),
      by = list(.data$Marker, .data$Dye, .data$Allele, .data$Size)
    ]
    peaks_mean_poly <- height_to_peak(mean_data)

    if (debug) {
      message("mean_data:")
      print(summary(mean_data$Height))
      message("peaks_mean_poly:")
      print(summary(peaks_mean_poly$Height))
    }
  }

  # ---------------------------------------------------------------------------
  # INITIALIZE PLOT
  # ---------------------------------------------------------------------------

  gp <- ggplot2::ggplot()

  # ---------------------------------------------------------------------------
  # PLOT: PEAK POLYGONS OR BOXPLOTS
  # ---------------------------------------------------------------------------

  if (!boxplot) {
    gp <- gp +
      geom_polygon(
        data = peaks_poly,
        aes(x = .data$Size, y = .data$Height, group = .data$group, fill = .data$Dye),
        colour = NA,
        alpha = 0.7
      )
  } else {
    n_per_group <- box_data[, .N, by = list(.data$Marker, .data$Allele)]
    if (!any(n_per_group$N > 1)) {
      warning("Boxplot requested, but no allele has multiple observations; boxes may collapse to lines.")
    }

    # Distribution (uses raw uncollapsed data)
    gp <- gp +
      geom_boxplot(
        data = box_data,
        aes(
          x = .data$Size, # canonical allele size
          y = .data$Height,
          group = interaction(.data$Marker, .data$Allele), # allele-based grouping
          colour = .data$Dye
        ),
        width = 1,
        outlier.size = 1
      )

    # Optional: overlay mean as pseudo peak
    if (peaks) {
      mean_data <- data.table::data.table(box_data)[
        , list(Height = mean(.data$Height)),
        by = list(.data$Marker, .data$Dye, .data$Allele, .data$Size)
      ]

      peaks_mean_poly <- height_to_peak(mean_data)

      gp <- gp +
        geom_polygon(
          data = peaks_mean_poly,
          aes(x = .data$Size, y = .data$Height, group = .data$group, 
              fill = .data$Dye),
          colour = NA,
          alpha = 0.6
        )
    }
  }

  # ---------------------------------------------------------------------------
  # MARKER HEADERS (floating above panel, independent of RFU range)
  # ---------------------------------------------------------------------------

  if (wrap) {
    marker_dye <- unique(raw_data[, c("Marker", "Dye")])

    kit_range_dye <- kit_range

    # Which height source should define the panel scale?
    height_source <- if (sum_profiles) peak_data else raw_data
    height_source <- data.table::as.data.table(height_source)

    # Per-dye maximum height (facet-local)
    y_by_dye <- height_source[
      , list(y_top = max(.data$Height, na.rm = TRUE)),
      by = .data$Dye
    ]
    y_by_dye$y_top[!is.finite(y_by_dye$y_top)] <- 0

    # Merge panel heights into range table
    kit_range_dye <- merge(
      kit_range_dye,
      y_by_dye,
      by = "Dye",
      all.x = TRUE
    )

    # Global fallback (if a dye has no peaks)
    global_y_top <- max(height_source$Height, na.rm = TRUE)
    kit_range_dye$y_top[is.na(kit_range_dye$y_top)] <- global_y_top

    # Decide header height logic depending on user scale settings
    # free_y -> panel-specific headers
    # free/free_x/fixed -> global header height
    if (scale == "free_y") {
      ref_y <- kit_range_dye$y_top
    } else {
      ref_y <- global_y_top
    }

    # Header band height (as fraction of local top)
    ymin_offset <- 0.03
    ymax_offset <- 0.11

    kit_range_dye$ymin_header <- ref_y * (1 + ymin_offset)
    kit_range_dye$ymax_header <- ref_y * (1 + ymax_offset)
    kit_range_dye$y_label <- (kit_range_dye$ymin_header + kit_range_dye$ymax_header) / 2

    # Add rectangles + labels (facet-aware)
    gp <- gp +
      ggplot2::geom_rect(
        data = kit_range_dye,
        aes(
          xmin = .data$Marker.Min,
          xmax = .data$Marker.Max,
          ymin = .data$ymin_header,
          ymax = .data$ymax_header
        ),
        fill = "blue",
        alpha = 0.2,
        colour = "black",
        linewidth = 0.3,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_text(
        data = kit_range_dye,
        aes(
          x = (.data$Marker.Min + .data$Marker.Max) / 2,
          y = .data$y_label,
          label = .data$Marker
        ),
        fontface = "bold",
        size = 3,
        vjust = 0.5,
        inherit.aes = FALSE
      )
  }

  # ---------------------------------------------------------------------------
  # ALLELE LABELS
  # ---------------------------------------------------------------------------

  allele_labels <- unique(peak_data[, c("Marker", "Allele", "Size", "Dye")])

  gp <- gp +
    geom_text(
      data = allele_labels,
      aes(x = .data$Size, y = 0, label = Allele),
      vjust = label_vjust,
      hjust = label_hjust,
      angle = label_angle,
      size = label_size
    )

  # ---------------------------------------------------------------------------
  # FACETING
  # ---------------------------------------------------------------------------

  if (wrap) {
    gp <- gp +
      facet_wrap(~Dye, ncol = 1, scales = scale)
  }

  # ---------------------------------------------------------------------------
  # AXIS LIMITS
  # ---------------------------------------------------------------------------

  if (limit_x) {
    gp <- gp +
      coord_cartesian(
        xlim = c(min(kit_range$Marker.Min), max(kit_range$Marker.Max))
      )
  }

  gp <- gp +
    scale_y_continuous(expand = ggplot2::expansion(mult = c(expand, 0.15))) +
    labs(title = title, x = "Size (bp)", y = "Peak height (RFU)") +
    theme(
      strip.text = element_blank(),
      strip.background = element_blank()
    )

  # ---------------------------------------------------------------------------
  # APPLY TRUE DYE COLORS (R.Color / R.Colors) WITH DEBUG OUTPUT
  # ---------------------------------------------------------------------------

  if ("R.Color" %in% names(raw_data)) {
    message("Using dye colors from column 'R.Color'.")

    dye_colors <- unique(raw_data[, c("Dye", "R.Color")])

    # Rename R.Color -> color so we can build a clean named color vector for
    # scale_fill_manual()/scale_colour_manual() without special-case handling.
    names(dye_colors)[names(dye_colors) == "R.Color"] <- "color"

    if (debug) print(dye_colors)
    
    dye_map <- setNames(dye_colors$color, dye_colors$Dye)

    gp <- gp +
      scale_fill_manual(values = dye_map, drop = FALSE) +
      scale_colour_manual(values = dye_map, drop = FALSE)
  } else {
    message("No 'R.Color' column found in data; using ggplot default dye colors.")
  }

  if (!silent) {
    print(gp)
  }

  return(gp)
}

