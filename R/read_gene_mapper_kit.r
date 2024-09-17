################################################################################
# CHANGE LOG (last 20 changes)
# 18.08.2024: New function to import kit from GeneMapper files.

#' @title Read GeneMapper Kit Definition
#'
#' @description
#' Import kit definition from GeneMapper bins and panel files.
#'
#' @details Takes the GeneMapper bins and panels file and creates a
#' kit definition data frame.
#' @param parent widget to get focus when finished.
#'
#' @return data.frame
#'
#' @export
#'
#' @importFrom xml2 read_xml xml_find_all xml_text xml_find_first xml_attr
#'

read_gene_mapper_kit <- function(bin_files = NULL, panel_files = NULL,
                                 debug = FALSE) {
  # Helper function to read Bins file
  read_bins_file <- function(file, debug = FALSE) {
    key_panel <- "Panel Name"
    key_marker <- "Marker Name"
    delimiter <- "\t"

    if (!file.exists(file)) {
      stop("Bins file does not exist: ", file)
    }

    if (debug) {
      message("Reading Bins file: ", file)
    }

    file_content <- tryCatch(
      {
        readLines(file)
      },
      error = function(e) {
        stop("Failed to read Bins file: ", e$message)
      }
    )

    split_content <- strsplit(file_content, delimiter)

    results <- list()
    panel_name <- NULL
    marker_name <- NULL

    i <- 1
    while (i <= length(split_content)) {
      current_row <- split_content[[i]]
      current_tag <- current_row[1]

      if (current_tag == key_panel) {
        panel_name <- current_row[2]
        if (debug) message("Processing Panel: ", panel_name)

        i <- i + 1
        next
      }

      if (current_tag == key_marker) {
        marker_name <- current_row[2]
        if (debug) message("Processing Marker: ", marker_name)

        i <- i + 1
        next
      }

      if (!is.null(panel_name) && !is.null(marker_name)) {
        allele_name <- current_row[1]
        allele_size <- as.numeric(current_row[2])
        allele_min <- as.numeric(current_row[3])
        allele_max <- as.numeric(current_row[4])
        allele_virtual <- ifelse(is.na(current_row[5]) ||
                                  nchar(current_row[5]) == 0, 0, 1)

        results[[length(results) + 1]] <- data.frame(
          Panel = panel_name,
          Marker = marker_name,
          Allele = allele_name,
          Size = allele_size,
          Size.Min = allele_size - allele_min,
          Size.Max = allele_size + allele_max,
          Virtual = allele_virtual,
          stringsAsFactors = FALSE
        )
      }

      i <- i + 1
    }

    bins_df <- do.call(rbind, results)
    return(bins_df)
  }

  # Helper function to read Panels file
  read_panels_file <- function(file, debug = FALSE) {
    key_panel <- "Panel"
    delimiter <- "\t"

    if (!file.exists(file)) {
      stop("Panels file does not exist: ", file)
    }

    if (debug) {
      message("Reading Panels file: ", file)
    }

    file_content <- tryCatch(
      {
        readLines(file)
      },
      error = function(e) {
        stop("Failed to read Panels file: ", e$message)
      }
    )

    split_content <- strsplit(file_content, delimiter)

    results <- list()
    panel_name <- NULL

    for (row in seq_along(split_content)) {
      current_row <- split_content[[row]]
      current_tag <- current_row[1]

      if (current_tag == key_panel) {
        panel_name <- current_row[2]
        if (debug) message("Processing Panel: ", panel_name)
        next
      }

      if (!is.null(panel_name) && !grepl("#", current_tag, fixed = TRUE)) {
        marker_name <- current_row[1]
        color_name <- tolower(current_row[2])
        range_min <- as.numeric(current_row[3])
        range_max <- as.numeric(current_row[4])
        repeat_unit <- as.numeric(current_row[6])

        results[[length(results) + 1]] <- data.frame(
          Panel = panel_name,
          Marker = marker_name,
          Color = color_name,
          Marker.Min = range_min,
          Marker.Max = range_max,
          Repeat = repeat_unit,
          stringsAsFactors = FALSE
        )
      }
    }

    panels_df <- do.call(rbind, results)
    return(panels_df)
  }

  # Check if files are provided
  if (is.null(bin_files) && is.null(panel_files)) {
    stop("At least one of 'bin_files' or 'panel_files' must be provided.")
  }

  bins_result <- NULL
  panels_result <- NULL

  if (!is.null(bin_files)) {
    bins_result <- read_bins_file(bin_files, debug)
  }

  if (!is.null(panel_files)) {
    panels_result <- read_panels_file(panel_files, debug)
  }

  # Combine the results if both files are provided using combine_bins_and_panels
  if (!is.null(bins_result) && !is.null(panels_result)) {
    combined_result <- combine_bins_and_panels(bins_result, panels_result)
    return(combined_result)
  } else if (!is.null(bins_result)) {
    return(bins_result)
  } else if (!is.null(panels_result)) {
    return(panels_result)
  }

  return(NULL)
}

# Combine bins and panels function
combine_bins_and_panels <- function(bin, panel) {
  kit <- bin

  # Add new columns
  kit$Color <- NA
  kit$Repeat <- NA
  kit$Marker.Min <- NA
  kit$Marker.Max <- NA
  kit$Offset <- NA

  # Get panels
  bin_panel <- unique(bin$Panel)
  bin_panel2 <- unique(panel$Panel)

  if (!all(bin_panel == bin_panel2)) {
    print(paste("bin panels:", paste(bin_panel, collapse = ",")))
    print(paste("panel panels:", paste(bin_panel2, collapse = ",")))
    stop("Panels in 'bin' and 'panel' files not identical")
  }

  # Loop over all panels
  for (p in seq(along = bin_panel)) {
    # Get markers for current panel
    bin_marker <- unique(bin$Marker[bin$Panel == bin_panel[p]])

    for (m in seq(along = bin_marker)) {
      # Add new info for current marker in current panel

      # Color
      kit$Color[kit$Panel == bin_panel[p] & kit$Marker == bin_marker[m]] <-
        panel[panel$Panel == bin_panel[p] & panel$Marker == bin_marker[m], ]$Color

      # Repeat unit size
      kit$Repeat[kit$Panel == bin_panel[p] & kit$Marker == bin_marker[m]] <-
        panel[panel$Panel == bin_panel[p] & panel$Marker == bin_marker[m], ]$Repeat

      # Marker size range min
      kit$Marker.Min[kit$Panel == bin_panel[p] & kit$Marker == bin_marker[m]] <-
        panel[panel$Panel == bin_panel[p] & panel$Marker == bin_marker[m], ]$Marker.Min

      # Marker size range max
      kit$Marker.Max[kit$Panel == bin_panel[p] & kit$Marker == bin_marker[m]] <-
        panel[panel$Panel == bin_panel[p] & panel$Marker == bin_marker[m], ]$Marker.Max
    }
  }

  # Estimate marker offset by taking the smallest ladder fragment
  # Round this to an integer
  # Subtract the number of base pair for that repeat

  # Get panels
  panel <- unique(kit$Panel)

  # Loop over all panels
  for (p in seq(along = panel)) {
    # Select current panel
    sel_panel <- kit$Panel == panel[p]

    # Get markers for current panel
    marker <- unique(kit$Marker[kit$Panel == panel[p]])

    # Loop over all markers
    for (m in seq(along = marker)) {
      # Select current marker
      sel_marker <- kit$Marker == marker[m]

      # Get smallest physical ladder fragment
      fragments <- kit$Size[sel_panel & sel_marker & kit$Virtual == 0]
      min_fragment <- min(fragments)

      # Get corresponding allele and convert to numeric
      min_allele <- kit$Allele[sel_panel & sel_marker & kit$Size == min_fragment]
      if (min_allele == "X") {
        min_allele <- 1
      }
      min_allele <- as.numeric(min_allele)

      # Get the repeat unit
      repeat_unit <- kit$Repeat[sel_panel & sel_marker & kit$Size == min_fragment]

      # Calculate offset
      min_fragment <- round(min_fragment)
      allele_size <- floor(min_allele) * repeat_unit + ((min_allele %% 1) * 10)
      marker_offset <- min_fragment - allele_size

      # Add new info for current marker in current panel
      kit$Offset[sel_panel & sel_marker] <- marker_offset
    }
  }

  return(kit)
}
