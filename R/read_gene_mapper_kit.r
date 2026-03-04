# ---------------------------------------------------------------------------
# Detect GeneMapper file version from header lines
#
# The GeneMapper text formats (bins/panels) always contain a line starting
# with "Version", e.g.:
#   Version    GM v 3.0
#   Version    GMID-X v 1.0
#
# We use this to distinguish between classic GeneMapper ID ("GM")
# and GeneMapper ID-X ("GMID-X") formats and select the correct parsing
# template.
#
# Args:
#   lines : character vector returned by readLines()
#
# Returns:
#   Character scalar: "GMID" or "GMID-X"
#
# Errors:
#   Stops if the Version line is missing or unrecognized.
# ---------------------------------------------------------------------------
detect_gm_version <- function(lines) {
  # Extract the Version header line
  ver_line <- grep("^Version", lines, value = TRUE)

  if (length(ver_line) == 0) {
    stop("Missing 'Version' header in GeneMapper file")
  }

  # Match GeneMapper ID-X:
  if (grepl("GMID-X", ver_line, ignore.case = TRUE)) {
    return("GMID-X")
  }

  # Match plain GeneMapper ID:
  # Word boundaries \\bGM\\b ensures that we match "GM" as a standalone word,
  # not as part of "GMID" or "GMID-X"
  if (grepl("\\bGM\\b", ver_line, ignore.case = TRUE)) {
    return("GMID")
  }

  stop("Unknown GeneMapper version string: ", ver_line)
}


#' @title Read GeneMapper Kit Definition
#'
#' @description
#' Import kit definitions from GeneMapper ID and GeneMapper ID-X *bins* and
#' *panels* files to create a standardized kit table used by STR-validator.
#'
#' @details
#' The function reads GeneMapper ID and GeneMapper ID-X `*.bins` and/or `*.panels`
#' files and parses the allele, marker, and dye information into a unified data
#' frame. If both types of files are provided, the parsed data are merged using
#' \code{\link{combine_bins_and_panels}} to produce a complete kit definition
#' table.
#'
#' @param bin_files Character with path to GeneMapper `.bins` file.
#' @param panel_files Character with path to GeneMapper `.panels` file.
#' @param debug Logical; if \code{TRUE}, print detailed messages during import.
#'
#' @return
#' A \code{data.frame} containing kit information with at least the following
#' columns:
#' \itemize{
#'   \item \code{Panel} - panel name
#'   \item \code{Marker} - marker name
#'   \item \code{Allele} - allele designation
#'   \item \code{Size}, \code{Size.Min}, \code{Size.Max} - size information
#'   \item \code{Color} - dye color channel
#'   \item \code{Repeat} - repeat unit length
#'   \item \code{Offset} - estimated marker offset
#' }
#'
#' @seealso
#' \code{\link{combine_bins_and_panels}} for merging bins and panels,
#' \code{\link[xml2]{read_xml}} for XML file reading.
#'
#' @examples
#' \dontrun{
#' kit <- read_gene_mapper_kit(
#'   bin_files = "C:/kits/GlobalFiler.bins",
#'   panel_files = "C:/kits/GlobalFiler.panels",
#'   debug = TRUE
#' )
#' head(kit)
#' }
#'
#' @export
#'
#' @importFrom xml2 read_xml xml_find_all xml_text xml_find_first xml_attr

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

    # Detect GeneMapper version
    gm_version <- detect_gm_version(file_content)
    bin_schema <- bin_schema[[gm_version]]

    split_content <- strsplit(file_content, delimiter)

    results <- list()
    panel_name <- NULL
    marker_name <- NULL

    # Loop over all rows
    for (current_row in split_content) {
      current_tag <- current_row[1]

      if (current_tag == key_panel) {
        panel_name <- current_row[2]
        if (debug) message("Processing Panel: ", panel_name)
        next
      }

      if (current_tag == key_marker) {
        marker_name <- current_row[2]
        if (debug) message("Processing Marker: ", marker_name)
        next
      }

      if (!is.null(panel_name) && !is.null(marker_name)) {
        allele_name <- current_row[1]
        allele_size <- as.numeric(current_row[2])
        allele_min <- as.numeric(current_row[3])
        allele_max <- as.numeric(current_row[4])

        allele_virtual <- 0L # Default to physical allele fragment
        if (bin_schema$has_virtual &&
          length(current_row) >= 5 &&
          grepl(bin_schema$virtual_value,
            current_row[5],
            ignore.case = TRUE
          )) {
          allele_virtual <- 1L # Virtual allele bin
        }

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

    # Detect GeneMapper version
    gm_version <- detect_gm_version(file_content)
    panel_schema <- panel_schema[[gm_version]]

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

        repeat_unit <- as.numeric(current_row[panel_schema$repeat_col])

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

  if (length(bin_files) > 1 || length(panel_files) > 1) {
    stop("Please provide only one .bins and one .panels file per import call.")
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

  # ---------------------------------------------------------------------------
  # Enforce matching GeneMapper versions between bins and panels
  # ---------------------------------------------------------------------------

  if (!is.null(bin_files) && !is.null(panel_files)) {
    bin_version <- detect_gm_version(readLines(bin_files))
    panel_version <- detect_gm_version(readLines(panel_files))

    if (bin_version != panel_version) {
      stop(
        "Bins and panels files are from different GeneMapper versions: ",
        bin_version, " vs ", panel_version
      )
    }
  }

  # Combine the results if both files are provided using combine_bins_and_panels
  if (!is.null(bins_result) && !is.null(panels_result)) {
    combined_result <- combine_bins_and_panels(bins_result, panels_result)

    attr(combined_result, "GeneMapperVersion") <- bin_version
    attr(combined_result, "SourceFiles") <- list(
      bins = bin_files,
      panels = panel_files
    )

    return(combined_result)
  } else if (!is.null(bins_result)) {
    return(bins_result)
  } else if (!is.null(panels_result)) {
    return(panels_result)
  }

  return(NULL)
}

#' @title Combine GeneMapper Bins and Panels
#'
#' @description
#' Merge parsed GeneMapper \code{.bins} and \code{.panels} data frames into a
#' unified kit definition table used by STR-validator.
#'
#' @details
#' This function takes two data frames - one from a GeneMapper bins file and one
#' from a panels file - and merges them based on shared panel and marker names.
#' It adds color channel, repeat unit, and marker range information from the
#' panels file to the bins data, and estimates an offset value for each marker.
#'
#' The offset is calculated from the smallest non-virtual fragment within each
#' marker as:
#' \deqn{Offset = round(MinFragment) - (AlleleValue * RepeatUnit)}
#' where \code{AlleleValue} is the numeric equivalent of the smallest allele.
#'
#' @param bin A \code{data.frame} parsed from a GeneMapper \code{.bins} file,
#' containing columns \code{Panel}, \code{Marker}, \code{Allele},
#' \code{Size}, \code{Size.Min}, \code{Size.Max}, and \code{Virtual}.
#' @param panel A \code{data.frame} parsed from a GeneMapper \code{.panels} file,
#' containing columns \code{Panel}, \code{Marker}, \code{Color},
#' \code{Marker.Min}, \code{Marker.Max}, and \code{Repeat}.
#'
#' @return
#' A merged \code{data.frame} containing the full kit definition with added
#' columns:
#' \itemize{
#'   \item \code{Color} - dye color channel
#'   \item \code{Repeat} - repeat unit length
#'   \item \code{Marker.Min}, \code{Marker.Max} - marker range
#'   \item \code{Offset} - calculated size offset
#' }
#'
#' @seealso
#' \code{\link{read_gene_mapper_kit}} for reading bins and panel files before combining.
#'
#' @examples
#' \dontrun{
#' bins <- read_gene_mapper_kit(bin_files = "GlobalFiler.bins")
#' panels <- read_gene_mapper_kit(panel_files = "GlobalFiler.panels")
#' kit <- combine_bins_and_panels(bins, panels)
#' head(kit)
#' }
#'
#' @export

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

  if (!setequal(bin_panel, bin_panel2)) {
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
      idx <- kit$Panel == bin_panel[p] & kit$Marker == bin_marker[m]

      # Color
      val <- panel$Color[
        panel$Panel == bin_panel[p] &
          panel$Marker == bin_marker[m]
      ]

      if (length(val) != 1) {
        stop(
          "Expected exactly one panel definition for marker '",
          bin_marker[m], "' in panel '", bin_panel[p],
          "', found ", length(val)
        )
      }

      kit$Color[idx] <- val

      # Repeat unit size
      val <- panel$Repeat[
        panel$Panel == bin_panel[p] &
          panel$Marker == bin_marker[m]
      ]

      if (length(val) != 1) {
        stop(
          "Expected exactly one repeat unit for marker '",
          bin_marker[m], "' in panel '", bin_panel[p],
          "', found ", length(val)
        )
      }

      kit$Repeat[idx] <- val

      # Marker size range min
      val <- panel$Marker.Min[
        panel$Panel == bin_panel[p] &
          panel$Marker == bin_marker[m]
      ]

      if (length(val) != 1) {
        stop(
          "Expected exactly one marker size range min for marker '",
          bin_marker[m], "' in panel '", bin_panel[p],
          "', found ", length(val)
        )
      }

      kit$Marker.Min[idx] <- val

      # Marker size range max
      val <- panel$Marker.Max[
        panel$Panel == bin_panel[p] &
          panel$Marker == bin_marker[m]
      ]

      if (length(val) != 1) {
        stop(
          "Expected exactly one marker size range max for marker '",
          bin_marker[m], "' in panel '", bin_panel[p],
          "', found ", length(val)
        )
      }

      kit$Marker.Max[idx] <- val
    }
  }

  # ---------------------------------------------------------------------------
  # Estimate marker offset using the smallest non-virtual ladder fragment
  # Round this to an integer
  # Subtract the number of base pair for that repeat
  # Microvariants: allele .1/.2/.3 correspond to 1-3 extra base pairs
  #
  # Offset is only estimated for markers with numeric allele designations.
  # If the smallest allele is non-numeric (e.g. X/Y or letter-based systems),
  # Offset is set to NA and estimation is skipped for that marker.
  # ---------------------------------------------------------------------------

  # Get panels
  panel <- unique(kit$Panel)

  # Loop over all panels
  for (p in seq(along = panel)) {
    # Select current panel
    sel_panel <- kit$Panel == panel[p]

    # Get markers for current panel
    marker <- unique(kit$Marker[sel_panel])

    # Loop over all markers
    for (m in seq(along = marker)) {
      # Select current marker
      sel_marker <- kit$Marker == marker[m]

      # Get smallest physical (non-virtual) ladder fragment
      fragments <- kit$Size[sel_panel & sel_marker & kit$Virtual == 0]

      if (length(fragments) == 0) {
        kit$Offset[sel_panel & sel_marker] <- NA_real_
        next
      }

      min_fragment <- min(fragments)

      # Get corresponding allele(s)
      alleles <- kit$Allele[
        sel_panel & sel_marker & kit$Size == min_fragment
      ]
      alleles <- unique(alleles)

      if (length(alleles) != 1) {
        stop(
          "Expected exactly one smallest allele for marker '",
          marker[m], "' in panel '", panel[p],
          "', found: ", paste(alleles, collapse = ", ")
        )
      }

      min_allele <- alleles

      # Offset can only be estimated for numeric allele designations
      min_allele_num <- suppressWarnings(as.numeric(min_allele))
      if (is.na(min_allele_num)) {
        kit$Offset[sel_panel & sel_marker] <- NA_real_
        next
      }

      # Get the repeat unit
      repeat_unit <- unique(
        kit$Repeat[sel_panel & sel_marker & kit$Size == min_fragment]
      )

      if (length(repeat_unit) != 1 || is.na(repeat_unit)) {
        kit$Offset[sel_panel & sel_marker] <- NA_real_
        next
      }

      # Calculate offset
      min_fragment <- round(min_fragment)
      allele_size <- floor(min_allele_num) * repeat_unit +
        ((min_allele_num %% 1) * 10)

      marker_offset <- min_fragment - allele_size

      # Assign offset for all alleles of this marker
      kit$Offset[sel_panel & sel_marker] <- marker_offset
    }
  }

  return(kit)
}
