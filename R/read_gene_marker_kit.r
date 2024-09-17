#' @title Read GeneMarker Kit Definition
#'
#' @description
#' Import kit definition from GeneMarker XML-files.
#'
#' @details Takes the GeneMarker kit XML-file and creates a
#' kit definition data frame.
#' @param xml_file_path the path to the XML file.
#' @param panel_name the name of the panel to be imported.
#'
#' @return data.frame
#'
#' @export
#'
#' @importFrom xml2 read_xml xml_find_all xml_text xml_find_first xml_attr
#'
read_gene_marker_kit <- function(xml_file_path, panel_name) {
  # Check if file exists
  if (!file.exists(xml_file_path)) {
    stop("The specified XML file does not exist.")
  }

  # Validate panel name
  if (missing(panel_name) || panel_name == "") {
    stop("The panel name must be provided.")
  }

  # Read the XML file
  xml_data <- read_xml(xml_file_path)

  # Determine if all panels should be processed
  if (panel_name == "All Panels") {
    # Extract all panels
    panels <- xml_find_all(xml_data, "//Panel")
  } else {
    # Extract the panel based on the specified panel name
    panel <- xml_find_all(
      xml_data,
      paste0("//Panel[PanelName='", panel_name, "']")
    )

    if (length(panel) == 0) {
      stop(paste(
        "The panel named",
        panel_name,
        "was not found in the XML file."
      ))
    }
  }

  if (length(panels) == 0) {
    stop("No panels found in the XML file.")
  }

  # Initialize a list to collect the rows
  result_list <- list()
  index <- 1

  # Loop through each panel
  for (panel in panels) {
    panel_name <- xml_text(xml_find_first(panel, "./PanelName"))
    message("Processing Panel: ", panel_name)

    # Extract markers for the specified panel
    markers <- xml_find_all(panel, ".//Loci/Locus")

    # Loop through each marker
    for (marker in markers) {
      marker_title <- xml_text(xml_find_first(marker, "./MarkerTitle"))
      dye_index <- as.integer(
        xml_text(xml_find_first(marker, "./DyeIndex"))
      )
      nucleotide_repeats <- as.integer(
        xml_text(xml_find_first(marker, "./n_NucleotideRepeats"))
      )

      # Extracting marker boundaries (if available)
      marker_min <- as.numeric(
        xml_text(xml_find_first(marker, "./LowerBoundary"))
      )
      marker_max <- as.numeric(
        xml_text(xml_find_first(marker, "./UpperBoundary"))
      )

      # Debugging output to verify marker boundaries
      message("Processing Marker: ", marker_title)
      message("  LowerBoundary: ", marker_min)
      message("  UpperBoundary: ", marker_max)

      alleles <- xml_find_all(marker, "./Allele")

      # Loop through each allele
      for (allele in alleles) {
        size <- as.numeric(xml_attr(allele, "Size"))
        left_binning <- as.numeric(xml_attr(allele, "Left_Binning"))
        right_binning <- as.numeric(xml_attr(allele, "Right_Binning"))

        result_list[[index]] <- data.frame(
          Panel = panel_name,
          Marker = marker_title,
          Allele = xml_attr(allele, "Label"),
          Size = size,
          Size.Min = size - left_binning,
          Size.Max = size + right_binning,
          Virtual = as.integer(xml_attr(allele, "Control")),
          Color = dye_index,
          Repeat = nucleotide_repeats,
          Marker.Min = marker_min,
          Marker.Max = marker_max,
          Offset = NA,
          Short.Name = NA,
          Full.Name = NA,
          Sex.Marker = FALSE,
          Quality.Sensor = FALSE,
          stringsAsFactors = FALSE
        )
        index <- index + 1
      }
    }
  }

  # Convert the list to a data frame
  df <- do.call(rbind, result_list)

  # Replace integer values for Color with the color mapping
  color_mapping <- c(
    "blue", "green", "yellow", "red", "orange",
    "purple", "cyan", "brown"
  )
  df$Color <- ifelse(df$Color > length(color_mapping) | df$Color < 1,
    NA,
    color_mapping[df$Color]
  )

  return(df)
}
