################################################################################
# CHANGE LOG (last 20 changes)
# 28.09.2024: Fixed plot order.
# 17.08.2024: New function to import kit from GeneMarker files.

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
read_gene_marker_kit <- function(xml_file_path, panel_name, debug = FALSE) {
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

      if (debug) {
        # Debugging output to verify marker boundaries
        message("Processing Marker: ", marker_title)
        message("  LowerBoundary: ", marker_min)
        message("  UpperBoundary: ", marker_max)
      }

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
          Color = NA,
          Repeat = nucleotide_repeats,
          Marker.Min = marker_min,
          Marker.Max = marker_max,
          Offset = NA,
          Short.Name = NA,
          Full.Name = NA,
          Sex.Marker = FALSE,
          Quality.Sensor = FALSE,
          Dye.Index = dye_index,
          stringsAsFactors = FALSE
        )
        index <- index + 1
      }
    }
  }

  # Convert the list to a data frame
  df <- do.call(rbind, result_list)

  # Define the dye translation table
  dye_translation_table <- data.frame(
    Dye.Index = c(1, 2, 7, 3, 4, 6, 5, 8),
    Plot.Order = c(1, 2, 3, 4, 5, 6, 7, 8),
    Color = c(
      "blue", "green", "cyan",
      "yellow", "red", "purple", "orange", "brown"
    ),
    # Are fluorescent marker constant over all kits?
    # Dye.Marker = c(
    #  "Fluorescein", "JOE", "AQA",
    #  "TMR", "CXR", "TOM", "WEN", "CCO"
    # ),
    stringsAsFactors = FALSE
  )

  # # Merge with the translation table based on Dye_Index
  # df <- merge(df, dye_translation_table,
  #   by = "Dye.Index",
  #   all.x = TRUE, sort = FALSE
  # )

  # Use 'match' to map 'Dye.Index' to 'Color'
  df$Color <- dye_translation_table$Color[match(
    df$Dye.Index, dye_translation_table$Dye.Index
  )]

  # Assign 'Plot.Order' using 'match'
  df$Plot.Order <- dye_translation_table$Plot.Order[match(
    df$Dye.Index, dye_translation_table$Dye.Index
  )]


  # Handle unmapped Dye_Index values
  if (any(is.na(df$Color))) {
    message("Unmapped Dye.Index values:")
    print(df[is.na(df$Color), ])
    warning("Some dyes could not be mapped. Update dye translation table.")
  }
  
  # Reorder the data frame based on Dye_Index and marker order
  df <- df[order(df$Plot.Order), ]

  # Remove unused columns
  df$Dye.Index <- NULL
  df$Plot.Order <- NULL

  return(df)
}
