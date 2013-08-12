################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 06.08.2013: Copied from 'pcrsim' to break dependency.

#' @title Convert dye letter to color name.
#'
#' @description
#' \code{dyeToColor} converts dye letters used in forensic 
#' DNA typing kits to color names in R.
#'
#' @details
#' Primers in forensic STR typing kits are labelled with a fluorescent
#' dye. The dyes are represented with single letters in exported result
#' files. For visualisation in R these are matched to R color names.
#' 
#' @param data data frame containing a 'Dye' column.
#' 
#' @return data.frame with additional column 'Color'.
#' 
#' @keywords internal
#' @export
#' @examples
#' # Some common designation for fluorescent dyes.
#' dyes <- data.frame(Dye=c("B","B","B","G","G","Y","R","R"))
#' # Get corresponding R colors.
#' colors <- dyeToColor(data=dyes)


dyeToColor <- function(data){
  
  # Available letter abreviations for colors:
  dyeLetters <- c("X", "B", "G", "Y", "R")
  
  # Numeric values corresponding to color abreviations:
  # NB! There are 8 colors that can be represented by a single number character, palette():
  # 1="black", 2="red", 3="green3", 4="blue", 5="cyan", 6="magenta", 7="yellow", 8="gray" 
  colorNames <- c("black", "blue", "green3", "black", "red")
  
  # Convert dye vector to numeric vector.
  data$Color <- colorNames[match(data$Dye, dyeLetters)]
  
  return(data)
}
