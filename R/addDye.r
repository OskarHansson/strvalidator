################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 01: First version

#' @title Adds dye information to a data frame
#'
#' @description
#' \code{addDye} Adds a 'Dye' column to 'data' based on the information in 'kit'.
#'
#' @details
#' Information about which dye each marker is labelled with in the specified
#' STR typing kit 'kit' is added as a new column to a data frame containing
#' at least a column 'Marker'.
#'   
#' @param data Data frame containing at least a column 'Marker'.
#' @param kit String or integer specifying the STR typing kit.
#' 
#' @return data.frame the original data frame containing an additional column.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' # Get marker names for Promega PowerPlex ESX 17.
#' x <- data.frame(Marker = getKit("ESX17")$locus)
#' # Get other kit information using string name.
#' y <- addDye(data=x, kit="ESX17")
#' print(x)
#' print(y)

addDye <- function(data, kit){
# Adds a 'Dye' column to 'data' based on the information in 'kit'.

	kitData <- getKit(kit)

	markers <- unique(data$Marker)
	for(m in seq(along=markers)){

		data$Dye[data$Marker==markers[m]] <- kitData$dye[kitData$locus==markers[m]]
	}
	return(data)
}

