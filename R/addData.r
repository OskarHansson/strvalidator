################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 01: First version

# TODO: optional columns.

#' @title Adds new data columns to a data frame
#'
#' @description
#' \code{addData} Adds columns in 'newData' to 'data' by column 'byCol'.
#'
#' @details
#' Information in columns in data frame 'newData' is added to data frame 'data'
#' based on value in column 'byCol'.
#'   
#' @param newData Data frame containing information you want to add to 'data'.
#' @param data Data frame containing your main data.
#' 
#' @return data.frame the original data frame containing additional columns.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' # Get marker names for Promega PowerPlex ESX 17. Expand data frame.
#' x <- data.frame(Marker = rep(getKit("ESX17")$locus, c(1,2,3)))
#' # Get offsets for Promega PowerPlex ESX 17.
#' y <- data.frame(Marker = getKit("ESX17")$locus, Offset = getKit("ESX17")$offset)
#' # Get other kit information using string name.
#' z <- addData(data=x, newData=y, byCol="Marker")
#' print(x)
#' print(y)
#' print(z)

addData <- function(data, newData, byCol, exact=TRUE){
# Adds columns in 'newData' to 'data' by column 'byCol'.

	colNames <- names(data)

	colNamesNew <- names(newData)
	colNamesNew <- colNamesNew[colNamesNew!=byCol]

	# Add new columns to data.
	for(c in seq(along=colNamesNew)){
		data[colNamesNew[c]] <- NA
	}

	# Get unique identifiers.
	keys <- unique(as.character(data[ , byCol]))
	keysNew <- unique(as.character(newData[ , byCol]))

	# Loop through keys.
	for(k in seq(along=keysNew)){

		# Select rows.
		if(exact){
			selectedData <- data[ , byCol]==keys[k]
			selectedNewData <- newData[ , byCol]==keys[k]
		} else {
			selectedData <- grepl(keysNew[k], data[ , byCol])
			selectedNewData <- grepl(keysNew[k], newData[ , byCol])
		}

		for(c in seq(along=colNamesNew)){

			# Add new data.
			data[selectedData ,] [colNamesNew[c]] <- 
				as.matrix(newData[selectedNewData ,][colNamesNew[c]])
				
		}
	}
	return(data)
}