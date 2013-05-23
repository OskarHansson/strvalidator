################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 02: Roxygenized and renamed from 'calculatePrecision' to 'calculateSummary'.
# 01: First version.

#' @title Calculate summary
#'
#' @description
#' \code{calculateSummary} calculates summary statistics
#'
#' @details
#' Calculates summary statistics for 'target' columns for each unique
#'  'key' combination.
#'  NB! Requires a 'slimmed' data frame.
#'  
#' @param data a data frame containing at least the 'key' and 'target' columns.
#' @param key a vector indicating the columns from wich the keys will be created.
#' @param target a vector indicating the columns for wich summary statistics should be calculated.
#' 
#' @return data.frame with columns 'Min', 'Max', 'Mean', n', and
#' 'Sd' for each target column.
#' 


calculateSummary <- function(data, key, target){

	# Find all key combinations.
	keyComb <- data[!duplicated(data[,key]), key]

	# Create new data frame.
	statistics <- c("Min", "Max", "Mean", "n", "Sd")
	statHead <- NULL
	# Create full heading.
	for(c in seq(along=target)){
		targetRep <- rep(target[c], length(statistics))
		targetPaste <- paste(targetRep, statistics, sep=".")
		statHead <- c(statHead, targetPaste)
	}
	# Add key headings.
	heading <- c(key, statHead)

	# Pre-allocate data frame.
	res <- data.frame(matrix(NA, nrow(keyComb), length(heading)))
	# Add column names.
	names(res) <- heading

	# Get key indexes.
	keyIndex <- vector()
	for(k in seq(along=key)){
		keyIndex[k] <- grep(key[k], names(data))
	}

	# Get columns.
	colSelection <- NULL
	for(c in seq(along=target)){
		colSelection[c] <- list(grepl(target[c], names(data)))
	}

	# Initiate counter.
	resRow <- 0

	# Loop through all rows.
	for(c in 1:nrow(keyComb)){

		# Get current combination.
		combination <- as.character(keyComb[c,])

		bool <- rep(TRUE, nrow(data))
		for(k in seq(along=key)) {
			bool = bool & data[,key[k]]==keyComb[c, k]
		}

		# Subset data.	
		dataSubset <- data[bool, ]

		# Calculate statistics.
		s.min <- min(as.numeric(dataSubset[,target]))
		s.max <- max(as.numeric(dataSubset[,target]))
		s.mean <- mean(as.numeric(dataSubset[,target]))
		s.n <- sum(!is.na(dataSubset[,target]))
		s.sd <- sd(as.numeric(dataSubset[,target]))

		# Increase counter and place in data frame.
		resRow <- resRow + 1
		res[resRow , ] <- c(combination, s.min, s.max, s.mean, s.n, s.sd)
 
	}

	# Convert 'stat' columns to numeric.
	for(c in match(statHead, names(res))){
		res[ , c] <- as.numeric(res[ , c] )
	}

	return(res)

}
