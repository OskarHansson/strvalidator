################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 04: Roxygenized.
# 03: New parameter 'addMissingLevels'
# 02: Handles df without factors??
# 01: First working version.

#' @title Sort markers
#'
#' @description
#' \code{sortMarkers} Sort markers as they appear in the EPG.
#'
#' @details
#' Re-level 'Marker' and 'Dye' as defined in 'kit'.
#' Levels in data must be identical with kit information.
#' 
#' @param data data frame with genotype data in 'slim' format?
#' @param kit string or integer indicating kit.
#' @param addMissingLevels logical, TRUE missing markers are added, 
#' FALSE missing markers are not added.
#' 
#' @return data.frame with factor levels sorted according to 'kit'.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' print("Example will come")


sortMarkers <- function(data, kit, addMissingLevels = FALSE){

	if(is.null(levels(data$Marker))){
		current.marker.factors <- factor(data$Marker)
	}
	current.marker.levels <- levels(data$Marker)
	if(is.null(levels(data$Marker))){
		current.dye.factors <- factor(data$Dye)
	}
	current.dye.levels <- levels(data$Dye)

	# Get kit information.
	kit <- getKit(kit)
	new.marker.levels <- kit$locus
	new.dye.levels <- unique(kit$dye)

	# Check if identical levels.
	if(all(current.marker.levels %in% new.marker.levels)){

		# Add any missing factor levels.
		if(addMissingLevels){

			for(m in seq(along=new.marker.levels)){

				if(!new.marker.levels[m] %in% current.marker.levels){
					levels(data$Marker)[length(levels(data$Marker))+1] <- new.marker.levels[m]
				}
			}
		}

		# Change marker order as defined in kit.
		data$Marker<-factor(data$Marker, levels=new.marker.levels)

	} else {

		warning("Locus names in 'data' are not identical with locus names in 'kit'",
			call. = TRUE, immediate. = FALSE, domain = NULL)
	}

	# Check if identical levels.
	if(all(current.dye.levels %in% new.dye.levels)){

		# Change marker order as defined in kit.
		data$Dye<-factor(data$Dye, levels=new.dye.levels)

	} else {

		warning("Dye names in 'data' are not identical with dye names in 'kit'",
			call. = TRUE, immediate. = FALSE, domain = NULL)
	}

	return(data)

}
