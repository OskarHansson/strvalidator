################################################################################
# TODO LIST
# TODO: Switch to xml?
# TODO: Degradation method dependent degradation factor?
# TODO: Kit dependent quant-rfu factor?
# TODO: Automatically create info from GM panels etc?
# TODO: Check offset. Do not mix actual bp with min range in marker range (panel/bins).
#        Best so far: estimate the offset by taking the smallest ladder fragment i.e. 98.28.
#        and round this to an integer (98) and subtract the number of base pair for that repeat i.e. 4*9=36,
#        which gives an offset of 98-36 = 62 bp for D3.
# TODO: Save information in text file / RData rather than in function.

################################################################################
# CHANGE LOG
# 05.06.2013: Added 'gender.marker'
# 19.05.2013: Re-written for reading data from text file.

#' @title Get kit
#'
#' @description
#' \code{getKit} provides information about STR kits.
#'
#' @details
#' The function returns the following information for a kit specified in kits.txt:
#'  Short kit name, Full kit name, Marker/locus names, Dye for each marker/locus,
#'  Start offset (in base pairs) for each marker, Size of repeating unit 
#'  (in base pairs) for each marker.
#'  If no matching kit or kit index is found NA is returned.
#'  If NULL a vector of available kits is printed and NA returned.
#' 
#' @param kitNameOrIndex string or integer specifying the kit.
#' @param showMessages logical, default TRUE for printing messages to the R promt.
#' @param debug logical indicating printing debug information.
#' 
#' @return list with kit information.
#' 
#' @keywords internal
#' 
#' @export 
#' @examples
#' # Show all information stored for kit with short name 'ESX17'.
#' getKit("ESX17")

getKit<-function(kitNameOrIndex=NULL, showMessages=FALSE, .kitInfo=NULL, debug=FALSE) {

  if(debug){
    print(paste("IN:", match.call()[[1]]))
  }

  .separator <- .Platform$file.sep # Platform dependent path separator.
  
  # LOAD KIT INFO  ############################################################

  if(is.null(.kitInfo)){
    # Get package path.
    packagePath <- path.package("strvalidator", quiet = FALSE)
    subFolder <- "extdata"
    fileName <- "kits.txt"
    
    filePath <- paste(packagePath, subFolder, fileName, sep=.separator)
    
    .kitInfo <- read.delim(file=filePath, header = TRUE, sep = "\t", quote = "\"",
                           dec = ".", fill = TRUE, stringsAsFactors=FALSE)
    
  }

  # Available kits. Must match else if construct.
  kits<-unique(.kitInfo$Short.Name)
  
	# Check if NULL
	if (is.null(kitNameOrIndex)) {

		# Print available kits
		if (showMessages){
			print("Available kits:")
			print(kits)
		}
		kit<-kits

	# String provided.
	} else {

		# Check if number or string.
		if (is.numeric(kitNameOrIndex)) {

			# Set index to number.
			index<-kitNameOrIndex

		} else {

			# Find matching kit index (case insensitive)
			index<-match(toupper(kitNameOrIndex),toupper(kits))

		}

		# No matching kit.
		if (is.na(index)) {
			
			# Print available kits
			if (showMessages){
				print("No matching kit!")
				print("Available kits:")
				print(kits)
			}
			kit<-NA

		# Assign matching kit information.
		} else {
      
      kit <- list(
        shortName = kits[index],
				fullName = unique(.kitInfo$Full.Name[.kitInfo$Short.Name==kits[index]]),
				locus = .kitInfo$Loci[.kitInfo$Short.Name==kits[index]],
				dye = .kitInfo$Dye[.kitInfo$Short.Name==kits[index]],
				offset = .kitInfo$Offset[.kitInfo$Short.Name==kits[index]],
				repeatUnit = .kitInfo$Repeat.Unit[.kitInfo$Short.Name==kits[index]],
				locusBalanceMean = .kitInfo$Locus.Balance.Mean[.kitInfo$Short.Name==kits[index]],
				locusBalanceSd = .kitInfo$Locus.Balance.Sd[.kitInfo$Short.Name==kits[index]],
        rangeMin = .kitInfo$Range.Min[.kitInfo$Short.Name==kits[index]],
        rangeMax = .kitInfo$Range.Max[.kitInfo$Short.Name==kits[index]],
        probPCR = .kitInfo$PCR.Efficiency[.kitInfo$Short.Name==kits[index]],
        genderMarker = .kitInfo$Gender.Marker[.kitInfo$Short.Name==kits[index]]
      )
		} 

	}

	# Return kit information (or NA)
	return(kit)

}
