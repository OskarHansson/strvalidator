################################################################################
# TODO LIST
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
# 09: Identifiler Plus added.
# 08: Return list of kits if no argument.
# 07: Roxygenized.
# 06: probPCR, and corrected size for ESX17, rangeMin, rangeMax
# 05: Simple test kit added.
# 04: ESX17 and corrected  an error in NGM.
# 03: interlocus.balance # Test values

#' @title Get kit
#'
#' @description
#' \code{getKit} provides information about STR kits.
#'
#' @details
#' The function returns the following information for a supported kit:
#'  Short kit name, Full kit name, Marker/locus names, Dye for each marker/locus,
#'  Start offset (in base pairs) for each marker, Size of repeating unit 
#'  (in base pairs) for each marker.
#'  If no matching kit or kit index is found NA is returned.
#'  If NULL a vector of available kits is printed and NA returned.
#' 
#' @param kitNameOrIndex string or integer specifying the kit.
#' @param showMessages logical, default TRUE for printing messages to the R promt.
#' 
#' @return list with kit information.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' getKit("ESX17")


getKit<-function(kitNameOrIndex=NULL, showMessages=FALSE) {

	# TODO: Error in getKit(typingKit, showMessages = TRUE) : 
 	# unused argument(s) (showMessages = TRUE)
	# No error with the line below:
	if(showMessages){
		print(paste("showMessages:", showMessages))
	}

  # Available kits. Must match else if construct.
	kits<-c("SGM Plus","NGM","ESX17","Identifiler","TestKit")
	
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
		} else if (index == 1) {
		
			# SGM Plus
			kit<-list(
				shortName = kits[index],
				fullName = "AmpFlSTR® SGM Plus® PCR Amplification Kit",
				locus = c("D3S1358","vWA","D16S539","D2S1338","Amelogenin","D8S1179","D21S11","D18S51","D19S433","TH01","FGA"),
				dye = c("B","B","B","B","G","G","G","G","Y","Y","Y"),
				offset = c(65,112,213,233,100,95,90,236,70,148,146),
				repeatUnit = c(4,4,4,4,4,4,4,4,4,4,4),
				locusBalanceMean = c(1,0.7,1.1,0.9,0.8,1,0.8,0.7,1.2,0.9,0.9),
				locusBalanceSd = c(0.1,0.07,0.11,0.09,0.08,0.1,0.08,0.07,0.12,0.09,0.09)
			)
		} else if (index == 2) {

			kit<-list(
				shortName = kits[index],
				fullName = "AmpFlSTR® NGM ™ PCR Amplification Kit",
				locus = c("D10S1248","vWA","D16S539","D2S1338","Amelogenin","D8S1179","D21S11","D18S51","D22S1045","D19S433","TH01","FGA","D2S441","D3S1358","D1S1656","D12S391"),
				dye = c("B","B","B","B","G","G","G","G","Y","Y","Y","Y","R","R","R","R"),
				offset = c(72,149,223.6,281.6,100,117.9,178.8,259.5,76,122.3,176.4,221.6,74.5,114.4,170,225),
				repeatUnit = c(4,4,4,4,9,4,4,4,3,4,4,4,4,4,4),
				locusBalanceMean = c(1,0.7,1.1,0.9,0.8,1,0.8,0.7,1.2,0.9,0.9,1,1,0.6,0.8),
				locusBalanceSd = c(0.1,0.07,0.11,0.09,0.08,0.1,0.08,0.07,0.12,0.09,0.09,0.1,0.1,0.06,0.08)
			)

		} else if (index == 3) {

			kit<-list(
				shortName = kits[index],
				fullName = "PowerPlex® ESX 17 System",
				locus = c("AMEL","D3S1358","TH01","D21S11","D18S51","D10S1248","D1S1656","D2S1338","D16S539","D22S1045","vWA","D8S1179","FGA","D2S441","D12S391","D19S433","SE33"),
				dye = c("B","B","B","B","B","G","G","G","G","Y","Y","Y","Y","R","R","R","R"),
				offset = c(76,62,137,103,253,45,95,152,253,57,85,176,208,56,77,171,253),
				repeatUnit = c(6,4,4,4,4,4,4,4,4,3,4,4,4,4,4,4,4),
				locusBalanceMean = c(1,0.7,1.1,0.9,0.8,1,0.8,0.7,1.2,0.9,0.9,1,1,0.6,0.8,1),
				locusBalanceSd = c(0.1,0.07,0.11,0.09,0.08,0.1,0.08,0.07,0.12,0.09,0.09,0.1,0.1,0.06,0.08,0.5),
				probPCR = c(0.6850, 0.7110, 0.7085, 0.6355, 0.6325, 0.6435, 0.7130, 0.6905, 0.6710, 0.6360, 0.6275, 0.6830, 0.6680, 0.9500, 0.7230, 0.8385, 0.7295),
				rangeMin = c(78, 92.01, 148.21, 195.01, 270.01, 70, 125.01, 184.01, 265.01, 70, 120.01, 200.01, 257.51, 80, 127.01, 188.01, 262.01),
				rangeMax = c(92, 148.2, 195, 270, 370, 125, 184, 265, 330, 120, 200, 257.5, 420, 127, 188, 262, 460)

			)
      
		} else if (index == 4) {
		  
		  kit<-list(
		    shortName = kits[index],
		    fullName = "AmpFlSTR® Identifiler® Plus PCR Amplification Kit",
		    locus = c("D8S1179","D21S11","D7S820","CSF1PO","D3S1358","TH01","D13S317","D16S539","D2S1338","D19S433","vWA","TPOX","D18S51","AMEL","D5S818","FGA"),
		    dye = c("B","B","B","B","G","G","G","G","G","Y","Y","Y","Y","R","R","R"),
		    offset = c(86,88,227,278,50,143,173,235,244,65,107,193,236,97,100,138),
		    repeatUnit = c(4,4,4,4,4,4,4,4,4,4,4,4,4,9,4,4),
		    locusBalanceMean = NA,
		    locusBalanceSd = NA,
		    probPCR = NA,
		    rangeMin = c(118,184.5,251,302.12,98,159,205.65,255.3,304.8,101,151,216.99,264.49,106,128,206.25),
		    rangeMax = c(183.5,247.5,298.5,348.63,148,205,250.16,301.81,370.31,148,213.5,260.99,350,114,180,360)
		    
		  )
		  
		} else if (index == 5) {
		  
		  kit<-list(
		    shortName = kits[index],
		    fullName = "A Simple Test Kit",
		    locus = c("A","B","C","D"),
		    dye = c("B","B","G","R"),
		    offset = c(72,149,223.6,281.6),
		    repeatUnit = c(2,3,4,5),
		    locusBalanceMean = c(1,0.7,1.1,0.9),
		    locusBalanceSd = c(0.1,0.07,0.11,0.09),
		    probPCR = c(0.6850, 0.7110, 0.7085, 0.9500)
		  )
		  
		  # No matching index. Available kits vector does not match else if construction.
		} else {

			if (showMessages){
				print("ERROR: Kit details not defined in function!")
			}
			kit<-NA
		}	

	}

	# Return kit information (or NA)
	return(kit)

}