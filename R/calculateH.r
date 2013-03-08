################################################################################
# TODO LIST
# TODO: replace 'z' with homozygote status??

################################################################################
# CHANGE LOG
# 03: Roxygenized and changed name from 'averagePeakHeight' to 'calculateH'.
# 02: Added if condition for 'slim' and option 'debugInfo'.
# 01: First version

#' @title Calculate average peak height.
#'
#' @description
#' \code{calculateH} calculates the average peak height for a sample.
#'
#' @details
#' Calculates the average peak height (H) for a sample.
#' Takes (GM-formatted) data for samples as input.
#' Sample data must contain a column "Z", where 1 = heterozygote loci,
#' and 2 = homozygote loci as known from the reference sample.
#' Calculates H according to the formula:
#' \eqn{H = sum(peak heights)/(n[het] + 2n[hom]}
#' Where:
#' n[het] = number of observed heterozygote alleles
#' n[hom] = number of observed homozygote alleles
#' 
#' @param data a data frame with at least 'Sample.Name', 'Z', and 'Height' columns.
#' 
#' @return data.frame with with three columns: 'Sample.Name', 'H', and 'Peaks'.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' # Create test data.
#' x <- data("PositiveControl")
#' y <- alleleToSize(data=x, kit="ESX17")
#' print(x)
#' print(y)

calculateH <- function(data){


	# Create empty result data frame with NAs.
	res <- data.frame(t(rep(NA,3)))
	# Add column names.
	names(res) <- c("Sample.Name","H","Peaks")
	# Remove all NAs
	res <- res[-1,]

	# Get the sample names.
	sample.names <- unique(data$Sample.Name)

		# Loop through all samples.
		for (s in seq(along = sample.names)) {
		
			# Get sample name.
			current.sample.name <- sample.names[s]

			# Subset sample data.
			current.sample.rows <- data$Sample.Name == current.sample.name
			current.sample.data <- data[current.sample.rows,]
			
			# Sum all peak heights.
			height.columns<-grepl("Height",names(current.sample.data))
			total.peak.height <- sum(current.sample.data[,height.columns], na.rm=TRUE)

			# Sum number of peaks.
			total.observed.peaks <- sum(!is.na(current.sample.data[,height.columns]))
 
			# Apply the sum function over the height colums for each row in order
			# to count the number of values (!NA).
			# Then multiply with the Z-value.
			# Finally sum the products.
			data.sub <- current.sample.data[,height.columns]
			z <- current.sample.data$Z
			total.adjusted.peaks <- sum(apply(data.sub,1,function(x) sum(!is.na(x),na.rm=T))*z)

			# Calculate the average peak height.
			avg.peak.height <- total.peak.height / total.adjusted.peaks 

			# Save result in temporary data frame.
			tmp <- data.frame(Sample.Name = current.sample.name,
					H = avg.peak.height,
					Peaks = total.observed.peaks)

			# Add result to data frame.
			res <- rbind(res, tmp)

		}

	# Return result.
	return(res)

}
