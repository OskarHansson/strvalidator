################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 03: Roxygenized.

#' @title Calculate Mx
#'
#' @description
#' \code{calculateMx} calculates the mixture proportion (Mx) given a set of
#'  reference samples.
#'
#' @details
#' calculates the mixture proportion (Mx) given a set of reference samples.
#' NB! sample name for ref1 and ref2 must be so that either one uniquely match 
#' the correct mix samples in data. e.g. data="A1-B2", ref1="A", ref2="B".
#' 
#' @param data data set in GeneMapper format containing at least
#'  columns 'Sample.Name', 'Marker', 'Allele', and 'Height'.
#' @param ref1 a data frame containing known reference profile 1.
#' @param ref2 a data frame containing known reference profile 2.
#' 
#' @return data.frame with columns 'Sample.Name', 'Marker', 'C1.Height.1',
#' 'C1.Height.2', 'C2.Height.1', 'C2.Height.2', 'C1.Mx', and 'C2.Mx'.
#' 
#' @keywords internal
#' 
#' @export true
#' @examples
#' print("Example will come later")


calculateMx <- function(data, ref1, ref2){


	# Combine reference profiles.
	ref12 <- combineRef(ref1, ref2)
	# Can't use combined sample name to match samples in data.
	ref12$Sample.Name <- ref1$Sample.Name

	# Filter data.
	data <- filterProfile(data=data, ref=ref12)

	# Create dataframe.
	res <- data[ , grepl("Sample.Name|Marker",names(data))]
	res$C1.Height.1 <- NA
	res$C1.Height.2 <- NA
	res$C2.Height.1 <- NA
	res$C2.Height.2 <- NA
	res$C1.Mx <- NA
	res$C2.Mx <- NA
	
	# Get columns.
	alleleCol <- grepl("Allele",names(data))
	alleleColRef1 <- grepl("Allele",names(ref1))
	alleleColRef2 <- grepl("Allele",names(ref2))
	heightCol <- grepl("Height",names(data))

	# Get number of alleles (including row names).
	alleles<-rowSums(!is.na(data[ , alleleCol]))

	# Get subset with 4 expected alleles.
	expAlleles <- countPeaks(ref12, col="Allele")

	# Loop over data.
	for(r in seq(along=data[ , 1])){

		# Get row name.
		rowName <- row.names(data[r,])

		# Get current marker.			
		marker <- data$Marker[r]

		# Get expected number of alleles.
		nAlleles <- expAlleles[expAlleles$Marker==marker, ]$Peaks

		if(nAlleles==4){

			# Map contributor data.
			m1<-match(ref1[ref1$Marker==marker, alleleColRef1], 
				data[r, alleleCol])
			m2<-match(ref2[ref2$Marker==marker, alleleColRef2], 
				data[r, alleleCol])

			#Remove NA.
			m1 <- m1[!is.na(m1)]
			m2 <- m2[!is.na(m2)]

			# Get peak heights per contributor.
			if(!length(m1)){
				h1 <- 0
			} else {
				h1 <- data[r,heightCol][,m1]
			}
			if(!length(m2)){
				h2 <- 0
			} else {
				h2 <- data[r,heightCol][,m2]
			}

			# Sum peak heights per contributor.
			s1<-sum(h1)
			s2<-sum(h2)

			# Calculate mixture proportion.
			mx1 <- 1-s1/(s1+s2)
			mx2 <- 1 - mx1

			# Save result in data frame.
			res[rowName ,]$C1.Height.1 <- h1[1]
			res[rowName ,]$C1.Height.2 <- h1[2]
			res[rowName ,]$C2.Height.1 <- h2[1]
			res[rowName ,]$C2.Height.2 <- h2[2]
			res[rowName ,]$C1.Mx <- mx1
			res[rowName ,]$C2.Mx <- mx2

		}

		if(nAlleles==4){

			# Map contributor data.
			m1<-match(ref1[ref1$Marker==marker, alleleColRef1], 
				data[r, alleleCol])
			m2<-match(ref2[ref2$Marker==marker, alleleColRef2], 
				data[r, alleleCol])

			# Remove NA.
			m1 <- m1[!is.na(m1)]
			m2 <- m2[!is.na(m2)]

			# Discard shared alleles.
			m1d <- setdiff(m1, m2)
			m2d <- setdiff(m2, m1)

			# Get peak heights per contributor.
			if(!length(m1d)){
				h1 <- 0
			} else {
				h1 <- data[r,heightCol][,m1d]
			}
			if(!length(m2d)){
				h2 <- 0
			} else {
				h2 <- data[r,heightCol][,m2d]
			}
	
			# Sum peak heights per contributor.
			s1<-sum(h1)
			s2<-sum(h2)

			# Calculate mixture proportion.
			mx1 <- 1-s1/(s1+s2)
			mx2 <- 1 - mx1

			# Save result in data frame.
			res[rowName ,]$C1.Height.1 <- h1[1]
			res[rowName ,]$C1.Height.2 <- h1[2]
			res[rowName ,]$C2.Height.1 <- h2[1]
			res[rowName ,]$C2.Height.2 <- h2[2]
			res[rowName ,]$C1.Mx <- mx1
			res[rowName ,]$C2.Mx <- mx2

		}
	}

	return (res)
}
