\name{calculateDropout}
\alias{calculateDropout}
\title{Calculate drop-out events}
\usage{
  calculateDropout(data, ref, ignoreCase = TRUE,
    debug = FALSE)
}
\arguments{
  \item{data}{data frame in GeneMapper format containing at
  least a column 'Allele'.}

  \item{ref}{data frame in GeneMapper format.}

  \item{ignoreCase}{logical, default TRUE for case
  insensitive.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
  data.frame with columns 'Sample.Name', 'Marker',
  'Allele', 'Height', 'Dropout', 'Rfu', and 'Heterozygous'.
  Dropout: 0 indicate no dropout, 1 indicate allele
  dropout, and 2 indicate locus dropout. Rfu: height of
  surviving allele. Heterozygous: 1 for heterozygous and 0
  for homozygous.
}
\description{
  \code{calculateDropout} calculate drop-out events (allele
  and locus) and records the surviving peak height.
}
\details{
  Calculates drop-out events. In case of allele dropout the
  peak height of the surviving allele is given. Homozygous
  alleles in the reference set can be either single or
  double notation (X or X X). Markers present in the
  reference set but not in the data set will be added to
  the result. NB! "Sample Names" in 'ref' must be unique
  'core' name of replicate sample names in 'data'. Use
  \code{checkSubset} to make sure subsetting works as
  intended.
}
\examples{
data(set4)
data(ref4)
drop <- calculateDropout(data=set4, ref=ref4, ignoreCase=TRUE)
}

