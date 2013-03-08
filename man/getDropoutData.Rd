\name{getDropoutData}
\alias{getDropoutData}
\title{Get drop-out events}
\usage{
  getDropoutData(data)
}
\arguments{
  \item{data}{data frame with genotype data in "GeneMapper"
  format. Requires at least columns 'Marker' (locus names),
  'Height' (Height.1 and Height.2), 'Dropout' (no dropout
  '0', allele '1', and locus dropout '2'), 'Heterozygous'
  (heterozygous '1' or homozygous '0' loci).}
}
\value{
  data.frame a data frame with the following columns
  'Marker' (locus name), 'Height' (peak height in rfu of
  the surviving allele), 'Dropout' (indicates if the sister
  allele has dropped out '1' or not '0').
}
\description{
  \code{getDropoutData} extracts the drop-out information
  from a data set where drop-outs have been calculated.
}
\details{
  Extracts loci, heterozygote peak heights, and dropout
  status from dataset. Required columns: 'Marker' with
  locus names, 'Height.1' and 'Height.2', 'Dropout' with
  dropout status as described for \code{calculateDropout},
  'Heterozygous' with 1 for heterozygous loci and 0 for
  homozygous loci. Gets all observations (e.g. 90/0, 105/95
  -> 90-1, 105-0, 95-0)
}
\examples{
print("Example will come")
}
\keyword{internal}

