\name{calculateDropout}
\alias{calculateDropout}
\title{Calculate drop-out events}
\usage{
  calculateDropout(data, ref, ignoreCase = TRUE)
}
\arguments{
  \item{data}{data frame in GeneMapper format containing at
  least a column 'Allele'.}

  \item{ref}{data frame in GeneMapper format.}

  \item{ignoreCase}{logical, default TRUE for case
  insensitive.}
}
\value{
  data.frame with columns 'Dropout', indicating no dropout
  (0), allele (1) and locus dropout (2), and 'Rfu',
  'Heterozygous'.
}
\description{
  \code{calculateDropout} calculate drop-out events (allele
  and locus) and records the surviving peak height.
}
\details{
  Calculates drop-out events. In case of allele dropout the
  peak height of the surviving allele is given. NB! "Sample
  Names" in 'ref' must be unique 'core' name of replicate
  sample names in 'data'. NB! Homozygous alleles must be
  doubled (X -> X/X).
}
\examples{
#newData<-dropStat(data=data, ref=ref)
}
\keyword{internal}

