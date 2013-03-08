\name{calculateMx}
\alias{calculateMx}
\title{Calculate Mx}
\usage{
  calculateMx(data, ref1, ref2)
}
\arguments{
  \item{data}{data set in GeneMapper format containing at
  least columns 'Sample.Name', 'Marker', 'Allele', and
  'Height'.}

  \item{ref1}{a data frame containing known reference
  profile 1.}

  \item{ref2}{a data frame containing known reference
  profile 2.}
}
\value{
  data.frame with columns 'Sample.Name', 'Marker',
  'C1.Height.1', 'C1.Height.2', 'C2.Height.1',
  'C2.Height.2', 'C1.Mx', and 'C2.Mx'.
}
\description{
  \code{calculateMx} calculates the mixture proportion (Mx)
  given a set of reference samples.
}
\details{
  calculates the mixture proportion (Mx) given a set of
  reference samples. NB! sample name for ref1 and ref2 must
  be so that either one uniquely match the correct mix
  samples in data. e.g. data="A1-B2", ref1="A", ref2="B".
}
\examples{
print("Example will come later")
}
\keyword{internal}

