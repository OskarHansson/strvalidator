\name{addZygosity}
\alias{addZygosity}
\title{Add zygosity}
\usage{
  addZygosity(data, ref, byName = TRUE, ignoreCase = TRUE,
    matchSource = "ref")
}
\arguments{
  \item{data}{Data frame containing at least a column named
  'Alleles'.}

  \item{byName}{if TRUE matching of samples and ref by
  name, if FALSE by position.}

  \item{matchSource}{if 'ref' sample names from 'ref' is
  used as source for matching. Allowes for partial matching
  of sample names (e.g. 'AB' matches '01_ABc'). NB! names
  in 'ref' must be unique to sample names refering to a
  single DNA source. If 'data' sample names from 'data' is
  used as source for matching.  Only identical sample names
  match.}

  \item{ignoreCase}{If TRUE name matching is case
  insensitive.}
}
\value{
  data.frame the original data frame containing additional
  columns.
}
\description{
  \code{addZygosity} Adds the zygosity for each marker.
}
\details{
  Compares a data set to reference profiles. Adds zygosity
  (i.e. the number of expected peaks) to a new column in
  'data'.
}
\examples{
# Make some test data
x <- data.frame(Marker=c("D3","D18","FGA"), Allele.1=c(10,8,20), Allele.2=c(12,11,22))
addZygosity(x)
}
\keyword{internal}

