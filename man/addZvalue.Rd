\name{addZvalue}
\alias{addZvalue}
\title{Adds all kit information}
\usage{
  addZvalue(data, ref, byName = TRUE, ignoreCase = TRUE,
    matchSource = "ref")
}
\arguments{
  \item{data}{Data frame containing at least a column named
  'Marker'.}

  \item{ref}{...}

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
  data.frame the original data frame containing an
  additional column.
}
\description{
  \code{addZvalue} Applies the Z value to dna results.
}
\details{
  Takes (GM-formatted) data for samples and references
  (with Z column) as input. Adds a column 'Z' to the
  data.frame.
}
\examples{
# Get marker names for Promega PowerPlex ESX 17.
x <- data.frame(Marker = getKit("ESX17")$locus)
# Get other kit information using string name.
y <- addKitInfo(data=x, kit="ESX17")
print(x)
print(y)
}
\keyword{internal}

