\name{sortMarkers}
\alias{sortMarkers}
\title{Sort markers}
\usage{
  sortMarkers(data, kit, addMissingLevels = FALSE)
}
\arguments{
  \item{data}{data frame with genotype data in 'slim'
  format?}

  \item{kit}{string or integer indicating kit.}

  \item{addMissingLevels}{logical, TRUE missing markers are
  added, FALSE missing markers are not added.}
}
\value{
  data.frame with factor levels sorted according to 'kit'.
}
\description{
  \code{sortMarkers} Sort markers as they appear in the
  EPG.
}
\details{
  Re-level 'Marker' and 'Dye' as defined in 'kit'. Levels
  in data must be identical with kit information.
}
\examples{
print("Example will come")
}
\keyword{internal}

