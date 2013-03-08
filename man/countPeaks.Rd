\name{countPeaks}
\alias{countPeaks}
\title{Calculate summary}
\usage{
  countPeaks(data, col = "Height", unique = TRUE)
}
\arguments{
  \item{data}{a data frame containing at least the 'key'
  and 'target' columns.}

  \item{col}{'Height' count using height columns, 'Allele'
  count using allele columns.}

  \item{target}{a vector indicating the columns for wich
  summary statistics should be calculated.}
}
\value{
  data.frame with columns 'Sample.Name', 'Marker', and
  'Peaks'.
}
\description{
  \code{countPeaks} Calculates the number of observed peaks
  per marker.
}
\details{
  Takes (GM-formatted) data for samples as input. Counts
  the number of peaks per marker (values in 'col' columns).
}
\examples{
print("Example will come later")
}
\keyword{internal}

