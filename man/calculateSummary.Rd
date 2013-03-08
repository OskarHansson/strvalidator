\name{calculateSummary}
\alias{calculateSummary}
\title{Calculate summary}
\usage{
  calculateSummary(data, key, target)
}
\arguments{
  \item{data}{a data frame containing at least the 'key'
  and 'target' columns.}

  \item{key}{a vector indicating the columns from wich the
  keys will be created.}

  \item{target}{a vector indicating the columns for wich
  summary statistics should be calculated.}
}
\value{
  data.frame with columns 'Min', 'Max', 'Mean', n', and
  'Sd' for each target column.
}
\description{
  \code{calculateSummary} calculates summary statistics
}
\details{
  Calculates summary statistics for 'target' columns for
  each unique 'key' combination.  NB! Requires a 'slimmed'
  data frame.
}
\examples{
calculateSummary(data=data, key=c("Marker","Allele"), target=c("Size"))
}
\keyword{internal}

