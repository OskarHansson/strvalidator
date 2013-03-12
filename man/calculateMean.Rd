\name{calculateMean}
\alias{calculateMean}
\title{Calculate mean}
\usage{
  calculateMean(data, colBaseName, ignoreNA = TRUE,
    calculateNA = NULL, replaceNA = NULL)
}
\arguments{
  \item{data}{a data frame containing at least
  'Sample.Name' and 'colBaseName'.}

  \item{ignoreNA}{logical indicating how to handle NA.}

  \item{calculateNA}{value to replace NAs with before
  calculation.}

  \item{replaceNA}{value to replace NAs with in the final
  data frame.}
}
\value{
  data.frame with additional columns 'N' and 'Mean'.
}
\description{
  \code{calculateMean} calculates the mean of given one or
  more replicate measurments.
}
\details{
  Calculates the average given (one or) more replicate
  measurements.
}
\examples{
data(set1)
# Remove all except positive control samples.
set1 <- trim(data=set1, samples="PC")

calculateMean(data=set1, colBaseName="Height")
}
\keyword{internal}

