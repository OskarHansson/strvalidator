\name{trim}
\alias{trim}
\title{Trim data}
\usage{
  trim(data, samples = NULL, columns = NULL, word = FALSE,
    ignoreCase = TRUE, invertS = FALSE, invertC = FALSE,
    rmNaCol = TRUE, rmEmptyCol = TRUE, missing = NA)
}
\arguments{
  \item{data}{data frame with genotype data in 'slim'
  format.}

  \item{samples}{string giving sample names separated by
  pipe.}

  \item{columns}{string giving column names separated by
  pipe.}

  \item{word}{logical indicating if a word boundary should
  be added to \code{samples} and \code{columns}.}

  \item{ignoreCase}{logical, TRUE ignore case in sample
  names.}

  \item{exact}{logical, FALSE match if sample/column
  contain string while TRUE requires exact matching of
  sample/column names.}

  \item{invertS}{logical, FALSE samples given will be
  removed from 'data' while TRUE will remove samples NOT
  given.}

  \item{invertC}{logical, FALSE columns given will be
  removed from 'data' while TRUE will remove columns NOT
  given.}

  \item{rmNAcols}{logical, TRUE columns with no values are
  removed from 'data' while FALSE will preserve all
  columns.}

  \item{missing}{value to replace missing values with.}
}
\value{
  data.frame with extracted result.
}
\description{
  \code{trim} Extracts data from a larger data set.
}
\details{
  Simplifies extraction of specific data from a larger set
  of typing data.
}
\examples{
# Remove all except ladder.
set1 <- trim(data=set1, samples="Ladder")
}
\keyword{internal}

