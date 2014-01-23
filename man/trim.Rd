\name{trim}
\alias{trim}
\title{Trim data}
\usage{
trim(data, samples = NULL, columns = NULL, word = FALSE,
  ignoreCase = TRUE, invertS = FALSE, invertC = FALSE, rmNaCol = TRUE,
  rmEmptyCol = TRUE, missing = NA, debug = FALSE)
}
\arguments{
  \item{data}{data frame with genotype data.}

  \item{samples}{string giving sample names separated by
  pipe.}

  \item{columns}{string giving column names separated by
  pipe.}

  \item{word}{logical indicating if a word boundary should
  be added to \code{samples} and \code{columns}.}

  \item{ignoreCase}{logical, TRUE ignore case in sample
  names.}

  \item{invertS}{logical, FALSE samples given will be
  removed from 'data' while TRUE will remove samples NOT
  given.}

  \item{invertC}{logical, FALSE columns given will be
  removed from 'data' while TRUE will remove columns NOT
  given.}

  \item{rmNaCol}{logical, TRUE columns with only NA are
  removed from 'data' while FALSE will preserve the
  columns.}

  \item{rmEmptyCol}{logical, TRUE columns with no values
  are removed from 'data' while FALSE will preserve the
  columns.}

  \item{missing}{value to replace missing values with.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with extracted result.
}
\description{
\code{trim} Extracts data from a larger data set.
}
\details{
Simplifies extraction of specific data from a larger set of
typing data.
}

