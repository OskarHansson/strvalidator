\name{checkSubset}
\alias{checkSubset}
\title{check subset}
\usage{
checkSubset(data, ref, console = TRUE, ignoreCase = TRUE, word = FALSE,
  debug = FALSE)
}
\arguments{
  \item{data}{a data frame in GeneMapper format containing
  column 'Sample.Name'.}

  \item{ref}{a data frame in GeneMapper format containing
  column 'Sample.Name', OR an atomic vector e.g. a single
  sample name string.}

  \item{console}{logical, if TRUE result is printed to R
  console, if FALSE a string is returned.}

  \item{ignoreCase}{logical, if TRUE case insesitive
  matching is used.}

  \item{word}{logical, if TRUE only exact match.}

  \item{debug}{logical indicating printing debug
  information.}
}
\description{
\code{checkSubset} checks the result of subsetting
}
\details{
Check if ref and sample names are unique for subsetting.
Prints the result to the R-prompt.
}

