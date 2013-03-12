\name{checkSubset}
\alias{checkSubset}
\title{check subset}
\usage{
  checkSubset(data, ref)
}
\arguments{
  \item{data}{a data frame in GeneMapper format containing
  column 'Sample.Name'.}

  \item{ref}{a data frame in GeneMapper format containing
  column 'Sample.Name', OR an atomic vector e.g. a single
  sample name string.}
}
\description{
  \code{checkSubset} checks the result of subsetting
}
\details{
  Check if ref and sample names are unique for subsetting.
  Prints the result to the R-prompt.
}

