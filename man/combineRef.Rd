\name{combineRef}
\alias{combineRef}
\title{Combine reference samples}
\usage{
  combineRef(ref1, ref2, sort = FALSE, unique = FALSE)
}
\arguments{
  \item{ref1}{a data frame containing reference sample 1.}

  \item{ref2}{a data frame containing reference sample 2.}

  \item{sort}{logical for sorting. Not implemented.}

  \item{unique}{logical for keeping only unique alleles.
  Not implemented.}
}
\value{
  data.frame with columns 'Min', 'Max', 'Mean', n', and
  'Sd' for each target column.
}
\description{
  \code{combineRef} Combines two reference samples into
  one.
}
\details{
  Combines two data frames with reference samples into one
  data frame.
}

