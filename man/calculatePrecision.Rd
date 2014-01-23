\name{calculatePrecision}
\alias{calculatePrecision}
\title{Calculate precision}
\usage{
calculatePrecision(data, key = c("Marker", "Allele"), target = c("Size"),
  debug = FALSE)
}
\arguments{
  \item{data}{Data frame containing at least columns
  defined in 'key' and 'target'.}

  \item{key}{vector containing column names to create keys
  from.}

  \item{target}{vector containing column <base> names to
  calculate precision for.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with results.
}
\description{
\code{calculatePrecision} Calculates the precision.
}
\details{
Calculates summary statistics for 'target' columns for each
unique 'key' combination. For example the precision of
determined size for alleles in multiple allelic ladders.
Requires a 'slimmed' and 'filtered' data frame.
}

