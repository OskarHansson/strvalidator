\name{filterProfile}
\alias{filterProfile}
\title{Filter out profiles from DNA results}
\usage{
  filterProfile(data, ref)
}
\arguments{
  \item{data}{data frame with genotype data in 'slim'
  format.}

  \item{ref}{data frame with reference profile in 'slim'
  format.}
}
\value{
  data.frame with extracted result.
}
\description{
  \code{filterProfile} Filters out the result matching a
  specified known profiles from typing data containing
  'noise' such as stutters.
}
\details{
  Returns data where allele names match 'ref' allele names.
  Required columns are: 'Sample.Name', 'Marker', and
  'Allele'.
}

