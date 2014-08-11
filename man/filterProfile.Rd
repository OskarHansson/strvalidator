\name{filterProfile}
\alias{filterProfile}
\title{Filter out profiles from DNA results}
\usage{
filterProfile(data, ref, addMissingLoci = FALSE, keepNA = FALSE,
  ignoreCase = TRUE, debug = FALSE)
}
\arguments{
  \item{data}{data frame with genotype data in 'slim'
  format.}

  \item{ref}{data frame with reference profile in 'slim'
  format.}

  \item{keepNA}{logical. FALSE discards NA alleles.  TRUE
  keep loci/sample even if no matching allele.}

  \item{addMissingLoci}{logical. TRUE add loci present in
  ref but not in data. Overrides keepNA=FALSE.}

  \item{ignoreCase}{logical TRUE ignore case.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with extracted result.
}
\description{
\code{filterProfile} Filters out the result matching a specified
known profiles from typing data containing 'noise' such as stutters.
If 'ref' does not contain a 'Sample.Name' column it will be used
as reference for all samples in 'data'.
NB! addMissingLoci overrides keepNA.
}
\details{
Returns data where allele names match 'ref' allele names.
Required columns are: 'Sample.Name', 'Marker', and 'Allele'.
}

