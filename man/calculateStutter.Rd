\name{calculateStutter}
\alias{calculateStutter}
\title{Calculate stutter}
\usage{
calculateStutter(data, ref, back = 2, forward = 1, interference = 0,
  replaceVal = NULL, byVal = NULL, debug = FALSE)
}
\arguments{
  \item{data}{data frame with genotype data. Requires
  columns 'Sample.Name', 'Marker', 'Allele', 'Height'.}

  \item{ref}{data frame with the known profiles. Requires
  columns 'Sample.Name', 'Marker', 'Allele'.}

  \item{back}{integer for the maximal number of backward
  stutters (max size difference 2 = n-2 repeats).}

  \item{forward}{integer for the maximal number of forward
  stutters (max size difference 1 = n+1 repeats).}

  \item{interference}{integer specifying accepted level of
  allowed overlap.}

  \item{replaceVal}{numeric vector with 'false' stutters to
  replace.}

  \item{byVal}{numeric vector with correct stutters.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with extracted result.
}
\description{
\code{calculateStutter} calculates statistics for stutters.
}
\details{
Calculates stutter ratios based on the 'reference' data set
and a defined analysis range aroung the true allele.

NB! Off-ladder alleles ('OL') is NOT included in the
analysis. NB! Labelled pull-ups or artefacts within stutter
range IS included in the analysis.

There are three levels of allowed overlap (interference). 0
= no interference (default): calculate the ratio for a
stutter only if there are no overlap between the stutter or
its allele with the analysis range of another allele. 1 =
stutter-stutter interference: calculate the ratio for a
stutter even if the stutter or its allele overlap with a
stutter within the analysis range of another allele. 2 =
stutter-allele interference: calculate the ratio for a
stutter even if the stutter and its allele overlap with the
analysis range of another allele.
}

