% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guessProfile.r
\name{guessProfile}
\alias{guessProfile}
\title{Guess Profile}
\usage{
guessProfile(
  data,
  ratio = 0.6,
  height = 50,
  na.rm = FALSE,
  ol.rm = TRUE,
  debug = FALSE
)
}
\arguments{
\item{data}{a data frame containing at least 'Sample.Name', 'Marker', 'Allele', Height'.}

\item{ratio}{numeric giving the peak height ratio threshold.}

\item{height}{numeric giving the minimum peak height.}

\item{na.rm}{logical indicating if rows with no peak should be discarded.}

\item{ol.rm}{logical indicating if off-ladder alleles should be discarded.}

\item{debug}{logical indicating printing debug information.}
}
\value{
data.frame 'data' with genotype rows only.
}
\description{
Guesses the correct profile based on peak height.
}
\details{
Takes typing data from single source samples and filters out the presumed
profile based on peak height and a ratio. Keeps the two highest peaks if
their ratio is above the threshold, or the single highest peak if below
the threshold.
}
\examples{
# Load an example dataset.
data(set2)
# Filter out probable profile with criteria at least 70\% Hb.
guessProfile(data = set2, ratio = 0.7)
}
