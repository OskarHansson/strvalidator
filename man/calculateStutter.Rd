\name{calculateStutter}
\alias{calculateStutter}
\title{Calculate stutter}
\usage{
  calculateStutter(data, ref, back = 2, forward = 1,
    interference = 0)
}
\arguments{
  \item{data}{data frame with genotype data in 'slim'
  format. Requires one or more allele columns 'Allele'.}

  \item{ref}{data frame in slim format with the known
  profiles.}

  \item{back}{integer for the maximal number of backward
  stutters (max size difference 2 = n-2 repeats).}

  \item{forward}{integer for the maximal number of forward
  stutters (max size difference 1 = n+1 repeats).}

  \item{interference}{integer specifying accepted level of
  interference between peaks.  0 = no overlap between
  stutters and alleles is allowed.  1 = stutter-stutter
  interference is allowed.  2 = stutter-allele interference
  is allowed.}
}
\value{
  data.frame with extracted result.
}
\description{
  \code{calculateStutter} calculates statistics for
  stutters.
}
\details{
  Calculates stutter ratios based on the 'reference' data
  set. NB! Be careful when there are pull-ups in stutter
  positions.
}
\examples{
print("Example will come later")
}
\keyword{internal}

