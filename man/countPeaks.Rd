\name{countPeaks}
\alias{countPeaks}
\title{Calculate summary}
\usage{
  countPeaks(data, col = "Height", unique = TRUE)
}
\arguments{
  \item{data}{a data frame containing at least columns
  'Sample.Name', 'Marker', and \code{col}.}

  \item{col}{string specifying the column(s) to count
  values in.}

  \item{unique}{logical indicating if counting occurences
  or unique values per marker.}
}
\value{
  data.frame with columns 'Sample.Name', 'Marker', and
  'Peaks'.
}
\description{
  \code{countPeaks} Calculates the number of observed peaks
  per marker.
}
\details{
  Takes (GM-formatted) data for samples as input. Counts
  the number of peaks per marker (values in 'col' columns).
}

