\name{calculatePeaks_gui}
\alias{calculatePeaks_gui}
\title{Calculate peaks}
\usage{
calculatePeaks_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data
  frames.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{calculatePeaks_gui} is a GUI wrapper for the
\code{\link{calculatePeaks}} function.
}
\details{
Counts the number of peaks in samples and markers with option to
 discard off-ladder peaks and to label groups according to maximum number
 of peaks.
}
\seealso{
\code{\link{calculatePeaks}}
}

