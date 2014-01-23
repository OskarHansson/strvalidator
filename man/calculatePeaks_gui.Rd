\name{calculatePeaks_gui}
\alias{calculatePeaks_gui}
\title{Calculate peaks GUI}
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
\description{
\code{calculatePeaks_gui} is a GUI simplifying counting of
peaks.
}
\details{
Counts the number of peaks in samples and markers with
option to discard off-ladder peaks and to label groups
according to maximum number of peaks.
}

