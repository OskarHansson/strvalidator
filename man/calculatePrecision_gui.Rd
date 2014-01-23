\name{calculatePrecision_gui}
\alias{calculatePrecision_gui}
\title{Calculate Precision}
\usage{
calculatePrecision_gui(env = parent.frame(), savegui = NULL,
  debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data frames
  and save result.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame
}
\description{
\code{calculatePrecision_gui} is a GUI wrapper for the
\code{calculatePrecision} function.
}
\details{
Simplifies the use of the \code{calculatePrecision}
function by providing a graphical user interface.
}

