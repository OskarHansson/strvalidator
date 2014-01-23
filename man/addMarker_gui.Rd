\name{addMarker_gui}
\alias{addMarker_gui}
\title{Adds dye information}
\usage{
addMarker_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
data.frame in slim format.
}
\description{
\code{addMarker_gui} is a GUI wrapper for the
\code{addMarker} function.
}
\details{
Simplifies the use of the \code{addMarker} function by
providing a graphical user interface to it.
}

