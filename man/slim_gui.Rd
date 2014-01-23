\name{slim_gui}
\alias{slim_gui}
\title{Slim data frames}
\usage{
slim_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{slim_gui} is a GUI wrapper for the \code{slim}
function.
}
\details{
Simplifies the use of the \code{slim} function by providing
a graphical user interface to it.
}

