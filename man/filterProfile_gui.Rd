\name{filterProfile_gui}
\alias{filterProfile_gui}
\title{Filter Profile}
\usage{
filterProfile_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{filterProfile_gui} is a GUI wrapper for the
\code{\link{filterProfile}} function.
}
\details{
Simplifies the use of the \code{\link{filterProfile}} function
by providing a graphical user interface to it.
All data not matching the reference will be discarded. Useful for
filtering stutters and artifacts from raw typing data.
}
\seealso{
\code{\link{filterProfile}}, \code{\link{checkSubset}}
}

