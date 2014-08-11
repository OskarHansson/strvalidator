\name{guessProfile_gui}
\alias{guessProfile_gui}
\title{Guess profile}
\usage{
guessProfile_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{guessProfile_gui} is a GUI wrapper for the \code{\link{guessProfile}} function.
}
\details{
Simplifies the use of the \code{\link{guessProfile}} function by providing
a graphical user interface to it.
}
\seealso{
\code{\link{guessProfile}}, \code{\link{checkSubset}}
}

