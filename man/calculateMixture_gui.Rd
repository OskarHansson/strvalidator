\name{calculateMixture_gui}
\alias{calculateMixture_gui}
\title{Calculate Mixture}
\usage{
calculateMixture_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
TRUE
}
\description{
\code{calculateMixture_gui} is a GUI wrapper for the
\code{\link{calculateMixture}} function.
}
\details{
Simplifies the use of the \code{\link{calculateMixture}} function by
providing a graphical user interface.
}
\seealso{
\code{\link{calculateMixture}}
}

