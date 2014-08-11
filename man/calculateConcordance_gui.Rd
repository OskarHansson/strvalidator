\name{calculateConcordance_gui}
\alias{calculateConcordance_gui}
\title{Calculate Concordance}
\usage{
calculateConcordance_gui(env = parent.frame(), savegui = NULL,
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
TRUE
}
\description{
\code{calculateConcordance_gui} is a GUI wrapper for the
\code{\link{calculateConcordance}} function.
}
\details{
Simplifies the use of the \code{\link{calculateConcordance}} function by
providing a graphical user interface.
}
\seealso{
\code{\link{calculateConcordance}}
}

