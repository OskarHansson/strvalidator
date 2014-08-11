\name{checkSubset_gui}
\alias{checkSubset_gui}
\title{Check subsetting of a dataset}
\usage{
checkSubset_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{checkSubset_gui} is a GUI wrapper for the \code{\link{checkSubset}}
function.
}
\details{
Simplifies the use of the \code{\link{checkSubset}} function by providing
a graphical user interface to it.
}
\seealso{
\code{\link{checkSubset}}
}

