\name{calculateDropout_gui}
\alias{calculateDropout_gui}
\title{Calculate Dropout}
\usage{
calculateDropout_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{calculateDropout_gui} is a GUI wrapper for the
\code{\link{calculateDropout}} function.
}
\details{
Scores dropouts for a dataset.
}
\seealso{
\code{\link{calculateDropout}}, \code{\link{checkSubset}}
}

