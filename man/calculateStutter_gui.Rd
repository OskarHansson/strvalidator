\name{calculateStutter_gui}
\alias{calculateStutter_gui}
\title{Calculate Stutters}
\usage{
calculateStutter_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{calculateStutter_gui} is a GUI wrapper for the
\code{\link{calculateStutter}} function.
}
\details{
Simplifies the use of the \code{\link{calculateStutter}} function by providing
a graphical user interface to it.
}
\seealso{
\code{\link{calculateStutter}}, \code{\link{checkSubset}}
}

