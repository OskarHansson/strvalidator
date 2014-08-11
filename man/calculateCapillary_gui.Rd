\name{calculateCapillary_gui}
\alias{calculateCapillary_gui}
\title{Capillary balance}
\usage{
calculateCapillary_gui(env = parent.frame(), savegui = NULL,
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
\code{calculateCapillary_gui} is a GUI wrapper for the
\code{\link{calculateCapillary}} function.
}
\details{
Simplifies the use of the \code{\link{calculateCapillary}} function by providing
a graphical user interface.
}
\seealso{
\code{\link{calculateCapillary}}
}

