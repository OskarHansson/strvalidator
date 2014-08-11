\name{calculateResultType_gui}
\alias{calculateResultType_gui}
\title{Calculate Result Type}
\usage{
calculateResultType_gui(env = parent.frame(), savegui = NULL,
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
\code{calculateResultType_gui} is a GUI wrapper for the
\code{\link{calculateResultType}} function.
}
\details{
Simplifies the use of \code{\link{calculateResultType}} by providing a
graphical user interface.
}
\seealso{
\code{\link{calculateResultType}}
}

