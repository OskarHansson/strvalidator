\name{calculateBalance_gui}
\alias{calculateBalance_gui}
\title{Calculate Balance}
\usage{
calculateBalance_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{calculateBalance_gui} is a GUI wrapper for the
\code{calculateBalance} function.
}
\details{
Simplifies the use of the \code{calculateBalance} function
by providing a graphical user interface.
}

