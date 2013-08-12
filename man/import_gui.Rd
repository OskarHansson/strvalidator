\name{import_gui}
\alias{import_gui}
\title{Import from text file}
\usage{
  import_gui(env = parent.frame(), savegui = NULL,
    debug = FALSE)
}
\arguments{
  \item{env}{environment into which the object will be
  saved. Default is the current environment.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\description{
  \code{import_gui} is a GUI wrapper for the \code{import}
  function.
}
\details{
  Simplifies the use of the \code{import} function by
  providing a graphical user interface to it.
}

