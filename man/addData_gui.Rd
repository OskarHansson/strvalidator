\name{addData_gui}
\alias{addData_gui}
\title{Add data GUI}
\usage{
  addData_gui(env = parent.frame(), savegui = NULL,
    debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data
  frames.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\description{
  \code{addData_gui} is a GUI wrapper for \code{addData}.
}
\details{
  Add new information to a dataset from a second dataset by
  a key column.
}

