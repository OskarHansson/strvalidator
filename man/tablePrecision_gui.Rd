\name{tablePrecision_gui}
\alias{tablePrecision_gui}
\title{Calculate Precision}
\usage{
tablePrecision_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{tablePrecision_gui} is a GUI wrapper for the \code{\link{tablePrecision}}
 function.
}
\details{
Simplifies the use of the \code{\link{tablePrecision}} function by providing
a graphical user interface.
}
\seealso{
\code{\link{tablePrecision}}, \code{\link{checkSubset}}
}

