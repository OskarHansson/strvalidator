\name{addDye_gui}
\alias{addDye_gui}
\title{Add dye information}
\usage{
addDye_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{addDye_gui} is a GUI to the \code{\link{addColor}} function.
}
\details{
Convenience GUI for the use of \code{\link{addColor}} to add 'Dye' to a dataset.
'Dye' is the one letter abbreviations for the fluorophores commonly used
to label primers in forensic STR typing kits (e.g. R=Red, B=Blue).
NB! If column 'Color' and 'Dye' is present they will be overwritten.
}
\seealso{
\code{\link{addColor}}
}

