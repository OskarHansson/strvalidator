\name{filterProfile_gui}
\alias{filterProfile_gui}
\title{Filter Profile GUI}
\usage{
filterProfile_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{filterProfile_gui} is a GUI simplifying the filtering
of typing data.
}
\details{
All data not matching the reference will be discarded.
Useful for filtering stutters and artifacts from raw typing
data.
}

