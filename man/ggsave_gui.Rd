\name{ggsave_gui}
\alias{ggsave_gui}
\title{Save image}
\usage{
ggsave_gui(ggplot = NULL, name = "", parent = NULL,
  env = parent.frame(), savegui = NULL, debug = FALSE)
}
\arguments{
  \item{ggplot}{plot object.}

  \item{name}{optional string providing a file name.}

  \item{parent}{object specifying the parent GUI object to
  center the message box.}

  \item{env}{environment where the objects exist. Default
  is the current environment.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{ggsave_gui} is a simple GUI wrapper for \code{\link{ggsave}}.
}
\details{
Simple GUI wrapper for ggsave.
NB! Uses a workaround bypassing the class check for saving 'complex plots':
Step 1 is performed in the \code{strvalidator} plot functions.
\url{http://stackoverflow.com/a/20433318/2173340}
Step 2 is performed in this function \code{ggsave_gui}.
\url{http://stackoverflow.com/a/18407452/2173340}
}
\seealso{
\code{\link{ggsave}}
}

