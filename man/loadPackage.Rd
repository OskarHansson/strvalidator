\name{loadPackage}
\alias{loadPackage}
\title{Load packages}
\usage{
loadPackage(packages, halt = TRUE, silent = FALSE)
}
\arguments{
  \item{packages}{string vector of package names.}

  \item{halt}{logical if TRUE a 'stop' is issued on
  failure, else a 'warning' is given.}

  \item{silent}{logical if TRUE messages are printed.}
}
\description{
\code{loadPackages} loads the specified packages.
}
\details{
This is a helper function. Provided a string vector of
package names it tries to load the packages. If
unsuccessful it tries to install the package and then load
it. If still unsuccessful the execution is halted or a
warning is issued (option).
}
\keyword{internal}

