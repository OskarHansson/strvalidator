\name{calculateH_gui}
\alias{calculateH_gui}
\title{Calculate average peak height}
\usage{
calculateH_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
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
\code{calculateH_gui} is a GUI wrapper for the \code{\link{calculateH}}
function.
}
\details{
Simplifies the use of the \code{\link{calculateH}} function by providing a graphical
user interface to it.
}
\references{
Torben Tvedebrink, Poul Svante Eriksen, Helle Smidt Mogensen, Niels Morling,
 Evaluating the weight of evidence by using quantitative short tandem repeat data in DNA mixtures
 Journal of the Royal Statistical Society: Series C (Applied Statistics),
 Volume 59, Issue 5, 2010,
 Pages 855-874, 10.1111/j.1467-9876.2010.00722.x.
\url{http://dx.doi.org/10.1111/j.1467-9876.2010.00722.x}
}
\seealso{
\code{\link{calculateH}}
}

