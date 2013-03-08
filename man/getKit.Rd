\name{getKit}
\alias{getKit}
\title{Get kit}
\usage{
  getKit(kitNameOrIndex = NULL, showMessages = FALSE)
}
\arguments{
  \item{kitNameOrIndex}{string or integer specifying the
  kit.}

  \item{showMessages}{logical, default TRUE for printing
  messages to the R promt.}
}
\value{
  list with kit information.
}
\description{
  \code{getKit} provides information about STR kits.
}
\details{
  The function returns the following information for a
  supported kit: Short kit name, Full kit name,
  Marker/locus names, Dye for each marker/locus, Start
  offset (in base pairs) for each marker, Size of repeating
  unit (in base pairs) for each marker.  If no matching kit
  or kit index is found NA is returned.  If NULL a vector
  of available kits is printed and NA returned.
}
\examples{
getKit("ESX17")
}
\keyword{internal}

