\name{getDb}
\alias{getDb}
\title{Get allele frequency database}
\usage{
getDb(dbNameOrIndex = NULL, debug = FALSE)
}
\arguments{
  \item{dbNameOrIndex}{string or integer specifying the
  database.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with allele frequency database information.
}
\description{
\code{getDb} gives access to allele frequency databases.
}
\details{
The function provides access to allele frequency databases
stored in the file database.txt in the package directory.
It returns the specified allele frequency database. If no
matching database or database index is found NA is
returned. If NULL a vector of available databases is
returned.
}
\examples{
# Show available allele frequency databases.
getDb()
}
\keyword{internal}

