% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDb.r
\name{getDb}
\alias{getDb}
\title{Get Allele Frequency Database}
\usage{
getDb(db.name.or.index = NULL, debug = FALSE)
}
\arguments{
\item{db.name.or.index}{string or integer specifying the database.
If NULL a vector of available databases is returned.}

\item{debug}{logical indicating printing debug information.}
}
\value{
data.frame with allele frequency database information.
If no matching database or database index is found NA is returned.
If the database was not found NULL is returned.
}
\description{
Gives access to allele frequency databases.
}
\details{
The function provides access to allele frequency databases stored in
the file database.txt in the package directory.
It returns the specified allele frequency database.
}
\examples{
# Show available allele frequency databases.
getDb()
}
\keyword{internal}
