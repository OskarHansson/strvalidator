% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auditTrail.R
\name{auditTrail}
\alias{auditTrail}
\title{Log Audit Trail.}
\usage{
auditTrail(
  obj,
  f.call = NULL,
  key = NULL,
  value = NULL,
  label = NULL,
  arguments = TRUE,
  exact = TRUE,
  remove = FALSE,
  package = NULL,
  rversion = TRUE,
  timestamp = TRUE
)
}
\arguments{
\item{obj}{object to add or update the audit trail.}

\item{f.call}{the function call i.e. \code{match.call()}.}

\item{key}{list or vector of additional keys to log.}

\item{value}{list or vector of additional values to log.}

\item{label}{optional label used if \code{f.call=NULL}.}

\item{arguments}{logical. \code{TRUE} log function arguments.}

\item{exact}{logical for exact matching of attribute name.}

\item{remove}{logical. If \code{TRUE} the 'audit trail' attribute is removed.}

\item{package}{character to log the package version.}

\item{rversion}{logical to log the R version.}

\item{timestamp}{logical to add or update timestamp.}
}
\value{
object with added or updated attribute 'audit trail'.
}
\description{
Adds an audit trail to a dataset.
}
\details{
Automatically add or updates an attribute 'audit trail' with arguments
and parameters extracted from the function call. To list the arguments
with the default set but not overridden \code{arguments=TRUE} must be set
(default). Additional custom key-value pairs can be added. The \code{label}
is extracted from the function name from \code{f.call}. Specify \code{package}
to include the version number of a package.
}
\examples{
# A simple function with audit trail logging.
myFunction <- function(x, a, b = 5) {
  x <- x + a + b
  x <- auditTrail(obj = x, f.call = match.call(), package = "strvalidator")
  return(x)
}
# Run the function.
myData <- myFunction(x = 10, a = 2)
# Check the audit trail.
cat(attr(myData, "audit trail"))

# Remove the audit trail.
myData <- auditTrail(myData, remove = TRUE)
# Confirm that the audit trail is removed.
cat(attr(myData, "audit trail"))
}
