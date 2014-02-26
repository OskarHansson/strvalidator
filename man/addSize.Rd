\name{addSize}
\alias{addSize}
\title{Add color information.}
\usage{
addSize(data, kit = NA, debug = FALSE)
}
\arguments{
  \item{data}{data.frame with at least columns 'Marker' and
  'Allele'.}

  \item{kit}{data.frame with columns 'Marker', 'Allele',
  and 'Size'.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with additional columns for added colors, or
vector with converted values.
}
\description{
\code{addSize} add size information to alleles.
}
\details{
Adds a column 'Size' with estimated size in base pair (bp)
for alleles from kit.
}

