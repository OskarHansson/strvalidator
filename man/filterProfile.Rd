\name{filterProfile}
\alias{filterProfile}
\title{Filter out profiles from DNA results}
\usage{
  filterProfile(data, ref)
}
\arguments{
  \item{data}{data frame with genotype data in 'slim'
  format.}

  \item{ref}{data frame with reference profile in 'slim'
  format.}
}
\value{
  data.frame with extracted result.
}
\description{
  \code{filterProfile} Filters out the result matching a
  specified known profiles from typing data containing
  'noise' such as stutters.
}
\details{
  Returns data where allele names match 'ref' allele names.
  Required columns are: 'Sample.Name', 'Marker', and
  'Allele'.
}
\examples{
# Format data frame.
ref1 <- slim(data=ref1, fix=c("Sample.Name","Marker"), stack=c("Allele"))

# Remove all except positive control samples.
set1 <- trim(data=set1, samples="PC")

# Format data frame.
set1 <- slim(data=set1, fix=c("Sample.Name","Marker"), stack=c("Allele","Height"))
print(head(set1))

# Filter out data matching alleles in reference.
set1 <- filterProfile(data=set1, ref=ref1)

print(head(set1))
}
\keyword{internal}

