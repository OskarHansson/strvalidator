\name{addZygosity}
\alias{addZygosity}
\title{Add zygosity}
\usage{
  addZygosity(data, ref, byName = TRUE, ignoreCase = TRUE,
    matchSource = "ref")
}
\arguments{
  \item{data}{data frame containing at least columns
  'Sample.Name', 'Marker', and 'Alleles'.}

  \item{ref}{data frame containing at least columns
  'Sample.Name', 'Marker', and 'Alleles'.}

  \item{byName}{if TRUE matching of samples and ref by
  name, if FALSE by position.}

  \item{matchSource}{if 'ref' sample names from 'ref' is
  used as source for matching. Allowes for partial matching
  of sample names (e.g. 'AB' matches '01_ABc'). NB! names
  in 'ref' must be unique to sample names refering to a
  single DNA source. If 'data' sample names from 'data' is
  used as source for matching.  Only identical sample names
  match.}

  \item{ignoreCase}{If TRUE name matching is case
  insensitive.}
}
\value{
  data.frame the original data frame containing additional
  columns.
}
\description{
  \code{addZygosity} Adds the zygosity for each marker.
}
\details{
  Compares a data set to reference profiles. Adds zygosity
  (i.e. the number of expected peaks) to a new column in
  'data'.
}

