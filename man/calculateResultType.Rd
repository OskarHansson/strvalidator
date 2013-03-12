\name{calculateResultType}
\alias{calculateResultType}
\title{Calculate result type}
\usage{
  calculateResultType(data, dropoutT = NULL,
    mixtureLimits = NULL, partialLimits = NULL,
    subsetName = NA, markerSubset = NULL)
}
\arguments{
  \item{data}{a data frame containing at least the column
  'Sample.Name'.}

  \item{dropoutT}{integer indicating the dropout
  threshold.}

  \item{mixtureLimits}{integer or vector indicating
  subtypes of 'Mixture'.}

  \item{partialLimits}{integer or vector indicating
  subtypes of 'Partial'.}

  \item{subsetName}{string naming the subset of
  'Complete'.}

  \item{markerSubset}{string with marker names defining the
  subset of 'Complete'.}
}
\value{
  data.frame with columns 'Sample.Name','Type','Sub.Type'
}
\description{
  \code{calculateResultType} calculate the result type for
  samples.
}
\details{
  Calculates result types for samples in 'data'. Defined
  types are: 'No result', 'Mixture', 'Partial', and
  'Complete'. Subtypes can be defined by parameters. An
  integer passed to 'dropoutT' defines a subtype of
  'Complete' "Complete profile all peaks >dropoutT". An
  integer or vector passed to 'mixtureLimits' define
  subtypes of 'Mixture' "> [mixtureLimits] markers". An
  integer or vector passed to 'partialLimits' define
  subtypes of 'Partial' "> [partialLimits] peaks". A string
  with marker names separated by pipe (|) passed to
  'markerSubset' and a string 'subsetName' defines a
  subtype of 'Partial' "Complete [subsetName]".
}

