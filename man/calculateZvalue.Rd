\name{calculateZvalue}
\alias{calculateZvalue}
\title{Adds all kit information}
\usage{
  calculateZvalue(data, homoCols = 2)
}
\arguments{
  \item{data}{Data frame containing at least a column named
  'Marker'.}

  \item{homoCols}{'1' if homozygotes are given as a single
  allele name (e.g. 16) '2' if homozygotes are given as a
  double allele names (e.g. 16/16)}
}
\value{
  data.frame the original data frame containing additional
  columns.
}
\description{
  \code{calculateZvalue} Indicates heterozygosity (1) or
  homozygosity (2) for reference data.
}
\details{
  'data' contains short tandem repeat (STR) marker/locus
  names in the required column. Based on the provided kit
  name other information is attached to 'data' in new
  columns.
}

