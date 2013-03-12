\name{tableStutter}
\alias{tableStutter}
\title{Table stutter}
\usage{
  tableStutter(data, per = "stutter")
}
\arguments{
  \item{data}{data frame from a stutter analysis by
  \code{calculateStutter}.}

  \item{per}{string, summarize the analysis 'global', by
  'locus', or by 'stutter'.}
}
\value{
  data.frame with summarized result.
}
\description{
  \code{tableStutter} summarizes a stutter analysis in
  table format.
}
\details{
  Summarize the stutter analysis in table format with
  different scope.
}

