\name{tableStutter}
\alias{tableStutter}
\title{Table stutter}
\usage{
tableStutter(data, scope = "stutter", quant = 0.95)
}
\arguments{
  \item{data}{data frame from a stutter analysis by
  \code{calculateStutter}.}

  \item{scope}{string, summarize 'global', by 'locus', or
  by 'stutter'.}

  \item{quant}{numeric, quantile to calculate.}
}
\value{
data.frame with summarized result.
}
\description{
\code{tableStutter} summarizes a stutter analysis in table
format.
}
\details{
Summarize the stutter analysis in table format with
different scope.
}

