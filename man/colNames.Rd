\name{colNames}
\alias{colNames}
\title{colNames}
\usage{
colNames(df, slim = TRUE, concatenate = NULL, debug = FALSE)
}
\arguments{
  \item{df}{data.frame.}

  \item{slim}{logical, TRUE returns column names occuring
  once, FALSE returns column names occuring multiple
  times.}

  \item{concatenate}{string, if not NULL returns a single
  string with column names concatenated by the provided
  string instead of a vector.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
character, vector or string.
}
\description{
\code{colNames} is an internal helper function.
}
\details{
\code{colNames} takes a data frame as input and return
either column names occuring once or multiple times.
Matching is done by the 'base name' (the substring to the
left of the last period, if any). The return type is a
string vector by default, or a single string of colum names
separated by a string 'concatenate' (see 'collapse' in
\code{paste} for details).
}

