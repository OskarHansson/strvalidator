\name{addColor}
\alias{addColor}
\title{Add color information.}
\usage{
addColor(data, kit = NA, have = NA, need = NA, overwrite = FALSE,
  debug = FALSE)
}
\arguments{
  \item{data}{data frame or vector.}

  \item{kit}{string representing the forensic STR kit used.
  Default is NA, in which case 'have' must contain a valid
  column.}

  \item{have}{string specifying color column to be matched.
  Default is NA, in which case color information is derived
  from 'kit' and added to a column named 'Color'. If 'data'
  is a vector 'have' must be a single string.}

  \item{need}{string or string vector specifying color
  columns to be added. Default is NA, in which case all
  columns will be added. If 'data' is a vector 'need' must
  be a single string.}

  \item{overwrite}{logical if TRUE and column exist it will
  be overwritten.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with additional columns for added colors, or
vector with converted values.
}
\description{
\code{addColor} add color information 'Color', 'Dye' or 'R
Color'.
}
\details{
Primers in forensic STR typing kits are labelled with a
fluorescent dye. The dyes are represented with single
letters (Dye) in exported result files or with strings
(Color) in 'panels' files. For visualisation in R these R
color names is used (R.Color). The function can add new
color schemes matched to the existing, or it can convert a
vector containing on scheme to another.
}
\examples{
# Get marker and colors for ESX17.
df <- getKit("ESX17", what="Color")
# Add dye color.
dfDye <- addColor(data=df, need="Dye")
# Add all color alternatives.
dfAll <- addColor(data=df)
# Convert a dye vector to R colors
addColor(data=c("R","G","Y","B"), have="dye", need="r.color")
}

