\name{cropData_gui}
\alias{cropData_gui}
\title{Crop or replace}
\usage{
cropData_gui(env = parent.frame(), savegui = NULL, debug = FALSE)
}
\arguments{
  \item{env}{environment in wich to search for data
  frames.}

  \item{savegui}{logical indicating if GUI settings should
  be saved in the environment.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
TRUE
}
\description{
\code{cropData_gui} is a GUI simplifying cropping and replacing values in
data frames.
}
\details{
Select a data frame from the dropdown and a target column.
Select to discard or replace values and additional options.
Click button to 'Apply' changes.
Multiple actions can be performed on one dataset before saving as
a new dataframe.
NB! Check that datatype is correct before click apply to avoid strange behaviour.
}
\seealso{
\code{\link{trim_gui}}, \code{\link{editData_gui}}, \code{\link{combine_gui}}
}

