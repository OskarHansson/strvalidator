\name{import}
\alias{import}
\title{Import GeneMapper.}
\usage{
  import(folder = TRUE, extension = "txt", suffix = NA,
    prefix = NA, resultFiles = NA, resultFolder = NA)
}
\arguments{
  \item{folder}{logical, TRUE all files in folder will be
  imported, FALSE only selected file will be imported.}

  \item{suffix}{string, only files with specified suffix
  will be imported.}

  \item{prefix}{string, only files with specified prefix
  will be imported.}

  \item{resultFiles}{string if file name is provided file
  will be imported without showing the file open dialogue.}

  \item{resultFolder}{string if fodler name is provided
  files in folder will be imported without showing the
  select folder dialogue.}

  \item{extension}{string providing the file extension.}
}
\value{
  data.frame with imported result.
}
\description{
  \code{import} imports text files exported from
  GeneMapper.
}
\details{
  Imports GeneMapper results exported as tab delimited text
  files.
}

