\name{import}
\alias{import}
\title{Import GeneMapper.}
\usage{
import(folder = TRUE, extension = "txt", suffix = NA, prefix = NA,
  fileName = NA, folderName = NA, debug = FALSE)
}
\arguments{
  \item{folder}{logical, TRUE all files in folder will be
  imported, FALSE only selected file will be imported.}

  \item{suffix}{string, only files with specified suffix
  will be imported.}

  \item{prefix}{string, only files with specified prefix
  will be imported.}

  \item{fileName}{string if file name is provided file will
  be imported without showing the file open dialogue.}

  \item{folderName}{string if folder name is provided files
  in folder will be imported without showing the select
  folder dialogue.}

  \item{extension}{string providing the file extension.}

  \item{debug}{logical indicating printing debug
  information.}
}
\value{
data.frame with imported result.
}
\description{
\code{import} imports text files exported from GeneMapper.
}
\details{
Imports GeneMapper results exported as tab delimited text files.
NB! Empty strings ("") and NA strings ("NA") are converted to NA.
}

