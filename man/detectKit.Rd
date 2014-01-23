\name{detectKit}
\alias{detectKit}
\title{Detect kit}
\usage{
detectKit(data, index = TRUE, debug = FALSE)
}
\arguments{
  \item{data}{data frame.}

  \item{index}{logical, returns kit index if TRUE or short
  name if FALSE.}

  \item{debug}{logical, prints debug information if TRUE.}
}
\value{
integer or string indicating the detected kit.
}
\description{
\code{detectKit} finds the most likely STR kit for a
dataset.
}
\details{
The function looks at the markers in the dataset and
returns the most likely kit(s).
}

